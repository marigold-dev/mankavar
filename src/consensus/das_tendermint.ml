open Das_helpers
open Das_network

module TCQ = TaskClockedQueue
module Addr = Crypto.Address
module Sig = Crypto.Signature
module Send = Node.Send

let nb_endorsers = 10
(* let nb_endorsers = 4 *)
let step_time_ms = 600 (* Duration of regular steps *)
let commitment_time_ms = 600
let postcommitment_time_ms = 600
let block_time_ms = step_time_ms * 3 + commitment_time_ms + postcommitment_time_ms
let max_byzantine = nb_endorsers / 3
let prevote_threshold = nb_endorsers - max_byzantine
let precommitment_threshold = nb_endorsers - max_byzantine
let commitment_threshold = nb_endorsers - max_byzantine

open Structs

module Transition = Transition
module Message = Message
module Tendermint_state = Tendermint_state

module RawMempoolNode = struct

end

module Content = struct
  open Tendermint_state

  type t = {
    network : string ;
    account : Addr.t ;
    clock : Ptime.t ;
    endorsers : Addr.public_key list ;
    height : Height.t ;
    round : Round.Index.t ;
    lock : Lock.t option ;
    commitments : CommitmentState.t ;
    blocks : Blocks.t ;
    step_start_time : Ptime.t ;
    step_state : StepState.t ;
    chain_state : Transition.State.t ;
    mempool : Mempool.t ;
  }
  [@@deriving ez]
  let pp_id : _ -> t -> unit = fun ppf t ->
    t |> account |> Addr.public_key |> Addr.pp_public_key ppf
  let set_step_state s t =
    let t =
      if StepState.compare_kind s (step_state t) <> 0
      then t |> set_step_start_time (t |> clock)
      else t
    in
    set_step_state s t

  (*
    `get_proposer` should fail when the node can't know the proposer
    of given height and round
  *)
  let get_proposer
  : endorsers -> height -> Round.Index.t -> t -> Addr.public_key
  = fun lst h r _ ->
    let l = List.length lst in
    let h' =
      h
      |> Height.to_int32
      |> fun h -> Int32.(rem h @@ of_int l)
      |> Int32.to_int
    in
    let r' = r |> Round.Index.to_int32 |> Int32.to_int in
    let i = (h' + r') mod l in
    List.nth lst i

  let get_current_proposer : t -> Addr.public_key = fun t ->
    get_proposer (t|>endorsers) (t|>height) (t|>round) t

  let empty ~network ~chain_state clock endorsers account : t =
    let height = Height.zero in
    let step_state = StepState.proposal @@ ProposalState.no_proposal in
    let commitments = CommitmentState.empty in
    let step_start_time = clock in
    let lock = Option.none in
    let round = Round.Index.zero in
    let blocks = Blocks.empty in
    let mempool = Mempool.empty in
    make
      ~account ~clock ~endorsers ~height ~step_state
      ~commitments ~blocks ~lock ~round ~step_start_time
      ~network ~chain_state ~mempool

end

module type TENDERMINT_PARAMETER = sig
  type 'a t
  val view : 'a t -> 'a
  val update : 'a -> 'a t -> 'a t
end

module RawTendermintNode(Parameter : TENDERMINT_PARAMETER) = struct
  (*
    Tendermint Node
    To check manually:
    - All gossiped messages must have been signed by an endorser
  *)
  open Tendermint_state
  module P = Parameter
  module C = Content

  type t = C.t P.t

  let (!.) = P.view
  let push t x = P.update x t
  let (|>.) t f = push t @@ f !.t
  let (|>!) t f = f !.t

  let sign t hash =
    let secret_key = !.t |> C.account |> Addr.secret_key in
    Sig.sign_hash ~hash ~secret_key 
  let check_sig_by_endorser = fun ~hash s t ->
    let pk = Sig.signer s in
    List.mem pk (!.t |> C.endorsers) &&
    Sig.check_hash ~hash ~public_key:pk ~signature:s
  
  let unlock t = t |>. C.set_lock Option.none
  let do_lock l t =
    PseudoEffect.fail_opt @@ fun { fail } ->
    (l |> Lock.content |> Prevote.content
    |> Prevote.get_block_opt
    |> Option.iter (fun bh -> 
      let h = l |> Lock.content |> Prevote.height in
      if not @@ Blocks.mem h bh (t |>! C.blocks)
      then fail () ;
    )) ;
    t |>. C.set_lock @@ Option.some l

  let propose_block : Block.t -> t -> BlockProposal.t = fun block t ->
    let height = t |>! C.height in
    let round = t |>! C.round in
    let unsigned = BlockProposal.Unsigned.make ~height ~round ~block in
    let proposer_signature =
      let hash = do_hash @@ BlockProposal.Unsigned.to_bytes @@ unsigned in
      hash |> sign t |> Sig.get
    in
    BlockProposal.make ~unsigned ~proposer_signature

  let do_prevote_current
  : t -> Prevote.content -> Message.t = fun t content ->
    let height = !.t |> C.height in
    let round = !.t |> C.round in
    let unsigned = Prevote.unsigned_make ~content ~height ~round in
    let hash = unsigned |> Prevote.unsigned_to_bytes |> do_hash in
    hash |> sign t
    |> Sig.get |> Prevote.make_tpl unsigned
    |> ConsensusMessage.prevote |> Message.consensus
  
  let do_precommit : t -> Precommitment.block_hash -> Message.t = fun t bh ->
    let unsigned
    = Precommitment.unsigned_make ~block_hash:bh ~height:(!.t |> C.height) in
    let hash = unsigned |> Precommitment.unsigned_to_bytes |> do_hash in
    hash |> sign t
    |> Sig.get |> Precommitment.make_tpl unsigned
    |> ConsensusMessage.precommitment |> Message.consensus

  let do_commit : t -> Commitment.unsigned -> Message.t = fun t unsigned ->
    let hash = unsigned |> Commitment.unsigned_to_bytes |> do_hash in
    hash |> sign t
    |> Sig.get |> Commitment.make_tpl unsigned
    |> ConsensusMessage.commitment |> Message.consensus

  type message = Message.t
  type consensus_message = ConsensusMessage.t


  (*
    This should increase as round increases to allow for bad networks. Adaptative.
    TODO: make it adaptative
    TODO: test this behavior
   *)
  let get_step_time_ms : t -> Ptime.Span.t = fun _ -> XPtime.ms_int step_time_ms

  let noop (t : t) _ = [] , t

  (* Used by the block proposer to build a block *)
  let build_block : t -> Block.t * t = fun t ->
    let height = t |>! C.height in
    let block =
      let prev_height = Height.predecessor height in
      let prev_hash =
        if Height.(equal height zero) then
          Hash'.dummy
        else
          t |>! C.blocks |> Blocks.find_final prev_height |> Option.get
      in
      let header =
        let previous_hash = prev_hash in
        let state_hash = Transition.State.do_hash (!.t |> C.chain_state) in
        let time = t |>! C.clock in
        let network_name = t |>! C.network in
        Block.Header.make
          ~height ~time ~previous_hash ~network_name ~state_hash
      in
      let operations , t =
        let ops , mempool =
          let gas_limit = Transition.Bunch.max_gas in
          Mempool.get ~gas_limit (C.mempool !.t)
        in
        let bunch = Transition.Bunch.of_list ops in
        let t = t |>. C.set_mempool mempool in
        bunch , t
      in
      let previous_commitments =
        let prevs =
          if Height.(equal height zero) then
            []
          else (
            let c =
              let block_hash = prev_hash in
              let height = prev_height in
              Commitment.unsigned_make ~block_hash ~height
            in
            !.t
            |> C.commitments |> CommitmentState.commits
            |> Commitments.find_opt c |> Option.get
          )
        in
        Block.Previous_commitments.make_tpl prevs
      in
      Block.make ~header ~operations ~previous_commitments
    in
    block , t


  let propose_start_proposer t =
    let block_proposal , t =
      match !.t |> C.lock with
      | None -> (
        let b , t = build_block t in
        let bp = propose_block b t in
        bp , t
      )
      | Some lock -> (
        match lock.Lock.content.Prevote.content with
        | Nil () -> (
          let b , t = build_block t in
          let bp = propose_block b t in
          bp , t
        )
        | Block bh -> (
          let b = Blocks.find (Lock.height lock) bh !.t.blocks in
          let bp = propose_block b t in
          bp , t
        )
      )
    in
    let t =
      t |>. C.set_step_state
      (StepState.proposal @@ ProposalState.proposed block_proposal)
    in
    [Send.broadcast @@ Message.consensus @@
      ConsensusMessage.block_proposal block_proposal] ,
    t

  let propose_start_follower t =
    let t =
      t |>. C.set_step_state
      (StepState.proposal ProposalState.no_proposal)
    in
    [] , t

  let propose_start t =
    if Addr.public_key_equal
      (C.get_current_proposer !.t) (!.t |> C.account |> Addr.public_key)
    then propose_start_proposer t
    else propose_start_follower t

  module Timeout = struct
    let proposal s t =
      match t |>! C.lock with
      | Some l -> (
        let prevote =
          Lock.content l
          |> Prevote.content
          |> do_prevote_current t |> Send.broadcast
        in
        [prevote] ,
        t |>. C.set_step_state StepState.prevote_empty    
      )
      | None -> (
        match s with
        | ProposalState.No_proposal -> (
          [Prevote.nil |> do_prevote_current t |> Send.broadcast] ,
          t |>. C.set_step_state StepState.prevote_empty
        )
        | Proposed bc -> (
          let hash = bc |> BlockProposal.block |> Block.hash in
          [hash |> Prevote.block |> do_prevote_current t |> Send.broadcast] ,
          t |>. C.set_step_state StepState.prevote_empty
        )
      )

    let prevote s t : _ * t =
      (* Format.printf "Prevote timeout@;%!" ; *)
      match PrevoteState.threshold s with
      | None -> (
        (* Format.printf "No threshold@;%!" ; *)
        [] ,
        t
        |>. C.set_step_state @@
            StepState.precommitment PrecommitmentState.empty
      )
      | Some lproof -> (
        (* Format.printf "Lock:%a@;%!" Lock.pp lproof ; *)
        let lock_content = lproof |> Lock.content |> Prevote.content in
        match lock_content with
        | Nil () -> (
          (* Format.printf "nil lock@;%!" ; *)
          [] ,
          t
          |>. C.set_step_state @@
              StepState.precommitment PrecommitmentState.empty
          |>. C.set_lock Option.none
        )
        | Block bp -> (
          (* Format.printf "block lock@;%!" ; *)
          [bp |> do_precommit t |> Send.broadcast] ,
          t
          |>. C.set_step_state @@
              StepState.precommitment PrecommitmentState.empty
          |>. C.set_lock @@ Option.some lproof
        )
      )

    let precommitment s t =
      (* Format.printf "Precommitment timeout@;%!" ; *)
      match PrecommitmentState.precommitted_block s with
      | None -> (
        t
        |>. C.map_round Round.Index.increment
        |> propose_start
      )
      | Some pc -> (
        let h = Precommitment.block_hash pc in
        let precommitment =
          Commitment.unsigned_make_tpl h (!.t |> C.height)
          |> do_commit t
          |> Send.broadcast
        in
        let t =
          t |>. C.set_step_state @@ StepState.commitment ()
        in
        [precommitment] , t
      )

    let commitment () t =
      (* Format.printf "Commitment timeout@;%!" ; *)
      t
      |>. C.map_round Round.Index.increment
      |> propose_start

    let postcommitment _pc t =
      t
      |>. C.map_height Height.increment
      |> propose_start

  end

  (* TODO: Add round drift for bad network *)
  let step_timeout_duration t =
    !.t |> C.step_state |> StepState.destruct
    ~proposal:(fun _ -> step_time_ms)
    ~prevote:(fun _ -> step_time_ms)
    ~precommitment:(fun _ -> step_time_ms)
    ~commitment:(fun _ -> commitment_time_ms)
    ~postcommitment:(fun _ -> postcommitment_time_ms)
    |> XPtime.ms_int

  let synchronize new_clock t =
    PseudoEffect.returner @@ fun { return } ->
    let t = t |>. C.set_clock new_clock in
    let time_since_step = Ptime.diff new_clock (!.t |> C.step_start_time) in
    (* Format.printf "time since step: %a@;%!" Ptime.Span.pp time_since_step ; *)
    (* Format.printf "New clock: %a@;%!" XPtime.pp_ms new_clock ;
    Format.printf "Step Start Time: %a@;%!" XPtime.pp_ms (t |> step_start_time) ;
    Format.printf "Time since step: %a@;%!" Ptime.Span.pp time_since_step ;
    Format.printf "Timeout: %a@;%!" Ptime.Span.pp (t |> step_timeout) ; *)
    (* let print_lock s t =
      Format.printf "%s Lock? %b@;%!" s (
        t |> step_state
        |> StepState.get_prevote_opt |> Option.fold ~none:false ~some:(fun x -> x
          |> PrevoteState.threshold |> Option.is_some
        )
      )
    in *)
    if Ptime.Span.(compare time_since_step (t|>step_timeout_duration) < 0) then
      return ([] , t) ;
    let open Timeout in
    t
    |>. C.set_step_start_time new_clock
    |> fun t ->
    t |> (
      t
      |>! C.step_state
      |> StepState.destruct
        ~proposal
        ~prevote
        ~precommitment
        ~commitment
        ~postcommitment
    )
    (* |> fun x -> print_lock "post" (snd x) ; x *)

  let process_block_proposal t bp =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    (* Format.printf "Receiving block proposal@;%!" ; *)
    (* Check Previous Commitments *) (
      XBool.do_if_true (not @@ Height.equal (!.t |> C.height) Height.zero)
      @@ fun () ->
      let bh =
        bp
        |> BlockProposal.block |> Block.header
        |> Block.Header.previous_hash
      in
      let uc
      = Commitment.unsigned_make_tpl bh (Height.predecessor @@ C.height !.t) in
      (* Format.printf "grep uc : %a@;%!" Commitment.pp_unsigned uc ; *)
      let cs
      = !.t |> C.commitments |> CommitmentState.find_opt uc |> Option.get in
      assert (List.length cs >= commitment_threshold)
    ) ;
    (* Format.printf "Checked@;%!" ; *)
    (* TODO: Check Operation Gas/Size limit from Block *)
    t
    |>! C.step_state
    |> StepState.get_proposal_opt |> XOption.value' noop
    |> ProposalState.destruct
    ~no_proposal:(ProposalState.proposed bp)
    ~proposed:(ProposalState.proposed)
    |> fun ps ->
      (* Format.printf "reading proposal@;%!" ; *)
      [Send.gossip @@ Message.block_proposal bp] ,
      t
      |>. C.map_blocks (Blocks.add (bp |> BlockProposal.block))
      |>. C.set_step_state @@ StepState.proposal ps

  let process_prevote t pv =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    (* Format.printf "Receiving prevote in step_state %a@;%!" StepState.pp (t |> step_state) ; *)
    let threshold = prevote_threshold in
    let t =
      t
      |>! C.step_state
      |> StepState.get_prevote_opt |> XOption.value' noop
      (* |> fun x -> Format.printf "adding prevote@;%!" ; x *)
      |> PrevoteState.add ~threshold pv
      |> StepState.prevote |> fun x -> t |>. C.set_step_state x
    in
    (* Format.printf "Lock? %b@;%!" (
      t |> step_state
      |> StepState.get_prevote_opt |> XOption.value' noop
      |> PrevoteState.threshold |> Option.is_some
    ); *)
    [Send.gossip @@ Message.prevote pv] , t
 
  let process_precommitment t pc =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in

    (* Format.printf "Processing precommitment@;%!" ; *)
    let threshold = precommitment_threshold in
    t
    |>! C.step_state
    |> StepState.get_precommitment_opt |> XOption.value' noop
    |> PrecommitmentState.add ~threshold pc
    |> StepState.precommitment |> fun x -> t |>. C.set_step_state x
    |> fun t -> [Send.gossip @@ Message.precommitment pc] , t

  let process_block t bi =
    [Send.gossip @@ Message.block bi] ,
    t |>. C.map_blocks (Blocks.add (bi |> BlockInfo.block))

  let get_committed_block_hash t =
    t
    |>! C.commitments |> CommitmentState.committed_block
  let get_block t bh =
    t
    |>! C.blocks |> Blocks.find_opt (t |>! C.height) bh

  let commit_block t =
    PseudoEffect.fail_default t @@ fun { fail } ->
    let bh = t |> get_committed_block_hash |> XOption.value' fail in
    let b = bh |> get_block t |> XOption.value' fail in
    let h = t |>! C.height in
    Format.printf "Committing block@;%!" ;
    (* Format.printf "Got committed block@;%!" ; *)
    (* Format.printf "last Number of commitments:%d@;%!" @@ CommitmentState.all_commitments_nb (t |> commitments) ; *)
    t
    |>. C.set_step_state @@ StepState.postcommitment bh
    |>. C.map_chain_state @@ Transition.do_bunch (Block.operations b)
    |>. C.map_blocks @@ Blocks.finalize h bh
    |>. C.set_lock None
    |>. C.map_mempool @@ (fun m ->
    Transition.Bunch.to_list (Block.operations b) |> List.fold_left (fun m op ->
      let oph = Mempool.Op.do_hash op in
      Mempool.remove oph m
    ) m )

  let process_commitment t c =
    let threshold = commitment_threshold in
    (* Format.printf "Processing commitment in state %a@;%!"
      StepState.pp (step_state t) ; *)
    let t = t |>. C.map_commitments (CommitmentState.add ~threshold c) in
    let t = match StepState.get_commitment_opt @@ C.step_state !.t with
    | None -> t
    | Some () -> (
      let committed_block_opt =
        t |>! C.commitments |> CommitmentState.committed_block
      in
      if committed_block_opt |> Option.is_some
      then commit_block t
      else t
    )
    in
    (* Format.printf "Commitment processed@;%!" ; *)
    [Send.gossip @@ Message.commitment c] , t

  let is_height_committed h = fun t ->
    Height.(h > C.height !.t) || (
      Height.(h = C.height !.t) &&
      (StepState.is_postcommitment @@ C.step_state !.t)
    )

  let get_height : consensus_message -> t -> Height.t = fun m _ ->
    m |> ConsensusMessage.destruct 
    ~block_proposal:BlockProposal.(fun x -> x |> height)
    ~prevote:Prevote.(fun x -> x |> unsigned |> height)
    ~precommitment:Precommitment.(fun x -> x |> unsigned |> height)
    ~commitment:Commitment.(fun x -> x |> unsigned |> height)
    ~block:BlockInfo.(fun x -> x |> block |> Block.height)

  (* Checks the signature from a message, and that it comes from an endorser *)
  let has_signature_from_endorser : consensus_message -> t -> bool = fun m t ->
    PseudoEffect.returner @@ fun { return } ->
    let nope () = return false in

    let is_endorser public_key =
      let endorsers = t |>! C.endorsers in
      if not @@ List.exists (Addr.public_key_equal public_key) endorsers
      then nope ()
    in

    m |> ConsensusMessage.destruct 
    ~block_proposal:BlockProposal.(fun x ->
      let signature = x |> proposer_signature in
      let hash = x |> to_unsigned_hash in
      let public_key = signature |> Sig.signer in

      is_endorser public_key ;
      if not @@ Sig.check_hash ~public_key ~hash ~signature
      then nope () ;
      true
    )
    ~prevote:Prevote.(fun x ->
      let signature = x |> prevoter_signature in
      let hash = x |> unsigned |> unsigned_to_hash in
      let public_key = signature |> Sig.signer in

      is_endorser public_key ;
      if not @@ Sig.check_hash ~public_key ~hash ~signature
      then nope () ;
      true
    )
    ~precommitment:Precommitment.(fun x ->
      let signature = x |> precommitter_signature in
      let hash = x |> unsigned |> unsigned_to_hash in
      let public_key = signature |> Sig.signer in

      is_endorser public_key ;
      if not @@ Sig.check_hash ~public_key ~hash ~signature
      then nope () ;
      true
    )
    ~commitment:Commitment.(fun x ->
      let signature = x |> committer_signature in
      let hash = x |> unsigned |> unsigned_to_hash in
      let public_key = signature |> Sig.signer in

      is_endorser public_key ;
      if not @@ Sig.check_hash ~public_key ~hash ~signature
      then nope () ;
      true
    )
    ~block:BlockInfo.(fun x ->
      let signature = x |> endorser_signature in
      let hash = x |> block |> block_to_hash in
      let public_key = signature |> Sig.signer in

      is_endorser public_key ;
      if not @@ Sig.check_hash ~public_key ~hash ~signature
      then nope () ;
      true
    )


  let process_consensus_message m t =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in

    (* Discard messages from different height *)
    (
      let h = get_height m t in
      if not @@ Height.equal h (t |>! C.height)
      then noop () ;
    ) ;
    (* Discard messages not coming from endorsers or with incorrect signatures *)
    (
      if not @@ has_signature_from_endorser m t
      then noop () ;
    ) ;
    m |> ConsensusMessage.destruct
    ~block_proposal:(process_block_proposal t)
    ~prevote:(process_prevote t)
    ~precommitment:(process_precommitment t)
    ~commitment:(process_commitment t)
    ~block:(process_block t)

  let process_message m t =
    match m with
    | Message.Consensus cm -> process_consensus_message cm t
    | Operation om -> (
      [Gossip m] ,
      t |>. C.map_mempool (Mempool.add om)
    )

end

module TendermintNodeWitness(P : TENDERMINT_PARAMETER) : Node.TYPE =
  RawTendermintNode(P)

module RawDeadNode = struct
  (* This node does nothing *)
  type t = unit
  type message = Message.t
  let empty = ()
  let synchronize _ () = [] , ()
  let process_message _ () = [] , ()
end

module DeadNodeWitness : Node.TYPE = RawDeadNode

module RawHookNode = struct
  type t = {
    mutable synchronize_hook : Ptime.t -> Message.t list ;
    mutable message_hook : Message.t -> Message.t list ;
  }
  [@@deriving ez]
  type message = Message.t
  let make = make_tpl
  let empty () = make (fun _ -> []) (fun _ -> [])
  let synchronize time t = (
    List.map Send.broadcast @@ t.synchronize_hook time ,
    t
  )
  let process_message msg t = (
    List.map Send.broadcast @@ t.message_hook msg ,
    t
  )
end

module HookNodeWitness : Node.TYPE = RawHookNode
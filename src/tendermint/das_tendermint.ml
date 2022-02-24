open Das_helpers
open Das_network

module TCQ = TaskClockedQueue
module Addr = Account.Address
module Sig = Account.Signature
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

module Message = Message
module Tendermint_state = Tendermint_state

module RawTendermintNode = struct
  (*
    Tendermint Node
    To check manually:
    - All gossiped messages must have been signed by an endorser
  *)

  open Tendermint_state

  type t = {
    network : string ;
    account : Addr.t ;
    clock : Ptime.t ;
    endorsers : Account.Address.public_key list ;
    height : Height.t ;
    round : Round.Index.t ;
    lock : Lock.t option ;
    commitments : CommitmentState.t ;
    blocks : Blocks.t ;
    step_start_time : Ptime.t ;
    state : StepState.t ;
  }
  [@@deriving ez]

  let pp_id : _ -> t -> unit = fun ppf t ->
    t |> account |> Addr.public_key |> Addr.pp_public_key ppf

  let set_state s t =
    t |> set_step_start_time (t |> clock) |> set_state s

  let sign t hash =
    Sig.sign_hash ~hash ~secret_key:(t |> account |> Addr.secret_key) 
  let check_sig_by_endorser = fun ~hash s t ->
    let pk = Sig.signer s in
    List.mem pk (t |> endorsers) &&
    Sig.check_hash ~hash ~public_key:pk ~signature:s
  
  let unlock t = t |> set_lock Option.none
  let do_lock l t =
    PseudoEffect.fail_opt @@ fun { fail } ->
    (l |> Lock.content |> Prevote.content
    |> Prevote.get_block_opt
    |> Option.iter (fun bh -> 
      let h = l |> Lock.content |> Prevote.height in
      if not @@ Blocks.mem h bh (t |> blocks)
      then fail () ;
    )) ;
    t |> set_lock @@ Option.some l

  let propose_block : Block.t -> t -> BlockProposal.t = fun block t ->
    let height = t |> height in
    let round = t |> round in
    let unsigned = BlockProposal.Unsigned.make ~height ~round ~block in
    let proposer_signature =
      let hash = do_hash @@ BlockProposal.Unsigned.to_bytes @@ unsigned in
      hash |> sign t |> Sig.get
    in
    BlockProposal.make ~unsigned ~proposer_signature

  (*
    `get_proposer` should fail when the node can't know the proposer
    of given height and round
  *)
  let get_proposer : endorsers -> height -> Round.Index.t -> t -> Account.Address.public_key
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

  let get_current_proposer : t -> Account.Address.public_key = fun t ->
    get_proposer (t|>endorsers) (t|>height) (t|>round) t

  let do_prevote_current : t -> Prevote.content -> Message.t = fun t content ->
    let height = t |> height in
    let round = t |> round in
    let unsigned = Prevote.unsigned_make ~content ~height ~round in
    let hash = unsigned |> Prevote.unsigned_to_bytes |> do_hash in
    hash |> sign t
    |> Sig.get |> Prevote.make_tpl unsigned |> Message.prevote
  
  let do_precommit : t -> Precommitment.block_hash -> Message.t = fun t bh ->
    let unsigned = Precommitment.unsigned_make ~block_hash:bh ~height:(t |> height) in
    let hash = unsigned |> Precommitment.unsigned_to_bytes |> do_hash in
    hash |> sign t
    |> Sig.get |> Precommitment.make_tpl unsigned |> Message.precommitment

  let do_commit : t -> Commitment.unsigned -> Message.t = fun t unsigned ->
    let hash = unsigned |> Commitment.unsigned_to_bytes |> do_hash in
    hash |> sign t
    |> Sig.get |> Commitment.make_tpl unsigned |> Message.commitment

  type message = Message.t

  let empty ~network clock endorsers account : t =
    let height = Height.zero in
    let state = StepState.proposal @@ ProposalState.no_proposal in
    let commitments = CommitmentState.empty in
    let step_start_time = clock in
    let lock = Option.none in
    let round = Round.Index.zero in
    let blocks = Blocks.empty in
    make
      ~account ~clock ~endorsers ~height ~state
      ~commitments ~blocks ~lock ~round ~step_start_time
      ~network

  (*
    This should increase as round increases to allow for bad networks. Adaptative.
    TODO: make it adaptative
    TODO: test this behavior
   *)
  let get_step_time_ms : t -> Ptime.Span.t = fun _ -> XPtime.ms_int step_time_ms

  let noop (t : t) _ = [] , t

  let dummy_state_hash = Hash'.dummy

  let dummy_block_proposal : t -> BlockProposal.t = fun t ->
    let height = t |> height in
    let block =
      let prev_height = Height.predecessor height in
      let prev_hash =
        if Height.(equal height zero) then
          Hash'.dummy
        else
          t |> blocks |> Blocks.find_final prev_height |> Option.get
      in
      let header =
        (* let state_hash = dummy_state_hash in *)
        let time = t |> clock in
        let network_name = t |> network in
        Block.Header.make ~height ~time ~previous_hash:prev_hash ~network_name
      in
      let operations = Block.Operations.make_tpl [] in
      let previous_commitments =
        let prevs =
          if Height.(equal height zero) then
            []
          else (
            let c = Commitment.unsigned_make ~block_hash:prev_hash ~height:prev_height in
            t |> commitments |> CommitmentState.commits |> Commitments.find_opt c |> Option.get
          )
        in
        Block.Previous_commitments.make_tpl prevs
      in
      Block.make ~header ~operations ~previous_commitments
    in
    let unsigned =
      let round = t |> round in
      BlockProposal.Unsigned.make ~height ~round ~block
    in
    let hash = unsigned |> BlockProposal.Unsigned.to_bytes |> do_hash in
    let signed = sign t hash in
    let proposer_signature = signed |> Sig.get in
    BlockProposal.make ~unsigned ~proposer_signature

  let propose_start t =
    if Addr.public_key_equal
      (get_current_proposer t) (t |> account |> Addr.public_key)
    then (
      let block_proposal =
        t |> lock |> Option.fold
        ~none:(dummy_block_proposal t)
        ~some:(fun l ->
          l |> Lock.content |> Prevote.content |> Prevote.content_destruct
          ~nil:(fun () -> dummy_block_proposal t)
          ~block:(fun bh ->
            let block = Blocks.find (l |> Lock.height) bh (t |> blocks) in
            propose_block block t
          )
        )
      in
      [Send.broadcast @@ Message.block_proposal block_proposal] ,
      t |> set_state
        (StepState.proposal @@ ProposalState.proposed block_proposal)
    ) else (
      [] ,
      t |> set_state
        (StepState.proposal ProposalState.no_proposal)
    )

  let propose_timeout t s =
    (* Format.printf "Propose timeout@;%!" ; *)
    t |> lock |> Option.fold
    ~some:(fun l ->
      [Lock.content l |> Prevote.content |> do_prevote_current t |> Send.broadcast] ,
      t |> set_state StepState.prevote_empty    
    )
    ~none:(
      s |> ProposalState.destruct
      ~no_proposal:(
        [Prevote.nil |> do_prevote_current t |> Send.broadcast] ,
        t |> set_state StepState.prevote_empty
      )
      ~proposed:(fun bc ->
        let hash = bc |> BlockProposal.block |> Block.hash in
        [hash |> Prevote.block |> do_prevote_current t |> Send.broadcast] ,
        t |> set_state StepState.prevote_empty
      )
    )

  let prevote_timeout t s =
    (* Format.printf "Prevote timeout@;%!" ; *)
    s |> PrevoteState.threshold |> XOption.fold'
    ~none:(fun () ->
      (* Format.printf "No threshold@;%!" ; *)
      [] ,
      t |> set_state @@ StepState.precommitment PrecommitmentState.empty
    )
    ~some:(fun lproof ->
      (* Format.printf "Lock:%a@;%!" Lock.pp lproof ; *)
      lproof |> Lock.content |> Prevote.content |> Prevote.content_destruct
      ~nil:(fun () ->
        (* Format.printf "nil lock@;%!" ; *)
        [] ,
        t
        |> set_state @@ StepState.precommitment PrecommitmentState.empty
        |> set_lock Option.none
      )
      ~block:(fun bp ->
        (* Format.printf "block lock@;%!" ; *)
        [bp |> do_precommit t |> Send.broadcast] ,
        t
        |> set_state @@ StepState.precommitment PrecommitmentState.empty
        |> set_lock @@ Option.some lproof
      )
    )

  let precommitment_timeout t s =
    (* Format.printf "Precommitment timeout@;%!" ; *)
    s |> PrecommitmentState.precommitted_block |> Option.fold
    ~none:(
      t
      |> map_round Round.Index.increment
      |> propose_start
    )
    ~some:(fun pc -> (
      let h = Precommitment.block_hash pc in
      [Commitment.unsigned_make_tpl h (t |> height) |> do_commit t |> Send.broadcast] ,
      t
      |> set_state @@ StepState.commitment ()
    ))

  let commit_timeout t () =
    (* Format.printf "Commitment timeout@;%!" ; *)
    t
    |> map_round Round.Index.increment
    |> propose_start

  let postcommitment_timeout t _pc =
  t
  |> map_height Height.increment
  |> fun t ->
  [] , t



  (* TODO: Add round drift for bad network *)
  let step_timeout t =
    t |> state |> StepState.destruct
    ~proposal:(fun _ -> step_time_ms)
    ~prevote:(fun _ -> step_time_ms)
    ~precommitment:(fun _ -> step_time_ms)
    ~commitment:(fun _ -> commitment_time_ms)
    ~postcommitment:(fun _ -> postcommitment_time_ms)
    |> XPtime.ms_int

  let synchronize new_clock t =
    PseudoEffect.returner @@ fun { return } ->
    let t = set_clock new_clock t in
    let time_since_step = Ptime.diff new_clock (t |> step_start_time) in
    (* Format.printf "New clock: %a@;%!" XPtime.pp_ms new_clock ;
    Format.printf "Step Start Time: %a@;%!" XPtime.pp_ms (t |> step_start_time) ;
    Format.printf "Time since step: %a@;%!" Ptime.Span.pp time_since_step ;
    Format.printf "Timeout: %a@;%!" Ptime.Span.pp (t |> step_timeout) ; *)
    (* let print_lock s t =
      Format.printf "%s Lock? %b@;%!" s (
        t |> state
        |> StepState.get_prevote_opt |> Option.fold ~none:false ~some:(fun x -> x
          |> PrevoteState.threshold |> Option.is_some
        )
      )
    in *)
    if Ptime.Span.(compare time_since_step (t|>step_timeout) <= 0) then
      return ([] , t) ;
    t
    |> set_step_start_time new_clock
    |> fun t -> t
    |> state
    |> StepState.destruct
    ~proposal:(propose_timeout t)
    ~prevote:(prevote_timeout t)
    ~precommitment:(precommitment_timeout t)
    ~commitment:(commit_timeout t)
    ~postcommitment:(postcommitment_timeout t)
    (* |> fun x -> print_lock "post" (snd x) ; x *)

  let process_block_proposal t bp =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    (* Format.printf "Receiving block proposal@;%!" ; *)

    (* Check Previous Commitments *) (
      XBool.do_if_true (not @@ Height.equal (t |> height) Height.zero) @@ fun () ->
      let bh = bp |> BlockProposal.block |> Block.hash in
      let uc = Commitment.unsigned_make_tpl bh (Height.predecessor @@ height t) in
      let cs = t |> commitments |> CommitmentState.find_opt uc |> Option.get in
      assert (List.length cs >= commitment_threshold)
    ) ;

    t
    |> state
    |> StepState.get_proposal_opt |> XOption.value' noop
    |> ProposalState.destruct
    ~no_proposal:(ProposalState.proposed bp)
    ~proposed:(ProposalState.proposed)
    |> fun ps ->
      [Send.gossip @@ Message.block_proposal bp] ,
      t
      |> map_blocks (Blocks.add (bp |> BlockProposal.block))
      |> set_state @@ StepState.proposal ps

  let process_prevote t pv =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    (* Format.printf "Receiving prevote in state %a@;%!" StepState.pp (t |> state) ; *)

    let threshold = prevote_threshold in
    t |> state
    |> StepState.get_prevote_opt |> XOption.value' noop
    (* |> fun x -> Format.printf "adding prevote@;%!" ; x *)
    |> PrevoteState.add ~threshold pv
    |> StepState.prevote |> fun x -> set_state x t
    |> fun t ->
    (* Format.printf "Lock? %b@;%!" (
      t |> state
      |> StepState.get_prevote_opt |> XOption.value' noop
      |> PrevoteState.threshold |> Option.is_some
    ); *)
    [Send.gossip @@ Message.prevote pv] , t
 
  let process_precommitment t pc =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in

    (* Format.printf "Processing precommitment@;%!" ; *)
    let threshold = precommitment_threshold in
    t |> state
    |> StepState.get_precommitment_opt |> XOption.value' noop
    |> PrecommitmentState.add ~threshold pc
    |> StepState.precommitment |> fun x -> set_state x t
    |> fun t -> [Send.gossip @@ Message.precommitment pc] , t

  let process_block t bi =
    [Send.gossip @@ Message.block bi] ,
    t |> map_blocks (Blocks.add (bi |> BlockInfo.block))

  let process_commitment t c =
    let threshold = commitment_threshold in
    PseudoEffect.returner @@ fun { return } ->
    let default t _ = return ([Send.gossip @@ Message.commitment c] , t) in

    (* Format.printf "Processing commitment@;%!" ; *)

    let get_committed_block_hash t =
      t |> commitments |> CommitmentState.committed_block |> XOption.fold'' (default t) Fun.id
    in
    let get_block t bh =
      let default x =
        (* Format.printf "Missing committed block@;%!" ; *)
        default t x
      in
      t |> blocks |> Blocks.find_opt (t |> height) bh |> XOption.fold'' default Fun.id
    in
    t
    |> map_commitments (CommitmentState.add ~threshold c)
    |> fun t ->
    let commitment () =
      (* Format.printf "Processing commitment in state@;%!" ; *)
      (* Format.printf "Number of commitments:%d@;%!" @@ CommitmentState.all_commitments_nb (t |> commitments) ; *)
      t
      |> get_committed_block_hash
      |> fun bh ->
      (* Format.printf "Got committed block@;%!" ; *)
      bh
      |> get_block t
      |> fun _ ->
      (* Format.printf "last Number of commitments:%d@;%!" @@ CommitmentState.all_commitments_nb (t |> commitments) ; *)
      [ Send.gossip @@ Message.commitment c ] ,
      t |> set_state @@ StepState.postcommitment bh
    in
    let default x = default t x in
    t |> state |> StepState.destruct
    ~proposal:default ~prevote:default ~precommitment:default
    ~commitment ~postcommitment:default

  let get_height : message -> t -> Height.t = fun m _ ->
    m |> Message.destruct 
    ~block_proposal:BlockProposal.(fun x -> x |> height)
    ~prevote:Prevote.(fun x -> x |> unsigned |> height)
    ~precommitment:Precommitment.(fun x -> x |> unsigned |> height)
    ~commitment:Commitment.(fun x -> x |> unsigned |> height)
    ~block:BlockInfo.(fun x -> x |> block |> Block.height)

  (* Checks the signature from a message, and that it comes from an endorser *)
  let has_signature_from_endorser : message -> t -> bool = fun m t ->
    PseudoEffect.returner @@ fun { return } ->
    let nope () = return false in

    let is_endorser public_key =
      if not @@ List.exists (Addr.public_key_equal public_key) (t |> endorsers)
      then nope ()
    in

    m |> Message.destruct 
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


  let process_message m t =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in

    (* Discard messages from different height *)
    (
      let h = get_height m t in
      if not @@ Height.equal h (t |> height)
      then noop () ;
    ) ;
    (* Discard messages not coming from endorsers or with incorrect signatures *)
    (
      if not @@ has_signature_from_endorser m t
      then noop () ;
    ) ;
    m |> Message.destruct
    ~block_proposal:(process_block_proposal t)
    ~prevote:(process_prevote t)
    ~precommitment:(process_precommitment t)
    ~commitment:(process_commitment t)
    ~block:(process_block t)
end

module TendermintNodeWitness : Node.TYPE = RawTendermintNode

module RawDeadNode = struct
  (* This node does nothing *)
  type t = unit
  type message = Message.t
  let empty = ()
  let synchronize _ () = [] , ()
  let process_message _ () = [] , ()
end

module DeadNodeWitness : Node.TYPE = RawDeadNode

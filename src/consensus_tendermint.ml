[@@@warning "-23-34"]

(* TODO:
  - Only sign hashes
*)

let hash : bytes -> bytes = fun _ -> assert false

open Das_helpers

module TCQ = TaskClockedQueue
module Addr = Account.Address
module Sig = Account.Signature
module Send = Node.Send

let nb_endorsers = 10

let step_time_ms = 1_000
let commitment_time_ms = 1_000
let block_time_ms = step_time_ms * 3 + commitment_time_ms

module Operation = struct
  type t = unit
  let encoding : t Encoding.t = Encoding.unit
end

module Round = struct
  module Index = Index.Make()
  module Step = struct
    type t =
    | Propose
    | Prevote
    | Precommitment
    [@@deriving ez]
  end
  type t = {
    step : Step.t ;
    duration : Ptime.Span.t ;
  }
  [@@deriving ez]
end

module State = struct
  module Hash = struct
    type t = unit
  end
end

module Block = struct
  module Header = struct
    type t = {
      network_name : string ;
      height : Height.t ;
      time : Ptime.t ;
      previous_hash : Hash.t ;
      state_hash : Hash.t ;
    }
    [@@deriving ez]
    let encoding = Encoding.(
      conv (fun (x1, x2, x3, x4, x5) -> make_tpl x1 x2 x3 x4 x5) destruct @@
      tuple_5 string Height.encoding XPtime.encoding Hash.encoding Hash.encoding
    )
  end
  module Operations = struct
    type t = {
      (* slots_headers : Slot.Header.t Slot.Map.t ; *)
      operations : Operation.t list ;
    }
    [@@deriving ez]
    let encoding : t Encoding.t = Encoding.(
      conv make_tpl destruct @@ list Operation.encoding
    )
  end
  module Previous_validation = struct
    (*
      The paper refers to "previous validations". But never explained anywhere.
      Only congruent meaning is "commitments".
    *)
    type t = {
      signatures : unit ; (* TODO: what is signed here? *)
    }
    [@@deriving ez]
    let encoding : t Encoding.t = Encoding.(
      conv make_tpl destruct unit
    )
  end
  type t = {
    header : Header.t ;
    operations : Operations.t ;
    previous_validation : Previous_validation.t ;
  }
  [@@deriving ez]
  let pp : _ -> t -> unit = fun ppf t ->
    Format.fprintf ppf "Block#%a"
      Height.pp (t |> header |> Header.height)
  let encoding = Encoding.(
    conv (fun (x,y,z) -> make_tpl x y z) destruct @@
    tuple_3 Header.encoding Operations.encoding Previous_validation.encoding
  )
  let to_bytes = Encoding.to_bytes encoding
  let hash : t -> bytes = fun t -> Encoding.to_bytes encoding t |> hash
end

module BlockProposal = struct
  type unsigned = {
    height : Height.t ;
    round : Round.Index.t ;
    block : Block.t ;
  }
  [@@deriving ez]
  let unsigned_encoding = Encoding.(
    conv (fun (x , y , z) -> unsigned_make_tpl x y z) unsigned_destruct @@
    tuple_3 Height.encoding Round.Index.encoding Block.encoding
  )
  let unsigned_to_bytes = Encoding.to_bytes unsigned_encoding

  type t = {
    unsigned : unsigned ;
    proposer_signature : unsigned Sig.t ;
  }
  [@@deriving ez]

  let pp ppf t =
    Format.fprintf ppf "Block <%a> signed by %a"
      Height.pp (t |> unsigned |> height)
      Addr.pp_public_key (t |> proposer_signature |> Sig.signer)
end

module Blocks = struct

  (* Map per height *)
  module HMap = Height.Map
  (* Map per block hash *)
  module BMap = XMap.Make(Hash)
  (*
    Double indexed map.
    At least one block should be added per height (else, memory leaks!)
  *)
  type t = Block.t BMap.t HMap.t

  (* Keep blocks no longer than this duration *)
  let keep_height = 10l

  let empty : t = HMap.empty

  let find_opt : Height.t -> Hash.t -> t -> Block.t option = fun h bh t ->
    t
    |> HMap.find_opt h
    |> fun x -> Option.bind x (BMap.find_opt bh)

  let mem h bh t = Option.is_some @@ find_opt h bh t

  (* The block must always exist (for instance, a locked block) *)
  let find : Height.t -> Hash.t -> t -> Block.t = fun h bh t ->
    t |> find_opt h bh |> Option.get

  let add : Block.t -> t -> t = fun b t ->
    let h = b |> Block.header |> Block.Header.height in
    let bh = Block.hash b in
    t
    |> HMap.remove (Height.map_int32 (fun h -> Int32.sub h keep_height) h)
    |> HMap.update h (fun bmap_opt ->
      let bmap = Option.value ~default:BMap.empty bmap_opt in
      bmap
      |> BMap.add bh b
      |> Option.some
    )
end


module Prevote = struct
  type content =
  | Nil of unit
  | Block of Hash.t
  [@@deriving ez , show { with_path = false } , ord]
  let content_encoding = Encoding.(
    union [
      case get_nil_opt nil unit ;
      case get_block_opt block Hash.encoding ;
    ]
  )
  let nil = nil ()

  type unsigned = {
    content : content ;
    height : Height.t ;
    round : Round.Index.t ;
  }
  [@@deriving ez , show { with_path = false } , ord]
  let unsigned_encoding = Encoding.(
    conv (fun (x,y,z) -> unsigned_make_tpl x y z) unsigned_destruct @@
    tuple_3 content_encoding Height.encoding Round.Index.encoding
  )
  let unsigned_to_bytes = Encoding.to_bytes unsigned_encoding

  type t = {
    unsigned : unsigned ;
    prevoter_signature : unsigned Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez , show { with_path = false }]
end

module Precommitment = struct
  (* Those are supposed to be Block Hashes, obtained from blocks *)
  type unsigned = Hash.t
  let unsigned_encoding : unsigned Encoding.t = Hash.encoding
  let unsigned_to_bytes = Encoding.to_bytes unsigned_encoding
  let unsigned_compare = Hash.compare
  type t = {
    unsigned : unsigned ;
    precommitter_signature : unsigned Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez]

  (* TODO *)
  let pp ppf _ = Format.fprintf ppf "precommit"
end

module Commitment = struct
  type unsigned = {
    block_hash : Hash.t ;
    height : Height.t ;
  }
  [@@deriving ez]
  let unsigned_encoding : unsigned Encoding.t = Encoding.(
    conv (fun (x,y) -> unsigned_make_tpl x y) unsigned_destruct @@
    tuple_2 Hash.encoding Height.encoding
  )
  let unsigned_to_bytes = Encoding.to_bytes unsigned_encoding

  type t = {
    unsigned : unsigned ;
    committer_signature : unsigned Sig.t ;
  }
  [@@deriving ez]
  (* TODO *)
  let pp ppf _ = Format.fprintf ppf "commit"
end

module BlockInfo = struct
  type t = {
    block : Block.t ;
    endorser_signature : Block.t Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez , show { with_path = false }]
  let block_to_bytes = Block.to_bytes
end

module Message = struct
  (* 
    `Block_proposal`, `Prevote` and `Precommitment` are Tendermint messages.
    `Block` is just the block, to populate the db.
  *)
  type t =
  | Block_proposal of BlockProposal.t
  | Prevote of Prevote.t
  | Precommitment of Precommitment.t
  | Commitment of Commitment.t
  | Block of BlockInfo.t
  [@@deriving ez]

  let prefix str f ppf x =
    Format.fprintf ppf "%s%a" str f x

  let pp : _ -> t -> unit = fun ppf ->
    destruct
    ~block_proposal:(prefix "Block_proposal: " (BlockProposal.pp) ppf)
    ~prevote:(prefix "Vote: " (Prevote.pp) ppf)
    ~precommitment:(prefix "Precommitment: " (Precommitment.pp) ppf)
    ~commitment:(prefix "Commitment: " (Commitment.pp) ppf)
    ~block:(prefix "Block: " (BlockInfo.pp) ppf)
end


module RawTendermintNode = struct
  (*
    Tendermint Node
  *)

  module Lock = struct
    (* All of the `prevote_signatures` should be for `content` *)
    (* The list `prevote_signatures` must have size > 2/3 of validators *)
    type t = {
      content : Prevote.unsigned ;
      prevote_signatures : Prevote.prevoter_signature list ;
    }
    [@@deriving ez]

    let height t = t |> content |> Prevote.height
  end

  module ProposalState = struct
    (* This only matters when the node has no lock *)
    type t =
    | Proposed of BlockProposal.t
    | No_proposal
    [@@deriving ez]
  end

  module PrevoteState = struct
    module CMap = XMap.Make(struct
      type t = Prevote.content
      let compare = Prevote.compare_content
    end)

    (* All signatures for a given Prevote.content should be different *)
    type t = {
      prevotes : Prevote.prevoter_signature list CMap.t ;
      threshold : Lock.t Option.t ;
    }
    [@@deriving ez]

    let empty threshold = make ~prevotes:CMap.empty ~threshold

    let add ~threshold_limit : Prevote.t -> t -> t = fun pv t ->
      let sign = pv |> Prevote.prevoter_signature in
      let key = pv |> Prevote.unsigned |> Prevote.content in
      t
      |> map_prevotes (CMap.update key (fun lst_opt ->
        let lst = Option.value lst_opt ~default:[] in
        Option.some @@
        if List.exists (Sig.equal sign) lst
        then lst
        else sign :: lst
      ))
      |> fun t ->
      let signs = CMap.find key (t |> prevotes) in
      if List.length signs > threshold_limit
      then t |> set_threshold (
        signs
        |> Lock.make_tpl (pv |> Prevote.unsigned)
        |> Option.some
      ) else t

  end

  module PrecommitmentState = struct
    module CMap = XMap.Make(struct
      type t = Precommitment.unsigned
      let compare = Precommitment.unsigned_compare
    end)

    type t = {
      precommits : Precommitment.precommitter_signature list CMap.t ;
      precommitted_block : Precommitment.unsigned Option.t ;
    }
    [@@deriving ez]

    let empty = make_tpl CMap.empty Option.none

    let add ~threshold_limit : Precommitment.t -> t -> t = fun pv t ->
      let sign = pv |> Precommitment.precommitter_signature in
      let key = pv |> Precommitment.unsigned in
      t
      |> map_precommits (CMap.update key (fun lst_opt ->
        let lst = Option.value lst_opt ~default:[] in
        Option.some @@
        if List.exists (Sig.equal sign) lst
        then lst
        else sign :: lst
      ))
      |> fun t ->
      let signs = CMap.find key (t |> precommits) in
      if List.length signs > threshold_limit
      then t |> set_precommitted_block (
        pv |> Precommitment.unsigned
        |> Option.some
      ) else t

  end

  module StepState = struct
    type t =
    | Proposal of ProposalState.t
    | Prevote of PrevoteState.t
    | Precommitment of PrecommitmentState.t
    | Commitment of unit
    [@@deriving ez]

    let prevote_empty = prevote @@ PrevoteState.empty Option.none
  end

  type t = {
    account : Addr.t ;
    clock : Ptime.t ;
    endorsers : Account.Address.public_key list ;
    height : Height.t ;
    round : Round.Index.t ;
    lock : Lock.t option ;
    commitments : Commitment.t list ;
    blocks : Blocks.t ;
    step_start_time : Ptime.t ;
    state : StepState.t ;
  }
  [@@deriving ez]

  let prevote_threshold = (nb_endorsers * 2 / 3 + 1)
  let precommit_threshold = (nb_endorsers * 2 / 3 + 1)

  let set_state s t =
    t |> set_step_start_time (t |> clock) |> set_state s

  let sign ~to_bytes t x =
    Sig.sign ~secret_key:(t |> account |> Addr.secret_key)
    ~content:x ~to_bytes
  let check_sig_by_endorser = fun ~content ~to_bytes s t ->
    let pk = Sig.signer s in
    List.mem pk (t |> endorsers) &&
    Sig.check ~public_key:pk ~signature:s ~content ~to_bytes
  
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
    let unsigned = BlockProposal.unsigned_make ~height ~round ~block in
    let proposer_signature =
      sign ~to_bytes:BlockProposal.unsigned_to_bytes t unsigned |> Sig.get
    in
    BlockProposal.make ~unsigned ~proposer_signature

  (*
    `get_proposer` should fail when the node can't know the proposer
    of given height and round
  *)
  let get_proposer : endorsers -> height -> Round.Index.t -> t -> Account.Address.public_key
  = fun _ _ _ ->
    assert false

  let get_current_proposer : t -> Account.Address.public_key = fun t ->
    get_proposer (t|>endorsers) (t|>height) (t|>round) t

  let do_prevote_current : t -> Prevote.content -> Message.t = fun t content ->
    let height = t |> height in
    let round = t |> round in
    let unsigned = Prevote.unsigned_make ~content ~height ~round in
    unsigned |> sign ~to_bytes:Prevote.unsigned_to_bytes t
    |> Sig.get |> Prevote.make_tpl unsigned |> Message.prevote
  
  let do_precommit : t -> Precommitment.unsigned -> Message.t = fun t unsigned ->
    unsigned |> sign ~to_bytes:Precommitment.unsigned_to_bytes t
    |> Sig.get |> Precommitment.make_tpl unsigned |> Message.precommitment

  let do_commit : t -> Commitment.unsigned -> Message.t = fun t unsigned ->
    unsigned |> sign ~to_bytes:Commitment.unsigned_to_bytes t
    |> Sig.get |> Commitment.make_tpl unsigned |> Message.commitment


  type message = Message.t

  let empty clock endorsers account =
    let height = Height.zero in
    let state = StepState.proposal @@ ProposalState.no_proposal in
    let commitments = [] in
    make ~account ~clock ~endorsers ~height ~state ~commitments

  (*
    This should increase as round increases to allow for bad networks. Adaptative.
    TODO: make it adaptative
    TODO: test this behavior
   *)
  let get_step_time_ms : t -> Ptime.Span.t = fun _ -> XPtime.ms_int step_time_ms

  let noop (t : t) _ = [] , t

  let dummy_block_proposal : t -> BlockProposal.t = fun _ ->
    assert false

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
        let hash = bc |> BlockProposal.unsigned |> BlockProposal.block |> Block.hash in
        [hash |> Prevote.block |> do_prevote_current t |> Send.broadcast] ,
        t |> set_state StepState.prevote_empty
      )
    )

  let prevote_timeout t s =
    s |> PrevoteState.threshold |> Option.fold
    ~none:(
      [] ,
      t |> set_state @@ StepState.precommitment PrecommitmentState.empty
    )
    ~some:(fun lproof ->
      lproof |> Lock.content |> Prevote.content |> Prevote.content_destruct
      ~nil:(fun () ->
        [] ,
        t
        |> set_state @@ StepState.precommitment PrecommitmentState.empty
        |> set_lock Option.none
      )
      ~block:(fun bp ->
        [bp |> do_precommit t |> Send.broadcast] ,
        t
        |> set_state @@ StepState.precommitment PrecommitmentState.empty
        |> set_lock @@ Option.some lproof
      )
    )

  let precommit_timeout t s =
    s |> PrecommitmentState.precommitted_block |> Option.fold
    ~none:(
      t
      |> map_round Round.Index.increment
      |> propose_start
    )
    ~some:(fun h -> (
      [Commitment.unsigned_make_tpl h (t |> height) |> do_commit t |> Send.broadcast] ,
      t
      |> set_state @@ StepState.commitment ()
    ))

  let commit_timeout t () =
    t
    |> map_round Round.Index.increment
    |> propose_start

  (* TODO: Add round drift for bad network *)
  let step_timeout t =
    t |> state |> StepState.destruct
    ~proposal:(fun _ -> step_time_ms)
    ~prevote:(fun _ -> step_time_ms)
    ~precommitment:(fun _ -> step_time_ms)
    ~commitment:(fun _ -> commitment_time_ms)
    |> XPtime.ms_int

  let synchronize new_clock t =
    PseudoEffect.returner @@ fun { return } ->
    let t = set_clock new_clock t in
    let time_since_step = Ptime.diff new_clock (t |> step_start_time) in
    if Ptime.Span.(compare time_since_step (t|>step_timeout) >= 0) then
      return ([] , t) ;
    t
    |> state
    |> StepState.destruct
    ~proposal:(propose_timeout t)
    ~prevote:(prevote_timeout t)
    ~precommitment:(precommit_timeout t)
    ~commitment:(commit_timeout t)

  let process_block_proposal t bp =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    t |> state
    |> StepState.get_proposal_opt |> XOption.value' noop
    |> ProposalState.destruct
    ~no_proposal:(ProposalState.proposed bp)
    ~proposed:(ProposalState.proposed)
    |> fun ps ->
      [Send.gossip @@ Message.block_proposal bp] ,
      t |> set_state @@ StepState.proposal ps

  let process_prevote t pv =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    let threshold_limit = prevote_threshold in
    (* Check sig is correct *)
    let () =
      let public_key = Sig.signer (pv |> Prevote.prevoter_signature) in
      let content = pv |> Prevote.unsigned in
      let signature = pv |> Prevote.prevoter_signature in
      let to_bytes = Prevote.unsigned_to_bytes in
      if not @@ Sig.check ~public_key ~content ~signature ~to_bytes
      then noop ()
    in
    t |> state
    |> StepState.get_prevote_opt |> XOption.value' noop
    |> PrevoteState.add ~threshold_limit pv
    |> StepState.prevote |> fun x -> set_state x t
    |> fun t -> [Send.gossip @@ Message.prevote pv] , t

  let process_precommitment t pc =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    let threshold_limit = precommit_threshold in
    (* Check sig is correct *)
    let () =
      let public_key = Sig.signer (pc |> Precommitment.precommitter_signature) in
      let content = pc |> Precommitment.unsigned in
      let signature = pc |> Precommitment.precommitter_signature in
      let to_bytes = Precommitment.unsigned_to_bytes in
      if not @@ Sig.check ~public_key ~content ~signature ~to_bytes
      then noop ()
    in
    t |> state
    |> StepState.get_precommitment_opt |> XOption.value' noop
    |> PrecommitmentState.add ~threshold_limit pc
    |> StepState.precommitment |> fun x -> set_state x t
    |> fun t -> [Send.gossip @@ Message.precommitment pc] , t


  let process_block t bi =
    PseudoEffect.returner @@ fun { return } ->
    let noop x = return @@ noop t x in
    let public_key = Sig.signer (bi |> BlockInfo.endorser_signature) in
    (* Check block comes from endorser *)
    let () =
      if not @@ List.exists (Addr.public_key_equal public_key) (t |> endorsers)
      then noop ()      
    in
    (* Check sig is correct *)
    let () =
      let content = bi |> BlockInfo.block in
      let signature = bi |> BlockInfo.endorser_signature in
      let to_bytes = BlockInfo.block_to_bytes in
      if not @@ Sig.check ~public_key ~content ~signature ~to_bytes
      then noop ()
    in
    [Send.gossip @@ Message.block bi] ,
    t |> map_blocks (Blocks.add (bi |> BlockInfo.block))

  let process_commitment t c =
    assert false

  let process_message m t =
    m |> Message.destruct
    ~block_proposal:(process_block_proposal t)
    ~prevote:(process_prevote t)
    ~precommitment:(process_precommitment t)
    ~block:(process_block t)
    ~commitment:(process_commitment t)
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

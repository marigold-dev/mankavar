[@@@warning "-23-34"]

open Das_helpers

module TCQ = TaskClockedQueue
module Addr = Account.Address
module Sig = Account.Signature

let nb_endorsers = 10

let step_time_ms = 1_000
let commitment_time_ms = 1_000
let block_time_ms = step_time_ms * 3 + commitment_time_ms

module Operation = struct
  type t
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
  end
  module Operations = struct
    type t = {
      (* slots_headers : Slot.Header.t Slot.Map.t ; *)
      operations : Operation.t list ;
    }
    [@@deriving ez]
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
  end
  type t = {
    header : Header.t ;
    operations : Operations.t ;
    previous_validation : Previous_validation.t ;
  }
  [@@deriving ez]
  let hash : t -> bytes = fun _ -> assert false
end

module BlockProposal = struct
  type unsigned = {
    height : Height.t ;
    round : Round.Index.t ;
    block : Block.t ;
    (* proof_of_lock : unit ; *)
  }
  [@@deriving ez]

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


module Prevote = struct
  type content =
  | Nil
  | Block of BlockProposal.t
  [@@deriving ez , show { with_path = false }]

  type unsigned = {
    content : content ;
    height : Height.t ;
    round : Round.Index.t ;
  }
  [@@deriving ez , show { with_path = false }]

  type t = {
    unsigned : unsigned ;
    prevoter_signature : unsigned Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez , show { with_path = false }]
end

module Precommitment = struct
  (* Those are supposed to be Block Hashes, obtained from blocks *)
  type unsigned = Hash.t
  type t = {
    unsigned : unsigned ;
    precommitter_signature : unsigned Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez]

  (* TODO *)
  let pp ppf _ = Format.fprintf ppf "precommit"
end

module Commit = struct
  type unsigned = {
    block_hash : Hash.t ;
    height : Height.t ;
  }
  [@@deriving ez]

  type t = {
    unsigned : unsigned ;
    committer_signature : unsigned Sig.t ;
  }
end

module Message = struct
  type t =
  | Block_proposal of BlockProposal.t
  | Prevote of Prevote.t
  | Precommitment of Precommitment.t
  [@@deriving ez]

  let prefix str f ppf x =
    Format.fprintf ppf "%s%a" str f x

  let pp : _ -> t -> unit = fun ppf ->
    destruct
    ~block_proposal:(prefix "Block_proposal: " (BlockProposal.pp) ppf)
    ~prevote:(prefix "Vote: " (Prevote.pp) ppf)
    ~precommitment:(prefix "Precommitment: " (Precommitment.pp) ppf)
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
      prevote_signatures : Prevote.unsigned Sig.t list ;
    }
    [@@deriving ez]
  end

  module ProposalState = struct
    (* This only matters when the node has no lock *)
    type t =
    | Proposed of BlockProposal.t
    | No_proposal
    [@@deriving ez]
  end

  module PrevoteState = struct
    module HMap = XMap.Make(Hash)

    type t = {
      prevotes : Prevote.t list ;
      treshold : Lock.t Option.t ;
    }
    [@@deriving ez]



  end

  module PrecommitmentState = struct
    type t = {
      precommits : Precommitment.t list ;
      precommitted_block : Hash.t Option.t ;
    }
    [@@deriving ez]

    let empty = make_tpl [] Option.none
  end

  module StepState = struct
    type t =
    | Proposal of ProposalState.t
    | Prevote of PrevoteState.t
    | Precommitment of PrecommitmentState.t
    | Commitment of unit
    [@@deriving ez]

    let prevote_empty = prevote @@ PrevoteState.(make_tpl [] Option.none)
  end

  type t = {
    account : Addr.t ;
    clock : Ptime.t ;
    endorsers : Account.Address.public_key list ;
    height : Height.t ;
    round : Round.Index.t ;
    lock : Lock.t option ;
    commitments : Commit.t list ;
    step_start_time : Ptime.t ;
    state : StepState.t ;
  }
  [@@deriving ez]

  let set_state s t =
    t |> set_step_start_time (t |> clock) |> set_state s

  let sign t x = Sig.sign (t |> account |> Addr.private_key) x

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
    unsigned |> sign t |> Sig.get |> Prevote.make_tpl unsigned |> Message.prevote
  
  let do_precommit : t -> Precommitment.unsigned -> Message.t = fun t unsigned ->
    unsigned |> sign t |> Sig.get |> Precommitment.make_tpl unsigned |> Message.precommitment

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
          ~nil:(dummy_block_proposal t)
          ~block:(fun b -> b)
        )
      in
      [(Message.block_proposal block_proposal)] ,
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
      [Lock.content l |> Prevote.content |> do_prevote_current t] ,
      t |> set_state StepState.prevote_empty    
    )
    ~none:(
      s |> ProposalState.destruct
      ~no_proposal:(
        [Prevote.nil |> do_prevote_current t] ,
        t |> set_state StepState.prevote_empty
      )
      ~proposed:(fun bc ->
        [Prevote.block bc |> do_prevote_current t] ,
        t |> set_state StepState.prevote_empty
      )
    )

  let prevote_timeout t s =
    s |> PrevoteState.treshold |> Option.fold
    ~none:(
      [] ,
      t |> set_state @@ StepState.precommitment PrecommitmentState.empty
    )
    ~some:(fun lproof ->
      lproof |> Lock.content |> Prevote.content |> Prevote.content_destruct
      ~nil:(
        [] ,
        t
        |> set_state @@ StepState.precommitment PrecommitmentState.empty
        |> set_lock Option.none
      )
      ~block:(fun bp ->
        let b = bp |> BlockProposal.unsigned |> BlockProposal.block in
        let bh = b |> Block.hash in
        [do_precommit t bh] ,
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
      [do_precommit t h] ,
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
    Format.printf "synchronize BP@;%!" ;
    let t = set_clock new_clock t in
    let time_since_step = Ptime.diff new_clock (t |> step_start_time) in
    if Ptime.Span.(compare time_since_step (t|>step_timeout) >= 0) then
      return ([] , t) ;
    Format.printf "diff steps@;%!" ;
    t
    |> state
    |> StepState.destruct
    ~proposal:(propose_timeout t)
    ~prevote:(prevote_timeout t)
    ~precommitment:(precommit_timeout t)
    ~commitment:(commit_timeout t)

  let process_message m t =
    m |> Message.destruct
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

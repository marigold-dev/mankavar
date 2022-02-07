[@@@warning "-23-34"]

open Das_helpers

module TCQ = TaskClockedQueue
module Addr = Account.Address
module Sig = Account.Signature

let nb_endorsers = 10
let block_time_ms = 1_000

module Operation = struct
  type t
end

module Block = struct
  module Candidate = struct
    type t = {
      (* slots_headers : Slot.Header.t Slot.Map.t ; *)
      operations : Operation.t list ;
      height : Height.t ;
    }
    [@@deriving ez]
  end
  type t = {
    content : Candidate.t ;
    endorsements : Candidate.t Account.Signature.t ;
    producer_signature : Candidate.t Account.Signature.t ;
  }
  [@@deriving ez]
end

module Message = struct
  type t =
  | Consensus_dummy of Consensus_dummy.Message.t
  [@@deriving ez]

  let prefix str f ppf x =
    Format.fprintf ppf "%s%a" str f x

  let pp : _ -> t -> unit = fun ppf ->
    destruct_tpl
    (prefix "Consensus:" (Consensus_dummy.Message.pp) ppf)
end

module RawBlockProducerNode = struct
  (*
    This node is the one that produces blocks. It is honest, and only has one block per height.
  *)

  type t = {
    consensus : Consensus_dummy.RawBlockProducerNode.t ;
  }
  [@@deriving ez]

  type message = Message.t

  let empty consensus =
    make ~consensus

  let noop (t : t) _ = [] , t

  let synchronize new_clock t =
    t |> consensus
    |> Consensus_dummy.RawBlockProducerNode.synchronize new_clock
    |> fun (x , y) -> (List.map Message.consensus_dummy x , set_consensus y t)

  let process_message m t =
    m |> Message.destruct
    ~consensus_dummy:(fun m ->
      t |> consensus
      |> Consensus_dummy.RawBlockProducerNode.process_message m
      |> fun (x , y) -> (List.map Message.consensus_dummy x , set_consensus y t)
    )
end

module BlockProducerNodeWitness : Node.TYPE = RawBlockProducerNode

module RawEndorserNode = struct
  (*
    This node assumes the Node BP is honest.
    It only checks for its signature and endorses whatever comes out of it.
    As a result, it is stateless.
  *)

  type t = {
    consensus : Consensus_dummy.RawEndorserNode.t ;
  }
  [@@deriving ez]

  type message = Message.t

  let empty consensus =
    make ~consensus

  let noop (t : t) _ = [] , t

  let synchronize new_clock t =
    t |> consensus
    |> Consensus_dummy.RawEndorserNode.synchronize new_clock
    |> fun (x , y) -> (List.map Message.consensus_dummy x , set_consensus y t)

  let process_message m t =
    m |> Message.destruct
    ~consensus_dummy:(fun m ->
      t |> consensus
      |> Consensus_dummy.RawEndorserNode.process_message m
      |> fun (x , y) -> (List.map Message.consensus_dummy x , set_consensus y t)
    )
end

module EndorserNodeWitness : Node.TYPE = RawEndorserNode

module RawDeadNode = struct
  (* This node does nothing *)
  type t = unit
  type message = Message.t
  let empty = ()
  let synchronize _ () = [] , ()
  let process_message _ () = [] , ()
end

module DeadNodeWitness : Node.TYPE = RawDeadNode

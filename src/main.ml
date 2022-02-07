open Das_helpers

module TCQ = TaskClockedQueue
module Addr = Account.Address
module Sig = Account.Signature

[@@@warning "-23-34"]


(* 
  The consensus is stupid:
  - A canonical node, the Block Producer Node (Node BP), is honest
  - There are `nb_endorsers` Endorser Nodes (Nodes E)
  - Consensus:
    1. At each level, Node BP sends a Block Candidate
    2. Once they receive the Block Candidate, honest Nodes E send an endorsement
    3. Once the Block Candidate reaches 2/3 of endorsements, Node BP sends a Block
      - A Block contains the Block Candidate, the signature of BP, the endorsements of Nodes E
    4. If the Block Candidate doesn't reach 2/3 of endorsements by end of the level
      - Go back to step 1
      - Else, increase height, then go back to step 1
  - One level every 10 second
*)


(* module SlotIndex = Keys.MakeIndex() *)
(* module Operation = struct
  type t =
  | Rollup_submit
  | Rollup_refute
end *)
(* module Slot = struct
  include SlotIndex

  let slots_quantity = 1_000
  let slot_size = 1_000_000
  module Header = struct
    type t
  end
  type t
end *)

module BlockIndex = Keys.MakeIndex()


(* module State = struct
  type t = {
    producer : Account.Address.public_key ;
    validators : Account.Address.public_key ;
    height : Height.t ;
  }
  [@@deriving ez]
  let append_block : Block.t -> t -> t = fun _b t ->
    t
    |> map_height Height.increment
    |> assert false
end *)

(* module Rollup = struct

  module Operation = struct
    type t
  end

  module Aggregator = struct
    type t = {
      operations : Operation.t list ;
    }
  end

end *)





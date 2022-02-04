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

let nb_endorsers = 10
let block_time_ms = 1_000

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

module Height = struct
  module Raw = struct
    type t =
    | Height of int
    [@@deriving ez]

    let of_int = height
    let to_int = get_height_exn
    let compare : t -> t -> int = compare
    let equal a b = compare a b = 0
    let zero = height 0
    let increment t =
      t
      |> map_height (fun i -> i + 1)
  end
  include Raw

  module Map = XMap.Make(Raw)
end

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

module Message = struct
  type t =
  | Block_proposal of Block.Candidate.t Account.Signature.signed
  | Endorsement of Block.Candidate.t Account.Signature.signed
  [@@deriving ez]

  let prefix str f ppf x =
    Format.fprintf ppf "%s%a" str f x

  let pp : _ -> t -> unit = fun ppf ->
    destruct_tpl
    (prefix "Block_proposal:" (Account.Signature.pp_signed) ppf)
    (prefix "Endorsement:" (Account.Signature.pp_signed) ppf)
end

module type NODE = sig
  type t
  val synchronize : Ptime.t -> t -> (Message.t list * t)
  val process_message : Message.t -> t -> (Message.t list * t)
end

module RawBlockProducerNode = struct
  (*
    This node is the one that produces blocks. It is honest, and only has one block per height.
  *)

  module LocalState = struct
    type t =
    | Will_propose_next_level
    | Waiting_for_endorsements
    [@@deriving ez]
  end

  type t = {
    account : Addr.t ;
    clock : Ptime.t ;
    endorsers : Account.Address.public_key list ;
    height : Height.t ;
    endorsements : Block.Candidate.t Account.Signature.t list Height.Map.t ;
    state : LocalState.t ;
  }
  [@@deriving ez]

  let empty clock endorsers account =
    let height = Height.zero in
    let state = LocalState.will_propose_next_level in
    let endorsements = Height.Map.empty in
    make ~account ~clock ~endorsers ~height ~state ~endorsements

  let dummy_block_candidate height : Block.Candidate.t = 
    let operations = [] in
    Block.Candidate.make ~operations ~height
  
  let dummy_block_candidate_signed t =
    let bc = dummy_block_candidate (height t) in
    Sig.sign (t |> account |> Addr.private_key) bc

  let noop (t : t) _ = [] , t

  let synchronize new_clock t =
    PseudoEffect.returner @@ fun { return } ->
    Format.printf "synchronize BP@;%!" ;
    let old_clock = clock t in
    let t = set_clock new_clock t in
    let old_block = XPtime.truncate_ms old_clock block_time_ms in
    let new_block = XPtime.truncate_ms new_clock block_time_ms in
    Format.printf "old block: %a@;%!" Ptime.pp old_block ;
    Format.printf "new block: %a@;%!" Ptime.pp new_block ;
    Format.printf "same: %b@;%!" Ptime.(equal
      (XPtime.truncate_ms new_clock block_time_ms)
      (XPtime.truncate_ms old_clock block_time_ms)
    ) ;
    if Ptime.(equal
      (XPtime.truncate_ms new_clock block_time_ms)
      (XPtime.truncate_ms old_clock block_time_ms)
    ) then return ([] , t) ;
    Format.printf "diff blocks@;%!" ;
    t
    |> state
    |> LocalState.destruct
    ~will_propose_next_level:(
      [Message.block_proposal @@ dummy_block_candidate_signed t] ,
      set_state LocalState.waiting_for_endorsements t
    )
    ~waiting_for_endorsements:(
      [Message.block_proposal @@ dummy_block_candidate_signed t] ,
      set_state LocalState.waiting_for_endorsements t
    )          

  let endorsement bc_signed t =
    PseudoEffect.returner @@ fun { return } ->
    Format.printf "BP is dealing with endorsement @;%!" ;
    t
    |> state
    |> LocalState.destruct
    ~will_propose_next_level:(lazy (return ([] , t)))
    ~waiting_for_endorsements:(lazy ())
    |> Lazy.force |> fun () ->
    let signature = Sig.get bc_signed in
    let pub = Sig.signer signature in
    if not @@ List.exists (Addr.public_key_equal pub) @@ endorsers t
      then return ([] , t) ;
    if not @@ Sig.check pub signature
      then return ([] , t) ;
    let bc = Sig.content bc_signed in
    if not @@ Height.equal (Block.Candidate.height bc) (height t)
      then return ([] , t) ;
    let block_endorsements =
      match Height.Map.find_opt (height t) (endorsements t) with
      | None -> []
      | Some x -> x
    in
    if List.exists
      (fun x -> Addr.public_key_equal pub (Sig.signer x)) block_endorsements
      then return ([] , t) ; 
    let block_endorsements = signature :: block_endorsements in
    let t =
      t
      |> map_endorsements
        (Height.Map.add (height t) block_endorsements)
    in
    Format.printf "BP got %d endorsements@;%!"
      (List.length block_endorsements) ;
    if not @@ ((List.length block_endorsements) > (2 / 3) * nb_endorsers)
      then return ([] , t) ;
    let t =
      t
      |> set_state LocalState.will_propose_next_level
      |> map_height Height.increment
    in
    (* TODO: send bloock with endorsements *)
    ([] , t)

  let process_message m t =
    m |> Message.destruct
    ~block_proposal:(noop t)
    ~endorsement:(fun x -> endorsement x t)
end

module BlockProducerNodeWitness : NODE = RawBlockProducerNode

module RawEndorserNode = struct
  (*
    This node assumes the Node BP is honest.
    It only checks for its signature and endorses whatever comes out of it.
    As a result, it is stateless.
  *)

  type t = {
    producer : Account.Address.public_key ;
    account : Account.Address.t ;
  }
  [@@deriving ez]

  let empty = make_tpl

  let synchronize _ t = [] , t

  let process_message = fun m t ->
    m |> Message.destruct
    ~endorsement:(fun _ -> [] , t)
    ~block_proposal:(fun bc_signed ->
      let signature = Account.Signature.get bc_signed in
      let bc = Account.Signature.content bc_signed in
      if not @@ Account.Signature.check (producer t) signature
        then [] , t
      else (
        let pk = t |> account |> Account.Address.private_key in
        let signed = Account.Signature.sign pk bc in
        [Message.endorsement signed] , t
      )
    )
end

module EndorserNodeWitness : NODE = RawEndorserNode

module Node_packed = struct
  module type T = sig
    module M : NODE
    val x : M.t
  end  

  type t = (module T)
  type 'a t' = (module T with type M.t = 'a)
  type 'a node = (module NODE with type t = 'a)

  type mapper = {
    mapper : 'a . 'a node -> 'a -> 'a ;
  }
  let map : mapper -> t -> t = fun m packed ->
    let module Packed = (val packed) in
    let x' = m.mapper (module Packed.M) (Packed.x) in
    let module Packed' = struct
      module M = Packed.M
      let x = x'
    end in
    (module Packed')

  let pack (type a) (module M : NODE with type t = a) (x : a) : (module T with type M.t = a) =
    (module struct
      module M = M
      let x = x  
    end)
  
  let abstract (type a) (module M : T with type M.t = a) : t = (module M)

  let pack_abstract x y = pack x y |> abstract

  let wrap : type a . a t' -> t = fun (module M) -> (module M)
end


module Network = struct
  (*
    Bad simulated network implementation.
    - Messages take at least `increment` time to be propagated to nodes.
    - All messages are propagated to all nodes
  *)
  type t = {
    nodes : Node_packed.t list ;
    clock : Ptime.t ;
    increment : Ptime.span ;
    messages : Message.t TCQ.t ;
  }
  [@@deriving ez]

  let empty ~increment clock nodes =
    make ~clock ~nodes ~increment ~messages:(TCQ.empty clock)

  let run_for : Ptime.span -> t -> t = fun d t ->
    let ns = ref @@ nodes t in
    let cl = ref @@ clock t in
    let msgs = ref @@ messages t in
    let limit = Option.get @@ Ptime.add_span !cl d in
    let inc = increment t in
    while Ptime.compare !cl limit <= 0 do
      let todos = ref [] in
      (* First, synchronize. Then, process messages.*)
      (* 1. Synchronize *)
      let ns' = List.map (Node_packed.map ({ mapper = fun (type a) ((module Node) : a Node_packed.node) node ->
        let (todos' , node') = Node.synchronize !cl node in
        List.iter (fun todo -> 
          let ping = XPtime.span_mul_int inc 3 in
          Format.printf "Ping: %a@;%!" Ptime.Span.pp ping ;
          Format.printf "Inc: %a@;%!" Ptime.Span.pp inc ;
          msgs := TCQ.add_task (Ptime.add_span !cl ping |> Option.get) todo !msgs ;
        ) todos' ;
        node'
      })) !ns in
      ns := ns' ;
      Format.printf "Tasks after sync: %d (%d)@;%!"
        (List.length !todos)
        (TCQ.size !msgs) ;
      List.iter (Format.printf "Will deal with: %a@;" Message.pp) !todos ;
      (* 2. Process messages *)
      let (todos' , msgs') = TCQ.flush_until !cl !msgs in
      todos := (todos' |> List.map snd) @ !todos ;
      msgs := msgs' ;
      while !todos <> [] do
        let (next , todos') = List.(hd !todos , tl !todos) in
        Format.printf "Dealing with: %a@;" Message.pp next ;
        todos := todos' ;
        let ns' = List.map (Node_packed.map ({ mapper = fun (type a) (m_node : a Node_packed.node) node ->
          let module Node = (val m_node) in
          let (todos' , node') = Node.process_message next node in
          todos := todos' @ !todos ;
          (* TODO: Add to TCQ instead, with a ping delay *)
          node'
        })) !ns in
        ns := ns' ;
      done ;
      cl := Option.get @@ Ptime.add_span !cl inc ;
    done ;
    t
    |> set_nodes !ns
    |> set_clock !cl
end


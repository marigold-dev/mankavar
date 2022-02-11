[@@@warning "-23-34"]

open Das_helpers

module Send = Node.Send
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
    let encoding = Encoding.(
      conv (make_tpl []) height Height.encoding
    )
    let to_bytes = Encoding.to_bytes encoding
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
  | Block_proposal of Block.Candidate.t Account.Signature.signed
  | Endorsement of Block.Candidate.t Account.Signature.signed
  [@@deriving ez]

  let prefix str f ppf x =
    Format.fprintf ppf "%s%a" str f x

  let pp : _ -> t -> unit = fun ppf ->
    destruct_tpl
    (prefix "Block_proposal:" (Account.Signature.pp_signed (fun _ _ -> ())) ppf)
    (prefix "Endorsement:" (Account.Signature.pp_signed (fun _ _ -> ())) ppf)
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

  type message = Message.t

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
    let secret_key = t |> account |> Addr.secret_key in
    Sig.sign_raw ~secret_key ~content:bc ~to_hash:Block.Candidate.to_bytes

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
      [Send.broadcast @@ Message.block_proposal @@ dummy_block_candidate_signed t] ,
      set_state LocalState.waiting_for_endorsements t
    )
    ~waiting_for_endorsements:(
      [Send.broadcast @@ Message.block_proposal @@ dummy_block_candidate_signed t] ,
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
    let public_key = Sig.signer signature in
    let content = Sig.content bc_signed in
    if not @@ List.exists (Addr.public_key_equal public_key) @@ endorsers t
      then return ([] , t) ;
    if not @@ Sig.check_raw ~public_key ~signature ~content ~to_hash:Block.Candidate.to_bytes
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
      (fun x -> Addr.public_key_equal public_key (Sig.signer x)) block_endorsements
      then return ([] , t) ; 
    let block_endorsements = signature :: block_endorsements in
    let t =
      t
      |> map_endorsements
        (Height.Map.add (height t) block_endorsements)
    in
    Format.printf "BP got %d endorsements@;%!"
      (List.length block_endorsements) ;
    if not @@ ((List.length block_endorsements) > 2 * nb_endorsers / 3)
      then return ([] , t) ;
    let t =
      t
      |> set_state LocalState.will_propose_next_level
      |> map_height Height.increment
    in
    (* TODO: send bloock with endorsements, so that endorsers height can increase *)
    ([] , t)

  let process_message m t =
    m |> Message.destruct
    ~block_proposal:(noop t)
    ~endorsement:(fun x -> endorsement x t)
end

module BlockProducerNodeWitness : Node.TYPE = RawBlockProducerNode

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

  type message = Message.t

  let empty = make_tpl

  let synchronize _ t = [] , t

  let process_message = fun m t ->
    m |> Message.destruct
    ~endorsement:(fun _ -> [] , t)
    ~block_proposal:(fun bc_signed ->
      let signature = Account.Signature.get bc_signed in
      let bc = Account.Signature.content bc_signed in
      if not @@ Account.Signature.check_raw
        ~public_key:(producer t) ~signature ~content:bc
        ~to_hash:Block.Candidate.to_bytes
        then [] , t
      else (
        let sk = t |> account |> Account.Address.secret_key in
        let signed =
          Account.Signature.sign_raw
            ~secret_key:sk ~content:bc ~to_hash:Block.Candidate.to_bytes
        in
        [Send.broadcast @@ Message.endorsement signed] , t
      )
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

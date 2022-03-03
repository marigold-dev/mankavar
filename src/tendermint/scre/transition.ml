[@@@warning "-34"]

open Das_helpers
open Das_vm



module Account_index = struct
  module Contract_index = XInt64
  module Key_index = XInt64

  type t =
  | Key_index of Key_index.t
  | Contract_index of Contract_index.t
  [@@deriving ez , ord]

  let key_encoding = Encoding.int64
  let encoding = Encoding.(
    union [
      case get_key_index_opt key_index key_encoding ;
      case get_contract_index_opt contract_index Contract_index.encoding ;
    ]
  )
  let pp = Fun.flip @@ destruct_tpl XFormat.int64' Contract_index.pp'
end
module Destination = struct
  type t =
  | Account of Account_index.t
  | Public_key of Crypto.Address.t
end

module AMap = XMap.Make(Account_index)
module CMap = XMap.Make(Account_index.Contract_index)
module AKBMap = struct
  include Bimap.Make(Crypto.Address)(Account_index.Key_index)
  let encoding = encoding Crypto.Address.encoding Account_index.key_encoding
end

module AV = struct
  type t = Account_index.t * Value.t
  [@@deriving ord]
  let compare = compare
  let pp ppf (a , v) = Format.fprintf ppf "(%a , %a)"
    Account_index.pp a Value.pp v
  let encoding = Encoding.tuple_2 Account_index.encoding Value.encoding
end
module AVMap = XMap.Make(AV)
type account = Account_index.t

module Transfer = struct
  type t = {
    source : Account_index.t ;
    destination : Account_index.t ;
    amount : int64 ;
    payload : int64 array ;
    max_gas : int64 ;
  }
  [@@deriving ez]

  let get_max_gas = max_gas
  let encoding = Encoding.(
    conv make_tpl' destruct @@
    tuple_5
      Account_index.encoding Account_index.encoding
      int64 (array int64) int64 
  )
  let payload_pp = XFormat.array XFormat.int64
  let pp ppf t =
    Format.fprintf ppf "@[<v 2>Transfer:@;%a -> %a@;"
      Account_index.pp (source t) Account_index.pp (destination t) ;
    Format.fprintf ppf "%Ld tokens with payload %a and max gas %Ld@;@]"
      (amount t) payload_pp (payload t) (max_gas t)
  let pp' = Fun.flip pp
end

module Contract = struct
  type t = {
    program : program ;
    storage : Scre.memory ;
  }
  [@@deriving ez]

  let program_encoding = Encoding.(
    conv program_make_tpl' program_destruct @@ tuple_2
      (list rt_instruction_encoding)
      int
  )

  let encoding = Encoding.(
    conv make_tpl' destruct @@ tuple_2
      program_encoding Scre.memory_encoding
  )
end

module Origination = struct
  type t = {
    source : Account_index.t ;
    amount : int64 ;
    contract : Contract.t ;
    slow_memory : Value.t VMap.t ;
    max_gas : int64 ;
  }
  [@@deriving ez]
  let get_max_gas = max_gas
  let slow_memory_pp = XFormat.(
    conv VMap.to_list @@ list @@ tuple_2 int64 int64
  )
  let slow_memory_encoding = Encoding.(
    conv VMap.of_list VMap.to_list @@ list @@ tuple_2 int64 int64
  )
  let storage_encoding = Encoding.(
    conv VMap.of_list VMap.to_list @@ list @@ tuple_2 int64 int64
  )
  let pp ppf t =
    Format.fprintf ppf "@[<v 2>Origination:@;%Ld@;"
      (amount t) ;
    Format.fprintf ppf "Memory %a and max gas %Ld@;@]"
      slow_memory_pp (slow_memory t) (max_gas t)
  let pp' = Fun.flip pp
  let encoding = Encoding.(
    conv make_tpl' destruct @@
    tuple_5
      Account_index.encoding int64 Contract.encoding
      slow_memory_encoding int64 
  )
  let size = Encoding.size encoding
end

module Operation = struct
  type t =
  | Transfer of Transfer.t
  | Origination of Origination.t
  [@@deriving ez]

  let get_max_gas = destruct_tpl
    Transfer.get_max_gas Origination.get_max_gas
  
  let pp = Fun.flip @@ destruct_tpl
    Transfer.pp' Origination.pp'
  let encoding = Encoding.(
    union [
      case get_transfer_opt transfer Transfer.encoding ;
      case get_origination_opt origination Origination.encoding ;
    ]
  )
  let do_hash' t = Crypto.blake2b @@ Encoding.to_bytes encoding t 
  let do_hash = Hash.make do_hash'
  let size = Encoding.size encoding
end

module Bunch = struct
  let max_gas = 1_000_000_000L
  let max_gas_per_op = 10_000_000L
  let max_size_per_op = 10_000 (* max size in int64 *)
  type t = Operation.t list
  let make lst : t =
    let assert_max_gas_per_op op =
      assert (Int64.compare (Operation.get_max_gas op) max_gas_per_op <= 0)
    in
    lst |> List.iter assert_max_gas_per_op ;
    let assert_max_payload_per_op op =
      assert (Operation.size op <= max_size_per_op)
    in
    lst |> List.iter assert_max_payload_per_op ;
    let gas_sum =
      lst |> List.map Operation.get_max_gas
      |> List.fold_left Int64.add 0L
    in
    assert (Int64.compare gas_sum max_gas <= 0) ;
    lst
  let dummy = make []
  let to_list (x : t) = x
  let encoding : t Encoding.t = Encoding.(list Operation.encoding)
end



module State = struct
  type t = {
    mutable next_key_index : int64 ;
    mutable next_contract_index : int64 ;
    mutable ledger : int64 AMap.t ;
    mutable contracts : Contract.t CMap.t ;
    mutable slow_memory : Value.t AVMap.t ;
    mutable keys : AKBMap.t ;
  }
  [@@deriving ez]

  let clone t = make_tpl' @@ destruct t

  let mk_empty () =
    make_tpl 0L 0L AMap.empty CMap.empty AVMap.empty AKBMap.empty

  module Test = struct
    let create_account_key pk amount t =
      let key_index = t.next_key_index in
      let account_index = Account_index.key_index key_index in
      t.next_key_index <- Int64.succ t.next_key_index ;
      t.keys <- AKBMap.forth_add pk key_index t.keys ;
      t.ledger <- AMap.add account_index amount t.ledger ;
      account_index
  end

  let encoding = Encoding.(
    conv make_tpl' destruct @@ tuple_6
    int64 int64
    (
      conv AMap.of_list AMap.to_list
        (list @@ tuple_2 Account_index.encoding int64)
    ) (
      conv CMap.of_list CMap.to_list
        (list @@ tuple_2
          Account_index.Contract_index.encoding Contract.encoding)
    ) (
      conv AVMap.of_list AVMap.to_list
        (list @@ tuple_2 AV.encoding int64)
    )
    AKBMap.encoding
  )
  let do_hash = Hash.make (fun x ->
    Encoding.to_bytes encoding x |> Crypto.blake2b
  )
end

(* Take care of gas and bytes *)
type do_operation_result = {
  state : State.t ;
  gas : int64 ;
  bytes : int64 ;
}
[@@deriving ez]

let do_transfer op state : do_operation_result =
  PseudoEffect.returner @@ fun { return } ->
  let noop () = return @@ do_operation_result_make_tpl state 0L 0L in
  let src , dst , amount , payload , _max_gas = Transfer.destruct op in
  let update_ledger l =
    let src_balance = XOption.value' noop @@ AMap.find_opt src l in
    let dst_balance = XOption.value' noop @@ AMap.find_opt dst l in
    if XInt64.(src_balance < amount) then noop () ;
    let src_balance' = Int64.sub src_balance amount in
    let dst_balance' = Int64.add dst_balance amount in
    l
    |> AMap.add src src_balance'
    |> AMap.add dst dst_balance'
  in
  state.ledger <- update_ledger state.State.ledger ;
  match dst with
  | Contract_index dst_index -> (
    let c = CMap.find dst_index state.State.contracts in
    let module Cache = Scre.Cache(struct
      let read_slow k =
        AVMap.find (dst , k) state.slow_memory    
    end)() in
    let module Run = Scre.Make(Cache.Rw_slow) in
    let input = op.payload in
    let storage = Contract.storage c in
    let (storage' : Contract.storage) =
      Run.eval (Contract.program c) ~input ~storage
    in
    let c = c |> Contract.set_storage storage' in
    (!Cache.content) |> VMap.to_list |> List.iter (fun (k , (v , to_write)) ->
      if to_write then
        state.slow_memory <- AVMap.add (dst , k) v state.slow_memory
    ) ;
    state.contracts <- CMap.add dst_index c state.contracts ;
    do_operation_result_make_tpl state 0L 0L
  )
  | Key_index _ ->
    assert (Array.length payload = 0) ;
    do_operation_result_make_tpl state 0L 0L

let do_origination op state : do_operation_result =
  PseudoEffect.returner @@ fun { return } ->
  let noop () = return @@ do_operation_result_make_tpl state 0L 0L in
  let src , amount , contract , slow_memory , _max_gas =
    Origination.destruct op
  in
  let src_balance = XOption.value' noop @@ AMap.find_opt src state.ledger in
  if XInt64.(src_balance < amount) then noop () ;
  let dst_index = state.next_contract_index in
  let dst = Account_index.contract_index dst_index in
  state.next_contract_index <- Int64.succ state.next_contract_index ;
  let update_ledger l =
    let src_balance' = Int64.sub src_balance amount in
    let dst_balance = amount in
    l
    |> AMap.add src src_balance'
    |> AMap.add dst dst_balance
  in
  let update_contracts cs =
    CMap.add dst_index contract cs
  in
  state.ledger <- update_ledger state.State.ledger ;
  state.contracts <- update_contracts state.contracts ;
  slow_memory |> VMap.iter (fun k v ->
    state.slow_memory <- AVMap.add (dst , k) v state.slow_memory ;
  ) ;
  return @@ do_operation_result_make_tpl state 0L 0L
  (* ignore op ; ignore state ; assert false *)

let do_operation op =
  Format.printf "DO OPERATION@;\n" ;
  Operation.destruct op ~transfer:do_transfer ~origination:do_origination

let do_bunch (bunch : Bunch.t) state =
  let aux state op =
    (do_operation op state).state
  in
  Format.printf "DO BUNCH %d@;\n" @@ List.length bunch ;
  List.fold_left aux state bunch

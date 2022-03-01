[@@@warning "-34"]

open Das_helpers
open Das_vm

type address = int64
module AMap = Map.Make(struct type t = address let compare = Int64.compare end)
module AVMap = Map.Make(struct
  type t = address * value
  let compare = compare
end)

module Transfer = struct
  type t = {
    source : address ;
    destination : address ;
    amount : int64 ;
    payload : int64 array ;
    max_gas : int64 ;
  }
  [@@deriving ez]

  let get_max_gas = max_gas
  let encoding = Encoding.(
    conv make_tpl' destruct @@
    tuple_5 int64 int64 int64 (array int64) int64 
  )
  let payload_pp = XFormat.array XFormat.int64
  let pp ppf t =
    Format.fprintf ppf "@[<v 2>Transfer:@;%Ld -> %Ld@;"
      (source t) (destination t) ;
    Format.fprintf ppf "%Ld tokens with payload %a and max gas %Ld@;@]"
      (amount t) payload_pp (payload t) (max_gas t)
  let pp' = Fun.flip pp
end

module Origination = struct
  type t = {
    amount : int64 ;
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
  let pp ppf t =
    Format.fprintf ppf "@[<v 2>Origination:@;%Ld@;"
      (amount t) ;
    Format.fprintf ppf "Memory %a and max gas %Ld@;@]"
      slow_memory_pp (slow_memory t) (max_gas t)
  let pp' = Fun.flip pp
  let encoding = Encoding.(
    conv make_tpl' destruct @@
    tuple_3 int64 slow_memory_encoding int64 
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
  let encoding : t Encoding.t = Encoding.(list Operation.encoding)
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

module State = struct

  type t = {
    mutable next_address : int64 ;
    mutable ledger : int64 AMap.t ;
    mutable contracts : Contract.t AMap.t ;
    mutable slow_memory : Value.t AVMap.t ;
  }
  [@@deriving ez]

  let empty = make_tpl 0L AMap.empty AMap.empty AVMap.empty

  let encoding = Encoding.(
    conv make_tpl' destruct @@ tuple_4
    int64
    (
      conv
        (fun x -> x |> List.to_seq |> AMap.of_seq)
        (fun x -> x |> AMap.to_seq |> List.of_seq)
        (list @@ tuple_2 int64 int64)
    ) (
      conv
        (fun x -> x |> List.to_seq |> AMap.of_seq)
        (fun x -> x |> AMap.to_seq |> List.of_seq)
        (list @@ tuple_2 int64 Contract.encoding)
    ) (
      conv
        (fun x -> x |> List.to_seq |> AVMap.of_seq)
        (fun x -> x |> AVMap.to_seq |> List.of_seq)
        (list @@ tuple_2 (tuple_2 int64 int64) int64)
    )
  )
  let do_hash = Hash.make (fun x ->
    Encoding.to_bytes encoding x |> Do_hash.blake2b
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
  let src = op |> Transfer.source in
  let dst = op |> Transfer.destination in
  let amount = op |> Transfer.amount in
  let update_ledger l =
    let src_balance = XOption.value' noop @@ AMap.find_opt src l in
    let dst_balance = Option.value ~default:0L @@ AMap.find_opt dst l in
    if XInt64.(src_balance < amount) then noop () ;
    let src_balance' = Int64.sub src_balance amount in
    let dst_balance' = Int64.add dst_balance amount in
    l
    |> AMap.add src src_balance'
    |> AMap.add dst dst_balance'
  in
  state.ledger <- update_ledger state.State.ledger ;
  match AMap.find_opt dst (state.State.contracts) with
  | Some c -> (
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
    state.contracts <- AMap.add dst c state.contracts ;
    do_operation_result_make_tpl state 0L 0L
  )
  | None ->
    assert (Array.length op.payload = 0) ;
    do_operation_result_make_tpl state 0L 0L

let do_origination op state : do_operation_result =
  (* PseudoEffect.returner @@ fun { return } ->
  let noop () = return @@ do_operation_result_make_tpl state 0L 0L in
  let addr = state.next_address in *)
  ignore op ; ignore state ; assert false

let do_operation op =
  Operation.destruct op ~transfer:do_transfer ~origination:do_origination

let do_bunch (bunch : Bunch.t) state =
  let aux state op =
    (do_operation op state).state
  in
  List.fold_left aux state bunch

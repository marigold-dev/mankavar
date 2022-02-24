[@@@warning "-34"]

open Das_helpers
open Das_vm

type address = int64
module AMap = Map.Make(struct type t = address let compare = Int64.compare end)
module AVMap = Map.Make(struct
  type t = address * value
  let compare = compare
end)

module Operation = struct
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
end

module Bunch = struct
  let max_gas = 1_000_000_000L
  let max_gas_per_op = 10_000_000L
  let max_payload_per_op = 10_000 (* max size in int64 *)
  type t = Operation.t list
  let make lst : t =
    let assert_max_gas_per_op op =
      assert (Int64.compare (Operation.max_gas op) max_gas_per_op <= 0)
    in
    lst |> List.iter assert_max_gas_per_op ;
    let assert_max_payload_per_op op =
      assert ((Array.length @@ Operation.payload op) <= max_payload_per_op)
    in
    lst |> List.iter assert_max_payload_per_op ;
    let gas_sum =
      lst |> List.map Operation.max_gas
      |> List.fold_left Int64.add 0L
    in
    assert (Int64.compare gas_sum max_gas <= 0) ;
    lst
end

module Contract = struct
  type t = {
    program : program ;
    storage : Scre.memory ;
  }
  [@@deriving ez]
end

module State = struct

  type t = {
    mutable ledger : int64 AMap.t ;
    mutable contracts : Contract.t AMap.t ;
    mutable slow_memory : Value.t AVMap.t ;
  }
  [@@deriving ez]
end

module Transition = struct
  (* Take care of gas and bytes *)
  type do_operation_result = {
    state : State.t ;
    gas : int64 ;
    bytes : int64 ;
  }
  [@@deriving ez]

  let do_operation state op : do_operation_result =
    PseudoEffect.returner @@ fun { return } ->
    let noop () = return @@ do_operation_result_make_tpl state 0L 0L in
    let src = op |> Operation.source in
    let dst = op |> Operation.destination in
    let amount = op |> Operation.amount in
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
      let module Run = Scre.Make(struct
        let read_slow k =
          AVMap.find (dst , k) state.slow_memory
        let write_slow k v =
          state.slow_memory <- AVMap.add (dst , k) v state.slow_memory
      end) in
      let input = op.payload in
      let storage = Contract.storage c in
      let (storage' : Contract.storage) =
        Run.eval (Contract.program c) ~input ~storage
      in
      let c = c |> Contract.set_storage storage' in
      state.contracts <- AMap.add dst c state.contracts ;
      do_operation_result_make_tpl state 0L 0L
    )
    | None ->
      assert (Array.length op.payload = 0) ;
      do_operation_result_make_tpl state 0L 0L

  let do_bunch state (bunch : Bunch.t) =
    let aux state op =
      (do_operation state op).state
    in
    List.fold_left aux state bunch
end

[@@@warning "-34"]

(* Take care of gas and bytes *)
type do_operation_result = {
  state : unit ;
  gas : int64 ;
  bytes : int64 ;
}
[@@deriving ez]

open Das_helpers
open Structs
module type STATE = Patricia_state.STATE

module Do_transfer = struct
  type continuation = {
    input : Transfer.payload ;
    storage : Eval.memory ;
    state : unit Das_vm.state ;
    cache : (module Eval.CACHE) ;
    dst_index : Contract_index.t ;
  }

  type continue =
  | Continuation of continuation
  | Finish

  type do_transfer =
  | Continue of continue
  | Finished of int

  let flush_contract
    (module State : STATE) (module Cache : Eval.CACHE) storage dst_index =
    let c = State.get_contract_exn dst_index in
    let c = c |> Contract.set_storage storage in
    (!Cache.content) |> Das_vm.VMap.to_list |> List.iter (fun (k , (v , to_write)) ->
      if to_write then State.write_slow dst_index k v
    ) ;
    Format.printf "Contract Storage preflush:%a@;%!" Eval.memory_pp storage ;
    State.set_contract_exn dst_index c ;
    ()

  let mk_state (module State : STATE) op =
    let src , dst , amount , input , _max_gas = Transfer.destruct op in
    let transfer () =
      match State.debit src amount with
      | Ok () -> (
        State.credit dst amount ;
      )
      | Error () -> ()
    in
    match dst with
    | Contract_index dst_index -> (
      let c = State.get_contract_exn dst_index in
      let module Cache = Eval.Cache(struct
        let read_slow k = State.read_slow dst_index k
      end)() in
      let module Run = Eval.Make(Cache.Rw_slow) in
      let input = op.payload in
      let storage = Contract.storage c in
      let state = Run.empty_state @@ Contract.program c in
        Continuation (
          { input ; storage ; state ; cache = (module Cache) ; dst_index }
        )
    )
    | Key_index _ -> (
      assert (Array.length input = 0) ;
      transfer () ;
      Finish
    )

  let step_n (module State : STATE) ~n s =
    match s with
    | Continuation { input ; storage ; state ; cache ; dst_index } -> (
      let module Cache = (val cache) in
      let module Run = Eval.Make(Cache.Rw_slow) in
      match Run.step_n ~n state ~input ~storage with
      | Finished (storage' , n') -> (
        flush_contract (module State) (module Cache) storage' dst_index ;
        Finished n'
      )
      | Pending (storage' , s') -> (
        Continue ( Continuation {
          input ;
          storage = storage' ;
          state = s' ;
          cache = (module Cache) ;
          dst_index ;
        } )
      )
    )
    | Finish -> Finished 1

  let eval (module State : STATE) op =
    let s = mk_state (module State) op in
    match s with
    | Continuation { input ; storage ; state ; cache ; dst_index } -> (
      let module Cache = (val cache) in
      let module Run = Eval.Make(Cache.Rw_slow) in
      let storage' = Run.step_until_stop state ~input ~storage in
      flush_contract (module State) (module Cache) storage' dst_index ;
      ()
    )
    | Finish -> ()
end

module Do_origination = struct
  let main (module State : STATE) op : unit =
    PseudoEffect.returner @@ fun { return } ->
    let noop () = return @@ () in
    let src , amount , contract , slow_memory , _max_gas =
      Origination.destruct op
    in
    XResult.value' noop @@
      State.debit src amount ;
    let dst_index = State.init_contract contract amount in
    slow_memory |> Das_vm.VMap.iter (fun k v ->
      State.write_slow dst_index k v
    ) ;
    ()

  let do_origination_n op (module State : STATE) ~n:_ =
    main (module State) op
end

module Do_operation = struct
  type continue =
  | Do_transfer of Do_transfer.continue
  | Do_origination of unit
  [@@deriving ez]
  type do_operation =
  | Continue of continue
  | Finished of int

  let eval (module State : STATE) op =
    (* Format.printf "DO OPERATION@;\n" ; *)
    Operation.destruct op
      ~transfer:(Do_transfer.eval (module State))
      ~origination:(Do_origination.main (module State))

  let mk_state (module State : STATE) op =
    match op with
    | Operation.Transfer tx -> Do_transfer (
      Do_transfer.mk_state (module State) tx
    )
    | Origination o -> Do_origination (
      Do_origination.main (module State) o
    )
  
  let step_n (module State : STATE) ~n s =
    match s with
    | Do_origination () -> Finished 1
    | Do_transfer tr -> (
      match Do_transfer.step_n (module State) ~n tr with
      | Do_transfer.Continue c -> Continue (Do_transfer c)
      | Finished n -> Finished n
    )
end  

let do_operation op (module State : STATE) : do_operation_result =
  Do_operation.eval (module State) op ;
  do_operation_result_make_tpl () 0L 0L

let start_operation_n (module State : STATE) op ~n =
  let s = Do_operation.mk_state (module State) op in
  Do_operation.step_n (module State) s ~n
let resume_operation_n (module State : STATE) s ~n =
  Do_operation.step_n (module State) s ~n

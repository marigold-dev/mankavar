[@@@warning "-34"]

(* Take care of gas and bytes *)
type do_operation_result = {
  state : unit ;
  gas : int64 ;
  bytes : int64 ;
}
[@@deriving ez]

open Das_helpers
open Das_vm
open Utils

module Destination = struct
  type t =
  | Account of Account_index.t
  | Public_key of Crypto.Address.t
end

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





module Do_transfer = struct
  type continuation = {
    input : Transfer.payload ;
    storage : memory ;
    state : unit Das_vm.state ;
    cache : (module CACHE) ;
    dst_index : Contract_index.t ;
  }

  type continue =
  | Continuation of continuation
  | Finish

  type do_transfer =
  | Continue of continue
  | Finished of int

  let flush_contract
    (module State : STATE) (module Cache : CACHE) storage dst_index =
    let c = State.get_contract_exn dst_index in
    let c = c |> Contract.set_storage storage in
    (!Cache.content) |> VMap.to_list |> List.iter (fun (k , (v , to_write)) ->
      if to_write then State.write_slow dst_index k v
    ) ;
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
      let module Cache = Utils.Cache(struct
        let read_slow k = State.read_slow dst_index k
      end)() in
      let module Run = Utils.Make_eval(Cache.Rw_slow) in
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
      let module Run = Utils.Make_eval(Cache.Rw_slow) in
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
      let module Run = Utils.Make_eval(Cache.Rw_slow) in
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
    let dst_index = State.init_contract contract in
    let dst =
      Account_index.contract_index dst_index
    in
    State.credit dst amount ;
    slow_memory |> VMap.iter (fun k v ->
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

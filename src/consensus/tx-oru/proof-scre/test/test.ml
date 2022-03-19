open Das_helpers
module PS = Oru_scre_raw.Patricia_state
module Scre = Oru_scre_raw.Scre
module Utils = Oru_scre_raw.Utils

let test_quick name f =
  Alcotest.test_case name `Quick f

let make init =
  let module P = struct let init = init end in
  let module S = PS.Make(P) in
  S.init () ;
  (module S : PS.MAKE_RETURN)

let mk_bootstrap () =
  let module S = (val make PS.Patricia.empty) in
  let lst =
    List.init 10 (fun i -> S.init_key @@ Int64.of_int @@ (i + 1) * 1000)
    |> List.map Utils.Account_index.key_index
  in
  (lst , (module S : PS.MAKE_RETURN))

let counter_contract =
  let open Das_vm.C_like in
  let c_program = [
    function_ "main" "_" [
      return @@ (
        Utils.Eval.C_like.write (literal 0L) @@
        add
          (Utils.Eval.C_like.read_data (literal 0L))
          (Utils.Eval.C_like.read (literal 0L))
      ) ;    
    ]
  ] in
  let program = Das_vm.Compile.compile_program c_program in
  let storage = Das_vm.VMap.of_list [(0L , 0L)] in
  Utils.Contract.make ~program ~storage

let test_simple_transfer_to_key =
  test_quick "transfer to key" @@ fun () ->
  let (lst , (module S)) = mk_bootstrap () in
  let a , b = List.nth lst 0 , List.nth lst 1 in
  assert (S.get_balance a = Some 1000L) ;
  assert (S.get_balance b = Some 2000L) ;
  let open Utils in
  let op = Operation.transfer @@
    Transfer.make_tpl b a 300L (XArray.empty ()) 100L in
  ignore @@ Scre.do_operation op (module S) ;
  assert (S.get_balance a = Some 1300L) ;
  assert (S.get_balance b = Some 1700L) ;
  ()

let test_simple_origination =
  test_quick "origination" @@ fun () ->
  let (lst , (module S)) = mk_bootstrap () in
  let a = List.nth lst 0 in
  let open Utils in
  let op = Operation.origination @@
    Origination.make_tpl a 300L counter_contract Das_vm.VMap.empty 100L in
  ignore @@ Scre.do_operation op (module S) ;
  assert (S.get_balance a = Some 700L) ;
  assert (S.get_balance
    (Account_index.contract_index @@ Contract_index.of_int 1) = Some 300L) ;
  assert (Contract.equal counter_contract @@ S.get_contract_exn 1L) ;
  ()

let test_simple_transfer_to_contract =
  test_quick "transfer to contract" @@ fun () ->
  let (lst , (module S)) = mk_bootstrap () in
  let push_op op =
    ignore @@ Scre.do_operation op (module S)
  in
  let a = List.nth lst 0 in
  let open Utils in
  push_op @@ Operation.origination @@
    Origination.make_tpl a 300L counter_contract Das_vm.VMap.empty 100L ;
  let b = Account_index.contract_index 1L in
  push_op @@ Operation.transfer @@
    Transfer.make_tpl a b 300L [|15L|] 100L ;
  let ctr = S.get_contract_exn 1L in
  assert (Utils.equal_memory ctr.storage Das_vm.VMap.(of_list [(0L ,15L)])) ;
  push_op @@ Operation.transfer @@
    Transfer.make_tpl a b 300L [|17L|] 100L ;
  let ctr = S.get_contract_exn 1L in
  assert (Utils.equal_memory ctr.storage Das_vm.VMap.(of_list [(0L ,32L)])) ;
  ()

let test_inc_transfer_to_key n =
  test_quick (Format.asprintf "transfer to key %d" n) @@ fun () ->
  let (lst , (module S)) = mk_bootstrap () in
  let a , b = List.nth lst 0 , List.nth lst 1 in
  let open Utils in
  let op = Operation.transfer @@
    Transfer.make_tpl b a 300L (XArray.empty ()) 100L in
  let open Scre.Do_operation in
  let start = mk_state (module S) op in
  let rec aux = function
  | Continue c -> aux @@ step_n (module S) ~n:1 c
  | Finished i -> i
  in
  ignore @@ aux (Continue start) ;
  assert (S.get_balance a = Some 1300L) ;
  assert (S.get_balance b = Some 1700L) ;
  ()

let test_inc_origination n =
  test_quick (Format.asprintf "origination %d" n) @@ fun () ->
  let (lst , (module S)) = mk_bootstrap () in
  let a = List.nth lst 0 in
  let open Utils in
  let op = Operation.origination @@
    Origination.make_tpl a 300L counter_contract Das_vm.VMap.empty 100L in
  let open Scre.Do_operation in
  let start = mk_state (module S) op in
  let rec aux = function
  | Continue c -> aux @@ step_n (module S) ~n:1 c
  | Finished i -> i
  in
  ignore @@ aux (Continue start) ;
  assert (S.get_balance a = Some 700L) ;
  assert (S.get_balance
    (Account_index.contract_index @@ Contract_index.of_int 1) = Some 300L) ;
  assert (Contract.equal counter_contract @@ S.get_contract_exn 1L) ;
  ()

let test_inc_transfer_to_contract n =
  test_quick (Format.asprintf "transfer to contract %d" n) @@ fun () ->
  let (lst , (module S)) = mk_bootstrap () in
  let push_op op =
    let open Scre.Do_operation in
    let start = mk_state (module S) op in
    let rec aux = function
    | Continue c -> aux @@ step_n (module S) ~n:1 c
    | Finished i -> i
    in
    ignore @@ aux (Continue start) ;
  in
  let a = List.nth lst 0 in
  let open Utils in
  push_op @@ Operation.origination @@
    Origination.make_tpl a 300L counter_contract Das_vm.VMap.empty 100L ;
  let b = Account_index.contract_index 1L in
  push_op @@ Operation.transfer @@
    Transfer.make_tpl a b 300L [|15L|] 100L ;
  let ctr = S.get_contract_exn 1L in
  assert (Utils.equal_memory ctr.storage Das_vm.VMap.(of_list [(0L ,15L)])) ;
  push_op @@ Operation.transfer @@
    Transfer.make_tpl a b 300L [|17L|] 100L ;
  let ctr = S.get_contract_exn 1L in
  assert (Utils.equal_memory ctr.storage Das_vm.VMap.(of_list [(0L ,32L)])) ;
  ()


let () =
  Printexc.record_backtrace true ;
  Alcotest.run "ORU SCRE" [
    ("full" , [
      test_simple_transfer_to_key ;
      test_simple_origination ;
      test_simple_transfer_to_contract ;
    ]) ;
    ("incremental" , [
      test_inc_transfer_to_key 1 ;
      test_inc_transfer_to_key 2 ;
      test_inc_transfer_to_key 10 ;
      test_inc_origination 1 ;
      test_inc_origination 2 ;
      test_inc_origination 10 ;
      test_inc_transfer_to_contract 1 ;
      test_inc_transfer_to_contract 2 ;
      test_inc_transfer_to_contract 10 ;
    ]) ;
  ]
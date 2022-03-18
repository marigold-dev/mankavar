open Das_helpers
module PS = Oru_scre_raw.Patricia_state
module Scre = Oru_scre_raw.Scre
module Utils = Oru_scre_raw.Utils

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


let test_simple_transfer () =
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

let () =
  Printexc.record_backtrace true ;
  test_simple_transfer ()
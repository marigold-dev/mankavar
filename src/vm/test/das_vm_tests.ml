open Das_vm

let ($$) f x = f x
let test_quick name f =
  Alcotest.test_case name `Quick f

let arithmetics = test_quick "arithmetics" @@ fun () ->
  let open C_like in
  let expr = add $$ literal 21L $$ literal 21L in
  let prog = expression_to_program expr in
  let asm = Compile.compile_program prog in
  let _hook =
    let step = ref 0 in
    fun s ->
    step := !step + 1 ;
    if !step > 100 then assert false ;
    Format.printf "step %d@;" !step ;
    Format.printf "State:@[<v 2>@;%a@]@;" (state_pp Eval.ReadWrite.pp) s
  in
  (* Format.printf "@[<v 2>" ; *)
  let state = Eval.ReadWrite.eval asm VMap.empty in
  let a = Eval.get_register state A in
  (* Format.printf "@]" ; *)
  assert (a = 42L) ;
  ()


let () =
  Printexc.record_backtrace true ;
  Alcotest.run "DAS VM" [
    "Simple" , [
      arithmetics ;
    ] ;
  ]
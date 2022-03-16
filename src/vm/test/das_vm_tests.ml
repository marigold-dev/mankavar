open Das_vm

let ($$) f x = f x
let test_quick name f =
  Alcotest.test_case name `Quick f

let hook =
  let step = ref 0 in
  fun s ->
  step := !step + 1 ;
  if !step > 20  then assert false ;
  Format.printf "step %d@;" !step ;
  Format.printf "State:@[<v 2>@;%a@]@;" (state_pp Eval.ReadWrite.pp) s

(*
  let pre_asm , _ = Compile.precompile_program prog in
  let _ , label_positions = Compile.preprocess pre_asm in
  Format.printf "Pre program:@;%a@;%!" Compile.pre_program_pp pre_asm ;
  Format.printf "Labels:@;%a@;%!" Compile.label_assoc_pp label_positions ;
  Format.printf "Program:@;%a@;%!" Das_vm.rt_program_pp asm.code ;
*)


let arithmetics = test_quick "arithmetics" @@ fun () ->
  let open C_like in
  let expr = add $$ literal 21L $$ literal 21L in
  let prog = expression_to_program expr in
  let asm = Compile.compile_program prog in
  let state = Eval.ReadWrite.eval asm VMap.empty in
  let a = Eval.get_register state A in
  assert (a = 42L) ;
  ()

let app = test_quick "app" @@ fun () ->
  let open C_like in
  let prog = [
    function_ "inc" "x" [
      return @@ (add $$ variable "x" $$ literal 1L) ;
    ] ;
    function_ "main" "_" [
      return @@ (application "inc" (literal 41L))
    ] ;
  ] in
  let asm = Compile.compile_program prog in
  let state = Eval.ReadWrite.eval asm VMap.empty in
  let a = Eval.get_register state A in
  assert (a = 42L) ;
  ()

let declarations = test_quick "declarations" @@ fun () ->
  let open C_like in
  let prog = [
    function_ "looper" "x" [
      declaration "y" (literal 1L) ;
      declaration "z" (variable "x") ;
      return @@ (add $$ variable "y" $$ variable "z") ;
    ] ;
    function_ "main" "_" [
      declaration "w" (literal 41L) ;
      return @@ (application "looper" (variable "w")) ;
    ] ;
  ] in
  let asm = Compile.compile_program prog in
  let state = Eval.ReadWrite.eval asm VMap.empty in
  let a = Eval.get_register state A in
  assert (a = 42L) ;
  ()


let loop = test_quick "loop" @@ fun () ->
  let open C_like in
  let prog = [
    function_ "looper" "n" [
      declaration "i" (variable "n") ;
      declaration "r" (literal 0L) ;
      loop_gz (variable "i") [
        assignment "r" (add $$ variable "r" $$ variable "i") ;
        assignment "i" (sub $$ variable "i" $$ literal 1L) ;
      ] ;
      return @@ variable "r" ;
      (* return @@ (variable "x") ; *)
    ] ;
    function_ "main" "_" [
      return @@ (application "looper" (literal 10L))
    ] ;
  ] in
  (* let pre_asm , _ = Compile.precompile_program prog in
  Format.printf "Pre program:@;%a@;%!" Compile.pre_program_pp pre_asm ; *)
  let asm = Compile.compile_program prog in
  (* let hook =
    let step = ref 0 in
    fun s ->
    step := !step + 1 ;
    if !step > 2000 then assert false ;
    Format.printf "step %d@;" !step ;
    Format.printf "State:@[<v 2>@;%a@]@;" (state_pp Eval.ReadWrite.pp) s
  in
  Format.printf "@[<v 2>" ; *)
  (* let state = Eval.ReadWrite.eval ~hook asm VMap.empty in *)
  let state = Eval.ReadWrite.eval asm VMap.empty in
  (* Format.printf "@]" ; *)
  let a = Eval.get_register state A in
  (* Format.printf "a : %Ld@;%!" a ; *)
  assert (a = 55L) ;
  ()


let () =
  Printexc.record_backtrace true ;
  Alcotest.run "Epic VM" [
    "Simple" , [
      arithmetics ;
      app ;
      declarations ;
      loop ;
    ] ;
  ]
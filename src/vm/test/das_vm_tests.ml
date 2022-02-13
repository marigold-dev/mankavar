(* open Das_vm *)

let test_quick name f =
  Alcotest.test_case name `Quick f

let () =
  Printexc.record_backtrace true ;
  Alcotest.run "DAS VM" [
    "Simple" , [

    ] ;
  ]
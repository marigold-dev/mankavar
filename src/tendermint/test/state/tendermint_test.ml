let () =
  Printexc.record_backtrace true ;
  Alcotest.run "DAS" [
    Simple_tests.tests ;
  ]
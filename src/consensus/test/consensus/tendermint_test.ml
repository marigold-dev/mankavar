let () =
  Printexc.record_backtrace true ;
  Alcotest.run "DAS" [
    Network_tests.tests ;
  ]
let () =
  Printexc.record_backtrace true ;
  Alcotest.run "DAS" [
    Consensus_dummy_tests.tests ;
  ]
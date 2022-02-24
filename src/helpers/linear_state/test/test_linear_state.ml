open Linear_state

let test_quick name f =
  Alcotest.test_case name `Quick f
let updates_work = test_quick "Updates work" @@ fun () ->
  let x = make 42 in
  let x = set x 32 in
  assert (get x = 32) ;
  let x = perform x (fun r -> r := 100) in
  assert (get x = 100) ;
  let x = perform' x Int.succ in
  assert (get x = 101) ;
  ()

let mismatch_fail = test_quick "Mismatch fail" @@ fun () ->
  let x = make 42 in
  let _x1 = set x 32 in
  try (
    let _x2 = set x 23 in
    assert false
  ) with Obsolete -> ()

let simple_tests = "Simple" , [
  updates_work ;
  mismatch_fail ;
]

let () =
  Printexc.record_backtrace true ;
  Alcotest.run "Linear State" [
    simple_tests ;
  ] ;
  ()
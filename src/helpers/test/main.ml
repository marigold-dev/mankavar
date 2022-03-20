let test_quick name f = Alcotest.test_case name `Quick f

open Das_helpers

let group_by_tests =
  let open XList in
  let cmp (x , _) (x' , _) = x = x' in
  (* let pp = XFormat.(tuple_2 int int) in *)
  [
    ("simple" , fun () ->
      let result = group_by_f cmp [
        (1 , 23) ;
        (1 , 24) ;
        (2 , 45) ;
        (2 , 64) ;
      ] in
      (* Format.printf "%a@;%!" XFormat.(list @@ list pp) result ; *)
      assert (result = [
        [
          (1 , 23) ;
          (1 , 24) ;
        ] ; [
          (2 , 45) ;
          (2 , 64) ;
        ]
      ]) ;
      ()
    ) ;
    ("empty" , fun () ->
      assert (group_by_f cmp [] = []) ;
      ()
    )
  ] |> List.map (fun (x , y) -> test_quick x y)

let () =
  Alcotest.run "Helpers" [
    ("XList.group_by" , group_by_tests) ;
  ]
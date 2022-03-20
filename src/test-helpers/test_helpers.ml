open Das_helpers

let test_quick name f = Alcotest.test_case name `Quick f

let run name lst =
  let lst' =
    let do_prefix str =
      match String.split_on_char '.' str with
      | [] -> "" , str
      | hd :: tl -> hd , String.concat "." tl
    in
    lst
    |> List.map (fun (name , f) ->
      let (prefix , name) = do_prefix name in
      (prefix , name , f)
    )
  in
  let lsts = XList.group_by_f
    (fun (prefix , _ , _) (prefix' , _ , _) -> prefix = prefix') lst' in
  lsts
  |> List.map (fun lst ->
    let prefix = lst |> List.hd |> fun (p , _ , _) -> p in
    let no_prefix = lst |> List.map (fun (_ , x , y) -> (x , y)) in
    prefix ,
    no_prefix |> List.map (fun (x , y) -> test_quick x y)
  )
  |> Alcotest.run name
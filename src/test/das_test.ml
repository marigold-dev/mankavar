open Das.Helpers

let test_quick name f =
  Alcotest.test_case name `Quick f

let dummy_accounts = List.init 100 (fun _ -> Das.Account.Address.generate ())

let nth_account i = List.nth dummy_accounts i

let dummy = test_quick "dummy" @@ fun () ->
  let module Start = struct
    let start_clock = XPtime.now ()
    let bp = nth_account 0
    let bp_pk = Das.Account.Address.public_key bp
    let endorsers = List.init Das.nb_endorsers (fun i -> nth_account (i + 1))
    let endorsers_pk = endorsers |> List.map Das.Account.Address.public_key
    let bp_node = Das.RawBlockProducerNode.empty start_clock endorsers_pk bp
    let endorsers_node = List.map (Das.RawEndorserNode.empty bp_pk) endorsers
    let bp_node_packed =
      Das.(Node_packed.pack_abstract (module RawBlockProducerNode) bp_node)
    let endorsers_node_packed =
      List.map
        Das.(Node_packed.pack_abstract (module RawEndorserNode))
        endorsers_node
    let network =
      let increment = XPtime.ms_int 100 in
      Das.Network.empty ~increment start_clock
        (bp_node_packed :: endorsers_node_packed)
  end in
  (* let duration = XPtime.ms_int 1_000 in *)
  let duration = XPtime.ms_int 11_000 in
  let network' = Das.Network.run_for duration Start.network in
  let module Bp = (val network' |> Das.Network.nodes |> List.hd) in
  let bp_node = (Obj.magic Bp.x : Das.RawBlockProducerNode.t) in
  Format.printf "Height : %d\n" @@ Das.Height.to_int @@ Das.RawBlockProducerNode.height bp_node ;
  assert (Das.RawBlockProducerNode.height bp_node >= Das.Height.of_int 10) ;
  ignore @@ network' ;
  ()


let () =
  Alcotest.run "DAS" [
    "simple" , [
      dummy ;
    ] ;
  ]
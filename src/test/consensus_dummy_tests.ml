open Das.Helpers

module CD = Das.Consensus_dummy
module ND = Das_network.Dummy
module Node = Das_network.Node
module Network = ND.Make(struct 
  module Message = CD.Message
  let increment = XPtime.ms_int 10
  let ping = ND.Ping.constant @@ XPtime.ms_int 300
end)

let test_quick name f =
  Alcotest.test_case name `Quick f

let dummy_accounts = List.init 100 (fun _ -> Das.Account.Address.generate ())

let nth_account i = List.nth dummy_accounts i

let honest = test_quick "Live when nodes honest" @@ fun () ->
  let module Start = struct
    let start_clock = XPtime.now ()
    let bp = nth_account 0
    let bp_pk = Das.Account.Address.public_key bp
    let endorsers = List.init CD.nb_endorsers (fun i -> nth_account (i + 1))
    let endorsers_pk = endorsers |> List.map Das.Account.Address.public_key
    let bp_node = CD.RawBlockProducerNode.empty start_clock endorsers_pk bp
    let endorsers_node = List.map (CD.RawEndorserNode.empty bp_pk) endorsers
    let bp_node_packed =
      Node.Packed.pack_abstract (module CD.RawBlockProducerNode) bp_node
    let endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module CD.RawEndorserNode))
        endorsers_node
    let network =
      Network.empty start_clock
        (bp_node_packed :: endorsers_node_packed)
  end in
  let duration = XPtime.ms_int 11_000 in
  let network' = Network.run_for duration Start.network in
  let module Bp = (val network' |> Network.nodes |> List.hd) in
  let bp_node = (Obj.magic Bp.x : CD.RawBlockProducerNode.t) in
  Format.printf "Height : %ld\n" @@ Height.to_int32 @@ CD.RawBlockProducerNode.height bp_node ;
  assert (CD.RawBlockProducerNode.height bp_node >= Height.of_int32 10l) ;
  ignore @@ network' ;
  ()

let third_dead = test_quick "Live when 1/3 endorsers dead" @@ fun () ->
  let module Start = struct
    let start_clock = XPtime.now ()
    let bp = nth_account 0
    let bp_pk = Das.Account.Address.public_key bp
    let dead_endorsers =
      List.init
        (CD.nb_endorsers / 3)
        (fun i -> nth_account (i + 1))
    let live_endorsers =
      List.init
        (CD.nb_endorsers - CD.nb_endorsers / 3)
        (fun i -> nth_account (i + 1 + CD.nb_endorsers / 3))
    let endorsers = dead_endorsers @ live_endorsers
    let endorsers_pk = endorsers |> List.map Das.Account.Address.public_key
    let bp_node = CD.RawBlockProducerNode.empty start_clock endorsers_pk bp
    let dead_endorsers_node = List.map (fun _ -> CD.RawDeadNode.empty) dead_endorsers
    let live_endorsers_node = List.map (CD.RawEndorserNode.empty bp_pk) live_endorsers
    let bp_node_packed =
      (Node.Packed.pack_abstract (module CD.RawBlockProducerNode) bp_node)
    let dead_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module CD.RawDeadNode))
        dead_endorsers_node
    let live_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module CD.RawEndorserNode))
        live_endorsers_node
    let network =
      Network.empty start_clock
        (bp_node_packed :: (dead_endorsers_node_packed @ live_endorsers_node_packed))
  end in
  let duration = XPtime.ms_int 11_000 in
  let network' = Network.run_for duration Start.network in
  let module Bp = (val network' |> Network.nodes |> List.hd) in
  let bp_node = (Obj.magic Bp.x : CD.RawBlockProducerNode.t) in
  Format.printf "Height : %ld\n" @@ Height.to_int32 @@ CD.RawBlockProducerNode.height bp_node ;
  assert (CD.RawBlockProducerNode.height bp_node >= Height.of_int32 10l) ;
  ignore @@ network' ;
  ()

let third_plus_dead = test_quick "Dead when 1+1/3 nodes dead" @@ fun () ->
  let module Start = struct
    let start_clock = XPtime.now ()
    let bp = nth_account 0
    let bp_pk = Das.Account.Address.public_key bp
    let dead_endorsers =
      List.init
        (1 + CD.nb_endorsers / 3)
        (fun i -> nth_account (i + 1))
    let live_endorsers =
      List.init
        (CD.nb_endorsers - 1 - CD.nb_endorsers / 3)
        (fun i -> nth_account (i + 1 + 1 + CD.nb_endorsers / 3))
    let endorsers = dead_endorsers @ live_endorsers
    let endorsers_pk = endorsers |> List.map Das.Account.Address.public_key
    let bp_node = CD.RawBlockProducerNode.empty start_clock endorsers_pk bp
    let dead_endorsers_node = List.map (fun _ -> CD.RawDeadNode.empty) dead_endorsers
    let live_endorsers_node = List.map (CD.RawEndorserNode.empty bp_pk) live_endorsers
    let bp_node_packed =
      (Node.Packed.pack_abstract (module CD.RawBlockProducerNode) bp_node)
    let dead_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module CD.RawDeadNode))
        dead_endorsers_node
    let live_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module CD.RawEndorserNode))
        live_endorsers_node
    let network =
      Network.empty start_clock
        (bp_node_packed :: (dead_endorsers_node_packed @ live_endorsers_node_packed))
  end in
  let duration = XPtime.ms_int 11_000 in
  let network' = Network.run_for duration Start.network in
  let module Bp = (val network' |> Network.nodes |> List.hd) in
  let bp_node = (Obj.magic Bp.x : CD.RawBlockProducerNode.t) in
  Format.printf "Height : %ld\n" @@ Height.to_int32 @@ CD.RawBlockProducerNode.height bp_node ;
  assert (CD.RawBlockProducerNode.height bp_node = Height.of_int32 0l) ;
  ignore @@ network' ;
  ()


let tests =
  "Consensus Dummy" , [
    honest ;
    third_dead ;
    third_plus_dead ;
  ] ;
open Das_helpers

module CD = Das_tendermint
module ND = Das_network.Dummy
module Node = Das_network.Node
module TendermintNode = CD.RawTendermintNode
module DeadNode = CD.RawDeadNode
module Network = ND.Make(struct
  module Message = CD.Message
  let increment = XPtime.ms_int 100
  let ping = ND.Ping.constant @@ XPtime.ms_int 300
end)

let test_quick name f =
  Alcotest.test_case name `Quick f

let dummy_accounts = List.init 100 (fun _ -> Crypto.Address.generate ())

let nth_account i = List.nth dummy_accounts i

let mk_empty_node endorsers account =
  let start_clock = XPtime.now () in
  let chain_state = (
    Obj.magic
    (Das_tendermint_noop_raw.Transition.State.mk_empty ())
    : Das_tendermint.Transition.State.t
  ) in
  TendermintNode.empty start_clock endorsers account
    ~network:"test" ~chain_state

let honest_live = test_quick "Live when nodes honest" @@ fun () ->
  (* ignore @@ assert false ; *)
  let module Start = struct
    let endorsers = List.init CD.nb_endorsers nth_account
    let endorsers_pk = endorsers |> List.map Crypto.Address.public_key
    let endorsers_node =
      List.map (mk_empty_node endorsers_pk) endorsers
    let endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module TendermintNode))
        endorsers_node
    let start_clock = XPtime.now ()
    let network =
      Network.empty start_clock endorsers_node_packed
  end in
  let duration = XPtime.ms_int @@ CD.block_time_ms * 10 in
  let network' = Network.run_for duration Start.network in
  let module Zero = (val network' |> Network.nodes |> List.hd) in
  let zero_node = (Obj.magic Zero.x : TendermintNode.t) in
  Format.printf "Height : %ld\n" @@ Height.to_int32 @@ TendermintNode.height zero_node ;
  assert (TendermintNode.height zero_node >= Height.of_int32 10l) ;
  ignore @@ network' ;
  ()

let third_live = test_quick "Live when 1/3 nodes dead" @@ fun () ->
  (* ignore @@ assert false ; *)
  let module Start = struct
    let start_clock = XPtime.now ()
    let endorsers = List.init CD.nb_endorsers nth_account
    let endorsers_pk = endorsers |> List.map Crypto.Address.public_key
    let ok_endorsers_node =
      List.map (mk_empty_node endorsers_pk)
      @@ List.filteri (fun i _ -> i >= CD.max_byzantine) endorsers
    let ok_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module TendermintNode))
        ok_endorsers_node
    let dead_endorsers_node =
      List.map (fun _ -> DeadNode.empty)
      @@ List.filteri (fun i _ -> i < CD.max_byzantine) endorsers
    let dead_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module DeadNode))
        dead_endorsers_node
    let network =
      Network.empty start_clock (dead_endorsers_node_packed @ ok_endorsers_node_packed)
  end in
  let duration = XPtime.ms_int @@ CD.block_time_ms * 30 in
  let network' = Network.run_for duration Start.network in
  let module Zero = (val network' |> Network.nodes |> fun lst -> List.nth lst CD.max_byzantine) in
  let zero_node = (Obj.magic Zero.x : TendermintNode.t) in
  Format.printf "Height : %ld\n" @@ Height.to_int32 @@ TendermintNode.height zero_node ;
  assert (TendermintNode.height zero_node >= Height.of_int32 10l) ;
  ignore @@ network' ;
  ()

let third_plus_dead = test_quick "Dead when 1+1/3 nodes dead" @@ fun () ->
  (* ignore @@ assert false ; *)
  let module Start = struct
    let start_clock = XPtime.now ()
    let endorsers = List.init CD.nb_endorsers nth_account
    let endorsers_pk = endorsers |> List.map Crypto.Address.public_key
    let ok_endorsers_node =
      List.map (mk_empty_node endorsers_pk)
      @@ List.filteri (fun i _ -> i >= 1 + CD.max_byzantine) endorsers
    let ok_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module TendermintNode))
        ok_endorsers_node
    let dead_endorsers_node =
      List.map (fun _ -> DeadNode.empty)
      @@ List.filteri (fun i _ -> i < 1 + CD.max_byzantine) endorsers
    let dead_endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module DeadNode))
        dead_endorsers_node
    let network =
      Network.empty start_clock (dead_endorsers_node_packed @ ok_endorsers_node_packed)
  end in
  let duration = XPtime.ms_int @@ CD.block_time_ms * 30 in
  let network' = Network.run_for duration Start.network in
  let module Zero = (val network' |> Network.nodes |> fun lst -> List.nth lst (1 + CD.max_byzantine)) in
  let zero_node = (Obj.magic Zero.x : TendermintNode.t) in
  Format.printf "Height : %ld\n" @@ Height.to_int32 @@ TendermintNode.height zero_node ;
  assert (TendermintNode.height zero_node = Height.of_int32 0l) ;
  ignore @@ network' ;
  ()

let tests =
  "Consensus Tendermint" , [
    honest_live ;
    third_live ;
    third_plus_dead ;
  ] ;

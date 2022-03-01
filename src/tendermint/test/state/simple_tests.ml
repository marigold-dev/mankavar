open Das_helpers

module CD = Das_tendermint
module ND = Das_network.Dummy
module Node = Das_network.Node
module TendermintNode = CD.RawTendermintNode
module Transition = Das_tendermint_sc_raw.Transition
module Operation = Transition.Operation
module DeadNode = CD.RawDeadNode
module HookNode = CD.RawHookNode
module Network = ND.Make(struct
  module Message = CD.Message
  let increment = XPtime.ms_int 100
  let ping = ND.Ping.constant @@ XPtime.ms_int 300
end)

let test_quick name f =
  Alcotest.test_case name `Quick f

let dummy_accounts = List.init 100 (fun _ -> Account.Address.generate ())

let nth_account i = List.nth dummy_accounts i

let honest_live = test_quick "Live when nodes honest" @@ fun () ->
  (* ignore @@ assert false ; *)
  let module Start = struct
    let start_clock = XPtime.now ()
    let endorsers = List.init CD.nb_endorsers nth_account
    let endorsers_pk = endorsers |> List.map Account.Address.public_key
    let endorsers_node =
      let mk_endorser =
        TendermintNode.empty start_clock endorsers_pk ~network:"test"
      in
      List.map mk_endorser endorsers
    let endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module TendermintNode))
        endorsers_node
    let network =
      Network.empty start_clock endorsers_node_packed
  end in
  let duration = XPtime.ms_int @@ CD.block_time_ms * 11 in
  let last_height = ref Height.zero in
  let hook _clock (nodes : Network.message Node.Packed.t list) =
    let module Zero = (val nodes |> List.hd) in
    let zero_node = (Obj.magic Zero.x : TendermintNode.t) in
    let prev_height = !last_height in
    last_height := TendermintNode.height zero_node ;
    if Height.(!last_height <> prev_height)
    then Format.printf "new height : %a@;%!" Height.pp !last_height ;
    if Height.(!last_height <> prev_height && !last_height = Height.of_int32 2l)
    then (
      let orig =
        Transition.Origination.make_tpl 100L Das_vm.VMap.empty 100L
      in
      [CD.Message.operation @@ Obj.magic @@ Operation.origination orig]
    ) else []
  in
  let network' = Network.run_for ~hook duration Start.network in
  Format.printf "last height : %ld\n%!" @@ Height.to_int32 !last_height ;
  assert Height.(!last_height >= of_int32 10l) ;
  ignore @@ network' ;
  ignore @@ assert false ;
  ()

let tests =
  "Consensus Tendermint" , [
    honest_live ;
  ] ;

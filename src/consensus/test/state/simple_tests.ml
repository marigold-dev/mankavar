open Das_helpers

module CD = Das_tendermint
module CD_raw = Das_tendermint_sc_raw
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

let dummy_accounts = List.init 100 (fun _ -> Crypto.Address.generate ())

let nth_account i = List.nth dummy_accounts i

let test_contract =
  let open Das_vm.C_like in
  let expr_c = add (literal 21L) (literal 21L) in
  let program_c = expression_to_program expr_c in
  let program = Das_vm.Compile.compile_program program_c in
  let storage = Das_vm.VMap.empty in
  Transition.Contract.make ~program ~storage

let bootstrap_amount = 1_000_000_000L

let bootstrap_accounts , bootstrap_state =
  let open CD_raw.Transition in
  let t = State.mk_empty () in
  let bootstraps_pk = List.init 10 @@ fun _ -> Crypto.Address.generate () in
  let bootstrap_accounts = bootstraps_pk |> List.map (fun bootstrap_pk ->
    State.Test.create_account_key bootstrap_pk bootstrap_amount t
  ) in
  bootstrap_accounts , t
let bootstrap_a = List.nth bootstrap_accounts 0

let put_node_state (node_state : CD_raw.Transition.State.t) =
  (Obj.magic node_state : CD.Transition.State.t)
let get_node_state (node_state : CD.Transition.State.t) =
  (Obj.magic node_state : CD_raw.Transition.State.t)

let mk_empty_node endorsers x =
  let start_clock = XPtime.now () in
  let node_state = CD_raw.Transition.State.clone bootstrap_state in
  let chain_state = put_node_state node_state in
  TendermintNode.empty start_clock endorsers ~network:"test" ~chain_state x

let honest_live = test_quick "Live when nodes honest" @@ fun () ->
  (* ignore @@ assert false ; *)
  let module Start = struct
    let start_clock = XPtime.now ()
    let endorsers = List.init CD.nb_endorsers nth_account
    let endorsers_pk = endorsers |> List.map Crypto.Address.public_key
    let endorsers_node =
      List.map (mk_empty_node endorsers_pk) endorsers
    let endorsers_node_packed =
      List.map
        (Node.Packed.pack_abstract (module TendermintNode))
        endorsers_node
    let network =
      Network.empty start_clock endorsers_node_packed
  end in
  let duration = XPtime.ms_int @@ CD.block_time_ms * 11 in
  let last_height = ref Height.zero in
  let origination =
    let orig =
      Transition.Origination.make_tpl bootstrap_a
        100L test_contract Das_vm.VMap.empty 100L
    in
    Operation.origination orig
  in
  let hook _clock (nodes : Network.message Node.Packed.t list) =
    PseudoEffect.returner @@ fun { return } ->
    let module Zero = (val nodes |> List.hd) in
    let zero_node = (Obj.magic Zero.x : TendermintNode.t) in
    let node_state = get_node_state zero_node.chain_state in
    Format.printf "Next Contract Index: %Ld@;%!"
      node_state.next_contract_index ;
    let prev_height = !last_height in
    last_height := TendermintNode.height zero_node ;
    if Height.(!last_height <> prev_height) then (
      Format.printf "new height : %a@;%!" Height.pp !last_height ;
      if !last_height = Height.of_int32 2l then (
        assert (node_state.next_contract_index = 0L) ;
        return [CD.Message.operation @@ Obj.magic origination] ;
      ) ;
      if !last_height = Height.of_int32 4l then (
        assert (node_state.next_contract_index = 1L) ;
      )
    ) ;
    []
  in
  let network' = Network.run_for ~hook duration Start.network in
  Format.printf "last height : %ld\n%!" @@ Height.to_int32 !last_height ;
  assert Height.(!last_height >= of_int32 10l) ;
  ignore @@ network' ;
  ()

let tests =
  "Consensus Tendermint" , [
    honest_live ;
  ] ;

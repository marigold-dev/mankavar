open Das_helpers

module CD = Das_tendermint
module CD_raw = Das_tendermint_sc_raw
module Transition = Das_tendermint_sc_raw.Transition
module Operation = Transition.Operation

let put_node_state (node_state : CD_raw.Transition.State.t) =
  (Obj.magic node_state : CD.Transition.State.t)
let get_node_state (node_state : CD.Transition.State.t) =
  (Obj.magic node_state : CD_raw.Transition.State.t)

let time f =
  let c = Mtime_clock.counter () in
  let ret = f () in
  ret , Mtime_clock.count c

let simple_transfer source destination amount =
  CD_raw.Transition.Transfer.make
  ~source ~destination ~amount ~payload:(Array.make 0 0L) ~max_gas:0L

let time_transfers a b t n =
  let ops = List.init n @@ fun _ ->
    Transition.Operation.transfer @@ simple_transfer a b  1L
  in
  time @@ fun () ->
  ops |> List.iter @@ fun op ->
  ignore @@ Transition.do_operation op t
let bootstrap_amount = 1_000_000_000L


let bench_state name n =
  let bootstrap_accounts , bootstrap_state =
    let open CD_raw.Transition in
    let t = State.mk_empty () in
    let bootstraps_pk = List.init 10 @@ fun _ -> Crypto.Address.generate () in
    let bootstrap_accounts = bootstraps_pk |> List.map (fun bootstrap_pk ->
      State.Test.create_account_key bootstrap_pk bootstrap_amount t
    ) in
    for _ = 1 to n do
      ignore @@ State.Test.create_account_key (Crypto.Address.generate ()) 1L t ;
      ()
    done ;
    bootstrap_accounts , t
  in
  let bootstrap_a = List.nth bootstrap_accounts 0 in
  let bootstrap_b = List.nth bootstrap_accounts 1 in
  Format.printf "Bench state %s (%d)@;%!" name n ;
  let bench_transfers n =
    let () , time =
      time_transfers bootstrap_a bootstrap_b bootstrap_state n
    in
    Format.printf "Bench %d: %a@;%!"
      n Mtime.Span.pp time
  in
  bench_transfers 100 ;
  bench_transfers 10_000 ;
  bench_transfers 1_000_000 ;
  ()

let () =
  bench_state "empty" 0 ;
  bench_state "full" 1_000_000 ;
  bench_state "super full" 10_000_000 ;

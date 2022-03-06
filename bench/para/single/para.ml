let random_to_check () =
  let sk , pk = Sodium.Sign.random_keypair () in
  let bytes = Sodium.Random.Bytes.generate 10 in
  let signature = Sodium.Sign.Bytes.sign sk bytes in
  (pk , signature , bytes)

let bytes_pp_hex ppf b =
  b |> Bytes.iter @@ fun c ->
    let i = Char.code c in
    Format.fprintf ppf "%x" i ;
    ()

let data = Array.init 10 (fun _ -> random_to_check ())

let bench n =
  let l = Array.length data in
  for i = 1 to n do
    let i' = i mod l in
    let (pk , signature , bytes) = data.(i') in
    let bytes' = Sodium.Sign.Bytes.sign_open pk signature in
    assert (bytes = bytes') ;
    ()
  done ;
  ()

let time f =
  let c = Mtime_clock.counter () in
  f () ;
  Mtime_clock.count c

let bench_time n =
  let t = time @@ fun () -> bench n in
  Format.printf "Bench %d: %a@;%!" n Mtime.Span.pp t ;
  ()

let () =
  (* data |> Array.iteri (fun i (pk , _signature , _bytes) -> (
    Format.printf "Key %d: 0x%a@;%!" i bytes_pp_hex @@
    Sodium.Sign.Bytes.of_public_key pk ;
  )) ; *)
  bench_time 10 ;
  bench_time 100 ;
  bench_time 1_000 ;
  bench_time 10_000 ;
  ()

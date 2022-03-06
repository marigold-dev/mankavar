let (_sk , pk) =
  Sodium.Sign.random_keypair ()

let bytes_pp_hex ppf b =
  b |> Bytes.iter @@ fun c ->
    let i = Char.code c in
    Format.fprintf ppf "%x" i ;
    ()

let () =
  Format.printf "Key: 0x%a@;%!" bytes_pp_hex @@
  Sodium.Sign.Bytes.of_public_key pk;
  ()

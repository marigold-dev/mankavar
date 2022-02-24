let blake2b : bytes -> bytes = fun b -> Digestif.BLAKE2B.(
  b |> digest_bytes |> to_raw_string |> Bytes.of_string
)
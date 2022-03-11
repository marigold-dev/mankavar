open Das_helpers

module State = struct
  type t = int
  let encoding = Encoding.int
  let of_bytes = Encoding.of_bytes encoding
  let to_bytes = Encoding.to_bytes encoding
  let hash' t = to_bytes t |> Crypto.blake2b
  let hash = Hash.make hash'
end

module Batch = struct
  type t = unit
  let encoding = Encoding.unit
end

module Proof = struct
  type t = {
    valid : bool ;
    asserted_result : int ;
  }
  [@@deriving ez]
  let encoding =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_2
    bool int
  let of_bytes = Encoding.of_bytes encoding
  let replay = fun proof h ->
    ignore h ;
    let valid , asserted_result = destruct proof in
    assert valid ;
    let h' = State.hash' asserted_result in
    h'

end
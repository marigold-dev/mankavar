open Das_helpers

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
  let replay = fun _ h -> h

end
open Das_helpers

module HMap = struct
  include XMap.Make(Height)
  let encoding x = encoding Height.encoding x
end

module BatchIndex = Index.Make()
module BMap = struct
  include XMap.Make(BatchIndex)
  let encoding x = encoding BatchIndex.encoding x
end

module Commitment = struct
  module Infra = struct
    type t = Hash'.t 
    let encoding : t Encoding.t = Hash'.encoding
  end
  type infra = Infra.t
  module HashIndex = Index.Make()
  type t = {
    previous_hash : Hash'.t ;
    hashes : infra list ;
  }
  [@@deriving ez]
  let encoding =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_2
      Hash'.encoding (list Infra.encoding)
  let get_previous_hash = previous_hash
  let get_hash i t =
    let i = HashIndex.to_int i in
    if i = -1
    then t.previous_hash
    else List.nth t.hashes i
end

module BatchCommitments = struct
  (* content * is_valid *)
  type t = (Commitment.t * bool) list
  let encoding : t Encoding.t =
    let open Encoding in
    list (tuple_2 Commitment.encoding bool)
  let append x (t : t) : t = t @ [x , true]
  module Index = Index.Make()
  let get_valid i (t : t) =
    let (c , v) = Index.to_int i |> List.nth t in
    assert v ;
    c
  let set_invalid i (t : t) : t =
    XList.nth_map (fun (c , _) -> (c , false)) (Index.to_int i) t
end
module RejectionIndex = Index.Make()

module Rejection = struct
  module HashIndex = Index.Make()
  type t = {
    first_bad_hash : Commitment.HashIndex.t ;
    inbetween_hashes : Hash'.t list ;
  }
  [@@deriving ez]
  let get_hash i t =
    let i = HashIndex.to_int i in
    List.nth t.inbetween_hashes i
  let encoding =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_2
      Commitment.HashIndex.encoding (list Hash'.encoding)
end

module CounterRejection = struct
  type t = {
    first_bad_hash : Rejection.HashIndex.t ;
    proof : Oru_param.Proof.t ;
  }
  [@@deriving ez]
  let encoding =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_2
      Rejection.HashIndex.encoding Oru_param.Proof.encoding
end

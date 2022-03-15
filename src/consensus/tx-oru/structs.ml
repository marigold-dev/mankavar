open Das_helpers
open Oru_param

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
  module Content = struct
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
  type t = {
    height : Height.t ;
    batch : BatchIndex.t ;
    content : Content.t ;
  }
  [@@deriving ez]
  let encoding : t Encoding.t =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_3
      Height.encoding BatchIndex.encoding Content.encoding
end
module BatchCommitments = struct
  (* content * is_valid *)
  type t = (Commitment.content * bool) list
  let encoding : t Encoding.t =
    let open Encoding in
    list (tuple_2 Commitment.Content.encoding bool)
  let append x (t : t) : t = t @ [x , true]
  module Index = Index.Make()
  let get_valid i (t : t) =
    let (c , v) = Index.to_int i |> List.nth t in
    assert v ;
    c
  let set_invalid i (t : t) : t =
    XList.nth_map (fun (c , _) -> (c , false)) (Index.to_int i) t
end
module Rejection = struct
  type t = {
    height : Height.t ;
    batch : BatchIndex.t ;
    batch_commitment : BatchCommitments.Index.t ;
    first_bad_hash : Commitment.Content.HashIndex.t ;
    proof : Proof.t ;
  }
  [@@deriving ez]
  let encoding =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_5
      Height.encoding BatchIndex.encoding BatchCommitments.Index.encoding
      Commitment.Content.HashIndex.encoding Proof.encoding
end

module Operation = struct
  type t =
  | Submit_batch of Batch.t
  | Commit of Commitment.t
  | Reject of Rejection.t
  [@@deriving ez]
  let get_max_gas = fun _ -> 1L
  let pp ppf t =
    destruct_tpl
      (XFormat.cst' "submit batch")
      (XFormat.cst' "commit")
      (XFormat.cst' "reject")
    t ppf
  let encoding =
    let open Encoding in
    union [
      case get_submit_batch_opt submit_batch Batch.encoding ;
      case get_commit_opt commit Commitment.encoding ;
      case get_reject_opt reject Rejection.encoding ;
    ]
end

module Bunch = struct
  type t = Operation.t list
  let make lst = lst
  let to_list lst = lst
  let encoding = Encoding.list Operation.encoding
  let dummy = []
  let max_gas = 1L
end

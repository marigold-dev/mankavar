open Das_helpers
open Oru_param
open Structs

module Rejection = struct
  type t = {
    height : Height.t ;
    batch : BatchIndex.t ;
    batch_commitment : BatchCommitments.Index.t ;
    content : Rejection.t ;
  }
  [@@deriving ez]
  let encoding =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_4
      Height.encoding BatchIndex.encoding BatchCommitments.Index.encoding
      Rejection.encoding

end

module CounterRejection = struct
  type t = {
    height : Height.t ;
    batch : BatchIndex.t ;
    batch_commitment : BatchCommitments.Index.t ;
    rejection : RejectionIndex.t ;
    content : CounterRejection.t ;
  }
  [@@deriving ez]
  let encoding =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_5
      Height.encoding BatchIndex.encoding BatchCommitments.Index.encoding
      RejectionIndex.encoding CounterRejection.encoding
end

module Commitment = struct
  type t = {
    height : Height.t ;
    batch : BatchIndex.t ;
    content : Commitment.t ;
  }
  [@@deriving ez]
  let encoding : t Encoding.t =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_3
      Height.encoding BatchIndex.encoding Commitment.encoding
end

type t =
| Submit_batch of Batch.t
| Commit of Commitment.t
| Reject of Rejection.t
| Counter_reject of CounterRejection.t
[@@deriving ez]
let get_max_gas = fun _ -> 1L
let pp ppf t =
  destruct_tpl
    (XFormat.cst' "submit batch")
    (XFormat.cst' "commit")
    (XFormat.cst' "reject")
    (XFormat.cst' "counter-reject")
  t ppf
let encoding =
  let open Encoding in
  union [
    case get_submit_batch_opt submit_batch Batch.encoding ;
    case get_commit_opt commit Commitment.encoding ;
    case get_reject_opt reject Rejection.encoding ;
    case get_counter_reject_opt counter_reject CounterRejection.encoding ;
  ]

module Bunch = struct
  type nonrec t = t list
  let make lst = lst
  let to_list lst = lst
  let encoding = Encoding.list encoding
  let dummy = []
  let max_gas = 1L
end

[@@@warning "-34"]

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

module State = struct

  type t = {
    height : Height.t ;
    current_batches_rev : Batch.t list ;
    batches : Batch.t list HMap.t ;
    commitments : BatchCommitments.t BMap.t HMap.t ;
  }
  [@@deriving ez]

  let encoding : t Encoding.t =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_4
      Height.encoding
      (list Batch.encoding)
      (HMap.encoding (list Batch.encoding))
      (HMap.encoding (BMap.encoding (BatchCommitments.encoding)))

  let do_hash' t = Encoding.to_bytes encoding t |> Crypto.blake2b
  let do_hash = Hash.make do_hash'
  let mk_empty () = make_tpl Height.zero [] HMap.empty HMap.empty

  let current_batches_flush t =
    t
    (* Add the current batches to the current height *)
    |> map_batches (HMap.add (height t) (List.rev @@ current_batches_rev t))
    (* Reset the current batches to [] *)
    |> set_current_batches_rev []
  let current_batches_append b t =
    t |> map_current_batches_rev @@ List.cons b
  let height_increment = map_height Height.increment
  let commitment_append : Commitment.t -> t -> t = fun c t ->
    let h = Commitment.height c in
    let b = Commitment.batch c in
    let c' = Commitment.content c in
    let add = HMap.update' h @@ BMap.update' b @@ BatchCommitments.append c' in
    t
    |> map_commitments add
  let commitment_get h b c : t -> Commitment.content = fun t ->
    t
    |> commitments
    |> HMap.find h
    |> BMap.find b
    |> BatchCommitments.get_valid c
  let commitment_remove h b c : t -> t = fun t ->
    let remove_batch = BatchCommitments.set_invalid c in
    let remove_batches = BMap.update' b remove_batch in
    let remove_height = HMap.update' h remove_batches in
    let remove_commitments = map_commitments remove_height in
    remove_commitments t

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

type do_operation_result = {
  state : State.t ;
  gas : int64 ;
  bytes : int64 ;
}

let submit_batch : Batch.t -> State.t -> do_operation_result = fun b t ->
  t
  |> State.current_batches_append b
  |> fun state -> { state ; gas = 0L ; bytes = 0L }

let commit : Commitment.t -> State.t -> do_operation_result = fun c t ->
  t
  |> State.commitment_append c
  |> fun state -> { state ; gas = 0L ; bytes = 0L }

let reject : Rejection.t -> State.t -> do_operation_result = fun r t ->
  let (h , b , bc , hash_i , proof) = Rejection.destruct r in
  let first_bad_commitment = State.commitment_get h b bc t in
  let first_bad_hash =
    Commitment.Content.get_hash hash_i first_bad_commitment in
  let last_ok_hash =
    Commitment.Content.get_hash
      (Commitment.Content.HashIndex.map_int (fun i -> i - 1) hash_i)
      first_bad_commitment
  in
  let replayed_hash = Proof.replay proof last_ok_hash in
  assert (first_bad_hash <> replayed_hash) ;
  t
  |> State.commitment_remove h b bc
  |> fun state -> { state ; gas = 0L ; bytes = 0L }

let flush_block : State.t -> State.t = fun t ->
  t
  |> State.current_batches_flush
  |> State.height_increment

let do_operation : Operation.t -> State.t -> do_operation_result = fun op s ->
  Operation.destruct ~submit_batch ~commit ~reject op s

let do_operation' s op =
  let { state ; gas = _ ; bytes = _ } = do_operation op s in
  state

let do_bunch : Bunch.t -> State.t -> State.t = fun bu t ->
  List.fold_left do_operation' t bu
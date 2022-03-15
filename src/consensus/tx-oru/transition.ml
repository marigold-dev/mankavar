[@@@warning "-34"]

open Das_helpers
open Oru_param
open Structs

module Bunch = Bunch
module Operation = Operation

let c_STEPS_PER_COMMIT = 10_000L

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
  let batch_get h b t =
    let b' = BatchIndex.to_int b in
    t |> batches |> HMap.find h |> fun lst -> List.nth lst b'
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
  let batch = State.batch_get h b t in
  let last_hash_i , last_nb_steps =
    let nb_steps = Batch.get_nb_steps batch in
    let nb_hashes = Int64.(succ @@ div nb_steps c_STEPS_PER_COMMIT) in
    Commitment.Content.HashIndex.of_int32 @@ Int64.to_int32 nb_hashes ,
    Int64.rem nb_steps c_STEPS_PER_COMMIT
  in
  let replayed_hash =
    if Commitment.Content.HashIndex.(equal zero hash_i) then (
      let prev_hash =
        Commitment.Content.get_previous_hash first_bad_commitment in
      Proof.replay_start proof batch prev_hash
    ) else if Commitment.Content.HashIndex.(equal last_hash_i hash_i) then (
      let prev_hash =
        Commitment.Content.get_hash
          (Commitment.Content.HashIndex.map_int Int.pred hash_i)
          first_bad_commitment
      in
      Proof.replay_finish proof prev_hash ~nb_steps:last_nb_steps
    ) else (
      let prev_hash =
        Commitment.Content.get_hash
          (Commitment.Content.HashIndex.map_int Int.pred hash_i)
          first_bad_commitment
      in
      Proof.replay proof prev_hash ~nb_steps:c_STEPS_PER_COMMIT
    )
  in
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
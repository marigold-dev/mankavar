[@@@warning "-34"]

open Das_helpers
open Oru_param
open Structs

module Bunch = Operation.Bunch
module Op = Operation

let c_STEPS_PER_BLOCK = 1_000_000_000L
let c_STEPS_PER_COMMIT_HASH = 1_000_000L
(* NB_COMMITS_PER_BLOCK_MAX = c_STEPS_PER_BLOCK / c_STEPS_PER_COMMIT = 1_000 *)
let c_STEPS_PER_REJECT_HASH = 100L
(* NB_REJECT_PER_COMMIT_MAX = c_STEPS_PER_COMMIT / c_STEPS_PER_REJECT = 10_000 *)
let c_COMMITMENT_TIMEOUT_IN_BLOCKS = 1000

module State = struct

  type t = {
    height : Height.t ;
    current_batches_rev : Batch.t list ;
    batches : Batch.t list HMap.t ;
    commitments : BatchCommitments.t BMap.t HMap.t ;
    rejections : Rejection.t MapList.t BMap.t HMap.t ;
  }
  [@@deriving ez]

  let encoding : t Encoding.t =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_5
      Height.encoding
      (list Batch.encoding)
      (HMap.encoding (list Batch.encoding))
      (HMap.encoding (BMap.encoding (BatchCommitments.encoding)))
      (HMap.encoding (BMap.encoding (MapList.encoding Rejection.encoding)))

  let do_hash' t = Encoding.to_bytes encoding t |> Crypto.blake2b
  let do_hash = Hash.make do_hash'
  let mk_empty () = make_tpl Height.zero [] HMap.empty HMap.empty HMap.empty

  let current_batches_flush t =
    t
    (* Add the current batches to the current height *)
    |> map_batches (HMap.add (height t) (List.rev @@ current_batches_rev t))
    (* Reset the current batches to [] *)
    |> set_current_batches_rev []
  let current_batches_append b t =
    t |> map_current_batches_rev @@ List.cons b
  let height_increment = map_height Height.increment
  let commitment_append = fun h b c t ->
    let add = HMap.update' h @@ BMap.update' b @@ BatchCommitments.append c in
    t
    |> map_commitments add
  let commitment_get h b c : t -> Commitment.t = fun t ->
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
  let rejection_append = fun h b r t ->
    let add =
      HMap.update' h @@ BMap.update' b @@ MapList.append r
    in
    t
    |> map_rejections add

  let rejection_get = fun h b r t ->
    t.rejections
    |> HMap.find h
    |> BMap.find b
    |> MapList.get (RejectionIndex.to_int r)
  let rejection_remove = fun h b r t ->
    let update_rejections = MapList.remove (RejectionIndex.to_int r) in
    let update_batches = BMap.update' b update_rejections in
    let update_height = HMap.update' h update_batches in
    map_rejections update_height t
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

let commit : Op.Commitment.t -> State.t -> do_operation_result = fun c' t ->
  let (h , b , c) = Op.Commitment.destruct c' in
  t
  |> State.commitment_append h b c
  |> fun state -> { state ; gas = 0L ; bytes = 0L }

let get_commitment_hash_nb_steps batch ch_i =
  let batch_nb_steps = Batch.get_nb_steps batch in
  let nb_hashes = Int64.(succ @@ div batch_nb_steps c_STEPS_PER_COMMIT_HASH) in
  let ch_i' = Commitment.HashIndex.to_int ch_i in
  let nb_hashes' = Int64.to_int nb_hashes in
  assert (ch_i' < nb_hashes') ;
  if ch_i' = nb_hashes' - 1 then 
    Int64.rem batch_nb_steps c_STEPS_PER_COMMIT_HASH
  else c_STEPS_PER_COMMIT_HASH

let get_rejection_hash_nb_steps ch_nb_steps rh_i =
  let nb_hashes = Int64.(succ @@ div ch_nb_steps c_STEPS_PER_REJECT_HASH) in
  let rh_i' = Rejection.HashIndex.to_int rh_i in
  let nb_hashes' = Int64.to_int nb_hashes in
  assert (rh_i' < nb_hashes') ;
  if rh_i' = nb_hashes' - 1 then 
    Int64.rem ch_nb_steps c_STEPS_PER_REJECT_HASH
  else c_STEPS_PER_REJECT_HASH

let reject
: Op.Rejection.t -> State.t -> do_operation_result = fun r' t ->
  let (h , b , _bc , r) = Op.Rejection.destruct r' in
  let hash_i , hashes_between = Rejection.destruct r in
  let batch = State.batch_get h b t in
  let ch_nb_steps = get_commitment_hash_nb_steps batch hash_i in
  let cr_nb_steps =
    Int64.(to_int @@ div ch_nb_steps c_STEPS_PER_REJECT_HASH)
  in
  assert (List.length hashes_between = cr_nb_steps) ;
  let t = State.rejection_append h b r t in
  t
  |> fun state -> { state ; gas = 0L ; bytes = 0L }

let counter_reject
: Op.CounterRejection.t -> State.t -> do_operation_result
= fun cr' t ->
  let (h , b , bc , r_i , cr) = Op.CounterRejection.destruct cr' in
  let rh_i , proof = CounterRejection.destruct cr in
  let ok_commitment = State.commitment_get h b bc t in
  let bad_rejection = State.rejection_get h b r_i t in
  let ch_i = bad_rejection.first_bad_hash in
  let batch = State.batch_get h b t in
  let rh_nb_steps =
    let ch_nb_steps = get_commitment_hash_nb_steps batch ch_i in
    get_rejection_hash_nb_steps ch_nb_steps rh_i
  in
  let replayed_hash =
    if Rejection.HashIndex.(equal zero rh_i) then (
      let prev_hash = Commitment.get_hash
        (Commitment.HashIndex.predecessor ch_i) ok_commitment in
      Proof.replay_start proof batch prev_hash
    ) else (
      let prev_hash = Rejection.get_hash
        (Rejection.HashIndex.predecessor rh_i)
        bad_rejection
      in
      Proof.replay proof prev_hash ~nb_steps:rh_nb_steps
    )
  in
  let first_bad_hash = Rejection.get_hash rh_i bad_rejection in
  assert (first_bad_hash <> replayed_hash) ;
  t
  |> State.rejection_remove h b r_i
  |> fun state -> { state ; gas = 0L ; bytes = 0L }


let flush_block : State.t -> State.t = fun t ->
  t
  |> State.current_batches_flush
  |> State.height_increment

let do_operation : Op.t -> State.t -> do_operation_result = fun op s ->
  Op.destruct ~submit_batch ~commit ~reject ~counter_reject op s

let do_operation' s op =
  let { state ; gas = _ ; bytes = _ } = do_operation op s in
  state

let do_bunch : Bunch.t -> State.t -> State.t = fun bu t ->
  List.fold_left do_operation' t bu
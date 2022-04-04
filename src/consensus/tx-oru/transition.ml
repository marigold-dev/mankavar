[@@@warning "-34-23"]

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
let c_FINALITY_IN_HEIGHT = 3

(* Must be at least 2 bigger than c_FINALITY_IN_HEIGHT *)
let c_FLUSH_PERIOD_IN_HEIGHT = 2 * c_FINALITY_IN_HEIGHT

module State = struct
  (*
  Shortcuts:
  - h(s) -> height (state)
  - b(i)(s) -> batch (index) (state)
  - c(i)(s) -> commitment (index) (state)
  - r(i)(s) -> rejection (index) (state)
  *)


  type commitment_state = {
    commitment : Commitment.t ;
    rejections : Rejection.t MapList.t ;
  }
  [@@deriving ez]
  let commitment_state_encoding =
    let open Encoding in
    conv commitment_state_make_tpl' commitment_state_destruct @@ tuple_2
      Commitment.encoding
      (MapList.encoding Rejection.encoding)
  let commitment_state_pp ppf t =
    let print x = Format.fprintf ppf x in
    print "@[<v>Rejections: #%d@;" @@ MapList.size t.rejections ;
    print "Content: %a@]" Commitment.pp t.commitment ;
    ()
  type batch_state = {
    at_commitments : commitment_state CMap.t ;
    next_commitment_index : Commitment.Index.t ;
    batch : Batch.t ;
  }
  [@@deriving ez]
  let batch_state_encoding =
    let open Encoding in
    conv batch_state_make_tpl' batch_state_destruct @@ tuple_3
      (CMap.encoding commitment_state_encoding)
      Commitment.Index.encoding
      Batch.encoding
  let batch_state_pp ppf t =
    let print x = Format.fprintf ppf x in
    print "@[<v>Next Commitment Index: %a@;"
      Commitment.Index.pp t.next_commitment_index ;
    print "Batch@;" ;
    print "Commitments:@[<v 2>@;%a@]@]"
      (CMap.pp Commitment.Index.pp commitment_state_pp) t.at_commitments ;
    ()
  type height_state = {
    at_batches : batch_state BMap.t ;
    finalized_hash : Hash'.t option ;
  }
  [@@deriving ez]
  let height_state_pp ppf t =
    let print x = Format.fprintf ppf x in
    print "@[<v>Finalized: %a@;" (XOption.pp Hash'.pp) t.finalized_hash ;
    print "Batches:@[<v 2>@;%a@]@]"
      (BMap.pp BatchIndex.pp batch_state_pp) t.at_batches ;
    ()
  let hash_state_empty = height_state_make_tpl BMap.empty Option.none
  let height_state_encoding =
    let open Encoding in
    conv height_state_make_tpl' height_state_destruct @@ tuple_2
      (BMap.encoding batch_state_encoding)
      (option Hash'.encoding)

  type t = {
    height : Height.t ;
    current_batches_rev : Batch.t list ;
    at_heights : height_state HMap.t ;
  }
  [@@deriving ez]
  let pp ppf t =
    let print x = Format.fprintf ppf x in
    print "@[<v>Height %a@;" Height.pp t.height ;
    print "@[<v 2>Heights:@;%a@]"
      (HMap.pp Height.pp height_state_pp) t.at_heights ;
    print "@]" ;
    ()

  let encoding : t Encoding.t =
    let open Encoding in
    conv make_tpl' destruct @@ tuple_3
      Height.encoding
      (list Batch.encoding)
      (HMap.encoding height_state_encoding)

  let do_hash' t = Encoding.to_bytes encoding t |> Crypto.blake2b
  let do_hash = Hash.make do_hash'
  let mk_empty () = make_tpl Height.zero [] HMap.empty

  let current_batches_flush t =
    let make_at_batch =
      batch_state_make_tpl CMap.empty Commitment.Index.zero in
    let current_batches =
      List.rev @@ current_batches_rev t
      |> List.map make_at_batch
      |> List.mapi (fun i x -> (BatchIndex.of_int i , x))
      |> BMap.of_list
    in
    Format.printf "Flush Batches: %a@;%!" Height.pp (height t) ;
    let new_height = height_state_make_tpl current_batches None in
    t
    (* Add the current batches to the current height *)
    |> map_at_heights (HMap.add (height t) new_height)
    (* Reset the current batches to [] *)
    |> set_current_batches_rev []
  let current_batches_append b t =
    t |> map_current_batches_rev @@ List.cons b
  let height_increment = map_height Height.increment
  let commitment_append = fun h bi c t ->
    let cs = commitment_state_make_tpl c MapList.empty in
    t
    |> map_at_heights @@ HMap.update' ~default:hash_state_empty h
    @@ map_at_batches @@ BMap.update' bi (fun bs ->
      let i = bs |> next_commitment_index in 
      bs
      |> map_next_commitment_index Commitment.Index.increment
      |> map_at_commitments (CMap.add i cs)
    )
  let commitment_get h bi ci : t -> Commitment.t = fun t ->
    t
    |> at_heights |> HMap.find h
    |> at_batches |> BMap.find bi
    |> at_commitments |> CMap.find ci
    |> commitment
    
  let batch_get h bi t =
    t
    |> at_heights |> HMap.find h
    |> at_batches |> BMap.find bi
    |> batch
  let rejection_append = fun h bi ci r t ->
    t
    |> map_at_heights @@ HMap.update' h
    @@ map_at_batches @@ BMap.update' bi
    @@ map_at_commitments @@ CMap.update' ci
    @@ map_rejections @@ MapList.append r

  let rejection_get = fun h bi ci ri t ->
    t
    |> at_heights |> HMap.find h
    |> at_batches |> BMap.find bi
    |> at_commitments |> CMap.find ci
    |> rejections |> MapList.get (Rejection.Index.to_int ri)
  let rejection_remove = fun h bi ci ri t ->
    t
    |> map_at_heights @@ HMap.update' h
    @@ map_at_batches @@ BMap.update' bi
    @@ map_at_commitments @@ CMap.update' ci
    @@ map_rejections @@ MapList.remove (Rejection.Index.to_int ri)

  let rejections_flush : Height.t -> t -> t = fun h t ->
    t
    |> map_at_heights @@ HMap.update' h
    @@ map_at_batches @@ BMap.map @@ fun bs ->
    CMap.fold (fun ci cs bs ->
      (*
        If there is a rejection that has not been countered,
        remove the associated commitment
      *)
      if MapList.is_empty (rejections cs) then bs else (
        map_at_commitments (CMap.remove ci) bs
      )
    ) (at_commitments bs) bs




  let commitments_flush
  : Height.t -> t -> t * (Commitment.Index.t * Commitment.t) BMap.t
  = fun h t ->
    (* Remove all commitments that didn't counter their rejections *)
    let t = t |> rejections_flush h in
    (* Retrieve winning commitments *)
    let commitments = t
      |> at_heights |> HMap.find h
      |> at_batches |> BMap.map (fun bs ->
        bs
        |> at_commitments |> CMap.find_first (fun _ -> true)
        |> fun (x , y) -> (x , commitment y)
      )
    in
    (* Apply last commitment *)
    let t =
      t
      |> map_at_heights @@ HMap.update' h (fun hs ->
        let final_hash =
          match BMap.find_last_opt (fun _ -> true) commitments with
          | Some (_ , (_ , last_commitment)) ->
            last_commitment |> Commitment.hashes |> XList.last
          | None -> (
            (* This only happens if a block is literally empty *)
            t
            |> at_heights |> HMap.find (Height.predecessor h)
            |> finalized_hash |> Option.get
          )
        in
        set_finalized_hash (Option.some final_hash) hs
      )
    in
    (t , commitments)
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
  let (h , bi , ci , r) = Op.Rejection.destruct r' in
  let hash_i , hashes_between = Rejection.destruct r in
  let batch = State.batch_get h bi t in
  let ch_nb_steps = get_commitment_hash_nb_steps batch hash_i in
  let cr_nb_steps =
    Int64.(to_int @@ div ch_nb_steps c_STEPS_PER_REJECT_HASH)
  in
  assert (List.length hashes_between = cr_nb_steps) ;
  let t = State.rejection_append h bi ci r t in
  t
  |> fun state -> { state ; gas = 0L ; bytes = 0L }

let counter_reject
: Op.CounterRejection.t -> State.t -> do_operation_result
= fun cr' t ->
  let (h , bi , ci , ri , cr) = Op.CounterRejection.destruct cr' in
  let rh_i , proof = CounterRejection.destruct cr in
  let ok_commitment = State.commitment_get h bi ci t in
  let bad_rejection = State.rejection_get h bi ci ri t in
  let ch_i = bad_rejection.first_bad_hash in
  let batch = State.batch_get h bi t in
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
  |> State.rejection_remove h bi ci ri
  |> fun state -> { state ; gas = 0L ; bytes = 0L }


let flush_block : State.t -> State.t = fun t ->
  let t = t |> State.current_batches_flush in
  let t , _commitments_opt =
    PseudoEffect.returner @@ fun { return } ->
    let final_h =
      Height.map_int (fun x -> x - c_FINALITY_IN_HEIGHT) t.height
    in
    (* Finality is before the start of the chain *)
    if Height.lz final_h then return (t , None) ;
    let t , commitments = State.commitments_flush final_h t in
    (t , Some commitments)
  in
  let t =
    PseudoEffect.returner @@ fun { return } ->
    let flush_h =
      Height.map_int (fun x -> x - c_FLUSH_PERIOD_IN_HEIGHT) t.height
    in
    (* Block to flush is before the start of the chain *)
    if Height.lz flush_h then return t ;
    t |> State.map_at_heights @@ HMap.remove flush_h
  in
  t
  |> State.height_increment

let do_operation : Op.t -> State.t -> do_operation_result = fun op s ->
  Op.destruct ~submit_batch ~commit ~reject ~counter_reject op s

let do_operation' s op =
  let { state ; gas = _ ; bytes = _ } = do_operation op s in
  state

let do_bunch : Bunch.t -> State.t -> State.t = fun bu t ->
  List.fold_left do_operation' t bu
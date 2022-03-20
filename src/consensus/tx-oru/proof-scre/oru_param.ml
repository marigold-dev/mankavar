open Das_helpers
open Structs

module Batch = struct
  type t = Operation.t list
  let encoding : t Encoding.t = assert false
  let get_nb_steps (t : t) =
    t
    |> List.map Operation.get_max_gas
    |> List.fold_left Int64.add 0L
end

module Replay_state = struct
  type t = {
    batch : Batch.t ;
    index : int32 ; (* = Batch.length when finished *)
    patricia_root : Hash'.t ;
    scre_state : Scre.Do_operation.continue option ;
  }
  [@@deriving ez]
  let encoding : t Encoding.t = assert false
  let of_bytes = Encoding.of_bytes encoding
  let to_bytes = Encoding.to_bytes encoding
  let do_hash' t = to_bytes t |> Crypto.blake2b
  let do_hash = Hash.make do_hash'
  let mk_start : Batch.t -> Hash'.t -> t = fun b h -> make_tpl b 0l h None
  let update ~next pr ss t =
    let t' = { t with patricia_root = pr ; scre_state = ss } in
    if next then (
      let i' = Int32.succ t.index in
      { t' with index = i' }
    ) else t'
  let get_current_op t =
    Option.get @@
      List.nth_opt t.batch (Int32.to_int t.index)
  let is_finished t =
    Int32.to_int t.index = List.length t.batch
end


module Proof = struct
  module Regular = struct
    type t = {
      initial_replay_state : Replay_state.t ;
      patricia_trace : Traced_storage.Stream.Consumer.t ;
    }
    [@@deriving ez]
    let encoding : t Encoding.t = assert false
    let of_bytes = Encoding.of_bytes encoding
  end
  module Start = XUnit
  type t =
  | Regular of Regular.t
  | Start of Start.t
  [@@deriving ez]
  let encoding =
    let open Encoding in
    union [
      case get_regular_opt regular Regular.encoding ;
      case get_start_opt start Start.encoding ;
    ]

  let replay_common = fun proof ~nb_steps h ->
    let proof_reg = Option.get @@ get_regular_opt proof in
    let rs , trace = Regular.destruct proof_reg in
    assert (Replay_state.do_hash rs = h) ;
    let module Parameter = struct
      let root = Replay_state.patricia_root rs
      let stream = trace
    end in
    let module State = Patricia_state.ConsumeMake(Parameter) in
    let rec aux rs n =
      let aux_continue ss =
        let pr = State.do_hash () in
        let rs' = Replay_state.update ~next:false pr (Some ss) rs in
        rs'
      in
      let aux_finished n' =
        let pr = State.do_hash () in
        if n > n' then (
          let rs' = Replay_state.update ~next:true pr None rs in
          aux rs' (n - n')
        ) else if n = n' then (
          let rs' = Replay_state.update ~next:true pr None rs in
          rs'
        ) else assert false
      in
      match Replay_state.scre_state rs with
      | None -> (
        let op = Replay_state.get_current_op rs in
        match Scre.start_operation_n (module State) op ~n:nb_steps with
        | Continue ss -> aux_continue ss
        | Finished n' -> aux_finished n'
      )
      | Some scre_state -> (
        match Scre.resume_operation_n (module State) ~n:nb_steps scre_state with
        | Continue ss -> aux_continue ss
        | Finished n' -> aux_finished n'
      )
    in
    aux rs nb_steps
  
  let replay = fun proof ~nb_steps h ->
    let rs = replay_common proof ~nb_steps h in
    Replay_state.do_hash' rs

  let replay_start = fun proof b h ->
    let () = Option.get @@ get_start_opt proof in
    let s = Replay_state.mk_start b h in
    let h' = Replay_state.do_hash s in
    h'

  let replay_finish = fun proof ~nb_steps h ->
    let rs = replay_common proof ~nb_steps h in
    assert (Replay_state.is_finished rs) ;
    rs |> Replay_state.patricia_root

end
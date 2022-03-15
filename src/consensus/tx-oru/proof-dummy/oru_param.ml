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
  let get_nb_steps () = 1L
end

module Proof = struct
  module Regular = struct
    type t = {
      valid : bool ;
      asserted_result : State.t ;
    }
    [@@deriving ez]
    let encoding =
      let open Encoding in
      conv make_tpl' destruct @@ tuple_2
      bool State.encoding
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
  let replay = fun proof ~nb_steps:_ h ->
    let proof_reg = Option.get @@ get_regular_opt proof in
    ignore h ;
    let valid , asserted_result = Regular.destruct proof_reg in
    assert valid ;
    let h' = State.hash' asserted_result in
    h'
  let replay_finish = replay
  let replay_start = fun proof () h ->
    let () = Option.get @@ get_start_opt proof in
    h

end
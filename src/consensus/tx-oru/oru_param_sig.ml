open Das_helpers

module type MAIN = sig
  module Batch : sig
    type t
    val encoding : t Encoding.t
    val get_nb_steps : t -> int64
  end

  module Proof : sig
    type t
    val encoding : t Encoding.t
    val replay : t -> nb_steps:int64 -> Hash'.t -> Hash'.t
    val replay_finish : t -> nb_steps:int64 -> Hash'.t -> Hash'.t
    val replay_start : t -> Batch.t -> Hash'.t -> Hash'.t
  end
end
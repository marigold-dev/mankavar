open Das_helpers

module type MAIN = sig
  module Proof : sig
    type t
    val encoding : t Encoding.t
    val replay : t -> Hash'.t -> Hash'.t
  end

  module Batch : sig
    type t
    val encoding : t Encoding.t
  end
  
end
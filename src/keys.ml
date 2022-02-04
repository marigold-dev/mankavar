module MakeKey() : sig
  type t
  val compare : t -> t -> int
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int
  let compare = Int.compare
  let to_int = fun x -> x
  let of_int = fun x -> x
end

module MakeIndex() = struct
  module Key = MakeKey()
  module Map = Map.Make(Key)
end
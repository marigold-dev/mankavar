module Make(P : sig
  type t
  val x : t
end) = struct
  type t = Param.t * int
  let make i : t = Param.x , i
end
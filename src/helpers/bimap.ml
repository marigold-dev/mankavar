module type PARAMETER = sig
  type t
  val compare : t -> t -> int
end

module Raw(A : PARAMETER)(B : PARAMETER) = struct
  type a = A.t
  type b = B.t
  module AMap = XMap.Make(A)
  module BMap = XMap.Make(B)
  type t = {
    forth : b AMap.t ;
    back : a BMap.t ;
  }
  [@@deriving ez]
  let empty = make_tpl AMap.empty BMap.empty
  let forth_add a b t =
    {
      forth = AMap.add a b t.forth ;
      back = BMap.add b a t.back ;
    }
  let forth_find a t = AMap.find a t.forth
  let back_add b a t =
    {
      forth = AMap.add a b t.forth ;
      back = BMap.add b a t.back ;
    }
  let back_find b t = BMap.find b t.back
  let encoding a b = Encoding.(
    conv make_tpl' destruct @@
    tuple_2 (AMap.encoding a b) (BMap.encoding b a)
  )
end

module type SIG = sig
  type a
  type b
  type t
  val empty : t
  val forth_add : a -> b -> t -> t
  val forth_find : a -> t -> b
  val back_add : b -> a -> t -> t
  val back_find : b -> t -> a
  val encoding : a Encoding.t -> b Encoding.t -> t Encoding.t
end

module Make(A : PARAMETER)(B : PARAMETER)
: SIG with type a = A.t and type b = B.t
= Raw(A)(B)
module type SIG = sig
  (* 
    The goal of this library is to avoid using obsolete representations of
    mutable data.
    It is done with runtime verification.
    - Use `make` to build a runtime-checked ref to some data
    - Use `get` to get the data
    - Use `perform` to update the data
  *)

  type 'a t
  val make : 'a -> 'a t

  exception Obsolete
  (* Raises `Obsolete` when out of sync *)
  val get : 'a t -> 'a

  val assert_live : 'a t -> unit
  val set : 'a t -> 'a -> 'a t
  val perform : 'a t -> ('a ref -> unit) -> 'a t
  val perform' : 'a t -> ('a -> 'a) -> 'a t
end

module Raw = struct
  exception Obsolete

  (*
    Each pointer to the data is tagged with a local and global counter.
    Whenever the data is mutated with a `perform`, the global counter is
    increased.
    If the local and global counter are not equal, it means that you're not
    using the latest version of the data.
  *)
  type 'a t = {
    mutable live : bool ;
    content : 'a ref ;
  }
  [@@deriving ez]

  let make x = { 
   live = true ;
   content = ref x ;
  }
  let assert_live t =
    if not t.live then
      raise Obsolete ;
  [@@inline]
  let get t =
    assert_live t ;
    ! (t.content)
  let perform t f =
    assert_live t ;
    f t.content ;
    t.live <- false ;
    { t with live = true }
  let perform' t f = perform t (fun x -> x := f !x)
  let set t x = perform t (fun r -> r := x)
end

include (Raw : SIG)

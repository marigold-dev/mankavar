module TBytes = struct
  module Raw = struct
    type t = int
    let pp ppf x = Format.fprintf ppf "%d" x
    external malloc
    : int -> int = "int_malloc_byte" "int_malloc"
    [@@noalloc]
    external get_int64
    : int -> (int64[@unboxed]) = "int64_get_byte" "int64_get"
    [@@noalloc]
    external get_int
    : int -> (int) = "int_get_byte" "int_get"
    [@@noalloc]
    external free
    : int -> unit = "int_free_byte" "int_free"
    [@@noalloc]
    (* external free
    : int -> int = "int_free_byte" "int_free" *)
  end

  include (Raw : sig
    type t
    external malloc
    : int -> t = "int_malloc_byte" "int_malloc"
    [@@noalloc]
    external get_int64
    : t -> (int64[@unboxed]) = "int64_get_byte" "int64_get"
    [@@noalloc]
    external get_int
    : t -> (int) = "int_get_byte" "int_get"
    [@@noalloc]
    external free
    : t -> unit = "int_free_byte" "int_free"
    [@@noalloc]
    val pp : Format.formatter -> t -> unit
    (* external free : int -> int = "int_free_byte" "int_free" *)
  end)
end

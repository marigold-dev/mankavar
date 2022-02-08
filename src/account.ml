module RawAddress = struct
  type t = int
  type public_key = t
  type secret_key = t
  let public_key x = x
  let pp_public_key : _ -> public_key -> unit = fun ppf -> Format.fprintf ppf "%d"
  let public_key_equal = Int.equal
  let secret_key x = x
  let counter = ref 0
  let generate () =
    counter := !counter + 1 ;
    !counter
end

module RawSignature = struct
  type 'a t = {
    content : 'a ;
    bytes : bytes ;
    signer : RawAddress.t ;
  }
  [@@deriving ez]

  let compare a b = Int.compare (a |> signer) (b |> signer)
  let equal a b = Int.equal (compare a b) 0

  let pp : _ -> 'a t -> unit = fun ppf x ->
    Format.fprintf ppf "<Signature from %a>"
      RawAddress.pp_public_key
      (RawAddress.public_key @@ signer x) 

  type 'a signed = 'a t
  let pp_signed f : _ -> 'a signed -> unit = fun ppf x ->
    Format.fprintf ppf "<%a> from %a"
      f (x|>content)
      RawAddress.pp_public_key
      (RawAddress.public_key @@ signer x) 
  let get x = x
  let check = fun ~public_key ~content ~signature ~to_bytes ->
    RawAddress.public_key_equal (signature |> signer) public_key &&
    Bytes.equal (to_bytes content) (signature |> bytes)
  let sign = fun ~secret_key ~content ~to_bytes ->
    make_tpl content (to_bytes content) secret_key
end

module type JOINT = sig
  module Address : sig
    type t

    type secret_key
    val secret_key : t -> secret_key

    type public_key
    val public_key : t -> public_key
    val public_key_equal : public_key -> public_key -> bool
    val pp_public_key : Format.formatter -> public_key -> unit

    val generate : unit -> t
  end 
  module Signature : sig
    type 'a t
    val pp : Format.formatter -> 'a t -> unit
    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
    type 'a signed
    val pp_signed : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a signed -> unit
    val get : 'a signed -> 'a t
    val content : 'a signed -> 'a
    val check :
      public_key:Address.public_key -> content:'a ->
      signature:'a t -> to_bytes:('a -> bytes) -> bool
    val sign :
      secret_key:Address.secret_key -> content:'a ->
      to_bytes:('a -> bytes) -> 'a signed
    val signer : 'a t -> Address.public_key
  end
end

include (struct
  module Address = RawAddress
  module Signature = RawSignature
end : JOINT)
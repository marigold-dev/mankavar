module RawAddress = struct
  type t = int
  type public_key = t
  type private_key = t
  let public_key x = x
  let pp_public_key : _ -> public_key -> unit = fun ppf -> Format.fprintf ppf "%d"
  let public_key_equal = Int.equal
  let private_key x = x
  let counter = ref 0
  let generate () =
    counter := !counter + 1 ;
    !counter
end

module RawSignature = struct
  type 'a t = {
    content : 'a ;
    signer : RawAddress.t ;
  }
  [@@deriving ez]
  let pp : _ -> 'a t -> unit = fun ppf x ->
    Format.fprintf ppf "<Signature from %a>"
      RawAddress.pp_public_key
      (RawAddress.public_key @@ signer x) 

  type 'a signed = 'a t
  let pp_signed = pp
  let get x = x
  let check : RawAddress.public_key -> 'a t -> bool = fun k t ->
    t
    |> signer
    |> Int.equal k      
  let sign : RawAddress.private_key -> 'a -> 'a signed = fun signer content ->
    make ~signer ~content 
end

module rec Address : sig
  type t

  type private_key
  val private_key : t -> private_key

  type public_key
  val public_key : t -> public_key
  val public_key_equal : public_key -> public_key -> bool

  val generate : unit -> t
end = RawAddress
and Signature : sig
  type 'a t
  val pp : Format.formatter -> 'a t -> unit
  type 'a signed
  val pp_signed : Format.formatter -> 'a signed -> unit
  val get : 'a signed -> 'a t
  val content : 'a signed -> 'a
  val check : Address.public_key -> 'a t -> bool
  val sign : Address.private_key -> 'a -> 'a signed
  val signer : 'a t -> Address.public_key
end = RawSignature


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
    content : 'a option ;
    bytes : bytes ;
    signer : RawAddress.t ;
  }
  [@@deriving ez]

  let content x = x |> content |> Option.get
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
  let get : 'a signed -> 'a t = fun x -> x
  let check_raw = fun ~public_key ~content ~signature ~to_hash ->
    RawAddress.public_key_equal (signature |> signer) public_key &&
    Bytes.equal (to_hash content) (signature |> bytes)

  let check_hash = fun ~public_key ~signature ~hash ->
    RawAddress.public_key_equal (signature |> signer) public_key &&
    Bytes.equal hash (signature |> bytes)
  let sign_raw = fun ~secret_key ~content ~to_hash ->
    make_tpl (Option.some content) (to_hash content) secret_key
  let sign_hash = fun ~secret_key ~hash ->
    make_tpl Option.none hash secret_key
  let encoding () : 'a t Encoding.t = Encoding.(
    conv
      (fun (x , y) -> make_tpl Option.none x y)
      (fun x -> x |> destruct |> fun (_ , b , s) -> (b , s))
    @@ tuple_2 bytes int
  )
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
    (* Contains signature and signer *)
    type 'a t
    val pp : Format.formatter -> 'a t -> unit
    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
    (* Contains signature, signer and value *)
    type 'a signed
    val pp_signed : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a signed -> unit
    val get : 'a signed -> 'a t
    val content : 'a signed -> 'a
    val check_raw :
      public_key:Address.public_key -> content:'a ->
      signature:'a t -> to_hash:('a -> bytes) -> bool
    val check_hash :
      public_key:Address.public_key ->
      signature:'a t -> hash:bytes -> bool
    val sign_raw :
      secret_key:Address.secret_key -> content:'a ->
      to_hash:('a -> bytes) -> 'a signed
    val sign_hash :
      secret_key:Address.secret_key ->
      hash:bytes -> 'a signed
    val signer : 'a t -> Address.public_key
    val encoding : unit -> 'a t Encoding.t
  end
end

include (struct
  module Address = RawAddress
  module Signature = RawSignature
end : JOINT)
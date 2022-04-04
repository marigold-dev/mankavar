module type S = sig
  include Map.S
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val encoding : key Encoding.t -> 'a Encoding.t -> 'a t Encoding.t
  val update' : ?default:'a -> key -> ('a -> 'a) -> 'a t -> 'a t
  val update_default : key -> 'a -> ('a -> 'a) -> 'a t -> 'a t
  val pp : key XFormat.pp -> 'a XFormat.pp -> 'a t XFormat.pp
end
module Make(P : Map.OrderedType) : S with type key = P.t = struct
  include Map.Make(P)
  let to_list t =
    t
    |> to_seq
    |> List.of_seq
  
  let of_list lst =
    lst
    |> List.to_seq
    |> of_seq

  let update' ?default k f = update k (function
    | None -> Option.map f default
    | Some x -> Some (f x))
  let update_default k d f =
    update k (function None -> Some d | Some x -> Some (f x))
  
  let encoding k e = Encoding.(
    conv of_list to_list @@
    list @@ tuple_2 k e
  )
  let pp k v ppf t = XFormat.(list @@ tuple_2 k v) ppf @@ to_list t
end
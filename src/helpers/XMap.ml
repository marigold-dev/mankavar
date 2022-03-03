module type S = sig
  include Map.S
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val encoding : key Encoding.t -> 'a Encoding.t -> 'a t Encoding.t
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
  
  let encoding k e = Encoding.(
    conv of_list to_list @@
    list @@ tuple_2 k e
  )
end
let value' : (unit -> 'a) -> 'a option -> 'a = fun f opt ->
  match opt with
  | None -> f ()
  | Some x -> x

let fold' ~none ~some = function
| None -> none ()
| Some x -> some x

let fold'' none some = function
| None -> none ()
| Some x -> some x

let bind' f x = Option.bind x f

module GADT = struct
  type full
  type empty
  type ('a , 'b) content =
  | Full : 'a -> ('a , full) content
  | Empty : ('a , empty) content
  type 'a t = Wrap : ('a , 'b) content -> 'a t
end

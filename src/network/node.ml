module Send = struct
  (*
    - A broadcast is a message that is originated
    - A gossip is a message that is forwarded
  *)
  type 'a t =
  | Broadcast of 'a
  | Gossip of 'a
  [@@deriving ez]
end

module type TYPE = sig
  type t
  type message
  val synchronize : Ptime.t -> t -> (message Send.t list * t)
  val process_message : message -> t -> (message Send.t list * t)
end

module Packed = struct
  module type T = sig
    module M : TYPE
    val x : M.t
  end  

  type 'message t = (module T with type M.message = 'message)
  type ('message , 'node) t' = (module T with type M.t = 'node and type M.message = 'message)
  type ('message , 't) node = (module TYPE with type t = 't and type message = 'message)

  type 'msg mapper = {
    mapper : 'a . ('msg , 'a) node -> 'a -> 'a ;
  }
  let map : type msg . msg mapper -> msg t -> msg t = fun m packed ->
    let module Packed = (val packed) in
    let x' = m.mapper (module Packed.M) (Packed.x) in
    let module Packed' = struct
      module M = Packed.M
      let x = x'
    end in
    (module Packed')

  let pack (type a) (type msg)
    (module M : TYPE with type t = a and type message = msg) (x : a)
    : (module T with type M.t = a and type M.message = msg) =
    (module struct
      module M = M
      let x = x  
    end)
  
  let abstract (type a) (type msg)
    (module M : T with type M.t = a and type M.message = msg) : msg t
  = (module M)

  let pack_abstract x y = pack x y |> abstract

  let wrap : type a msg . (msg , a) t' -> msg t = fun (module M) -> (module M)
end

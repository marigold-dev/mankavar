module type REDUCER = sig
  type t
  type msg
  val reduce : msg -> t -> t
end

module Message = struct
  module A = struct
    type t =
    | Add_i of int
    | Add_b
  end
  module B = struct
    type t = int
  end
  module AB = struct
    type t = A of A.t | B of B.t
  end
end


module type B_FOR_A = sig
  type 'a t
  val get : 'a t -> int
  val view : 'a t -> 'a
  val update : 'a -> 'a t -> 'a t
end
module A_make(B : B_FOR_A) = struct
  type msg = Message.A.t

  type content = int * int
  type t = content B.t

  let reduce msg t = match msg with
  | Message.A.Add_i i -> (
    let (bc , c) = B.view t in
    B.update (bc , c + i) t
  )
  | Add_b -> (
    let bi = B.get t in
    let (bc , c) = B.view t in
    B.update (bc , c + bi) t
  )
end


module type A_FOR_B = sig
  type 'a t
  val add : int -> 'a t -> 'a t
  val view : 'a t -> 'a
end
module B_make(A : A_FOR_B) = struct
  type content = int
  type t = content A.t
  type msg = Message.B.t

  let reduce i t =
    let c = A.view t in
    A.add (c + i) t
end
module AB = struct
  type a
  type b
  type t = a * b

  let a_add' = ref (Obj.magic ())
  let a_add x = !a_add' x
  let b_get' = ref (Obj.magic ())
  let b_get x = !b_get' x

  module B_for_A = struct
    type 'a t = ('a * b)
    let get ((_ , b) : 'a t) = b_get b
    let view ((a , _) : 'a t) = a
    let update a ((_ , b) : 'a t) = (a , b)
  end
  module A = A_make(B_for_A)
  let a_concrete : a -> A.content = Obj.magic
  let a_virtual : A.content -> a = Obj.magic
  module A_for_B = struct
    type 'b t = (a * 'b)
    let view ((_ , b) : _ t) = b
    let add i (a , b) = (a_add i a , b)
  end
  module B = B_make(A_for_B)
  let b_concrete : b -> B.content = Obj.magic
  let b_virtual : B.content -> b = Obj.magic

  let () =
    b_get' := (fun x -> b_concrete x) ;
    a_add' := (fun i x -> 
      let (bc , c) = a_concrete x in
      a_virtual (bc + i , c)
    ) ;
    ()
  let make (a : A.content) (b : B.content) : t = (a_virtual a , b_virtual b)
end

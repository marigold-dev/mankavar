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
module A = struct
  type content = int * int

  module Make(B : B_FOR_A) = struct
    type msg = Message.A.t

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
end

module type A_FOR_B = sig
  type 'a t
  val add : int -> 'a t -> 'a t
  val view : 'a t -> 'a
end
module B = struct
  type content = int
  module Make(A : A_FOR_B) = struct
    type t = content A.t
    type msg = Message.B.t

    let reduce i t =
      let c = A.view t in
      A.add (c + i) t
  end
end

module AB = struct
  type t = A.content * B.content

  let a_add i ((bc , c) : A.content) = bc + i , c
  let b_get (b : B.content) = b

  module B_for_A = struct
    type 'a t = ('a * B.content)
    let get ((_ , b) : 'a t) = b_get b
    let view ((a , _) : 'a t) = a
    let update a ((_ , b) : 'a t) = (a , b)
  end
  module A' = A.Make(B_for_A)
  module A_for_B = struct
    type 'b t = (A.content * 'b)
    let view ((_ , b) : 'a t) = b
    let add i (a , b) = (a_add i a , b)
  end
  module B' = B.Make(A_for_B)
  let make (a : A.content) (b : B.content) : t = (a , b)
end

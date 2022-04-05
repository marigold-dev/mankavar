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
  type t
  val get : t -> int
end
module A_make(B : B_FOR_A) = struct
  type msg = Message.A.t

  type b = B.t
  type t = b * int * int
  let reduce msg (bt , bc , c) = match msg with
  | Message.A.Add_i i -> bt , bc , c + i
  | Add_b -> bt , bc , c + (B.get bt)
end
module type A = sig
  type b
  include
    module type of A_make(val (Obj.magic ()) : B_FOR_A with type t = b)
      with type b := b
end

module type A_FOR_B = sig
  type t
  val add : int -> t -> t
end
module B_make(A : A_FOR_B) = struct
  type a = A.t
  type t = a * int
  type msg = Message.B.t

  let reduce i (at , c) =
    A.add i at , c + i
end
module type B = sig
  type a
  include
    module type of B_make(val (Obj.magic ()) : A_FOR_B with type t = a)
      with type a := a
end

module AB = struct
module rec A : A = A_make(AB.B_for_A)
and B : B = B_make(AB.A_for_B)
and AB : sig
  type t = A.t * B.t
  module A_for_B : A_FOR_B
  module B_for_A : B_FOR_A
  val make : A.t -> B.t -> t
end = struct
  type t = A.t * B.t
  let make x y = x , y
  module A_for_B : A_FOR_B = struct
    type nonrec t = t
    let add i ((bt , bc , c) , b) = ((bt , bc + i , c) , b)
  end
  module B_for_A : B_FOR_A = struct
    type nonrec t = t
    let get (_a , (_ , c)) = c
  end
end
end

(* 
  Doesn't work.
  - Too Verbose
  - Recursive Values are cursed in OCaml
  - Alternative would be an effectful interface
 *)
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
  type a
  type b
  type t = a * b
  module A_for_B = struct
    type nonrec t = t
    let add' = ref (Obj.magic ())
    let add i x = !add' i x
  end
  module B_for_A = struct
    type nonrec t = t
    let get' = ref (Obj.magic ())
    let get x = !get' x
  end
  module A = A_make(B_for_A)
  module B = B_make(A_for_B)
  let a_virtual : A.t -> a = Obj.magic
  let a_concrete : a -> A.t = Obj.magic
  let b_virtual : B.t -> b = Obj.magic
  let b_concrete : b -> B.t = Obj.magic
  let () =
    A_for_B.add' := (fun i (av , b) ->
      let (bt , bc , c) = a_concrete av in
      a_virtual (bt , bc + i , c) , b
    ) ;
    B_for_A.get' := (fun (_a , bv) ->
      let (_ , c) = b_concrete bv in
      c
    ) ;
    ()
end

(* 
  Doesn't work.
  - Too Verbose
  - Recursive Values are cursed in OCaml
  - Alternative would be an effectful interface
 *)
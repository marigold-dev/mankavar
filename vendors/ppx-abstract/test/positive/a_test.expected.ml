module My_int_container :
  sig
    type nonrec ints
    val of_ints : ints -> int list
    val to_ints : int list -> ints
  end =
  struct type nonrec ints = int list
         let of_ints x = x
         let to_ints x = x end 

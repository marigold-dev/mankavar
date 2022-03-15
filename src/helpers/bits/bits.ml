type t = bool list
let of_char c =
  let r = ref [] in
  let n = ref @@ Char.code c in
  for _ = 1 to 8 do
    r := (!n mod 2 = 1) :: !r ;
    n := !n / 2 ;
  done ;
  !r
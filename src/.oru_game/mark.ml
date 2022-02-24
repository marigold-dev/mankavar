(* 
  step_nb >= 0
*)
type t = {
  step_nb : int64 ;
  state_hash : Bytes.t ;
}
[@@deriving ez , eq]
let make' (a , b) = make_tpl a b


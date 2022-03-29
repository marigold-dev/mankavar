open Das_helpers
open Structs

type t = {
  (* Network *)
  network : string ;
  (* Identities *)
  self : Addr.t ;
  endorsers : Addr.public_key list ;
  (* Blocks *)
  height : Height.t ;
  blocks : Blocks.t ;
  (* State *)
  chain_state : Transition.State.t ;
  mempool : Mempool.t ;
}

let height_get t = t.height
let height_set t height = { t with height }
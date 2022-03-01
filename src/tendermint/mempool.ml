open Das_helpers

let do_hash : bytes -> bytes = Do_hash.blake2b

module Op = Transition.Operation
type op = Op.t
let do_op_hash' op =
  op
  |> Encoding.to_bytes Op.encoding 
let do_op_hash = Hash.make do_op_hash'



module Raw = struct
(*
  Worst mempool possible:
  - all operations are accepted
  - except if the max size is reached, at which points nothing happens
*)
let max_size = 100_000

let next_id = ref 0L
let get_next_id () = next_id := Int64.succ !next_id ; !next_id

type item = {
  content : op ;
  id : int64 ;
  hash : op Hash.t ;
}
[@@deriving ez]

let item_make op =
  item_make_tpl
    op (get_next_id ()) @@ do_op_hash op

type t = item list

let empty = []

let add x t =
  let item = item_make x in
  if List.length t >= max_size then t else
  if List.exists (fun x -> Hash.compare item.hash x.hash = 0) t
  then t
  else item :: t
let remove hash t =
  List.filter (fun x -> Hash.compare x.hash hash <> 0) t

let get ~gas_limit t =
  let rec aux gas kept_rev lst =
    match lst with
    | [] -> List.rev kept_rev , lst
    | hd :: tl  ->
      let gas = Int64.(add gas (Op.get_max_gas hd.content)) in
      if XInt64.(gas <= gas_limit)
      then aux gas (hd.content :: kept_rev) tl
      else List.rev kept_rev , lst
  in 
  aux 0L [] t
end

module type SIG = sig
  (* Discard extra operations *)
  val max_size : int

  type t

  val empty : t
  val add : op -> t -> t 
  val remove : op Hash.t -> t -> t
  val get : gas_limit:Int64.t -> t -> op list * t
end

include (Raw : SIG)
open Das_helpers

module Operation = struct
  type t = unit
  let get_max_gas () = 1L
  (* let get_height () = Height.zero *)
  let encoding : t Encoding.t = Encoding.unit
  let do_hash = Hash.make @@ fun () -> Bytes.empty
  let do_hash' () = Bytes.empty
  let pp ppf () = Format.dprintf "<noop operation>" ppf
end

module Bunch = struct
  let max_gas = 100_000_000L
  type t = Operation.t list
  let of_list lst = lst
  let encoding : t Encoding.t = Encoding.(list Operation.encoding)
  let to_list (x : t) = x
end

module State = struct
  type t = unit
  let mk_empty () = ()
  let do_hash () = Hash.make (Fun.const Hash'.dummy) ()
end

type do_operation_result = {
  state : State.t ;
  gas : int64 ;
  bytes : int64 ;
}
let do_operation
: Operation.t -> State.t -> do_operation_result
= fun () state ->
{ state ; gas = 0L ; bytes = 0L }

let do_bunch
: Bunch.t -> State.t -> State.t
= fun _ state -> state
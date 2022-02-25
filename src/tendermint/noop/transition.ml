open Das_helpers

module Operation = struct
  type t = unit
  let get_max_gas () = 1L
  let encoding : t Encoding.t = Encoding.unit
end

module Bunch = struct
  let max_gas = 100_000_000L
  type t = Operation.t list
  let dummy = []
  let encoding : t Encoding.t = Encoding.(list Operation.encoding)
end

module State = struct
  type t = unit
  let empty = ()
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
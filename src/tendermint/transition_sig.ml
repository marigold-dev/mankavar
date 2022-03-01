open Das_helpers

module type MAIN = sig

module Operation : sig
  type t
  (* max gas that can be used to process the operation *)
  val get_max_gas : t -> int64
  val encoding : t Encoding.t
  val pp : Format.formatter -> t -> unit
end

module Bunch : sig
  (*
    Maximal number of gas to be executed in the bunch
    Assuming 1 bunch / block and 1 block / sec, matches the required 1 core
    minimal requirement
  *)
  val max_gas : int64

  type t
  val dummy : t
  val encoding : t Encoding.t
end

module State : sig
  type t
  val empty : t
  val do_hash : t -> t Hash.t
end

(*
  - gas : number of operations run
  - bytes : number of new bytes persisted
  - gas <= Operation.get_max_gas op
  - if gas/bytes overflow, output.state = input.state
*)
type do_operation_result = {
  state : State.t ;
  gas : int64 ;
  bytes : int64 ;
}
val do_operation : Operation.t -> State.t -> do_operation_result
val do_bunch : Bunch.t -> State.t -> State.t

end
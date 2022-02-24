open Das_helpers

module type MAIN = sig

module Operation : sig
  type t
  (*
  max gas that can be executed to process the operation
  *)
  val get_max_gas : t -> int64
  val encoding : t Encoding.t
end

module Bunch : sig
  (*
    Maximal number of gas to be executed in the bunch
    Assuming 1 bunch / block and 1 block / sec, matches the required 1 core
    minimal requirement
  *)
  val max_gas : int64

  type t
end

module State : sig
  type t
end

module Transition : sig
  (*
    - gas : number of operations run
    - bytes : number of new bytes persisted
  *)
  type do_operation_result = {
    state : State.t ;
    gas : int64 ;
    bytes : int64 ;
  }
  val do_operation : State.t -> Operation.t -> do_operation_result
  val do_bunch : State.t -> Bunch.t -> State.t
end

end
open Vm_helpers

type value = Value.t
module VMap = Value.Map
type stack = Stack.t

type register = A | B | C | D

type op1 = Not

type op2 = Add | Sub | Mul | Div | And | Or

(*
  Instructions extended with a case for compile time stuff
*)
type 'ct instruction =
| Jump_value of value
| Jump of register
| JumpGez of register * register (* if greater x1, jump to x2 *)
| Stack_push of register (* Stack.push <- r *)
| Stack_get of int * register (* r <- stack[i] *)
| Stack_set of int * register (* stack[i] <- r *)
| Stack_drop of int
| Register_set_value of register * value (* r <- value *)
| Register_set of register * register (* r1 <- r2 *)
| Call_op1 of op1 * register * register (* r1 <- op r2 *)
| Call_op2 of op2 * register * register * register (* r1 <- op r2 r3 *)
| Custom of int (* external *)
| Compile_time of 'ct
| Halt
[@@deriving ez]

type empty = |

(*
  Runtime instructions don't have compile_time case
*)
type rt_instruction = empty instruction

type 'mem state = {
  instructions : rt_instruction array ;
  mutable instruction_pointer : value ;
  mutable a : value ;
  mutable b : value ;
  mutable c : value ;
  mutable d : value ;
  mutable memory : 'mem ;
  stack : stack ;
}
let state_empty instructions ip memory = {
  instructions ;
  instruction_pointer = ip ;
  a = Value.zero ;
  b = Value.zero ;
  c = Value.zero ;
  d = Value.zero ;
  memory = memory ;
  stack = Stack.empty ;
}

type program = {
  code : rt_instruction list ;
  start_pointer : int ;
}
[@@deriving ez]

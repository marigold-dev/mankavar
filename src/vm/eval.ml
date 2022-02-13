open Vm_helpers
open Types

let get_register : _ state -> register -> value = fun s r ->
  match r with
  | A -> s.a
  | B -> s.b
  | C -> s.c
  | D -> s.d
[@@inline]

let set_register : _ state -> register -> value -> unit = fun s r v ->
  match r with
  | A -> s.a <- v
  | B -> s.b <- v
  | C -> s.c <- v
  | D -> s.d <- v
[@@inline]

let goto : _ state -> value -> unit = fun s v ->
  s.instruction_pointer <- v
[@@inline]

let next : _ state -> unit = fun s ->
  goto s @@ Value.succ s.instruction_pointer
[@@inline]

let eval_op1 op v =
  match op with
  | Not -> Value.not_ v
[@@inline]
let eval_op2 op v1 v2 =
  match op with
  | Add -> Value.add v1 v2
  | Sub -> Value.sub v1 v2
  | Mul -> Value.mul v1 v2
  | Div -> Value.div v1 v2
  | Or -> Value.or_ v1 v2
  | And -> Value.and_ v1 v2
[@@inline]

type continue =
| Continue
| Stop
let step custom : _ state -> continue = fun s ->
  match Array.get s.instructions (s.instruction_pointer |> Int64.to_int)  with
  | Jump r -> goto s (get_register s r) ; Continue
  | Jump_value v -> goto s v ; Continue
  | JumpGez (r1 , r2) -> (
    if Value.gez (get_register s r1)
    then goto s (get_register s r2)
    else next s ;
    Continue
  )
  | Stack_push r ->
    Stack.push s.stack (get_register s r) ;
    Continue
  | Stack_get (i , r) ->
    set_register s r @@ Stack.get s.stack i ;
    Continue
  | Stack_set (i , r) ->
    Stack.set s.stack i @@ get_register s r ;
    Continue
  | Stack_drop i ->
    Stack.drop s.stack i ;
    Continue
  | Register_set_value (r , v) ->
    set_register s r v ;
    Continue
  | Register_set (r1 , r2) ->
    set_register s r1 @@ get_register s r2 ;
    Continue
  | Call_op1 (op , r1 , r2) ->
    set_register s r1 @@ eval_op1 op (get_register s r2) ;
    Continue
  | Call_op2 (op , r1 , r2 , r3) ->
    set_register s r1 @@ eval_op2 op (get_register s r2) (get_register s r3) ;
    Continue
  | Halt -> () ; Stop
  | Custom c -> custom s c ; Continue
  | Compile_time _ -> .
[@@inline]

let eval custom : program -> _ -> _ = fun p m ->
  let (code , ip) = program_destruct p in
  let code' = Array.of_list code in
  let state = state_empty code' (Value.of_int ip) m in
  let r = ref Continue in
  while !r = Continue do
    r := step custom state ;
  done ;
  state.memory

module ReadWrite = struct
  type memory = Value.t VMap.t
  let custom state i = match i with
  | 0 ->
    set_register state B @@
    VMap.find
      (get_register state A)
      state.memory
  | 1 ->
    state.memory <-
    VMap.add
      (get_register state A)
      (get_register state B)
      state.memory
  | _ -> assert false
  let read = 0
  let write = 1
  let eval = eval custom
end
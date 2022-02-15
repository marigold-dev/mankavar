open Vm_helpers

type value = Value.t
module VMap = Value.Map
type stack = Stack.t

type register = A | B | C | D
let register_pp ppf x =
  Format.fprintf ppf "r%s" @@ match x with
  | A -> "a"
  | B -> "b"
  | C -> "c"
  | D -> "d"

type op1 = Not
[@@deriving ez]
let op1_pp ppf x =
  let print x = Format.fprintf ppf "%s" x in
  print @@ op1_destruct_tpl ("Not") x

type op2 =
| Add
| Sub
| Mul
| Div
| And
| Or
[@@deriving ez]
let op2_pp ppf x =
  let print x = Format.fprintf ppf x in
  let print_s = print "%s" in
  print_s @@
  op2_destruct_tpl
    "+" "-" "*" "/"
    "&&" "||"
  x
(*
  Instructions extended with a case for compile time stuff
*)
type 'ct instruction =
| Jump_value of value
| Jump of register
| JumpGez of register * register (* if greater x1, jump to x2 *)
| Stack_push of register (* Stack.push r *)
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

let instruction_pp ~compile_time ppf x =
  let print x = Format.dprintf x in
  let vpp = Value.pp in
  let rpp = register_pp in
  let o1pp = op1_pp in
  let o2pp = op2_pp in
  instruction_destruct
  ~jump_value:(print "jmp to %a" vpp)
  ~jump:(print "jmp to %a" rpp)
  ~jumpgez:(fun r1 r2 -> print "jmp>%a to %a" rpp r1 rpp r2)
  ~stack_push:(print "stack push %a" rpp)
  ~stack_get:(fun i r -> print "stack[%d] -> %a" i rpp r)
  ~stack_set:(fun i r -> print "stack[%d] <- %a" i rpp r)
  ~stack_drop:(print "stack[0:%d] drop")
  ~register_set_value:(fun r v -> print "%a <- %a" rpp r vpp v)
  ~register_set:(fun r1 r2 -> print "%a <- %a" rpp r1 rpp r2)
  ~call_op1:(fun op r1 r2 -> print "%a <- %a %a" rpp r1 o1pp op rpp r2)
  ~call_op2:(fun op r1 r2 r3 ->
    print "%a <- %a %a %a" rpp r1 rpp r2 o2pp op rpp r3
  )
  ~halt:(print "halt")
  ~custom:(fun i -> print "custom %d" i)
  ~compile_time
  x ppf

type empty = |

(*
  Runtime instructions don't have compile_time case
*)
type rt_instruction = empty instruction
let rt_instruction_pp : _ -> rt_instruction -> _ =
  let compile_time : empty -> _ = function _ -> . in
  instruction_pp ~compile_time

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
let state_pp f ppf s =
  let {
    instructions ; instruction_pointer = ip ;
    a ; b ; c ; d ;
    memory ; stack ;
  } = s in
  let print x = Format.fprintf ppf x in
  print "@[<v>" ;
  print "@[<v 2>  " ;
  instructions |> Array.iteri (fun i instr -> (
    print "%d:\t%a" i rt_instruction_pp instr ;
    if i = Int64.to_int ip then (
      print "\t<--- current" ;
    ) ;
    print "@;" ;
  )) ;
  print "@]" ;
  print "ra:%a\trb:%a\trc:%a\trd:%a@;"
    Value.pp a Value.pp b Value.pp c Value.pp d ;
  print "%a@;" Stack.pp stack ;
  print "%a@;" f memory ;
  print "@]" ;
  ()

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

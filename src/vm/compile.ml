module I = C_like
module O = Types

module Environment = struct
  type t = string list
  let empty = []
  let rec get x i : t -> int = function
  | [] -> assert false
  | hd :: _ when hd = x -> i
  | _ :: tl -> get x (i + 1) tl
  let get x t = get x 0 t
  let push_dummy t = "" :: t
  let push x t : t = x :: t
end
type environment = Environment.t

module FunctionEnvironment = struct
  (* 
    - Each function is mapped to its position
    - Its position is the sum of all previous fun lens
    - Last is the last line
  *)
  type t = {
    content : (string * int) list ;
    last : int ;
  }
  [@@deriving ez]
  let empty = make_tpl [] 0
  let get : _ -> t -> _ = fun x t -> t |> content |> List.assoc x

  (* Pushes function `v` that is `l` lines long *)
  let push v l t : t =
    t
    |> map_content (fun lst -> (v , l + (last t)) :: lst)
    |> map_last (fun i -> i + l)
end
type function_environment = FunctionEnvironment.t

let compile_op1 : I.op1 -> O.op1 = function Not -> Not
[@@inline]
let compile_op2 : I.op2 -> O.op2 = function
| Add -> Add | Sub -> Sub | Mul -> Mul | Div -> Div
| And -> And | Or -> Or
[@@inline]


type ct_instruction =
| Register_set_shift of O.register * int (* r <- CURRENT_POSITION + i*)
[@@deriving ez]

type pre_instruction = ct_instruction O.instruction

let preprocess_compile_time i : ct_instruction -> O.rt_instruction = fun ct ->
  match ct with
  | Register_set_shift (r , i') ->
    O.register_set_value r @@ Vm_helpers.Value.of_int (i + i')

let preprocess_instruction i : pre_instruction -> O.rt_instruction = 
  let open O in
  O.instruction_destruct
    ~jump_value ~jump ~jumpgez ~stack_push ~stack_get
    ~stack_set ~stack_drop ~register_set_value
    ~register_set ~call_op1 ~call_op2 ~custom ~halt
    ~compile_time:(preprocess_compile_time i)

let preprocess : pre_instruction list -> O.rt_instruction list =
  List.mapi preprocess_instruction

(*
  - Result of the expression is put on register A
  - Before stack should = after stack
*)
let rec compile_expression
: function_environment -> environment -> I.expression -> pre_instruction list
= fun fenv env e ->
  let self = compile_expression fenv in
  match e with
| Literal i -> [ O.register_set_value A i ]
| Variable v ->
  [O.stack_get (Environment.get v env) A]
| Application (v , e) -> (
  self env e @ [
    O.compile_time @@ register_set_shift B 2 ;
    O.jump_value @@ Vm_helpers.Value.of_int @@ FunctionEnvironment.get v fenv
  ]
)
| Assignment (v , e) -> (
  self env e @
  [O.stack_set (Environment.get v env) A] 
)
| Call_op1 (op , e) -> (
  let op' = compile_op1 op in
  self env e @
  [O.call_op1 op' A A]
)
| Call_op2 (op , e1 , e2) -> (
  let op' = compile_op2 op in
  self env e1 @ (
    O.stack_push A ::
    self (Environment.push_dummy env) e2
  ) @ [
    O.stack_get 0 B ;
    O.stack_drop 1 ;
    O.call_op2 op' A B A ;
  ]
)

(*
  - Param is on register A
  - Return pointer is on register B
  - Result is on register A
*)
let compile_function
: function_environment -> I.function_ -> pre_instruction list
= fun fenv f ->
  let (v , body) = I.function__destruct f in
  let env = ref @@ Environment.(push v empty) in
  let instrs_rev = ref [] in
  let push_instr x = instrs_rev := x :: !instrs_rev in
  let push_instrs xs = List.iter push_instr xs in
  push_instr @@ O.stack_push B ;
  push_instr @@ O.stack_push A ;
  List.iter (fun s -> (
    match s with
  | C_like.Do e ->
    push_instrs @@ compile_expression fenv !env e
  | Declaration (v , e) -> (
    push_instrs @@ compile_expression fenv !env e ;
    push_instr @@ O.stack_push A ;
    env := Environment.(push v !env) ;
  )
  | Return e -> (
    push_instrs @@ compile_expression fenv !env e ;
    push_instr @@ O.stack_get 1 B ;
    push_instr @@ O.jump B ;    
  )
  )) body ;
  List.rev !instrs_rev

let compile_program
: I.program -> O.program
= fun lst ->
  let fenv = ref FunctionEnvironment.empty in
  let fun_code =
    List.map (fun f ->
      let instrs = compile_function !fenv f in
      fenv := FunctionEnvironment.push
        (I.variable f) (List.length instrs) !fenv ;
      instrs
    ) lst
    |> List.concat
  in
  let pre_code =
    let run =
      compile_expression !fenv Environment.empty @@
      I.application "main" (I.literal 0L)
    in
    fun_code @ run
  in
  let code = preprocess pre_code in
  let start_pointer = FunctionEnvironment.last !fenv in
  O.program_make ~code ~start_pointer
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
  let size t = List.length t
  let push_dummy t = "" :: t
  let push x t : t = x :: t
  let pop_n n t : t =
    let rec aux n lst =
      if n = 0
      then lst
      else aux (n - 1) @@ List.tl lst
    in
    aux n t
end
type environment = Environment.t

module FunctionEnvironment = struct
  (* 
    - Each function is mapped to its label
    - Its position is the sum of all previous fun lens
    - Last is the last line
  *)
  type t = (string * int) list
  [@@deriving ez]
  let empty = []
  let get : _ -> t -> _ = fun x t -> t |> List.assoc x

  (* Pushes function `v` that is `l` lines long *)
  let push v l t : t = (v , l) :: t
end
type function_environment = FunctionEnvironment.t

let compile_op1 : I.op1 -> O.op1 = function Not -> Not
[@@inline]
let compile_op2 : I.op2 -> O.op2 = function
| Add -> Add | Sub -> Sub | Mul -> Mul | Div -> Div
| And -> And | Or -> Or
[@@inline]


type ct_instruction =
| Register_set_shift of O.register * int (* r <- CURRENT_POSITION + i *)
| Register_set_label of O.register * int (* r <- POSITION_OF_LABEL i *)
| Label of int
| Comment of string
[@@deriving ez]

let ct_instruction_pp ppf x =
  let print x = Format.fprintf ppf x in
  let open Types in
  match x with
  | Register_set_shift (r , i) -> print "shift %d -> %a" i register_pp r
  | Register_set_label (r , i) -> print "label %d -> %a" i register_pp r
  | Label i -> print "label %d" i
  | Comment str -> print "// %s" str

type pre_instruction = ct_instruction O.instruction
let pre_instruction_pp = Types.instruction_pp ~compile_time:ct_instruction_pp
let pre_program_pp = Types.program_pp ~compile_time:ct_instruction_pp

let preprocess_compile_time lst i : ct_instruction -> O.rt_instruction = fun ct ->
  match ct with
  | Register_set_shift (r , i') ->
    O.register_set_value r @@ Vm_helpers.Value.of_int (i + i')
  | Register_set_label (r , i') ->
    O.register_set_value r @@ Vm_helpers.Value.of_int @@ List.assoc i' lst
  | Label _ -> assert false
  | Comment _ -> assert false

let preprocess_instruction lst i : pre_instruction -> O.rt_instruction = 
  let open O in
  instruction_destruct
    ~jump_value ~jump ~jumpgz ~jumplez ~stack_push ~stack_get
    ~stack_set ~stack_drop ~register_set_value
    ~register_set ~call_op1 ~call_op2 ~custom ~halt
    ~compile_time:(preprocess_compile_time lst i)

let preprocess : pre_instruction list -> O.rt_instruction list * _ = fun lst ->
  let label_positions , lst' =
    let rec aux acc_pos acc_rem i lst =
      match lst with
      | [] -> List.rev acc_pos , List.rev acc_rem
      | O.Compile_time (Label j) :: tl ->
        aux ((j , i) :: acc_pos) acc_rem i tl
      | O.Compile_time (Comment _) :: tl -> aux acc_pos acc_rem i tl
      | hd :: tl -> aux acc_pos (hd :: acc_rem) (i + 1) tl
    in
    aux [] [] 0 lst
  in
  List.mapi (preprocess_instruction label_positions) lst' , label_positions

let label_assoc_pp : _ -> (label * int) list -> unit = fun ppf lst ->
  let print x = Format.fprintf ppf x in
  print "@[<v>" ;
  lst |> List.iter (fun (l , i) -> 
    print "label %d -> %d@;" l i
  ) ;
  print "@]" ;
  ()

let i_comment str = O.compile_time @@ comment str

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
  [O.compile_time @@ comment "app" ] @ self env e @ [
    O.compile_time @@ register_set_shift B 3 ;
    O.compile_time @@ register_set_label C @@ FunctionEnvironment.get v fenv ;
    O.jump C ;    
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
  [i_comment "op2"] @
  self env e1 @ (
    O.stack_push A ::
    self (Environment.push_dummy env) e2
  ) @ [
    O.stack_get 0 B ;
    O.stack_drop 1 ;
    O.call_op2 op' A B A ;
  ] @ [i_comment "op2 end"]
)

let rec compile_block ~fenv ~env ~label_r : I.block -> pre_instruction list
= fun block ->
  let self = compile_block ~fenv ~env ~label_r in
  let next_label () = label_r := !label_r + 1 ; !label_r in
  let instrs_rev = ref [] in
  let push_instr x = instrs_rev := x :: !instrs_rev in
  let push_instrs xs = List.iter push_instr xs in
  let envc = ref 0 in
  let push_env v =
    env := Environment.(push v !env) ;
    envc := !envc + 1 ;
  in
  push_instr @@ i_comment "block" ;
  List.iter (fun s -> (
    match s with
  | C_like.Do e ->
    push_instrs @@ compile_expression fenv !env e
  | Declaration (v , e) -> (
    push_instrs @@ compile_expression fenv !env e ;
    push_instr @@ O.stack_push A ;
    push_env v ;
  )
  | Assignment (v , e) -> (
    push_instrs @@ compile_expression fenv !env e ;
    let i = Environment.get v !env in
    push_instr @@ O.stack_set i A ;
  )
  | Loop_gz (e , b) -> (
    push_instr @@ i_comment "loop" ;
    let la_precond = next_label () in
    let la_postbody = next_label () in
    push_instr @@ O.compile_time @@ label la_precond ;
    push_instrs @@ compile_expression fenv !env e ;
    push_instr @@ O.compile_time @@ register_set_label B la_postbody ;
    push_instr @@ O.jumplez A B ;
    push_instrs @@ self b ;
    push_instr @@ O.compile_time @@ register_set_label B la_precond ;
    push_instr @@ O.jump B ;
    push_instr @@ O.compile_time @@ label la_postbody ;
  )
  | Return e -> (
    push_instrs @@ compile_expression fenv !env e ;
    push_instr @@ O.stack_get (Environment.size !env) B ;
    push_instr @@ O.stack_drop (!envc + 2) ;
    push_instr @@ O.jump B ;    
  )
  )) block ;
  push_instr @@ O.stack_drop !envc ;
  env := Environment.pop_n !envc !env ;
  List.rev !instrs_rev

(*
  - Param is on register A
  - Return pointer is on register B
  - Result is on register A
*)
let compile_function f_label label_r
: function_environment -> I.function_ -> pre_instruction list
= fun fenv f ->
  let (name , v , body) = I.function__destruct f in
  let env = ref @@ Environment.(push v empty) in
  let instrs_rev = ref [] in
  let push_instr x = instrs_rev := x :: !instrs_rev in
  let push_instrs xs = List.iter push_instr xs in
  push_instr @@ O.compile_time @@ comment ("function " ^ name) ;
  push_instr @@ O.compile_time @@ label f_label ;
  push_instr @@ O.stack_push B ;
  push_instr @@ O.stack_push A ;
  push_instrs @@ compile_block ~fenv ~env ~label_r body ;
  List.rev !instrs_rev

let precompile_program
: I.program -> _
= fun lst ->
  let label_r = ref 0 in
  let next_label () = label_r := !label_r + 1 ; !label_r in
  let fenv = ref FunctionEnvironment.empty in
  let fun_code =
    List.map (fun f ->
      let f_label = next_label () in
      let instrs = compile_function f_label label_r !fenv f in
      fenv := FunctionEnvironment.push
        (I.name f) f_label !fenv ;
      instrs
    ) lst
    |> List.concat
  in
  let label_run = next_label () in
  let pre_code =
    let run =
      [O.compile_time @@ label label_run] @
      compile_expression !fenv Environment.empty @@
      I.application "main" (I.literal 0L)
    in
    fun_code @ run @ [ O.halt ]
  in
  pre_code , label_run

let compile_program : I.program -> O.program = fun lst ->
  let pre_code , label_run = precompile_program lst in
  let code , label_positions = preprocess pre_code in
  let start_pointer = List.assoc label_run label_positions in
  O.program_make ~code ~start_pointer

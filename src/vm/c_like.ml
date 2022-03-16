[@@@warning "-34"]
type variable = string

type op1 = Not
[@@deriving ez]

module Op2 = struct
  type t =
  | Add | Sub | Mul | Div
  | And | Or
  [@@deriving ez]
end
type op2 = Op2.t


type expression =
| Literal of int64
| Variable of variable
| Application of variable * expression
| Assignment of variable * expression
| Call_op1 of op1 * expression
| Call_op2 of op2 * expression * expression
[@@deriving ez]

type statement =
| Do of expression
| Declaration of variable * expression
| Assignment of variable * expression
| Loop_gz of expression * block
| Return of expression
[@@deriving ez]

and block = statement list

type function_ = {
  name : string ;
  input : variable ;
  body : block ;
}
[@@deriving ez]

type program = function_ list

let add x y = call_op2 Op2.Add x y
let sub x y = call_op2 Op2.Sub x y

let function_ = function__make_tpl

let expression_to_program e = [
  function__make_tpl "main" "_" [
    return e ;
  ]
]
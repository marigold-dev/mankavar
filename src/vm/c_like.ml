type variable = string

type op1 = Not
[@@deriving ez]

type op2 =
| Add | Sub | Mul | Div
| And | Or
[@@deriving ez]

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
| Return of expression
[@@deriving ez]

type function_ = {
  variable : variable ;
  body : statement list ;
}
[@@deriving ez]

type program = function_ list


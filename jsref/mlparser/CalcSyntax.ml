type position = {
  line : int ; [@f line]
  column : int [@f column]
}

type location = {
  file : string ; [@f file]
  start : position ; [@f start]
  stop : position [@f stop]
}

type token_value =
| Int of int [@f value]
| Float of float [@f value]
| Char of char [@f value]
| String of string [@f value]
| Kwd of string [@f value]
| Ident of string [@f value]

type 'a located_value = {
  loc : location ; [@f loc]
  value : 'a [@f value]
}

type token = token_value located_value

type literal =
| Literal_int of location * int [@f loc, value]
| Literal_float of location * float [@f loc, value]

type unary_op =
| Unary_op_add of location [@f loc]
| Unary_op_sub of location [@f loc]

type binary_op =
| Binary_op_add of location [@f loc]
| Binary_op_sub of location [@f loc]
| Binary_op_mul of location [@f loc]
| Binary_op_div of location [@f loc]

type expr =
| Expr_literal of location * literal [@f loc, value]
| Expr_unary_op of location * unary_op * expr [@f loc, op, arg]
| Expr_binary_op of location * expr * binary_op * expr [@f loc, arg1, op, arg2]

let get_location = function
| Expr_literal (loc, _) -> loc
| Expr_unary_op (loc, _, _) -> loc
| Expr_binary_op (loc, _, _, _) -> loc

let get_literal_location = function
| Literal_int (loc, _) -> loc
| Literal_float (loc, _) -> loc

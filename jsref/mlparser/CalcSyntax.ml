type literal =
| Literal_int of int [@f value]
| Literal_float of float [@f value]

type unary_op =
| Unary_op_add
| Unary_op_sub

type binary_op =
| Binary_op_add
| Binary_op_sub
| Binary_op_mul
| Binary_op_div
| Binary_op_mod

type expr =
| Expr_literal of literal [@f value]
| Expr_unary_op of unary_op * expr [@f op, arg]
| Expr_binary_op of expr * binary_op * expr [@f arg1, op, arg2]

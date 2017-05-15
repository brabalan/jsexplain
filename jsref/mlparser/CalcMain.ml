open CalcSyntax
open CalcParser

let rec print_expr = function
| Expr_literal lit -> print_literal lit
| Expr_unary_op (op, e) -> print_unary_op op ^ print_expr e
| Expr_binary_op (arg1, op, arg2) -> "(" ^ print_expr arg1 ^ print_binary_op op ^ print_expr arg2 ^ ")"

and print_literal = function
| Literal_int i -> string_of_int i
| Literal_float f -> string_of_float f

and print_unary_op = function
| Unary_op_add -> "+"
| Unary_op_sub -> "-"

and print_binary_op = function
| Binary_op_add -> "+"
| Binary_op_sub -> "-"
| Binary_op_mul -> "*"
| Binary_op_div -> "/"
| Binary_op_mod -> " mod "

let () =
  let code = Sys.argv.(1) in
  let ast = parse_string code in
  print_endline (print_expr ast)

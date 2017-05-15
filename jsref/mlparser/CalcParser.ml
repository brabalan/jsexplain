open CalcSyntax
open Genlex

let symbols = [ "+" ; "-" ; "*" ; "/" ; "mod" ; "(" ; ")" ]
let make_calc_lexer s = make_lexer symbols (Stream.of_string s)

let parse_literal = parser
| [< 'Int i >] -> Literal_int i
| [< 'Float f >] -> Literal_float f

let rec parse_binary_op stream =
  let rec parse_term_op =
    let rec aux left = parser
    | [< 'Kwd "+" ; right = parse_factor_op ; s >] -> aux (Expr_binary_op (left, Binary_op_add, right)) s
    | [< 'Kwd "-" ; right = parse_factor_op ; s >] -> aux (Expr_binary_op (left, Binary_op_sub, right)) s
    | [< >] -> left
    in parser [< left = parse_factor_op ; s >] -> aux left s

  and parse_factor_op =
    let rec aux left = parser
    | [< 'Kwd "*" ; right = parse_leaf ; s >] -> aux (Expr_binary_op (left, Binary_op_mul, right)) s
    | [< 'Kwd "/" ; right = parse_leaf ; s >] -> aux (Expr_binary_op (left, Binary_op_div, right)) s
    | [< 'Kwd "mod" ; right = parse_leaf ; s >] -> aux (Expr_binary_op (left, Binary_op_mod, right)) s
    | [< >] -> left
    in parser [< left = parse_leaf ; s >] -> aux left s

  and parse_unary_op = parser
  | [< 'Kwd "+" ; e = parse_leaf >] -> Expr_unary_op (Unary_op_add, e)
  | [< 'Kwd "-" ; e = parse_leaf >] -> Expr_unary_op (Unary_op_sub, e)

  and parse_leaf = parser
  | [< 'Kwd "(" ; e = parse_binary_op ; 'Kwd ")" >] -> e
  | [< u = parse_unary_op >] -> u
  | [< lit = parse_literal >] -> Expr_literal lit

  in parse_term_op stream

let parse_expr = parse_binary_op

let parse_string s =
  let stream = make_calc_lexer s in
  parse_expr stream

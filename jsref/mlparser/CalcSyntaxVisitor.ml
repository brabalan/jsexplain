open CalcSyntax

(* OCaml to JS conversion functions *)
let call_js_constructor name = Js.Unsafe.fun_call (Js.Unsafe.js_expr name) [| |]
let call_js_constructor1 name value = Js.Unsafe.fun_call (Js.Unsafe.js_expr name) [| Js.Unsafe.inject value |]
let call_js_constructorArray name values = Js.Unsafe.fun_call (Js.Unsafe.js_expr name) values

let js_of_literal = function
| Literal_int i -> call_js_constructor1 "CalcSyntax.Literal_int" i
| Literal_float f -> call_js_constructor1 "CalcSyntax.Literal_float" f

let js_of_unary_op = function
| Unary_op_add -> call_js_constructor "CalcSyntax.Unary_op_add"
| Unary_op_sub -> call_js_constructor "CalcSyntax.Unary_op_sub"

let js_of_binary_op = function
| Binary_op_add -> call_js_constructor "CalcSyntax.Binary_op_add"
| Binary_op_sub -> call_js_constructor "CalcSyntax.Binary_op_sub"
| Binary_op_mul -> call_js_constructor "CalcSyntax.Binary_op_mul"
| Binary_op_div -> call_js_constructor "CalcSyntax.Binary_op_div"
| Binary_op_mod -> call_js_constructor "CalcSyntax.Binary_op_mod"

let rec js_of_expr = function
| Expr_literal lit ->
  let js_lit = js_of_literal lit in
  call_js_constructor1 "CalcSyntax.Expr_literal" js_lit
| Expr_unary_op (op, arg) ->
  let js_op = js_of_unary_op op in
  let js_arg = js_of_expr arg in
  call_js_constructorArray "CalcSyntax.Expr_unary_op" [| js_op ; js_arg |]
| Expr_binary_op (arg1, op, arg2) ->
  let js_arg1 = js_of_expr arg1 in
  let js_op = js_of_binary_op op in
  let js_arg2 = js_of_expr arg2 in
  call_js_constructorArray "CalcSyntax.Expr_binary_op" [|js_arg1 ; js_op ; js_arg2|]

let () =
  Js.export "CalcParserLib"
  (object%js
    method parseExpr s =
      let ast = CalcParser.parse_string (Js.to_string s) in
      js_of_expr ast
    method parseLiteral s =
      let stream = CalcParser.make_calc_lexer (Js.to_string s) in
      let lit = CalcParser.parse_literal stream in
      js_of_literal lit
  end)

open CalcSyntax

(* OCaml to JS conversion functions *)
let js_of_position pos = Js.Unsafe.obj [|
  ("line", Js.Unsafe.inject pos.line) ;
  ("column", Js.Unsafe.inject pos.column) |]

let js_of_location loc =
  let js_file = Js.Unsafe.inject loc.file in
  let js_start = js_of_position loc.start in
  let js_stop = js_of_position loc.stop in
  Js.Unsafe.obj [| ("file", js_file) ; ("start", js_start) ; ("end", js_stop) |]

let call_js_constructor name loc =
  let js_loc = js_of_location loc in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr name) [| js_loc |]
let call_js_constructor1 name loc value =
  let js_loc = js_of_location loc in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr name) [| js_loc ; Js.Unsafe.inject value |]
let call_js_constructorArray name values = Js.Unsafe.fun_call (Js.Unsafe.js_expr name) values

let js_of_literal = function
| Literal_int (loc, i) -> call_js_constructor1 "CalcSyntax.Literal_int" loc i
| Literal_float (loc, f) -> call_js_constructor1 "CalcSyntax.Literal_float" loc f

let js_of_unary_op = function
| Unary_op_add loc -> call_js_constructor "CalcSyntax.Unary_op_add" loc
| Unary_op_sub loc -> call_js_constructor "CalcSyntax.Unary_op_sub" loc

let js_of_binary_op = function
| Binary_op_add loc -> call_js_constructor "CalcSyntax.Binary_op_add" loc
| Binary_op_sub loc -> call_js_constructor "CalcSyntax.Binary_op_sub" loc
| Binary_op_mul loc -> call_js_constructor "CalcSyntax.Binary_op_mul" loc
| Binary_op_div loc -> call_js_constructor "CalcSyntax.Binary_op_div" loc

let rec js_of_expr = function
| Expr_literal (loc, lit) ->
  let js_lit = js_of_literal lit in
  call_js_constructor1 "CalcSyntax.Expr_literal" loc js_lit
| Expr_unary_op (loc, op, arg) ->
  let js_loc = js_of_location loc in
  let js_op = js_of_unary_op op in
  let js_arg = js_of_expr arg in
  call_js_constructorArray "CalcSyntax.Expr_unary_op" [| js_loc ; js_op ; js_arg |]
| Expr_binary_op (loc, arg1, op, arg2) ->
  let js_loc = js_of_location loc in
  let js_arg1 = js_of_expr arg1 in
  let js_op = js_of_binary_op op in
  let js_arg2 = js_of_expr arg2 in
  call_js_constructorArray "CalcSyntax.Expr_binary_op" [| js_loc ; js_arg1 ; js_op ; js_arg2|]

let () =
  Js.export "CalcParserLib"
  (object%js
    method parseExpr name s =
      let ast = CalcParser.parse_string (Js.to_string name) (Js.to_string s) in
      js_of_expr ast
    method parseLiteral s =
      let stream = CalcParser.Lexer.make_calc_lexer (Js.to_string s) in
      let lit = CalcParser.parse_literal stream in
      js_of_literal lit
  end)

open Lexing
open Location
open Longident
open Asttypes
open Parsetree
open MLSyntax

(***************************************************************************************
 * Translation from an OCaml-side AST to a JS-side one
 ***************************************************************************************)

let translate_location file loc =
  let loc_start = loc.loc_start in
  let loc_stop = loc.loc_end in
  let start = {
    line = loc_start.pos_lnum ;
    column = loc_start.pos_cnum - loc_start.pos_bol } in
  let stop = {
    line = loc_stop.pos_lnum ;
    column = loc_stop.pos_cnum - loc_stop.pos_bol } in
  { file = file ; start = start ; stop = stop }

let translate_constant = function
| Pconst_integer (i, _) -> Constant_integer (int_of_string i)
| Pconst_float (f, _) -> Constant_float (float_of_string f)
| Pconst_char c -> Constant_char c
| Pconst_string (s, _) -> Constant_string s

let rec translate_expression file e = match e.pexp_desc with
| Pexp_constant pc ->
  let c = translate_constant pc in
  Expression_constant ((translate_location file e.pexp_loc), c)
| Pexp_ident li ->
  let loc = translate_location file li.loc in
  let id = Longident.last (li.txt) in
  Expression_ident (loc, id)
| Pexp_let (ir, [binding], exp) ->
  let is_rec = (ir = Recursive) in
  let patt = translate_pattern file binding.pvb_pat in
  let val_exp = translate_expression file binding.pvb_expr in
  let expr = translate_expression file exp in
  Expression_let (translate_location file e.pexp_loc, is_rec, patt, val_exp, expr)
| Pexp_tuple pel ->
  let el = List.map (fun pe -> translate_expression file pe) pel in
  Expression_tuple (translate_location file e.pexp_loc, Array.of_list el)

and translate_pattern file p = match p.ppat_desc with
| Ppat_any -> Pattern_any (translate_location file p.ppat_loc)
| Ppat_constant c -> Pattern_constant (translate_location file p.ppat_loc, translate_constant c)
| Ppat_var li ->
  let loc = translate_location file li.loc in
  let id = li.txt in
  Pattern_var (loc, id)

(***************************************************************************************
 * Translation from an OCaml-side AST to a JS-side one
 ***************************************************************************************)

let js_of_position pos = Js.Unsafe.obj [|
  ("line", Js.Unsafe.inject pos.line) ;
  ("column", Js.Unsafe.inject pos.column) |]

let js_of_location loc =
  let js_file = Js.Unsafe.inject (Js.string loc.file) in
  let js_start = js_of_position loc.start in
  let js_stop = js_of_position loc.stop in
  Js.Unsafe.obj [| ("file", js_file) ; ("start", js_start) ; ("end", js_stop) |]

let js_of_located translator value =
  let js_value = translator value.value in
  let js_loc = js_of_location value.loc in
  Js.Unsafe.obj [| ("loc", js_loc) ; ("value", js_value) |]

let js_of_constant = function
| Constant_integer i ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Constant_integer") [| Js.Unsafe.inject i |]
| Constant_float f ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Constant_float") [| Js.Unsafe.inject f |]
| Constant_char c ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Constant_char") [| Js.Unsafe.inject c |]
| Constant_string s ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Constant_string") [| Js.Unsafe.inject s |]

let rec js_of_expression = function
| Expression_constant (loc, c) ->
  let js_c = js_of_constant c in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Expression_constant") [| js_of_location loc ; js_c |]
| Expression_ident (loc, id) ->
  let js_ident = Js.Unsafe.inject (Js.string id) in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Expression_ident") [| js_of_location loc ; js_ident |]
| Expression_let (loc, is_rec, patt, val_exp, expr) ->
  let js_loc = js_of_location loc in
  let js_rec = Js.Unsafe.inject is_rec in
  let js_patt = js_of_pattern patt in
  let js_val_exp = js_of_expression val_exp in
  let js_expr = js_of_expression expr in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Expression_let") [| js_loc ; js_rec ; js_patt ; js_val_exp ; js_expr |]
| Expression_tuple (loc, el) ->
  let js_loc = js_of_location loc in
  let js_tuples = Array.map js_of_expression el in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Expression_tuple") [| js_loc ; Js.Unsafe.inject (Js.array js_tuples) |]

and js_of_pattern = function
| Pattern_any loc -> Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Pattern_any") [| js_of_location loc |]
| Pattern_constant (loc, c) ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "MLSyntax.Pattern_constant") [| js_of_location loc ; js_of_constant c |]
| Pattern_var (loc, id) ->
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "MLSyntax.Pattern_var")
    [| js_of_location loc ; Js.Unsafe.inject (Js.string id) |]


let () =
  Js.export "MLExplain"
  (object%js
    method parseExpr name str =
      let filename = Js.to_string name in
      let s = Js.to_string str in
      let lexbuffer = from_string s in
      let past = Parse.expression lexbuffer in
      let ast = translate_expression filename past in
      js_of_expression ast
  end)

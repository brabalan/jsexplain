open Lexing
open Location
open Longident
open Asttypes
open Typedtree
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
| Const_int i -> Constant_integer i
| Const_float f -> Constant_float (float_of_string f)
| Const_char c -> Constant_char c
| Const_string (s, _) -> Constant_string s

let rec translate_expression file e =
  let loc = translate_location file e.exp_loc in
  match e.exp_desc with
  | Texp_constant pc ->
    let c = translate_constant pc in
    Expression_constant (loc, c)
  | Texp_ident (_, li, _) ->
    let id = Longident.last (li.txt) in
    Expression_ident (loc, id)
  | Texp_let (ir, [binding], exp) ->
    let is_rec = (ir = Recursive) in
    let patt = translate_pattern file binding.vb_pat in
    let val_exp = translate_expression file binding.vb_expr in
    let expr = translate_expression file exp in
    Expression_let (loc, is_rec, patt, val_exp, expr)
  | Texp_function (_, cases, _) ->
    let map_f case = {
      patt = translate_pattern file case.c_lhs ;
      expr = translate_expression file case.c_rhs
    } in
    let case_array = Array.of_list (List.map map_f cases) in
    Expression_function (loc, case_array)
  | Texp_apply (pfunc, pargs) ->
    let bind opt func = match opt with
    | None -> None
    | Some v -> func v in
    let func = translate_expression file pfunc in
    let opt_list = List.map
      (fun (_, pe) -> bind pe (fun p -> Some (translate_expression file p)))
      pargs in
    let rec lift_option lst = match lst with
    | [] -> Some []
    | h :: t ->
      bind h (fun a ->
      bind (lift_option t) (fun rest ->
      Some (a :: rest))) in
    let args =
      match lift_option opt_list with
      | None -> failwith "labels not supported"
      | Some a -> a in
    Expression_apply (loc, func, Array.of_list args)
  | Texp_tuple pel ->
    let el = List.map (fun pe -> translate_expression file pe) pel in
    Expression_tuple (loc, Array.of_list el)
  | Texp_array pel ->
    let el = List.map (fun pe -> translate_expression file pe) pel in
    Expression_array (loc, Array.of_list el)
  | Texp_variant (label, eopt) ->
    let value_opt = Option.bind eopt (fun e -> Some (translate_expression file e)) in
    Expression_variant (loc, label, value_opt)
  | Texp_match (pexp, pcases, _, _) ->
    let map_f case = {
      patt = translate_pattern file case.c_lhs ;
      expr = translate_expression file case.c_rhs
    } in
    let cases = Array.of_list (List.map map_f pcases) in
    let expr = translate_expression file pexp in
    Expression_match (loc, expr, cases)

and translate_pattern file p =
  let loc = translate_location file p.pat_loc in
  match p.pat_desc with
  | Tpat_any -> Pattern_any loc
  | Tpat_constant c -> Pattern_constant (loc, translate_constant c)
  | Tpat_var (_, li) ->
    let id = li.txt in
    Pattern_var (loc, id)
  | Tpat_tuple ppatts ->
    let patt_list = List.map (translate_pattern file) ppatts in
    let patts = Array.of_list patt_list in
    Pattern_tuple (loc, patts)
  | Tpat_array ppatts ->
    let patt_list = List.map (translate_pattern file) ppatts in
    let patts = Array.of_list patt_list in
    Pattern_array (loc, patts)
  | Tpat_variant (label, popt, _) ->
    let patt_opt = Option.bind popt (fun p -> Some (translate_pattern file p)) in
    Pattern_variant (loc, label, patt_opt)

(***************************************************************************************
 * Translation from an OCaml-side AST to a JS-side one
 ***************************************************************************************)

let ctor_call ctor params = Js.Unsafe.fun_call (Js.Unsafe.js_expr ctor) params

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

let js_of_option func opt = match opt with
| None -> ctor_call "None" [| |]
| Some v ->
  let js_v = func v in
  ctor_call "Some" [| js_v |]

let js_of_constant = function
| Constant_integer i -> ctor_call "MLSyntax.Constant_integer" [| Js.Unsafe.inject i |]
| Constant_float f -> ctor_call "MLSyntax.Constant_float" [| Js.Unsafe.inject f |]
| Constant_char c -> ctor_call "MLSyntax.Constant_char" [| Js.Unsafe.inject c |]
| Constant_string s -> ctor_call "MLSyntax.Constant_string" [| Js.Unsafe.inject s |]

let rec js_of_expression = function
| Expression_constant (loc, c) ->
  let js_c = js_of_constant c in
  ctor_call "MLSyntax.Expression_constant" [| js_of_location loc ; js_c |]
| Expression_ident (loc, id) ->
  let js_ident = Js.Unsafe.inject (Js.string id) in
  ctor_call "MLSyntax.Expression_ident" [| js_of_location loc ; js_ident |]
| Expression_let (loc, is_rec, patt, val_exp, expr) ->
  let js_loc = js_of_location loc in
  let js_rec = Js.Unsafe.inject is_rec in
  let js_patt = js_of_pattern patt in
  let js_val_exp = js_of_expression val_exp in
  let js_expr = js_of_expression expr in
  ctor_call "MLSyntax.Expression_let" [| js_loc ; js_rec ; js_patt ; js_val_exp ; js_expr |]
| Expression_variant (loc, label, value_opt) ->
  let js_loc = js_of_location loc in
  let js_label = Js.Unsafe.inject (Js.string label) in
  let js_value_opt = js_of_option js_of_expression value_opt in
  ctor_call "MLSyntax.Expression_variant" [| js_loc ; js_label ; js_value_opt |]
| Expression_function (loc, cases) ->
  let js_loc = js_of_location loc in
  let js_cases = Js.Unsafe.inject (Js.array (Array.map js_of_case cases)) in
  ctor_call "MLSyntax.Expression_function" [| js_loc ; js_cases |]
| Expression_apply (loc, func, args) ->
  let js_loc = js_of_location loc in
  let js_func = js_of_expression func in
  let js_args = Js.Unsafe.inject (Js.array (Array.map js_of_expression args)) in
  ctor_call "MLSyntax.Expression_apply" [| js_loc ; js_func ; js_args |]
| Expression_tuple (loc, el) ->
  let js_loc = js_of_location loc in
  let js_tuples = Array.map js_of_expression el in
  let js_tuple_array = Js.Unsafe.inject (Js.array js_tuples) in
  ctor_call "MLSyntax.Expression_tuple" [| js_loc ; js_tuple_array |]
| Expression_array (loc, el) ->
  let js_loc = js_of_location loc in
  let js_elements = Array.map js_of_expression el in
  let js_array = Js.Unsafe.inject (Js.array js_elements) in
  ctor_call "MLSyntax.Expression_array" [| js_loc ; js_array |]
| Expression_match (loc, expr, cases) ->
  let js_loc = js_of_location loc in
  let js_expr = js_of_expression expr in
  let js_cases = Js.Unsafe.inject (Js.array (Array.map js_of_case cases)) in
  ctor_call "MLSyntax.Expression_match" [| js_loc ; js_expr ; js_cases |]

and js_of_pattern = function
| Pattern_any loc -> ctor_call "MLSyntax.Pattern_any" [| js_of_location loc |]
| Pattern_constant (loc, c) ->
  ctor_call "MLSyntax.Pattern_constant" [| js_of_location loc ; js_of_constant c |]
| Pattern_var (loc, id) ->
  let js_id = Js.Unsafe.inject (Js.string id) in
  ctor_call "MLSyntax.Pattern_var" [| js_of_location loc ; js_id |]
| Pattern_tuple (loc, patts) ->
  let js_patts = Js.Unsafe.inject (Js.array (Array.map js_of_pattern patts)) in
  ctor_call "MLSyntax.Pattern_tuple" [| js_of_location loc ; js_patts |]
| Pattern_array (loc, patts) ->
  let js_patts = Js.Unsafe.inject (Js.array (Array.map js_of_pattern patts)) in
  ctor_call "MLSyntax.Pattern_array" [| js_of_location loc ; js_patts |]
| Pattern_variant (loc, label, patt_opt) ->
  let js_label = Js.Unsafe.inject (Js.string label) in
  let js_patt_opt = js_of_option js_of_pattern patt_opt in
  ctor_call "MLSyntax.Pattern_variant" [| js_of_location loc ; js_label ; js_patt_opt |]

and js_of_case case =
  let js_patt = js_of_pattern case.patt in
  let js_expr = js_of_expression case.expr in
  Js.Unsafe.obj [| ("patt", js_patt) ; ("expr", js_expr) |]


let () =
  Js.export "MLExplain"
  (object%js
    method parseExpr name str =
      let filename = Js.to_string name in
      let s = Js.to_string str in
      let lexbuffer = from_string s in
      let past = Parse.expression lexbuffer in
      let typed_ast = Typecore.type_expression Env.empty past in
      let ast = translate_expression filename typed_ast in
      js_of_expression ast
  end)

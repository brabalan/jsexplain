open Asttypes
open Attributes
open Env
open Format
open Lexing
open Location
open Log
open Longident
open Misc
open Mytools
open Parse_type
open Print_type
open Types
open Typedtree
  
let hashtbl_size = 256
let type_tbl     = Hashtbl.create hashtbl_size
let record_tbl   = Hashtbl.create hashtbl_size
let module_list  = ref []
let module_code  = ref []
module L = Logged (Token_generator) (struct let size = 256 end)
  
(**
 * Debug-purpose functions
 *)
  
let print_type_tbl () =
  let rec print_str_list = function
    | [] -> ""
    | x :: [] -> (Printf.sprintf {|"%s"|} x)
    | x :: xs -> (Printf.sprintf {|"%s", |} x) ^ print_str_list xs
  in Hashtbl.iter (fun cstr elems -> Printf.printf ({|%s -> [%s]|} ^^ "\n") cstr (print_str_list elems)) type_tbl; ()

  
let env_diff_names env1 env2 =
  List.map Ident.unique_name (Env.diff env1 env2)

(**
 *  Functions to work with environment
 **)

let rec list_of_ident_from_summary = function
  | Env_empty -> []
  | Env_value (sum, id, vd) -> id :: list_of_ident_from_summary sum
  | Env_type (sum,_,_)
  | Env_extension (sum,_,_)
  | Env_module (sum,_,_)
  | Env_modtype (sum,_,_)
  | Env_class (sum,_,_)
  | Env_cltype (sum,_,_)
  | Env_open (sum,_)
  | Env_functor_arg (sum,_) -> list_of_ident_from_summary sum

let print_name_list l =
  let rec aux = function
    | [] -> ""
    | x :: [] -> x
    | x :: xs -> x ^ ", " ^ aux xs
  in "[ " ^ aux l ^ " ]"

let print_env env =
  let idents = env
               |> Env.summary
               |> list_of_ident_from_summary
               |> List.map Ident.name in
  Printf.printf "env: %s\n" (print_name_list idents)
                            
(**
 * Useful functions (Warning: shadows `show_list' from Mytools)
 *)
    
let show_list_f f sep l = l
  |> List.map f
  |> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) ""

let show_list sep l =
  List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) "" l

let is_sbool x = List.mem x ["true" ; "false"] 

let rec zip l1 l2 = match l1, l2 with
  | [], x :: xs | x :: xs, [] -> failwith "zip: list must have the same length."
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let unzip l = 
  let rec aux acc1 acc2 = function
  | [] -> List.rev acc1, List.rev acc2
  | (x, y) :: xs -> aux (x :: acc1) (y :: acc2) xs
in aux [] [] l

let is_infix f args = match args with
  | _ :: [] | [] -> false
  | x :: xs ->
     let open Location in
     let f_loc = (f.exp_loc.loc_start, f.exp_loc.loc_end) in
     let args_loc = (x.exp_loc.loc_start, x.exp_loc.loc_end) in
     if fst args_loc < fst f_loc then true else false
                                               
(**
 * Before-hand definitions of Pretty-Printer-Format for converting ocaml
 * to ECMAScript, therefore all of them are in a single place.
 *)

let ppf_lambda_wrap s =
  Printf.sprintf "(function () {@;<1 2>@[<v 0>%s@]@,}())" s
  
let ppf_branch case binders expr =
  Printf.sprintf "%s: @[<v 0>%s@,return %s;@]"
                 case binders expr

let ppf_let_in decl exp =
  let s =
    Printf.sprintf "%s@,return %s;"
                   decl exp
  in ppf_lambda_wrap s

let ppf_function args body=
  Printf.sprintf "function (%s) {@;<1 2>@[<v 0>return %s;@]@,}"
                 args body

let ppf_apply f args =
  Printf.sprintf "@[<v 0>%s(%s)@]"
                 f args

let ppf_apply_infix f arg1 arg2 =
  Printf.sprintf "@[<v 0>%s %s %s@]"
                 arg1 f arg2
    
let ppf_match value cases =
  let s =
    Printf.sprintf "switch (%s.type) {@,@[<v 0>%s@]@,}"
                   value cases
  in ppf_lambda_wrap s

let ppf_array values =
  Printf.sprintf "@[<v 0>[%s]@]"
                 values
                 
let ppf_tuple = ppf_array
    
let ppf_ifthen cond iftrue =
  Printf.sprintf "(function () {@,@,@[<v 2>@,if (%s) {@,@[<v 2>@,return  %s;@]@,}@]@,})()"
                 cond iftrue

let ppf_ifthenelse cond iftrue iffalse =
  Printf.sprintf "(function () {@,@,@[<v 2>@,if (%s) {@,@[<v 2>@,return  %s;@]@,} else {@,@[<v 2>@,return  %s;@]@,}@]@,})()"
                 cond iftrue iffalse

let ppf_sequence exp1 exp2 =
  Printf.sprintf "return %s,@,%s"
                 exp1 exp2

let ppf_while cd body =
  let s =
    Printf.sprintf "@[<v 0>@,while(%s) {@,@[<v 2>@,%s@]@,}@]"
                   cd body
  in ppf_lambda_wrap s
                     
let ppf_for id start ed flag body =
  let fl_to_string = function
    | Upto   -> "++"
    | Downto -> "--" in
  let fl_to_symbl = function
    | Upto   -> "<="
    | Downto -> ">=" in
  let s =
    Printf.sprintf "[<v 0>@,for (%s = %s ; %s %s %s ; %s%s) {@,@[<v 2>@,%s @]@,}@]"
                   id start id (fl_to_symbl flag) ed (fl_to_string flag) id body
  in ppf_lambda_wrap s

(*let ppf_single_cstr tag =
  Printf.sprintf "%s"
    tag
*)
let ppf_cstr tag value =
  Printf.sprintf "%s: %s"
    tag value

let ppf_single_cstrs typ =
   Printf.sprintf "@[<v 0>{type: \"%s\"}@]"
     typ
      
let ppf_multiple_cstrs typ rest =
  Printf.sprintf "@[<v 0>{type: \"%s\", @[<v 2>%s@]}@]"
    typ rest

let ppf_record llde =
  let rec aux acc = function
    | []               -> Printf.sprintf "@[<v 0>{@,@[<v 2>@,%s@,@]}@]" acc
    | (lbl, exp) :: [] -> aux (acc ^ Printf.sprintf "%s: %s" lbl exp) []
    | (lbl, exp) :: xs -> aux (acc ^ Printf.sprintf "%s: %s,@," lbl exp) xs
  in aux "" llde

let ppf_decl ?(mod_gen="") id expr =
  let assign_op, decl_kw, end_mark = if mod_gen = "" then " = ", "var ", ";" else ": ", "", "," in 
  Printf.sprintf "@[<v 0>%s%s%s%s%s@,@]" 
                 decl_kw id assign_op expr end_mark

let ppf_pat_array id_list array_expr =
  Printf.sprintf "@[<v 0>var __%s = %s;@,@]" "array" array_expr ^
    List.fold_left2 (fun acc (name, exp_type) y -> acc ^ Printf.sprintf "@[<v 0>var %s = __%s[%d];@,@]" name "array" y)
                    "" id_list @@ range 0 (List.length id_list - 1)
                 

(**
 * Module gestion part
 *)

let find_module_path mod_list =
  let open Config in
  let rec expand_names = function
    | [] -> []
    | x :: xs -> List.map (fun path -> Filename.concat path ((String.lowercase x) ^ ".ml")) !load_path 
                 :: expand_names xs in
  let first_valid paths = match List.filter Sys.file_exists paths with
    | [] -> None
    | x :: xs -> Some x in
  let rec prune = function
    | [] -> []
    | x :: xs -> match first_valid x with
        | None -> failwith "Unbound module"
        | Some m -> m :: prune xs in
  let res = zip mod_list (prune @@ expand_names @@ mod_list)
  in module_list := []; res

let rec parse_modules = function
  | [] -> []
  | (name, path) :: xs ->
   let ppf = Format.std_formatter in
   let (opt, inputfile) = process_implementation_file ppf path in
   let ((parsetree1 : Parsetree.structure), typedtree1) =
      match opt with
      | None -> failwith "Could not read and typecheck input file"
      | Some (parsetree1, (typedtree1, _)) -> parsetree1, typedtree1
      in
   let (_, unlogged, _) = to_javascript ~mod_gen:name typedtree1 in
   Printf.sprintf "%s = {\n%s\n}" name unlogged :: parse_modules xs
    
(**
 * Main part
 *)

and to_javascript ?(mod_gen="") typedtree =
  let pre_res = js_of_structure ~mod_gen Env.empty typedtree in
  let logged, unlogged, pre = L.logged_output pre_res, L.unlogged_output pre_res, pre_res in
  let mod_code = String.concat "\n" !module_code in
  (logged, mod_code ^ "\n" ^ unlogged, pre)

and show_value_binding ?(mod_gen="") vb =
  js_of_let_pattern ~mod_gen vb.vb_pat vb.vb_expr
    
and js_of_structure ?(mod_gen="") old_env s =
  let new_env = s.str_final_env in
  show_list_f (fun strct -> js_of_structure_item ~mod_gen new_env strct) "@,@," s.str_items
    
and js_of_structure_item ?(mod_gen="") old_env s =
  let new_env = s.str_env in
  match s.str_desc with
  | Tstr_eval (e, _)     -> Printf.sprintf "%s" @@ js_of_expression ~mod_gen new_env e
  | Tstr_value (_, vb_l) -> String.concat "@,@," @@ List.map (fun vb -> show_value_binding ~mod_gen vb) @@ vb_l
  | Tstr_type tl ->
   let explore_type = function
      | [] -> ()
      | x :: xs ->
        (match x.typ_kind with
        | Ttype_variant cdl ->
          let cl = List.map (fun cstr -> extract_cstr_attrs cstr) cdl in
          List.iter (fun (name, cstrs_name) -> Hashtbl.add type_tbl (if mod_gen <> "" then mod_gen ^ "." ^ name else name) cstrs_name) cl;
          print_type_tbl ()
        | Ttype_record ldl ->
          (* Beware silent shadowing for record labels *)
          List.iter (fun lbl -> Hashtbl.replace record_tbl (Ident.name lbl.ld_id) (Ident.name x.typ_id)) ldl
        | _ -> unsupported "open types, record and abstract type"
        )
   in explore_type tl; ""
  | Tstr_open       od -> 
    let name = (fun od -> if od.open_override = Fresh then js_of_longident od.open_txt else "") od in
    if name <> "" then
      module_list := name :: !module_list;
      module_code := parse_modules @@ find_module_path @@ !module_list @ !module_code;
    "" 
  | Tstr_primitive  _  -> out_of_scope "primitive functions"
  | Tstr_typext     _  -> out_of_scope "type extensions"
  | Tstr_exception  _  -> out_of_scope "exceptions"
  | Tstr_module     _  -> out_of_scope "modules"
  | Tstr_recmodule  _  -> out_of_scope "recursive modules"
  | Tstr_modtype    _  -> out_of_scope "module type"
  | Tstr_class      _  -> out_of_scope "objects"
  | Tstr_class_type _  -> out_of_scope "class types"
  | Tstr_include    _  -> out_of_scope "includes"
  | Tstr_attribute  attrs -> out_of_scope "attributes"

and js_of_branch ?(mod_gen="") old_env b obj =
  let spat, binders = js_of_pattern ~mod_gen b.c_lhs obj in
  let se = js_of_expression ~mod_gen old_env b.c_rhs in
  L.log_line (ppf_branch spat binders se) (L.Add binders)
    
and js_of_expression ?(mod_gen="") old_env e =
  let new_env = e.exp_env in
  match e.exp_desc with
  | Texp_ident (_, loc,  _)           -> js_of_longident loc
  | Texp_constant c                   -> js_of_constant c
  | Texp_let   (_, vb_l, e)           ->
    let sd = String.concat lin1 @@ List.map (fun vb -> show_value_binding ~mod_gen vb) @@ vb_l in
    let se = js_of_expression ~mod_gen new_env e
    in ppf_let_in sd se
  | Texp_function (_, c :: [], Total) ->
    let rec explore pats e = match e.exp_desc with
      | Texp_function (_, c :: [], Total) ->
        let p, e = c.c_lhs, c.c_rhs
        in explore (p :: pats) e
      | _                                 ->
        String.concat ", " @@ List.map ident_of_pat @@ List.rev @@ pats, js_of_expression ~mod_gen new_env e in
    let args, body = explore [c.c_lhs] c.c_rhs
    in ppf_function args body
  | Texp_apply (f, exp_l)                 ->
     let sl' = exp_l
               |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope "optional apply arguments" | Some ei -> ei) in
     let sl = exp_l
              |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope "optional apply arguments" | Some ei -> js_of_expression ~mod_gen new_env ei) in
    let se = js_of_expression ~mod_gen new_env f in
    if is_infix f sl' && List.length exp_l = 2
    then ppf_apply_infix se (List.hd sl) (List.hd (List.tl sl))
    else ppf_apply se (String.concat ", " sl)
  | Texp_match (exp, l, [], Total) ->
     let se = js_of_expression ~mod_gen new_env exp in
     let sb = String.concat "@," (List.map (fun x -> js_of_branch ~mod_gen old_env x se) l) in
     ppf_match se sb
  | Texp_tuple (tl)               -> ppf_tuple @@ show_list_f (fun exp -> js_of_expression ~mod_gen new_env exp) ", " tl
  | Texp_construct (loc, cd, el) ->
        let value = js_of_longident loc in
        if el = [] then
          if is_sbool value
          then value
          else ppf_single_cstrs value
        else
          let rec expand_constructor_list fields exprs = match fields, exprs with
            | [], [] -> []
            | [], x :: xs | x :: xs , [] -> failwith "argument lists should have the same length."
            | x :: xs, y :: ys -> (if y = "" then ppf_single_cstrs x else ppf_cstr x y) :: expand_constructor_list xs ys in
          let names = Hashtbl.find type_tbl (if mod_gen <> "" then mod_gen ^ "." ^ value else value)
             in ppf_multiple_cstrs value (show_list ", " (expand_constructor_list names (List.map (fun exp -> js_of_expression ~mod_gen new_env exp) el)))
  | Texp_array      (exp_l)           -> ppf_array @@ show_list_f (fun exp -> js_of_expression ~mod_gen new_env exp) ", " exp_l
  | Texp_ifthenelse (e1, e2, None)    -> ppf_ifthen (js_of_expression ~mod_gen new_env e1) (js_of_expression ~mod_gen new_env e2)
  | Texp_ifthenelse (e1, e2, Some e3) -> ppf_ifthenelse (js_of_expression ~mod_gen new_env e1) (js_of_expression ~mod_gen new_env e2) (js_of_expression ~mod_gen new_env e3)
  | Texp_sequence   (e1, e2)          -> ppf_sequence (js_of_expression ~mod_gen new_env e1) (js_of_expression ~mod_gen new_env e2)
  | Texp_while      (cd, body)        -> ppf_while (js_of_expression ~mod_gen new_env cd) (js_of_expression ~mod_gen new_env body)
  | Texp_for        (id, _, st, ed, fl, body) -> ppf_for (Ident.name id) (js_of_expression ~mod_gen new_env st) (js_of_expression ~mod_gen new_env ed) fl (js_of_expression ~mod_gen new_env body)
  | Texp_record     (llde,_)          -> ppf_record (List.map (fun (_, lbl, exp) -> (lbl.lbl_name, js_of_expression ~mod_gen new_env exp)) llde)
  | Texp_match      (_,_,_, Partial)  -> out_of_scope "partial matching"
  | Texp_match      (_,_,_,_)         -> out_of_scope "matching with exception branches"
  | Texp_try        (_,_)             -> out_of_scope "exceptions"
  | Texp_function   (_,_,_)           -> out_of_scope "powered-up functions"
  | Texp_variant    (_,_)             -> out_of_scope "polymorphic variant"
  | Texp_field      (_,_,_)           -> out_of_scope "accessing field"
  | Texp_setfield   (_,_,_,_)         -> out_of_scope "setting field"
  | Texp_send       (_,_,_)           -> out_of_scope "objects"
  | Texp_new        (_,_,_)           -> out_of_scope "objects"
  | Texp_instvar    (_,_,_)           -> out_of_scope "objects"
  | Texp_setinstvar (_,_,_,_)         -> out_of_scope "objects"
  | Texp_override   (_,_)             -> out_of_scope "objects"
  | Texp_letmodule  (_,_,_,_)         -> out_of_scope "local modules"
  | Texp_assert      _                -> out_of_scope "assert"
  | Texp_lazy        _                -> out_of_scope "lazy expressions"
  | Texp_object     (_,_)             -> out_of_scope "objects"
  | Texp_pack        _                -> out_of_scope "packing"
    
and js_of_constant = function
  | Const_int       n     -> string_of_int n
  | Const_char      c     -> String.make 1 c
  | Const_string   (s, _) -> "\"" ^ s ^ "\""
  | Const_float     f     -> f
  | Const_int32     n     -> Int32.to_string n
  | Const_int64     n     -> Int64.to_string n
  | Const_nativeint n     -> Nativeint.to_string n
    
and js_of_longident loc =
  let res = String.concat "." @@ Longident.flatten loc.txt in
  if res = "()" then "undefined" else res

and ident_of_pat pat = match pat.pat_desc with
  | Tpat_var (id, _) -> Ident.name id
  | _ -> error "functions can't deconstruct values"
    
and js_of_let_pattern ?(mod_gen="") pat expr =
  let new_env = pat.pat_env in
  let sexpr = js_of_expression ~mod_gen new_env expr in
  match pat.pat_desc with
  | Tpat_var (id, _) -> ppf_decl ~mod_gen (Ident.name id) sexpr
  | Tpat_tuple (pat_l)
  | Tpat_array (pat_l) ->
     let l = List.map
               (function pat ->
                         match pat.pat_desc with
                         | Tpat_var (id, _) -> (Ident.name id, string_of_type_exp pat.pat_type)
                         | _ -> out_of_scope "pattern-matching in arrays"
               ) pat_l in
     ppf_pat_array l sexpr
  | _ -> error "let can't deconstruct values"

and js_of_pattern ?(mod_gen="") pat obj =
  match pat.pat_desc with
  | Tpat_any                     -> "default", ""
  | Tpat_constant   c            -> js_of_constant c, ""
  | Tpat_var       (id, _)       -> Ident.name id, ""
  | Tpat_construct (loc, cd, el) ->
     let c = js_of_longident loc in
     let spat = Printf.sprintf "%s" ("case \"" ^ c ^ "\"") in
     let params = Hashtbl.find type_tbl (if mod_gen <> "" then mod_gen ^ "." ^ c else c) in
     let binders =
       if List.length el = 0 then ""
       else Printf.sprintf "@[<v 0>%s@]"
          ("var " ^ show_list ", " (List.map2 (fun x y -> x ^ " = " ^ obj ^ "." ^ y) (List.map (fun x -> fst (js_of_pattern ~mod_gen x obj)) el) params) ^ ";") in
     spat, binders
  | Tpat_tuple el -> unsupported "tuple matching"
  | Tpat_array el -> unsupported "array-match"
  | Tpat_record (_,_) -> unsupported "record"
  | Tpat_or (_,_,_) -> failwith "not implemented yet"
  | Tpat_alias (_,_,_) -> out_of_scope "alias-pattern"
  | Tpat_variant (_,_,_) -> out_of_scope "polymorphic variants in pattern matching"
  | Tpat_lazy _ -> out_of_scope "lazy-pattern"
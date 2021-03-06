open Asttypes
open Parsetree
open Typedtree
open Types
open Mytools

let builtin_attributes =
  [("::", ["head"; "tail"]);
   ("Some", ["value"])]

let ident_builtin_attributes =
  List.map (fun (fst, snd) -> (List.assoc fst Predef.builtin_idents), snd) builtin_attributes

let rec extract_attrs attrs =
  attrs
    |> List.map extract_attr
    |> List.flatten

and extract_attr (identifier, pl) =
  if identifier.txt = "f" then extract_payload pl else []

and extract_payload = function
  | PStr  s    -> extract_structure s
  | PTyp  _    -> error "Type found. A tuple or a single value was expected"
  | PPat (_,_) -> error "Pattern found. A tuple or a single value was expected"
  | PSig _ -> error "Signature found. A tuple or a single value was expected"

and extract_structure s =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (extract_structure_item x @ acc) xs
  in aux [] s

and extract_structure_item si = match si.pstr_desc with
  | Pstr_eval   (exp, _) -> extract_expression exp 
  | Pstr_value     (_,_) -> error "An expression was expected but a value declaration was found"
  | Pstr_primitive  _    -> error "An expression was expected but a primitive declaration was found"
  | Pstr_type       _    -> error "An expression was expected but a type declaration was found"
  | Pstr_typext     _    -> error "An expression was expected but a type extension was found"
  | Pstr_exception  _    -> error "An expression was expected but an exception definition was found"
  | Pstr_module     _    -> error "An expression was expected but a module definition was found"
  | Pstr_recmodule  _    -> error "An expression was expected but a recursive module definition was found"
  | Pstr_modtype    _    -> error "An expression was expected but a module type declaration was found"
  | Pstr_open       _    -> error "An expression was expected but an open statement was found"
  | Pstr_class      _    -> error "An expression was expected but a class declaration was found"
  | Pstr_class_type _    -> error "An expression was expected but a class type declaration was found"
  | Pstr_include    _    -> error "An expression was expected but an include declaration was found"
  | Pstr_attribute  _    -> error "An expression was expected but an attribute was found"
  | Pstr_extension (_,_) -> error "An expression was expected but an extension statement. was found"
  
and extract_expression e = match e.pexp_desc with
  | Pexp_ident       i          -> [String.concat "." @@ Longident.flatten @@ i.txt]
  | Pexp_constant    c          -> [extract_constant c]
  | Pexp_let        (_,_,_)     -> error "An identifier, a tuple or an array was expected but a let declaration was found"
  | Pexp_function    _          -> error "An identifier, a tuple or an array was expected but a function definition was found"
  | Pexp_fun        (_,_,_,_)   -> error "An identifier, a tuple or an array was expected but a fun definition was found"
  | Pexp_apply      (_,_)       -> error "An identifier, a tuple or an array was expected but a function application was found"
  | Pexp_match      (_,_)       -> error "An identifier, a tuple or an array was expected but a pattern matchingwas found"
  | Pexp_try        (_,_)       -> error "An identifier, a tuple or an array was expected but a try block was found"
  | Pexp_tuple      el          -> List.flatten @@ List.map extract_expression @@ el
  | Pexp_construct  (_,_)       -> error "An identifier, a tuple or an array was expected but a constructor was found"
  | Pexp_variant    (_,_)       -> error "An identifier, a tuple or an array was expected but a variant was found"
  | Pexp_record     (_,_)       -> error "An identifier, a tuple or an array was expected but a record was found"
  | Pexp_field      (_,_)       -> error "An identifier, a tuple or an array was expected but a field was found"
  | Pexp_setfield   (_,_,_)     -> error "An identifier, a tuple or an array was expected but a set-field was found"
  | Pexp_array       el         -> List.flatten @@ List.map extract_expression @@ el
  | Pexp_ifthenelse (_,_,_)     -> error "An identifier, a tuple or an array was expected but an if-then-else expression was found"
  | Pexp_sequence   (e1, e2)    -> extract_expression e1 @ extract_expression e2
  | Pexp_while      (_,_)       -> error "An identifier, a tuple or an array was expected but a while loop was found"
  | Pexp_for        (_,_,_,_,_) -> error "An identifier, a tuple or an array was expected but a for loop was found"
  | Pexp_constraint (_,_)       -> error "An identifier, a tuple or an array was expected but a constraint declaration was found"
  | Pexp_coerce     (_,_,_)     -> error "An identifier, a tuple or an array was expected but a coercion was found"
  | Pexp_send       (_,_)       -> error "An identifier, a tuple or an array was expected but a method application was found"
  | Pexp_new         _          -> error "An identifier, a tuple or an array was expected but an object instanciation was found"
  | Pexp_setinstvar (_,_)       -> error "An identifier, a tuple or an array was expected but an object instanciation was found"
  | Pexp_override    _          -> error "An identifier, a tuple or an array was expected but an override statement was found"
  | Pexp_letmodule  (_,_,_)     -> error "An identifier, a tuple or an array was expected but a let module declaration was found"
  | Pexp_assert      _          -> error "An identifier, a tuple or an array was expected but an assert statement was found"
  | Pexp_lazy        _          -> error "An identifier, a tuple or an array was expected but a lazy block was found"
  | Pexp_poly        _          -> error "An identifier, a tuple or an array was expected but a poly was found"
  | Pexp_object      _          -> error "An identifier, a tuple or an array was expected but an object declaration was found"
  | Pexp_newtype     _          -> error "An identifier, a tuple or an array was expected but a newtype definition was found"
  | Pexp_pack        _          -> error "An identifier, a tuple or an array was expected but a pack was found"
  | Pexp_open        _          -> error "An identifier, a tuple or an array was expected but an open statement was found"
  | Pexp_extension   _          -> error "An identifier, a tuple or an array was expected but an extension was found"

and extract_constant = function
  | Pconst_char     c     -> String.make 1 c
  | Pconst_string  (s, _) -> s
  | Pconst_integer  _     -> error "A string or a char was expected but an int was found"
  | Pconst_float    _     -> error "A string or a char was expected but a float was found"

let fetch_builtin_attrs cstr_name =
  List.assoc cstr_name builtin_attributes

let extract_cstr_attrs_basic cstr_name cstr_attributes =
  try fetch_builtin_attrs cstr_name
  with Not_found -> extract_attrs cstr_attributes


let extract_cstr_attrs (cstr : constructor_description) =
  extract_cstr_attrs_basic cstr.cstr_name cstr.cstr_attributes

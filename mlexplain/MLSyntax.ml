type position = {
  line : int ; [@f line]
  column : int [@f column]
}

type location = {
  start : position ; [@f start]
  stop : position [@f stop]
}

type 'a located = {
  loc : location ; [@f loc]
  value : 'a [@f value]
}

let new_position line column = { line = line ; column = column }
let new_location start stop = { start = start ; stop = stop }
let new_located_value loc value = { loc = loc ; value = value }

let dummy_position = { line = 0 ; column = 0 }
let dummy_location = { start = dummy_position ; stop = dummy_position }

type constant =
| Constant_integer of int [@f value]
| Constant_float of float [@f value]
| Constant_char of char [@f value]
| Constant_string of string [@f value]

type datatype =
| Datatype_simple of string [@f name]
| Datatype_variable of int [@f index]
| Datatype_arrow of datatype located * datatype located [@f arg, ret]
| Datatype_tuple of datatype located list [@f components]

type expression =
| Expression_constant of constant located [@f constant]
| Expression_ident of string located [@f id]
| Expression_let of bool * pattern located * expression located * expression located [@f is_rec, id, e1, e2]
| Expression_tuple of expression located list [@f components]

and pattern =
| Pattern_any of location [@f loc]
| Pattern_var of string located [@f id]
| Pattern_constant of constant located [@f constant]

(* type constructor = string

 type core_type =
| Core_type_any
| Core_type_var of string [@f name]
| Core_type_arrow of core_type located * core_type located [@f arg, ret]
| Core_type_tuple of core_type located list [@f component]
| Core_type_ctor of constructor located * core_type located list [@f ctor, params]
| Core_type_alias of core_type located * string [@f typename, alias]

type constant =
| Constant_integer of int [@f value]
| Constant_float of float [@f value]
| Constant_char of char [@f value]
| Constant_string of string [@f value]

type expression =
| Expression_constant of constant [@f constant]
| Expression_ident of string [@f id]
| Expression_let of bool * value_binding list * expression located [@f is_rec, bindings, expr]

and pattern =
| Pattern_any
| Pattern_var of string located [@f binding]
| Pattern_alias of pattern located * string [@f patt, alias]
| Pattern_constant of constant [@f constant]

and value_binding = {
  patt : pattern located ; [@f patt]
  expr : expression located ; [@f expr]
  loc : location [@f loc]
} *)

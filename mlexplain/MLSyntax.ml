type position = {
  line : int ; [@f line]
  column : int [@f column]
}

type location = {
  file : string ; [@f file]
  start : position ; [@f start]
  stop : position [@f stop]
}

type 'a located = {
  loc : location ; [@f loc]
  value : 'a [@f value]
}

let new_position line column = { line = line ; column = column }
let new_location file start stop = { file = file ; start = start ; stop = stop }
let new_located_value loc value = { loc = loc ; value = value }

let dummy_position = { line = 0 ; column = 0 }
let dummy_location = { file = "dummy.ml" ; start = dummy_position ; stop = dummy_position }

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
| Expression_constant of location * constant [@f loc, constant]
| Expression_ident of location * string [@f loc, id]
| Expression_let of location * bool * pattern * expression * expression [@f loc, is_rec, id, e1, e2]
| Expression_tuple of location * expression array [@f loc, components]
| Expression_array of location * expression array [@f loc, elements]
| Expression_fun of location * pattern * expression [@f loc, patt, expr]
| Expression_function of location * case array [@f loc, cases]
| Expression_apply of location * expression * expression array [@f loc, func, args]
| Expression_match of location * expression * case array [@f loc, expr, cases]

and pattern =
| Pattern_any of location [@f loc]
| Pattern_var of location * string [@f loc, id]
| Pattern_constant of location * constant [@f loc, constant]
| Pattern_tuple of location * pattern array [@f loc, patts]
| Pattern_array of location * pattern array [@f loc, patts]

and case = {
  patt : pattern ;
  expr : expression
}

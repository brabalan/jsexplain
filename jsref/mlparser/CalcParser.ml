open CalcSyntax

module Lexer = struct
  let column_add (pos : position) n = { line = pos.line ; column = pos.column + n }

  let inc_column pos = column_add pos 1
  let inc_line (pos : position) = { line = pos.line + 1 ; column = 0 }

  let kwds = []

  let rec skip_blanks pos = parser
  | [< '( ' ' | '\t') ; s >] -> pos := inc_column !pos ; skip_blanks pos s
  | [< ''\n' ; s >] -> pos := inc_line !pos ; skip_blanks pos s
  | [< s >] -> s

  let parse_char pos =
    let escape = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | '\'' -> '\''
    | c -> c
  
    in parser
    | [< ''\\' ; 'c ; ''\'' >] ->
      let loc = { file = "example0.ml" ; start = !pos ; stop = column_add !pos 4 } in
      let tok = Char (escape c) in
      pos := column_add !pos 3 ;
      { loc = loc ; value = tok }
    | [< 'c ; ''\'' >] ->
      let loc = { file = "example0.ml" ; start = !pos ; stop = column_add !pos 3 } in
      let tok = Char c in
      pos := column_add !pos 3 ;
      { loc = loc ; value = tok }

  let parse_string pos =
    let rec string_internal = parser
    | [< ''"' >] -> []
    | [< 'c ; s >] -> c :: string_internal s

    in let rec escape = function
    | [] -> ""
    | '\\'::c::s ->
      begin
        match c with
        | 'n' -> String.make 1 '\n' ^ escape s
        | 't' -> String.make 1 '\t' ^ escape s
        | 'r' -> String.make 1 '\r' ^ escape s
        | c -> String.make 1 c ^ escape s
      end
    | c::s -> String.make 1 c ^ escape s

    in parser
    | [< ''"' ; s = string_internal >] ->
      let strlen = List.length s in
      let s' = escape s in
      let loc = { file = "example0.ml" ; start = !pos ; stop = column_add !pos (strlen + 1) } in
      let tok = String s' in
      pos := column_add !pos (strlen + 1) ;
      { loc = loc ; value = tok }

  let parse_ident pos =
    let is_alpha c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') in
    let is_num c = ('0' <= c && c <= '9') in
    let is_symbol = String.contains "+-*/()" in

    let rec parse_symbol acc = parser
    | [< 'c when is_symbol c ; s >] -> parse_symbol (acc ^ String.make 1 c) s
    | [< >] -> acc

    in let rec parse_ident' acc = parser
    | [< 'c when (is_num c || is_alpha c || c = '\'' || c = '_') ; s >] ->
      let nacc = acc ^ String.make 1 c in
      parse_ident' nacc s
    | [< >] ->
      if List.mem acc kwds then
        Kwd acc
      else
        Ident acc

    in parser
    | [< 'c when is_symbol c ; s >] ->
      let sym = parse_symbol (String.make 1 c) s in
      let loc = { file = "example0.ml" ; start = !pos ; stop = column_add !pos (String.length sym) } in
      let tok = Kwd sym in
      pos := column_add !pos (String.length sym) ;
      { loc = loc ; value = tok }
    | [< 'c when (is_alpha c || c = '_') ; s >] ->
      let ident = parse_ident' (String.make 1 c) s in
      let idlen = match ident with
      | Kwd k -> String.length k
      | Ident i -> String.length i
      | _ -> -1 in (* absurd *)

      let loc = { file = "example0.ml" ; start = !pos ; stop = column_add !pos idlen } in
      pos := column_add !pos idlen ;
      { loc = loc ; value = ident }
      
  let parse_number pos =
    let rec parse_integer acc = parser
    | [< ' ('0'..'9') as c ; s >] -> parse_integer (acc ^ String.make 1 c) s
    | [< >] -> acc

    in let parse_float num = parser
    | [< ''.' ; dec = parse_integer "" >] ->
      let s_num = num ^ "." ^ dec in
      let f = float_of_string s_num in
      let flen = String.length s_num in
      let loc = { file = "example0.ml" ; start = !pos ; stop = column_add !pos flen } in
      let tok = Float f in
      pos := column_add !pos flen ;
      { loc = loc ; value = tok }
    | [< >] ->
      let s_num = num in
      let i = int_of_string s_num in
      let ilen = String.length s_num in
      let loc = { file = "example0.ml" ; start = !pos ; stop = column_add !pos ilen } in
      let tok = Int i in
      pos := column_add !pos ilen ;
      { loc = loc ; value = tok }

    in parser
    [< ' ('0'..'9') as c ; s >] ->
      (parser [< i = parse_integer (String.make 1 c) ; s>] -> parse_float i s)(s)

  let make_calc_lexer s =
    let pos = ref { line = 1 ; column = 0 } in
    let rec aux pos = parser
    | [< ''\'' ; tok = parse_char pos ; s = skip_blanks pos >] -> [< 'tok ; aux pos s >]
    | [< tok = parse_string pos ; s = skip_blanks pos >] -> [< 'tok ; aux pos s >]
    | [< tok = parse_ident pos ; s = skip_blanks pos >] -> [< 'tok ; aux pos s >]
    | [< tok = parse_number pos ; s = skip_blanks pos >] -> [< 'tok ; aux pos s >]
    | [< >] -> [< >]

    in let make_calc_lexer' = parser [< s = skip_blanks pos >] -> aux pos s

    in make_calc_lexer' (Stream.of_string s)
end

let symbols = [ "+" ; "-" ; "*" ; "/" ; "mod" ; "(" ; ")" ]

let parse_literal = parser
| [< '{ loc = loc ; value = Int i } >] -> Literal_int (loc, i)
| [< '{ loc = loc ; value = Float f } >] -> Literal_float (loc, f)

let rec parse_binary_op name stream =
  let rec parse_term_op =
    let rec aux left = parser
    | [< '{ loc = loc ; value = Kwd "+" } ; right = parse_factor_op ; s >] ->
      let start = (get_location left).start in
      let stop = (get_location right).stop in
      let n_loc = { file = name ; start = start ; stop = stop } in
      aux (Expr_binary_op (n_loc, left, Binary_op_add loc, right)) s

    | [< '{ loc = loc ; value = Kwd "-" } ; right = parse_factor_op ; s >] ->
      let start = (get_location left).start in
      let stop = (get_location right).stop in
      let n_loc = { file = name ; start = start ; stop = stop } in
      aux (Expr_binary_op (n_loc, left, Binary_op_sub loc, right)) s
    | [< >] -> left
    in parser [< left = parse_factor_op ; s >] -> aux left s

  and parse_factor_op =
    let rec aux left = parser
    | [< '{ loc = loc ; value = Kwd "*" } ; right = parse_leaf ; s >] ->
      let start = (get_location left).start in
      let stop = (get_location right).stop in
      let n_loc = { file = name ; start = start ; stop = stop } in
      aux (Expr_binary_op (n_loc, left, Binary_op_mul loc, right)) s

    | [< '{ loc = loc ; value = Kwd "/"} ; right = parse_leaf ; s >] ->
      let start = (get_location left).start in
      let stop = (get_location right).stop in
      let n_loc = { file = name ; start = start ; stop = stop } in
      aux (Expr_binary_op (n_loc, left, Binary_op_div loc, right)) s

    | [< >] -> left
    in parser [< left = parse_leaf ; s >] -> aux left s

  and parse_unary_op = parser
  | [< '{ loc = loc ; value = Kwd "+" } ; e = parse_leaf >] ->
    let stop = (get_location e).stop in
    let n_loc = { file = name ; start = loc.start ; stop = stop } in
    Expr_unary_op (n_loc, Unary_op_add loc, e)
  | [< '{ loc = loc ; value = Kwd "-" } ; e = parse_leaf >] ->
    let stop = (get_location e).stop in
    let n_loc = { file = name ; start = loc.start ; stop = stop } in
    Expr_unary_op (n_loc, Unary_op_sub loc, e)

  and parse_leaf = parser
  | [< '{ loc = loc ; value = Kwd "(" } ;
    e = parse_binary_op name ;
    '{ loc = stop_loc ; value = Kwd ")" } >] ->
      let start = loc.start in
      let stop = stop_loc.stop in
      let n_loc = { file = name ; start = start ; stop = stop } in
      begin
        match e with
        | Expr_literal (_, lit) -> Expr_literal (n_loc, lit)
        | Expr_unary_op (_, op, arg) -> Expr_unary_op (n_loc, op, arg)
        | Expr_binary_op (_, arg1, op, arg2) -> Expr_binary_op (n_loc, arg1, op, arg2)
      end
  | [< u = parse_unary_op >] -> u
  | [< lit = parse_literal >] -> Expr_literal (get_literal_location lit, lit)

  in parse_term_op stream

let parse_expr = parse_binary_op

let parse_string name s =
  let stream = Lexer.make_calc_lexer s in
  parse_expr name stream

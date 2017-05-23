open MLSyntax

type value =
| Value_int of int [@f value]
| Value_float of float [@f value]
| Value_char of char [@f value]
| Value_string of string [@f value]
| Value_tuple of value array [@f value]
| Value_fun of (value -> value option) [@f value]

let rev lst =
  let rec aux acc lst = match lst with
  | [] -> acc
  | h :: t -> aux (h :: acc) t

  in aux [] lst

let list_of_array ary =
  let len = array_length ary in
  let rec for_loop i =
    if i === len then [] else array_get ary i :: for_loop (i + 1)
  in for_loop 0

let map f lst =
  let rec aux acc lst = match lst with
  | [] -> rev acc
  | h :: t -> aux (f h :: acc) t

  in aux [] lst

let all_true ary =
  let f cur b = cur && b in
  array_fold f true ary

let min a b = if a <= b then a else b

let zipwith f a1 a2 =
  let flen_a1 = number_of_int (array_length a1) in
  let flen_a2 = number_of_int (array_length a2) in
  let min_size = int_of_number (min flen_a1 flen_a2) in
  let res = array_make min_size (f (array_get a1 0) (array_get a2 0)) in
  let rec for_loop i =
    if (number_of_int i) < (number_of_int min_size) then
    begin
      let res_f = f (array_get a1 i) (array_get a2 i) in
      array_set res i res_f ;
      for_loop (i + 1)
    end
    else
      res in
  for_loop 1

let lift_option ary =
  let f ary_opt opt = match ary_opt with
  | None -> None
  | Some ary ->
    begin
      match opt with
      | None -> None
      | Some v -> Some (array_append ary (array_make 1 v))
    end
  in array_fold f (Some [| |]) ary


let rec value_eq v1 v2 = match v1 with
| Value_int i1 ->
  begin
    match v2 with
    | Value_int i2 -> int_eq i1 i2
    | _ -> false
  end
| Value_float f1 ->
  begin
    match v2 with
    | Value_float f2 -> f1 = f2
    | _ -> false
  end
| Value_char c1 ->
  begin
    match v2 with
    | Value_char c2 -> c1 === c2
    | _ -> false
  end
| Value_string s1 ->
  begin
    match v2 with
    | Value_string s2 -> string_eq s1 s2
    | _ -> false
  end
| Value_tuple t1 ->
  begin
    match v2 with
    | Value_tuple t2 ->
      let blist = zipwith value_eq t1 t2 in
      all_true blist
    | _ -> false
  end
| Value_fun _ -> false

type environment = (string, value) Map.map

let run_constant = function
| Constant_integer i -> Value_int i
| Constant_float f -> Value_float f
| Constant_char c -> Value_char c
| Constant_string s -> Value_string (normalize_string s)

let rec run_expression ctx _term_ = match _term_ with
| Expression_constant (_, c) -> Some (run_constant c)
| Expression_ident (_, id) ->
  begin
    match Map.find id ctx with
    | None -> None
    | Some v -> Some v
  end
| Expression_let (_, _, patt, e1, e2) ->
  begin
    match run_expression ctx e1 with
    | None -> None
    | Some v ->
      match pattern_match ctx v patt with
      | Some ctx' -> run_expression ctx' e2
      | None -> None
  end
| Expression_fun (_, patt, expr) ->
  Some (Value_fun (fun value ->
    match pattern_match ctx value patt with
    | None -> None
    | Some ctx' -> run_expression ctx' expr))
| Expression_function (_, cases) ->
  let func value = pattern_match_many ctx value (list_of_array cases) in
  Some (Value_fun func)
| Expression_apply (_, fe, argse) ->
  let rec run_apply func args =
    let apply_fun func ctx arg args = match run_expression ctx arg with
    | None -> None
    | Some v ->
      match func v with
      | None -> None
      | Some res -> run_apply res args in
    match args with
    | [] -> Some func
    | x :: xs ->
      match func with
      | Value_fun f -> apply_fun f ctx x xs
      | _ -> None
  in begin
    match run_expression ctx fe with
    | None -> None
    | Some func -> run_apply func (list_of_array argse)
  end
| Expression_tuple (_, tuple) ->
  let value_opts = array_map (fun e -> run_expression ctx e) tuple in
  begin
    match lift_option value_opts with
    | None -> None
    | Some t -> Some (Value_tuple t)
  end
| Expression_match (loc, expr, cases) ->
  let func = Expression_function (loc, cases) in
  let app = Expression_apply (loc, func, [| expr |]) in
  run_expression ctx app

and pattern_match ctx value _term_ = match _term_ with
| Pattern_any _ -> Some ctx
| Pattern_var (_, id) -> Some (Map.add id value ctx)
| Pattern_constant (_, c) ->
  let v1 = run_constant c in
  if value_eq v1 value then Some ctx else None
| Pattern_tuple (_, patts) ->
  match value with
  | Value_tuple tuples ->
    let len = array_length patts in
    let vallen = array_length tuples in
    let rec for_loop ctx_opt i =
      if i === len then
        ctx_opt
      else
        match ctx_opt with
        | None -> None
        | Some ctx ->
          let vali = (array_get tuples i) in
          let patti = (array_get patts i) in
          for_loop (pattern_match ctx vali patti) (i + 1) in
    if len === vallen then
     for_loop (Some ctx) 0
    else
      None
  | _ -> None

and pattern_match_many ctx value cases = match cases with
| [] -> None
| x :: xs ->
  match pattern_match ctx value x.patt with
  | None -> pattern_match_many ctx value xs
  | Some ctx' -> run_expression ctx' x.expr

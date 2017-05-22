open MLSyntax

type value =
| Value_int of int [@f value]
| Value_float of float [@f value]
| Value_char of char [@f value]
| Value_string of string [@f value]
| Value_tuple of value array [@f value]

let rev lst =
  let rec aux acc lst = match lst with
  | [] -> acc
  | h::t -> aux (h::acc) t

  in aux [] lst

let map f lst =
  let rec aux acc lst = match lst with
  | [] -> rev acc
  | h::t -> aux (f h :: acc) t

  in aux [] lst

let all_true ary =
  let f cur b = cur && b in
  array_fold f true ary

let min a b = if a <= b then a else b

let zipwith f a1 a2 =
  let min_size = int_of_number (min (number_of_int (array_length a1)) (number_of_int (array_length a2))) in
  let res = array_make min_size (f (array_get a1 0) (array_get a2 0)) in
  let rec for_loop i =
    if (number_of_int i) < (number_of_int min_size) then
    begin
      array_set res i (f (array_get a1 i) (array_get a2 i)) ;
      for_loop (i+1)
    end
    else
      res
  in for_loop 1

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

type environment = (string, value) Map.map

let run_constant = function
| Constant_integer i -> Value_int i
| Constant_float f -> Value_float f
| Constant_char c -> Value_char c
| Constant_string s -> Value_string s

let rec run_expression ctx _term_ = match _term_ with
| Expression_constant c -> Some (run_constant c.value)
| Expression_ident id ->
  begin
    match Map.find id.value ctx with
    | None -> None
    | Some v -> Some v
  end
| Expression_let (_, patt, e1, e2) ->
  begin
    match pattern_match ctx e1.value patt.value with
    | Some ctx' -> run_expression ctx' e2.value
    | None -> None
  end
| Expression_tuple tuple ->
  let value_opts = array_map (fun e -> run_expression ctx e.value) tuple in
  match lift_option value_opts with
  | None -> None
  | Some t -> Some (Value_tuple t)

and pattern_match ctx expr patt = match patt with
| Pattern_any _ -> Some ctx
| Pattern_var id ->
  begin
    match run_expression ctx expr with
    | Some v -> Some (Map.add id.value v ctx)
    | None -> None
  end
| Pattern_constant c ->
  begin
    let v1 = run_constant c.value in
    match run_expression ctx expr with
    | None -> None
    | Some v2 -> if value_eq v1 v2 then Some ctx else None
  end

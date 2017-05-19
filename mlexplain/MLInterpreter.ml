open MLSyntax

type value =
| Value_int of int [@f value]
| Value_float of float [@f value]
| Value_char of char [@f value]
| Value_string of string [@f value]
| Value_tuple of value list [@f value]

let map f lst =
  let rec aux acc lst = match lst with
  | [] -> rev acc
  | h::t -> aux (f h :: acc) t

  in aux [] lst

let rec zipwith f l1 l2 = match l1 with
| [] -> []
| h1::t1 ->
  begin
    match l2 with
    | [] -> []
    | h2::t2 -> f h1 h2 :: zipwith f t1 t2
  end

(* val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold f fst rest = match rest with
| [] -> fst
| h::t -> fold f (f fst h) rest

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
    | Value_tuple t2 -> fold ( && ) true (zipwith value_eq t1 t2) 
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

open MLSyntax

type value =
| Value_int of int [@f value]
| Value_float of float [@f value]
| Value_char of char [@f value]
| Value_string of string [@f value]
| Value_tuple of value array [@f value]
| Value_list of value list [@f value]
| Value_array of value array [@f value]
| Value_fun of (value -> value option) [@f value]

let min a b = if a <= b then a else b

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
      let blist = MLArray.zipwith value_eq t1 t2 in
      MLArray.all_true blist
    | _ -> false
  end
| Value_list l1 ->
  begin
    match v2 with
    | Value_list l2 ->
      let blist = MLList.zipwith value_eq l1 l2 in
      MLList.all_true blist
    | _ -> false
  end
| Value_array a1 ->
  begin
    match v2 with
    | Value_array a2 ->
      let blist = MLArray.zipwith value_eq a1 a2 in
      MLArray.all_true blist
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
| Expression_ident (_, id) -> Map.find id ctx
| Expression_let (_, _, patt, e1, e2) ->
  (* Some v = run_expression ctx e1
   * Some ctx' = pattern_match ctx v patt *)
  Option.bind (run_expression ctx e1) (fun v ->
  Option.bind (pattern_match ctx v patt) (fun ctx' ->
  run_expression ctx' e2))
| Expression_function (_, cases) ->
  let func value = pattern_match_many ctx value (MLList.of_array cases) in
  Some (Value_fun func)
| Expression_apply (_, fe, argse) ->
  let rec apply_fun func ctx arg args =
    (* Some v = run_expression ctx arg
     * Some res = func v *)
    Option.bind (run_expression ctx arg) (fun v ->
      Option.bind (func v) (fun res -> run_apply res args))
  and run_apply func args =
    match args with
    (* No argument means a value to return *)
    | [] -> Some func
    (* Having arguments means that we have to apply a function to them *)
    | x :: xs ->
      match func with
      | Value_fun f -> apply_fun f ctx x xs
      | _ -> None in

    (* Some func = run_expression ctx fe *)
    Option.bind (run_expression ctx fe) (fun func ->
      run_apply func (MLList.of_array argse))
| Expression_tuple (_, tuple) ->
  let value_opts = array_map (fun e -> run_expression ctx e) tuple in
  (* Some t = lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun t -> Some (Value_tuple t))
| Expression_array (_, ary) ->
  let value_opts = array_map (fun e -> run_expression ctx e) ary in
  (* Some a = MLArray.lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun a -> Some (Value_array a))
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
  begin
    match value with
    (* No need to check if the pattern has the same number of components as the value
     * since the typing prevent different-length tuple matching *)
    | Value_tuple tuples -> pattern_match_array ctx tuples patts
    | _ -> None
  end
| Pattern_array (loc, patts) ->
  begin
    match value with
    | Value_array ary ->
      if array_length patts === array_length ary then
        pattern_match_array ctx ary patts
      else
        None
    | _ -> None
  end

and pattern_match_many ctx value cases = match cases with
| [] -> None
| x :: xs ->
  match pattern_match ctx value x.patt with
  | None -> pattern_match_many ctx value xs
  | Some ctx' -> run_expression ctx' x.expr

and pattern_match_array ctx ary patts =
  let len = array_length patts in
  let vallen = array_length ary in

  let flen = number_of_int len in
  let fvallen = number_of_int vallen in
  let min_len = int_of_number (min flen fvallen) in

  (* For each i in 0 to len,
   * the value i is matched with the pattern i to populate the new environment *)
  let rec for_loop ctx_opt i =
    if i === min_len then
      (* terminal case, the resulting environment is returned *)
      ctx_opt
    else
      let some_case_func ctx =
        let vali = (array_get ary i) in
        let patti = (array_get patts i) in
        for_loop (pattern_match ctx vali patti) (i + 1) in
      (* Some ctx = ctx_opt *)
      Option.bind ctx_opt some_case_func in
   for_loop (Some ctx) 0

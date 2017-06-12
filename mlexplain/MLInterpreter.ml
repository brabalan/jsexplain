open MLSyntax
open Value

let min a b = if a <= b then a else b

type environment = ExecutionContext.execution_ctx

type structure_item_result = {
  value : value ;
  ctx : environment
}

let run_constant = function
| Constant_integer i -> Value_int i
| Constant_float f -> Value_float f
| Constant_char c -> Value_char c
| Constant_string s -> Value_string (normalize_string s)

let rec run_expression s ctx _term_ = match _term_ with
| Expression_constant (_, c) -> Some (run_constant c)
| Expression_ident (_, id) ->
  Option.bind (ExecutionContext.find id ctx) (fun idx ->
  Option.bind (Vector.find s idx) (fun b -> value_of s ctx b))
| Expression_let (_, is_rec, patts, exp_ary, e2) ->
  if is_rec then
    let prealloc p = match p with
    | Pattern_var (_, id) -> Some id
    | _ -> None in
    let exps = MLList.of_array exp_ary in
    Option.bind (MLArray.lift_option (MLArray.map prealloc patts)) (fun id_ary ->
    let ids = MLList.of_array id_ary in
    let func ctx id exp =
      let idx = Vector.append s (Prealloc exp) in
      ExecutionContext.add id idx ctx in
    let ctx' = MLList.foldl2 func ctx ids exps in
    run_expression s ctx' e2)
  else
    let func ctx_opt patt exp =
      (* Some ctx = ctx_opt
       * Some v = run_expression ctx exp *)
      Option.bind ctx_opt (fun ctx ->
      Option.bind (run_expression s ctx exp) (fun v -> pattern_match s ctx v patt)) in
    let patt_list = MLList.of_array patts in
    let exps = MLList.of_array exp_ary in
    Option.bind (MLList.foldl2 func (Some ctx) patt_list exps) (fun ctx' ->
    run_expression s ctx' e2)
| Expression_function (_, cases) ->
  let func value = pattern_match_many s ctx value (MLList.of_array cases) in
  Some (Value_fun func)
| Expression_apply (_, fe, argse) ->
  let rec apply_fun func ctx arg args =
    (* Some v = run_expression ctx arg
     * Some res = func v *)
    Option.bind (run_expression s ctx arg) (fun v ->
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
    Option.bind (run_expression s ctx fe) (fun func ->
      run_apply func (MLList.of_array argse))
| Expression_tuple (_, tuple) ->
  let value_opts = MLArray.map (fun e -> run_expression s ctx e) tuple in
  (* Some t = lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun t -> Some (Value_tuple t))
| Expression_array (_, ary) ->
  let value_opts = MLArray.map (fun e -> run_expression s ctx e) ary in
  (* Some a = MLArray.lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun a -> Some (Value_array a))
| Expression_variant (_, label, expr_opt) ->
  let value_opt = Option.bind expr_opt (fun e -> run_expression s ctx e) in
  let variant = { label = label ; value_opt = value_opt } in
  Some (Value_variant variant)
| Expression_match (loc, expr, cases) ->
  let func = Expression_function (loc, cases) in
  let app = Expression_apply (loc, func, [| expr |]) in
  run_expression s ctx app
| Expression_constructor (_, ctor, args) ->
  let value_opts = MLArray.map (fun e -> run_expression s ctx e) args in
  (* Some a = MLArray.lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun values ->
  let sum = Sumtype { constructor = ctor ; args = values } in
  Some (Value_custom sum))
| Expression_record (_, bindings, base_opt) ->
  let func map_opt binding =
    (* Some map = map_opt
     * Some value = run_expression s ctx binding.expr *)
    Option.bind map_opt (fun map ->
    Option.bind (run_expression s ctx binding.expr) (fun value ->
    let idx = Vector.append s (Normal value) in
    Some (Map.add binding.name idx map))) in
  let string_eq s1 s2 = string_compare s1 s2 === 0 in
  (* If the value is a record, the function is applied, otherwise the default value is returned *)
  let map_from_value v = do_record_with_default v (Map.empty_map string_eq) (fun r -> r) in
  let base_map = match base_opt with
  | None -> Map.empty_map string_eq
  | Some base ->
    match run_expression s ctx base with
    | Some v -> map_from_value v
    | None -> Map.empty_map string_eq in
  (* Some map = MLArray.fold func (Some base_map) bindings *)
  Option.bind (MLArray.fold func (Some base_map) bindings) (fun map ->
  let r = Record map in
  Some (Value_custom r))
| Expression_field (_, record, fieldname) ->
  (* Some value = run_expression s ctx record *)
  Option.bind (run_expression s ctx record) (fun value ->
  do_record value (fun record ->
    (* Some idx = Map.find fieldname record
     * Some binding = Vector.find s idx *)
    Option.bind (Map.find fieldname record) (fun idx ->
    Option.bind (Vector.find s idx) (fun binding ->
    value_of s ctx binding))))
| Expression_setfield (_, record, fieldname, expr) ->
  (* Some value = run_expression s ctx record *)
  Option.bind (run_expression s ctx record) (fun value ->
  do_record value (fun record ->
    (* Some idx = Map.find fieldname record
     * Some v = run_expression s ctx expr *)
    Option.bind (Map.find fieldname record) (fun idx ->
    Option.bind (run_expression s ctx expr) (fun v ->
      let ignore = Vector.set s idx (Normal v) in
      Some nil))))
| Expression_ifthenelse (_, cond, e1, e2) ->
  (* Some cond_val = run_expression s ctx cond *)
  Option.bind (run_expression s ctx cond) (fun cond_val ->
    if is_sumtype_ctor "true" cond_val then
      run_expression s ctx e1
    else
      match e2 with
      | Some e -> run_expression s ctx e
      | None -> Some nil)
| Expression_sequence (_, e1, e2) ->
  run_expression s ctx e1 ;
  run_expression s ctx e2
| Expression_while (loc, cond_expr, body) ->
  (* Alias pattern not supported by the compiler *)
  let while_expr = Expression_while (loc, cond_expr, body) in
  (* Some cond = run_expression s ctx cond_expr *)
  Option.bind (run_expression s ctx cond_expr) (fun cond ->
  do_sumtype cond (fun b ->
  if b.constructor === "true" then
  begin
    run_expression s ctx body ;
    run_expression s ctx while_expr
  end
  else
    Some nil))

(** Get the actual value held by the binding b *)
and value_of s ctx b = match b with
| Normal v -> Some v
| Prealloc e -> run_expression s ctx e

and pattern_match s ctx value patt = match patt with
| Pattern_any _ -> Some ctx
| Pattern_var (_, id) ->
  let idx = Vector.append s (Normal value) in
  Some (ExecutionContext.add id idx ctx)
| Pattern_constant (_, c) ->
  let v1 = run_constant c in
  if value_eq v1 value then Some ctx else None
| Pattern_tuple (_, patts) ->
  begin
    match value with
    (* No need to check if the pattern has the same number of components as the value
     * since the typing prevent different-length tuple matching *)
    | Value_tuple tuples -> pattern_match_array s ctx tuples patts
    | _ -> None
  end
| Pattern_array (_, patts) ->
  begin
    match value with
    | Value_array ary ->
      if MLArray.length patts === MLArray.length ary then
        pattern_match_array s ctx ary patts
      else
        None
    | _ -> None
  end
| Pattern_variant (_, label, patt_opt) ->
  begin
    match value with
    | Value_variant variant ->
      if variant.label === label then
        match patt_opt with
        | None ->
          begin
            match variant.value_opt with
            | None -> Some ctx
            | Some _ -> None
          end
        | Some patt ->
          begin
            match variant.value_opt with
            | None -> None
            | Some v -> pattern_match s ctx v patt
          end
      else
        None
    | _ -> None
  end
| Pattern_alias (_, patt, id) ->
  (* Some ctx' = pattern_match s ctx value patt *)
  Option.bind (pattern_match s ctx value patt) (fun ctx' ->
  let idx = Vector.append s (Normal value) in
  Some (ExecutionContext.add id idx ctx'))
| Pattern_constructor (_, ctor, args) ->
  do_sumtype value (fun sum ->
    if sum.constructor === ctor then
      pattern_match_array s ctx sum.args args
    else
      None)
| Pattern_or (_, patt1, patt2) ->
  begin
    match pattern_match s ctx value patt1 with
    | Some ctx' -> Some ctx'
    | None -> pattern_match s ctx value patt2
  end

and pattern_match_many s ctx value cases = match cases with
| [] -> None
| x :: xs ->
  match pattern_match s ctx value x.patt with
  | None -> pattern_match_many s ctx value xs
  | Some ctx' -> run_expression s ctx' x.expr

and pattern_match_array s ctx ary patts =
  let len = MLArray.length patts in
  let vallen = MLArray.length ary in

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
        let vali = (MLArray.get ary i) in
        let patti = (MLArray.get patts i) in
        for_loop (pattern_match s ctx vali patti) (i + 1) in
      (* Some ctx = ctx_opt *)
      Option.bind ctx_opt some_case_func in
   for_loop (Some ctx) 0

let run_structure_item s ctx _term_ = match _term_ with
| Structure_eval (_, e) -> Option.bind (run_expression s ctx e) (fun v -> Some { value = v ; ctx = ctx })
| Structure_value (_, is_rec, patts, exp_ary) ->
  (* The data is recursive, a Prealloc binding is generated *)
  if is_rec then
    (* Prealloc only accept variable patterns *)
    let prealloc p = match p with
    | Pattern_var (_, id) -> Some id
    | _ -> None in
    let exps = MLList.of_array exp_ary in
    let prealloc_vars = MLArray.map prealloc patts in
    (* MLArray.lift_option : 'a option list -> 'a list option
     * Some id_ary = MLArray.lift_option prealloc_vars *)
    Option.bind (MLArray.lift_option prealloc_vars) (fun id_ary ->
    let ids = MLList.of_array id_ary in
    (* Auxiliary function adding a Prealloc of exp bound to id in the given context *)
    let func ctx id exp =
      let idx = Vector.append s (Prealloc exp) in
      ExecutionContext.add id idx ctx in
    (* Foldl of the lists simultaneously using func and the current context *)
    let ctx' = MLList.foldl2 func ctx ids exps in
    (* Return the last value bound by this toplevel phrase *)
    let id = MLArray.get id_ary (MLArray.length id_ary - 1) in
    Option.bind (ExecutionContext.find id ctx') (fun idx ->
    Option.bind (Vector.find s idx) (fun binding ->
    Option.bind (value_of s ctx' binding) (fun v ->
    Some { value = v ; ctx = ctx' }))))
  else
    let func ctx_opt patt exp =
      (* Some ctx = ctx_opt
       * Some v = run_expression ctx exp *)
      Option.bind ctx_opt (fun ctx ->
      Option.bind (run_expression s ctx exp) (fun v -> pattern_match s ctx v patt)) in
    let patt_list = MLList.of_array patts in
    let exps = MLList.of_array exp_ary in
    (* Generate a new environment binding patterns' variable to their respective value *)
    Option.bind (MLList.foldl2 func (Some ctx) patt_list exps) (fun ctx' ->
    (* Get the value to return *)
    let elems = Map.elems (ExecutionContext.execution_ctx_lexical_env ctx') in
    let rev_elems = MLList.rev elems in
    let idx_opt = match rev_elems with
    | [] -> None
    | h :: t -> Some h in
    Option.bind idx_opt (fun idx ->
    Option.bind (Vector.find s idx) (fun last ->
    Option.bind (value_of s ctx' last) (fun v ->
    Some { value = v ; ctx = ctx' }))))
| Structure_type _ -> Some { value = Value_tuple [| |] ; ctx = ctx }

let run_structure s ctx _term_ =
  let func opt _term_ =
    Option.bind opt (fun res ->
    run_structure_item s res.ctx _term_) in
  (* Fake result data used as first input of the fold function below *)
  let fake_res = Some { value = Value_int 0 ; ctx = ctx } in
  match _term_ with
  | Structure (_, items) -> MLArray.fold func fake_res items

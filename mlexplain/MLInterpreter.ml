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

let rec run_expression ctx _term_ = match _term_ with
| Expression_constant (_, c) -> Some (run_constant c)
| Expression_ident (_, id) -> Option.bind (ExecutionContext.find id ctx) (fun b -> value_of ctx b)
| Expression_let (_, is_rec, patts, exp_ary, e2) ->
  if is_rec then
    let prealloc p = match p with
    | Pattern_var (_, id) -> Some id
    | _ -> None in
    let exps = MLList.of_array exp_ary in
    Option.bind (MLArray.lift_option (MLArray.map prealloc patts)) (fun id_ary ->
    let ids = MLList.of_array id_ary in
    let func ctx id exp = ExecutionContext.add id (Prealloc exp) ctx in
    let ctx' = MLList.foldl2 func ctx ids exps in
    run_expression ctx' e2)
  else
    let func ctx_opt patt exp =
      (* Some ctx = ctx_opt
       * Some v = run_expression ctx exp *)
      Option.bind ctx_opt (fun ctx ->
      Option.bind (run_expression ctx exp) (fun v -> pattern_match ctx v patt)) in
    let patt_list = MLList.of_array patts in
    let exps = MLList.of_array exp_ary in
    Option.bind (MLList.foldl2 func (Some ctx) patt_list exps) (fun ctx' ->
    run_expression ctx' e2)
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
  let value_opts = MLArray.map (fun e -> run_expression ctx e) tuple in
  (* Some t = lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun t -> Some (Value_tuple t))
| Expression_array (_, ary) ->
  let value_opts = MLArray.map (fun e -> run_expression ctx e) ary in
  (* Some a = MLArray.lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun a -> Some (Value_array a))
| Expression_variant (_, label, expr_opt) ->
  let value_opt = Option.bind expr_opt (fun e -> run_expression ctx e) in
  let variant = { label = label ; value_opt = value_opt } in
  Some (Value_variant variant)
| Expression_match (loc, expr, cases) ->
  let func = Expression_function (loc, cases) in
  let app = Expression_apply (loc, func, [| expr |]) in
  run_expression ctx app
| Expression_constructor (_, ctor, args) ->
  let value_opts = MLArray.map (fun e -> run_expression ctx e) args in
  (* Some a = MLArray.lift_option value_opts *)
  Option.bind (MLArray.lift_option value_opts) (fun values ->
  let sum = Sumtype { constructor = ctor ; args = values } in
  Some (Value_custom sum))

and value_of ctx b = match b with
| Normal v -> Some v
| Prealloc e -> run_expression ctx e

and pattern_match ctx value patt = match patt with
| Pattern_any _ -> Some ctx
| Pattern_var (_, id) -> Some (ExecutionContext.add id (Normal value) ctx)
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
| Pattern_array (_, patts) ->
  begin
    match value with
    | Value_array ary ->
      if MLArray.length patts === MLArray.length ary then
        pattern_match_array ctx ary patts
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
            | Some v -> pattern_match ctx v patt
          end
      else
        None
    | _ -> None
  end
| Pattern_alias (_, patt, id) ->
  Option.bind (pattern_match ctx value patt) (fun ctx' ->
  Some (ExecutionContext.add id (Normal value) ctx'))
| Pattern_constructor (_, ctor, args) ->
  begin
    match value with
    | Value_custom cstm ->
      begin
        match cstm with
        | Sumtype sum ->
          if sum.constructor === ctor then
            pattern_match_array ctx sum.args args
          else
            None
      end
    | _ -> None
  end
| Pattern_or (_, patt1, patt2) ->
  begin
    match pattern_match ctx value patt1 with
    | Some ctx' -> Some ctx'
    | None -> pattern_match ctx value patt2
  end

and pattern_match_many ctx value cases = match cases with
| [] -> None
| x :: xs ->
  match pattern_match ctx value x.patt with
  | None -> pattern_match_many ctx value xs
  | Some ctx' -> run_expression ctx' x.expr

and pattern_match_array ctx ary patts =
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
        for_loop (pattern_match ctx vali patti) (i + 1) in
      (* Some ctx = ctx_opt *)
      Option.bind ctx_opt some_case_func in
   for_loop (Some ctx) 0

let run_structure_item ctx _term_ = match _term_ with
| Structure_eval (_, e) -> Option.bind (run_expression ctx e) (fun v -> Some { value = v ; ctx = ctx })
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
    let func ctx id exp = ExecutionContext.add id (Prealloc exp) ctx in
    (* Foldl of the lists simultaneously using func and the current context *)
    let ctx' = MLList.foldl2 func ctx ids exps in
    (* Return the last value bound by this toplevel phrase *)
    let id = MLArray.get id_ary (MLArray.length id_ary - 1) in
    Option.bind (ExecutionContext.find id ctx') (fun binding ->
    Option.bind (value_of ctx' binding) (fun v ->
    Some { value = v ; ctx = ctx' })))
  else
    let func ctx_opt patt exp =
      (* Some ctx = ctx_opt
       * Some v = run_expression ctx exp *)
      Option.bind ctx_opt (fun ctx ->
      Option.bind (run_expression ctx exp) (fun v -> pattern_match ctx v patt)) in
    let patt_list = MLList.of_array patts in
    let exps = MLList.of_array exp_ary in
    (* Generate a new environment binding patterns' variable to their respective value *)
    Option.bind (MLList.foldl2 func (Some ctx) patt_list exps) (fun ctx' ->
    (* Get the value to return *)
    let elems = Map.elems (ExecutionContext.execution_ctx_lexical_env ctx') in
    let rev_elems = MLList.rev elems in
    let last_opt = match rev_elems with
    | [] -> None
    | h :: t -> Some h in
    Option.bind last_opt (fun last ->
    Option.bind (value_of ctx' last) (fun v ->
    Some { value = v ; ctx = ctx' })))
| Structure_type _ -> Some { value = Value_tuple [| |] ; ctx = ctx }

let run_structure ctx _term_ =
  let func opt _term_ =
    Option.bind opt (fun res ->
    run_structure_item res.ctx _term_) in
  let fake_res = Some { value = Value_int 0 ; ctx = ctx } in
  match _term_ with
  | Structure (_, items) -> MLArray.fold func fake_res items

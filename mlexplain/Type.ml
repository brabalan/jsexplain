type type_var_value =
| Unknown
| Known of simple_type [@f value]

and type_var = {
  level : int ;
  value : type_var_value
}

and simple_type =
| Variable of type_var [@f type_var]
| Term of string * simple_type array [@f ctor, arguments]

and type_scheme = {
  parameters : type_var list ;
  body : simple_type
}

let trivial_scheme ty = { parameters = [] ; body = ty }

let rec type_var_value_eq v1 v2 =
  match v1 with
  | Unknown ->
    begin
      match v2 with
      | Unknown -> true
      | Known _ -> false
    end
  | Known t1 ->
    begin
      match v2 with
      | Unknown -> false
      | Known t2 -> simple_type_eq t1 t2
    end

and type_var_eq v1 v2 = v1.level === v2.level && type_var_value_eq v1.value v2.value

and simple_type_eq t1 t2 =
  match t1 with
  | Variable v1 ->
    begin
      match t2 with
      | Variable v2 -> type_var_eq v1 v2
      | Term _ -> false
    end
  | Term (ctor1, args1) ->
    begin
      match t2 with
      | Variable _ -> false
      | Term (ctor2, args2) ->
        let args_eq args1 args2 = MLArray.all_true (MLArray.zipwith simple_type_eq args1 args2) in
        ctor1 === ctor2
          && array_length args1 === array_length args2
          && args_eq args1 args2
    end

let int_type = Term ("int", [| |])
let float_type = Term ("float", [| |])
let char_type = Term ("char", [| |])
let string_type = Term ("string", [| |])
let tuple_type types = Term ("*", types)
let arrow_type t1 t2 = Term ("->", [| t1 t2 |])
let array_type t = Term ("array", t)

let is_known tvar = match tvar with
| Known _ -> true
| Unknown -> false

let get_known_value tvar = match tvar with
| Known v -> Some v
| Unknown -> None

(** val value_of : simple_type -> simple_type *)
let rec value_of t = match t with
| Variable tvar ->
  begin
    match get_known_value tvar.value with
    | Some ty -> value_of ty
    | None -> t
  end
| _ -> t

let occurency_test tvar ty =
  let rec test t =
    match value_of t with
    | Variable tvar' -> not (type_var_eq tvar tvar')
    | Term (_, args) -> MLArray.all_true (MLArray.map test args)
  in test ty

let rec fix_levels max_lvl ty =
  match ty with
  | Variable tvar ->
    let fmax = number_of_int max_lvl in
    let flvl = number_of_int tvar.level in
    if flvl > fmax then
      Variable { tvar with level = max_lvl }
    else
      ty
  | Term (ctor, args) ->
    let new_args = MLArray.map (fun t -> fix_levels max_lvl t) args in
    Term (ctor, args)

let rec unify ty1 ty2 =
  let val1 = value_of ty1 in
  let val2 = value_of ty2 in
  if simple_type_eq val1 val2 then
    Some val1
  else
    match val1 with
    | Term (ctor1, args1) ->
      begin
        match val2 with
        | Term (ctor2, args2) ->
          if not (ctor1 === ctor2) then
            None
          else
            let new_opt_args = MLArray.zipwith unify args1 args2 in
            let new_args_opt = MLArray.lift_option new_opt_args in
            Option.bind new_args_opt (fun new_args ->
              Some (Term (ctor1, new_args)))
        | Variable var2 ->
          if occurency_test var2 val1 then
            let val1' = fix_levels var2.level val1 in
            Some (Variable { var2 with value = Known val1' })
          else
            None
      end
    | Variable var1 ->
      if occurency_test var1 ty2 then
        let val2' = fix_levels var1.level ty2 in
        Some (Variable { var1 with value = Known val2' })
      else
        None

let new_unknown lvl = Variable { level = lvl ; value = Unknown }

let generalize lvl ty =
  let rec find_parameters params ty =
    match value_of ty with
    | Variable tvar ->
      let fvarlevel = number_of_int tvar.level in
      let flvl = number_of_int lvl in
      if fvarlevel > flvl && not (MLList.any type_var_eq tvar params) then
        tvar :: params
      else
        params
    | Term (ctor, args) ->
      MLArray.fold find_parameters params args in
  let params = find_parameters [] ty in
  { parameters = params ; body = ty }

open CalcSyntax

type ('a, 'b) either =
| Left of 'a [@f value]
| Right of 'b [@f value]

let either_is_left = function
| Left l -> true
| Right r -> false

let literal_is_null lit =
  match lit with
  | Literal_int i -> float_of_int i = 0.0
  | Literal_float f -> f = 0.0

let add_literals = function
| Literal_int lit1 ->
  begin
    function
    | Literal_int lit2 -> Right (Literal_int (lit1 + lit2))
    | Literal_float lit2 -> Right (Literal_float (float_of_int lit1 +. lit2))
  end
| Literal_float lit1 ->
  begin
    function
    | Literal_int lit2 -> Right (Literal_float (lit1 +. float_of_int lit2))
    | Literal_float lit2 -> Right (Literal_float (lit1 +. lit2))
  end

let sub_literals = function
| Literal_int lit1 ->
  begin
    function
    | Literal_int lit2 -> Right (Literal_int (lit1 - lit2))
    | Literal_float lit2 -> Right (Literal_float (float_of_int lit1 -. lit2))
  end
| Literal_float lit1 ->
  begin
    function
    | Literal_int lit2 -> Right (Literal_float (lit1 -. float_of_int lit2))
    | Literal_float lit2 -> Right (Literal_float (lit1 -. lit2))
  end

let mul_literals = function
| Literal_int lit1 ->
  begin
    function
    | Literal_int lit2 -> Right (Literal_int (lit1 * lit2))
    | Literal_float lit2 -> Right (Literal_float (float_of_int lit1 *. lit2))
  end
| Literal_float lit1 ->
  begin
    function
    | Literal_int lit2 -> Right (Literal_float (lit1 *. float_of_int lit2))
    | Literal_float lit2 -> Right (Literal_float (lit1 *. lit2))
  end

let div_literals = function
| Literal_int lit1 ->
  begin
    fun lit ->
      if literal_is_null lit then
        Left "Division by zero"
      else match lit with
      | Literal_int lit2 -> Right (Literal_int (lit1 / lit2))
      | Literal_float lit2 -> Right (Literal_float (float_of_int lit1 /. lit2))
  end
| Literal_float lit1 ->
  begin
    fun lit ->
      if literal_is_null lit then
        Left "Division by zero"
      else match lit with
      | Literal_int lit2 -> Right (Literal_float (lit1 /. float_of_int lit2))
      | Literal_float lit2 -> Right (Literal_float (lit1 /. lit2))
  end

let mod_literals = function
| Literal_int lit1 ->
  begin
    function
    | Literal_int lit2 -> Right (Literal_int (lit1 + lit2))
    | Literal_float lit2 -> Left "Expected an int and got a float"
  end
| Literal_float lit1 -> fun _ -> Left "Expected an int and got a float"

let rec eval_expr =
  let minus_lit = function
  | Literal_int i -> Literal_int (0 - i)
  | Literal_float f -> Literal_float (0.0 -. f)

  in let apply_binary_op lit1 lit2 = function
  | Binary_op_add -> add_literals lit1 lit2
  | Binary_op_sub -> sub_literals lit1 lit2
  | Binary_op_mul -> mul_literals lit1 lit2
  | Binary_op_div -> div_literals lit1 lit2
  | Binary_op_mod -> mod_literals lit1 lit2

  in let eval_unary_op op arg =
    let unsafe_eval_unary_op op lit =
      Right (match op with
      | Unary_op_add -> lit
      | Unary_op_sub -> minus_lit lit)
      
    in match eval_expr arg with
    | Left err -> Left err
    | Right lit -> unsafe_eval_unary_op op lit

  in let eval_binary_op op arg1 arg2 =
    match eval_expr arg1 with
    | Left err -> Left err
    | Right lit1 ->
      begin
        match eval_expr arg2 with
        | Left err -> Left err
        | Right lit2 -> apply_binary_op lit1 lit2 op
      end

  in function
  | Expr_literal lit -> Right lit
  | Expr_unary_op (op, arg) -> eval_unary_op op arg
  | Expr_binary_op (arg1, op, arg2) -> eval_binary_op op arg1 arg2

let float_of_literal = function
| Literal_int i -> float_of_int i
| Literal_float f -> f

(*
let run_calc =
  let code = Sys.argv.(1) in
  let ast = parse_string code in
  match eval_expr ast with
  | Left err -> prerr_endline err
  | Right lit -> match lit with
    | Literal_int i -> print_int i ; print_newline ()
    | Literal_float f -> print_float f ; print_newline ()
*)

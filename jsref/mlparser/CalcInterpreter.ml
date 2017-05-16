open CalcSyntax

(** Type used to represent a variable that can have two different values. It is used typically in computations
  * that can fail. Then, the Left constructor stores an error and the Right one stores a well-formed result. *)
type ('a, 'b) either =
| Left of 'a [@f err]
| Right of 'b [@f value]

(** Function used to test if a given literal value represents a zero, regardless of whether it is an integer or a float. *)
let literal_is_null lit =
  match lit with
  | Literal_int (_, i) -> float_of_int i = 0.0
  | Literal_float (_, f) -> f = 0.0

(********************************************************************************
 * Primitives performing the operations : +, -, *, / and mod
 ********************************************************************************)

let add_literals = function
| Literal_int (loc, lit1) ->
  begin
    function
    | Literal_int (_, lit2) -> Right (Literal_int (loc, lit1 + lit2))
    | Literal_float (_, lit2) -> Right (Literal_float (loc, float_of_int lit1 +. lit2))
  end
| Literal_float (loc, lit1) ->
  begin
    function
    | Literal_int (_, lit2) -> Right (Literal_float (loc, lit1 +. float_of_int lit2))
    | Literal_float (_, lit2) -> Right (Literal_float (loc, lit1 +. lit2))
  end

let sub_literals = function
| Literal_int (loc, lit1) ->
  begin
    function
    | Literal_int (_, lit2) -> Right (Literal_int (loc, lit1 - lit2))
    | Literal_float (_, lit2) -> Right (Literal_float (loc, float_of_int lit1 -. lit2))
  end
| Literal_float (loc, lit1) ->
  begin
    function
    | Literal_int (_, lit2) -> Right (Literal_float (loc, lit1 -. float_of_int lit2))
    | Literal_float (_, lit2) -> Right (Literal_float (loc, lit1 -. lit2))
  end

let mul_literals = function
| Literal_int (loc, lit1) ->
  begin
    function
    | Literal_int (_, lit2) -> Right (Literal_int (loc, lit1 * lit2))
    | Literal_float (_, lit2) -> Right (Literal_float (loc, float_of_int lit1 *. lit2))
  end
| Literal_float (loc, lit1) ->
  begin
    function
    | Literal_int (_, lit2) -> Right (Literal_float (loc, lit1 *. float_of_int lit2))
    | Literal_float (_, lit2) -> Right (Literal_float (loc, lit1 *. lit2))
  end

let div_literals = function
| Literal_int (loc, lit1) ->
  begin
    fun lit ->
      if literal_is_null lit then
        (* Left { loc = loc ; value = "Division by zero" } *)
        Left "Division by zero"
      else match lit with
      | Literal_int (_, lit2) -> Right (Literal_int (loc, lit1 / lit2))
      | Literal_float (_, lit2) -> Right (Literal_float (loc, float_of_int lit1 /. lit2))
  end
| Literal_float (loc, lit1) ->
  begin
    fun lit ->
      if literal_is_null lit then
        (* Left { loc = loc ; value = "Division by zero" } *)
        Left "Division by zero"
      else match lit with
      | Literal_int (_, lit2) -> Right (Literal_float (loc, lit1 /. float_of_int lit2))
      | Literal_float (_, lit2) -> Right (Literal_float (loc, lit1 /. lit2))
  end

let mod_literals = function
| Literal_int (loc, lit1) ->
  begin
    function
    | Literal_int (_, lit2) -> Right (Literal_int (loc, lit1 + lit2))
    (* The modulo applies only on integers *)
    (* | Literal_float (_, lit2) -> Left { loc = loc ; value = "Expected an int and got a float" } *)
    | Literal_float (_, lit2) -> Left "Expected an int and got a float"
  end
(* Same here *)
(* | Literal_float (loc, lit1) -> fun _ -> Left { loc = loc ; value = "Expected an int and got a float" } *)
| Literal_float (loc, lit1) -> fun _ -> Left "Expected an int and got a float"

(* Function used to evaluate a formula AST. It is split in a lot of sub-functions to help the JS generator *)
let rec eval_expr _term_ =
  (* Implementation of the unary minus *)
  let minus_lit = function
  | Literal_int (loc, i) -> Literal_int (loc, 0 - i)
  | Literal_float (loc, f) -> Literal_float (loc, 0.0 -. f)

  (* Calls the right primitive regarding the given binary operator *)
  in let apply_binary_op lit1 lit2 = function
  | Binary_op_add _ -> add_literals lit1 lit2
  | Binary_op_sub _ -> sub_literals lit1 lit2
  | Binary_op_mul _ -> mul_literals lit1 lit2
  | Binary_op_div _ -> div_literals lit1 lit2
  | Binary_op_mod _ -> mod_literals lit1 lit2

  (* Checks whether the given unary operator imposes some computation or not and than do it *)
  in let eval_unary_op op arg =
    let unsafe_eval_unary_op op lit =
      Right (match op with
      | Unary_op_add _ -> lit
      | Unary_op_sub _ -> minus_lit lit)
      
    (* Returns the error if the argument was one *)
    in match eval_expr arg with
    | Left err -> Left err
    | Right lit -> unsafe_eval_unary_op op lit

  (* Binary-operation evaluation *)
  in let eval_binary_op op arg1 arg2 =
    match eval_expr arg1 with
    | Left err -> Left err
    | Right lit1 ->
      begin
        match eval_expr arg2 with
        | Left err -> Left err
        | Right lit2 -> apply_binary_op lit1 lit2 op
      end

  (* eval_expr main expression *)
  in match _term_ with
  | Expr_literal (_, lit) -> Right lit
  | Expr_unary_op (_, op, arg) -> eval_unary_op op arg
  | Expr_binary_op (_, arg1, op, arg2) -> eval_binary_op op arg1 arg2

(* Conversion from literal to float *)
let float_of_literal = function
| Literal_int (_, i) -> float_of_int i
| Literal_float (_, f) -> f

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

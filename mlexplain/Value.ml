type value =
| Value_int of int [@f value]
| Value_float of float [@f value]
| Value_char of char [@f value]
| Value_string of string [@f value]
| Value_tuple of value array [@f value]
| Value_list of value list [@f value]
| Value_array of value array [@f value]
| Value_fun of (value -> value Unsafe.value) [@f value]
| Value_variant of variant [@f value]
| Value_struct of record [@f value]
| Value_functor of (value -> value Unsafe.value) [@f value]
| Value_custom of custom_type [@f value]
| Value_exception of sumtype [@f value]

and variant = {
  label : string ;
  value_opt : (value Unsafe.value) option
}

and custom_type =
| Sumtype of sumtype [@f sumtype]
| Record of record [@f record]

and sumtype = {
  constructor : string ;
  args : value array
}

(** Map associating a name to an index in the program's state *)
and record = (string, int) Map.map

(** Type used two handle bindings, Prealloc is useful for recursive functions *)
type binding =
| Normal of value [@f normal_alloc]
| Prealloc of MLSyntax.expression [@f prealloc]

(** Apply the function to the sumtype if the value is one *)
let do_sumtype value func = match value with
| Value_custom custom ->
  begin
    match custom with
    | Sumtype s -> func s
    | _ -> Unsafe.error "Not a sumtype"
  end
| _ -> Unsafe.error "Not a sumtype"

(** Convert a value to a boolean *)
let bool_of_value x =
  do_sumtype x (fun s ->
  match s.constructor with
  | "true" -> Unsafe.box true
  | "false" -> Unsafe.box false
  | _ -> Unsafe.error "Not a boolean")

(** Convert a boolean to a value *)
let value_of_bool b =
  let res ctor =  Value_custom (Sumtype { constructor = ctor ; args = [| |] }) in
  match b with
  | true -> res "true"
  | false -> res "false"

(** Compare two values *)
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
| Value_variant vr1 ->
  begin
    match v2 with
    | Value_variant vr2 ->
      let cmp v1 v2 = Unsafe.eq value_eq value_eq v1 v2 in
      let val_eq = Option.eq cmp vr1.value_opt vr2.value_opt in
      vr1.label === vr2.label && val_eq
    | _ -> false
  end
| Value_custom c1 ->
  begin
    match v2 with
    | Value_custom c2 -> custom_eq c1 c2
    | _ -> false
  end
| _ -> false

and custom_eq c1 c2 = match c1 with
| Sumtype s1 ->
  begin
    match c2 with
    | Sumtype s2 -> sumtype_eq s1 s2
    | _ -> false
  end
| _ -> false

and sumtype_eq s1 s2 =
  let t1 = Value_tuple s1.args in
  let t2 = Value_tuple s2.args in
  s1.constructor === s2.constructor && value_eq t1 t2

let value_inf v1 v2 = match v1 with
| Value_int i1 ->
  begin
    match v2 with
    | Value_int i2 ->
      let num1 = number_of_int i1 in
      let num2 = number_of_int i2 in
      Unsafe.box (value_of_bool (num1 < num2))
    | _ -> Unsafe.error "Expected an integer"
  end
| Value_float f1 ->
  begin
    match v2 with
    | Value_float f2 -> Unsafe.box (value_of_bool (f1 < f2))
    | _ -> Unsafe.error "Expected a float"
  end
| Value_char c1 ->
  begin
    match v2 with
    | Value_char c2 ->
      let num1 = number_of_int (int_of_char c1) in
      let num2 = number_of_int (int_of_char c2) in
      Unsafe.box (value_of_bool (num1 < num2))
    | _ -> Unsafe.error "Expected a character"
  end
| Value_string s1 ->
  begin
    match v2 with
    | Value_string s2 ->
      let b = string_compare s1 s2 === -1 in
      Unsafe.box (value_of_bool b)
    | _ -> Unsafe.error "Expected a string"
  end
| _ -> Unsafe.except (Value_custom (Sumtype { constructor = "Invalid_argument" ; args = [| |] }))

(** Create a unit value *)
let nil = Value_custom (Sumtype { constructor = "()" ; args = [| |] })

(** Check if the given value is a sumtype and is of given constructor *)
let is_sumtype_ctor ctor v = match v with
| Value_custom c ->
  begin
    match c with
    | Sumtype s -> ctor === s.constructor
    | _ -> false
  end
| _ -> false

(** Apply the function on the record if the value is one *)
let do_record value func = match value with
| Value_custom custom ->
  begin
    match custom with
    | Record r -> func r
    | _ -> Unsafe.error "Not a record"
  end
| _ -> Unsafe.error "Not a record"

(** Apply the function to the record if the value is one, return the default value otherwise *)
let do_record_with_default value dflt func = match value with
| Value_custom custom ->
  begin
    match custom with
    | Record r -> func r
    | _ -> dflt
  end
| _ -> dflt

let get_function = function
| Value_fun f -> Unsafe.box f
| _ -> Unsafe.error "Not a function"

(************************************************************
 * Language primitives
 ************************************************************)

let int_bin_op op =
  let func = function
  | Value_int a ->
    let curry = function
    | Value_int b -> Unsafe.box (Value_int (op a b))
    | _ -> Unsafe.error "Expected an int" in
    Unsafe.box (Value_fun curry)
  | _ -> Unsafe.error "Expected an int" in
  Value_fun func

let float_bin_op op =
  let func = function
  | Value_float a ->
    let curry = function
    | Value_float b -> Unsafe.box (Value_float (op a b))
    | _ -> Unsafe.error "Expected a float" in
    Unsafe.box (Value_fun curry)
  | _ -> Unsafe.error "Expected a float" in
  Value_fun func

let bool_bin_op op =
  let func a =
    Unsafe.bind (bool_of_value a) (fun b1 ->
    let curry b = Unsafe.bind (bool_of_value b) (fun b2 -> Unsafe.box (value_of_bool (op b1 b2))) in
    Unsafe.box (Value_fun curry)) in
  Value_fun func

let cmp_bin_op op =
  let func a =
    let curry b = Unsafe.box (value_of_bool (op a b)) in
    Unsafe.box (Value_fun curry) in
  Value_fun func

let raise_function = Value_fun (fun v -> Unsafe.except v)

let prim_int_plus = int_bin_op ( fun a b -> a + b )
let prim_int_sub = int_bin_op ( fun a b -> a - b )
let prim_int_mul = int_bin_op ( fun a b -> a * b )
let prim_int_div = int_bin_op ( fun a b -> a / b )

let prim_float_plus = float_bin_op ( fun a b -> a +. b )
let prim_float_sub = float_bin_op ( fun a b -> a -. b )
let prim_float_mul = float_bin_op ( fun a b -> a *. b )
let prim_float_div = float_bin_op ( fun a b -> a /. b )

let prim_bool_and = bool_bin_op ( fun a b -> a && b )
let prim_bool_or = bool_bin_op ( fun a b -> a || b )
(* let prim_mod x = int_bin_op ( mod ) x *)

let prim_eq = cmp_bin_op value_eq
let prim_neq =
  let func a b = not (value_eq a b) in
  cmp_bin_op func
let prim_lt = Value_fun (fun a -> Unsafe.box (Value_fun (fun b -> value_inf a b)))
let prim_le =
  let func a b =
    Unsafe.bind (value_inf a b) (fun iv ->
    Unsafe.bind (bool_of_value iv) (fun ib ->
    let eqb = value_eq a b in
    Unsafe.box (value_of_bool (ib || eqb)))) in
  Value_fun (fun a -> Unsafe.box (Value_fun (fun b -> func a b)))
let prim_gt = Value_fun (fun a -> Unsafe.box (Value_fun (fun b -> value_inf b a)))
let prim_ge =
  let func a b =
    Unsafe.bind (value_inf a b) (fun iv ->
    Unsafe.bind (bool_of_value iv) (fun ib ->
    let eqb = value_eq a b in
    Unsafe.box (value_of_bool (ib || eqb)))) in
  Value_fun (fun a -> Unsafe.box (Value_fun (fun b -> func b a)))

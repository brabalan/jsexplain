type value =
| Value_int of int [@f value]
| Value_float of float [@f value]
| Value_char of char [@f value]
| Value_string of string [@f value]
| Value_tuple of value array [@f value]
| Value_list of value list [@f value]
| Value_array of value array [@f value]
| Value_fun of (value -> value option) [@f value]
| Value_variant of variant [@f value]
| Value_custom of custom_type [@f value]

and variant = {
  label : string ;
  value_opt : value option
}

and custom_type =
| Sumtype of sumtype [@f sumtype]
(*| Record of record [@f record]*)

and sumtype = {
  constructor : string ;
  args : value array
}

and record = (string, value) Map.map

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
      let val_eq = Option.eq value_eq vr1.value_opt vr2.value_opt in
      vr1.label === vr2.label && val_eq
    | _ -> false
  end
| Value_custom c1 ->
  begin
    match v2 with
    | Value_custom c2 -> custom_eq c1 c2
    | _ -> false
  end

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

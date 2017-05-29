let fold f first ary = array_fold f first ary
let length ary = array_length ary
let map f ary = array_map f ary

let all_true ary =
  let f cur b = cur && b in
  array_fold f true ary

(* Create a new array zipping both input arrays using the function f :
 * forall i e, res.(i) = e -> e = f a1.(i) a2.(i) *)
let zipwith f a1 a2 =
  let len_a1 = array_length a1 in
  let len_a2 = array_length a2 in
  (* min_size is the size of the shortest array of a1 and a2 *)
  let min_size = min len_a1 len_a2 in
  let res = array_make min_size (f (array_get a1 0) (array_get a2 0)) in
  (* For each i from 0 to min_size
   * populate the resulting array with (f a1.(i) a2.(i)) *)
  let rec for_loop i =
    if (number_of_int i) < (number_of_int min_size) then
    begin
      let res_f = f (array_get a1 i) (array_get a2 i) in
      array_set res i res_f ;
      for_loop (i + 1)
    end
    else
      res in
  for_loop 1

(** val lift_option : 'a option array -> 'a array option *)
let lift_option ary =
  (* val f : 'a array option -> 'a option -> 'a array option *)
  let f ary_opt opt =
    (* Some ary = ary_opt
     * Some v = opt *)
    Option.bind ary_opt (fun ary ->
      Option.bind opt (fun v ->
        Some (array_append ary (array_make 1 v))))
  (* If the array contains no None value, the resulting value is Some A,
   * with A being an array containing input-array inner values *)
  in array_fold f (Some [| |]) ary


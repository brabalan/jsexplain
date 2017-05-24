(** Reverse a list *)
let rev lst =
  let rec aux acc lst = match lst with
  | [] -> acc
  | h :: t -> aux (h :: acc) t
  in aux [] lst

(** Convert an array to a list *)
let of_array ary =
  let len = array_length ary in
  (*
   * Iterates on each element of the array and concatenate it to the accumulator
   * It iterates down to 0 to get the elements from last to first, not to reverse the list afterward
   *)
  let rec for_loop acc i =
    if i === -1 then acc else for_loop (array_get ary i :: acc) (i - 1) in
  for_loop [] (len - 1)

(** Apply the given function on each element of the list and return the resulting list *)
let map f lst =
  let rec aux acc lst = match lst with
  | [] -> rev acc
  | h :: t -> aux (f h :: acc) t
  in aux [] lst

(** Apply the given function *)
let rec foldl f first rest = match rest with
| [] -> first
| h :: t -> foldl f (f first h) t

(** Test if every elements in the list accept the predicate *)
let for_all pred lst = foldl (fun b e -> b && pred e) true lst

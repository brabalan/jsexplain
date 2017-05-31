(** Alias for equality-test function type *)
type 'a equality_test = 'a -> 'a -> bool

(** Type representing a set *)
type 'a set = {
  eq : 'a equality_test ;
  values : 'a list
}

(** Create an empty set using the given equality test *)
let empty_set eq = { eq = eq ; values = [] }

(** Check if the value exists in the set *)
let rec mem value set = match set.values with
| [] -> false
| h :: t ->
  let set' = { set with values = t } in
  set.eq h value || mem value set'

(** Add the value to the set, if it does not exist already *)
let add value set =
  if mem value set then
    set
  else
    { set with values = value :: set.values }

(** Union between the two sets *)
let rec union set1 set2 = match set1.values with
| [] -> set2
| h :: t ->
  let set1' = { set1 with values = t } in
  let set2' = add h set2 in
  union set1' set2'

(** Set-theoretic difference between the two sets *)
let rec sub set1 set2 =
  (** Remove the element from the set *)
  let rec sub_elem set e = match set.values with
  | [] -> set
  | h :: t ->
    if set.eq h e then
      { set with values = t }
    else
      let set' = sub_elem { set with values = t } e in
      { set' with values = h :: set'.values } in
  match set2.values with
  | [] -> set1
  | h :: t ->
    let set1' = sub_elem set1 h in
    sub set1' { set2 with values = t }

(** Create a set from a list of values, duplicates are removed *)
let from_list eq lst = MLList.foldl (fun set v -> add v set) (empty_set eq) lst

(** Get values from the set *)
let elems set = set.values

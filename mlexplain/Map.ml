(** Type representing a pair in a map *)
type ('k, 'v) binding = {
  key : 'k ;
  value : 'v
}

(** Alias for an equality-test function *)
type 'a equality_test = 'a -> 'a -> bool

(** The actual map datatype *)
type ('a, 'b) map = {
  eq : 'a equality_test ;
  bindings : ('a, 'b) binding list
}

(** Create a map with the given equality-test function *)
let empty_map eq = { eq = eq ; bindings = [] }

(** Accessors *)
let key_from_binding b = b.key
let value_from_binding b = b.value

(** Add the pair (key, value) in the map *)
let add key value m = { m with bindings = { key = key ; value = value } :: m.bindings }

(** Retrieve the value associated with the given key in the map *)
let rec find key m = match m.bindings with
| [] -> None
| h :: t ->
  if m.eq h.key key then
    Some h.value
  else
    find key { m with bindings = t }

(** Check if the given key exists in the map *)
let rec mem key = function
| [] -> false
| h :: t -> key = h.key || mem key t

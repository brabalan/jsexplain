type ('k, 'v) binding = {
  key : 'k ;
  value : 'v
}

type 'a equality_test = 'a -> 'a -> bool
type ('a, 'b) map = {
  eq : 'a equality_test ;
  bindings : ('a, 'b) binding list
}

let empty_map eq = { eq = eq ; bindings = [] }

let key_from_binding b = b.key
let value_from_binding b = b.value

let add key value m = { m with bindings = { key = key ; value = value } :: m.bindings }

let rec find key m = match m.bindings with
| [] -> None
| h :: t ->
  if m.eq h.key key then
    Some h.value
  else
    find key { m with bindings = t }

let rec mem key = function
| [] -> false
| h :: t -> key = h.key || mem key t

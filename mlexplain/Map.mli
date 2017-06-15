(** Alias for comparison functions *)
type 'a equality_test = 'a -> 'a -> bool

(** Type for a map using keys of 'a and values of 'b *)
type ('a, 'b) map

(** Create a empty map using the given equality-testing and show functions *)
val empty_map : 'a equality_test -> ('a -> string) -> ('a, 'b) map

(** Try to find the value associated to the given key in the given map *)
val find : 'a -> ('a, 'b) map -> ('c, 'b) Unsafe.t

(** Add the given entry to the given map *)
val add : 'a -> 'b -> ('a, 'b) map -> ('a, 'b) map

(** Return the left-biased union of both maps *)
val union : ('a, 'b) map -> ('a, 'b) map -> ('a, 'b) map

(** Apply the function on each value in the map *)
val map : ('b -> 'c) -> ('a, 'b) map -> ('a, 'c) map

(** Delete the pair identified with the given key from the map *)
val remove : 'a -> ('a, 'b) map -> ('a, 'b) map

(** Get the values stored in the map *)
val elems : ('a, 'b) map -> 'b list

(** Alias for comparison functions *)
type 'a equality_test = 'a -> 'a -> bool

(** Type for a map using keys of 'a and values of 'b *)
type ('a, 'b) map

(** Create a empty map using the given equality-testing function *)
val empty_map : 'a equality_test -> ('a, 'b) map

(** Add the given entry to the given map *)
val add : 'a -> 'b -> ('a, 'b) map -> ('a, 'b) map

(** Try to find the value associated to the given key in the given map *)
val find : 'a -> ('a, 'b) map -> 'b option

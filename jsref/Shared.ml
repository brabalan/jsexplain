open Datatypes
open Heap
(*open String0*)




(** val option_case : 'a2 -> ('a1 -> 'a2) -> 'a1 option -> 'a2 **)

let option_case d f o = match o with
| Some x -> f x
| None -> d

(** val int_of_char : char -> float **)

  (* let int_of_char = (fun c -> float_of_int (int_of_char c)) *)

(** val string_sub : string -> int -> int -> string **)

let string_sub s n l =
  substring n l s

(** val lt_int_decidable : float -> float -> bool **)

let lt_int_decidable x y =  x < y

(** val le_int_decidable : float -> float -> bool **)

let le_int_decidable x y =  x <= y

(** val ge_nat_decidable : int -> int -> bool **)

let ge_nat_decidable x y = int_ge x y


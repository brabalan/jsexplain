
(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst p = let (x, y) = p in x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd p = let (x, y) = p in y

type comparison =
| Eq
| Lt
| Gt


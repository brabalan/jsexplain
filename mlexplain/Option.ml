(** Monadic bind operation
 * Haskell notation : Monad m => m a -> (a -> m b) -> m b
 * In this case, it takes an option, applies the function iff the given option in not None
 * and return the result :
 * val bind : 'a option -> ('a -> 'b option) -> 'b option *)
let bind opt f = match opt with
| Some v -> f v
| None -> None

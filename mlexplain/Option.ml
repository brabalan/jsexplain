(** Monadic bind operation
 * Haskell notation : Monad m => m a -> (a -> m b) -> m b
 * In this case, it takes an option, applies the function iff the given option in not None
 * and return the result :
 * val bind : 'a option -> ('a -> 'b option) -> 'b option *)
let bind opt f = match opt with
| Some v -> f v
| None -> None

(** Equality-test function using a helper function to test the inner value *)
let eq func o1 o2 = match o1 with
| None ->
  begin
    match o2 with
    | None -> true
    | Some _ -> false
  end
| Some v1 ->
  begin
    match o2 with
    | None -> false
    | Some v2 -> func v1 v2
  end

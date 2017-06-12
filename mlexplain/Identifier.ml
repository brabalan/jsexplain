type t =
| Lident of string [@f id]
| Ldot of t * string [@f path, id]

(*
let rec ident_eq id1 id2 = match id1 with
| Lident i1 ->
  begin
    match id2 with
    | Lident i2 -> i1 === i2
    | Ldot _ -> false
  end
| Ldot (d1, s1) ->
  begin
    match id2 with
    | Lident _ -> false
    | Ldot (d2, s2) -> s1 === s2 && ident_eq d1 d2
  end *)

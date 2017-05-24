let bind opt f = match opt with
| Some v -> f v
| None -> None

type execution_ctx = {
  execution_ctx_lexical_env : (string, int) Map.map ;
  opened_modules : ((string, int) Map.map) list
}

let empty = {
  execution_ctx_lexical_env = Map.empty_map (fun s1 s2 -> s1 === s2) (fun s -> s) ;
  opened_modules = []
}

let from_map map = {
  execution_ctx_lexical_env = map ;
  opened_modules = []
}

let find n ctx =
  let fst = Map.find n ctx.execution_ctx_lexical_env in
  let func fst md = match fst with
  | Unsafe.Result _ -> fst
  | _ -> Map.find n md in
  MLList.foldl func fst ctx.opened_modules

let add n v ctx = { ctx with execution_ctx_lexical_env = Map.add n v ctx.execution_ctx_lexical_env }

let execution_ctx_lexical_env ctx = ctx.execution_ctx_lexical_env

let open_module map ctx = { ctx with opened_modules = map :: ctx.opened_modules }

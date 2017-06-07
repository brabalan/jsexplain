type execution_ctx = {
  execution_ctx_lexical_env : (string, Value.binding) Map.map ;
  execution_ctx_strict : bool
}

let empty = {
  execution_ctx_lexical_env = Map.empty_map (fun s1 s2 -> s1 === s2) ;
  execution_ctx_strict = true
}

let find n ctx = Map.find n ctx.execution_ctx_lexical_env

let add n v ctx = { ctx with execution_ctx_lexical_env = Map.add n v ctx.execution_ctx_lexical_env }

let execution_ctx_lexical_env ctx = ctx.execution_ctx_lexical_env

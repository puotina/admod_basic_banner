type t = {
  batsh : Parser.t;
  batch_ast : Winbat_ast.t;
  batch_ast_expanded : Winbat_ast.t;
}

let compile (bat
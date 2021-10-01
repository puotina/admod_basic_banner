type t = {
  batsh : Parser.t;
  batch_ast : Winbat_ast.t;
  batch_ast_expanded : Winbat_ast.t;
}

let compile (batsh : Parser.t) : t =
  let batch_ast = Winbat_compile.compile batsh in
  let batch_ast_expanded = Winbat
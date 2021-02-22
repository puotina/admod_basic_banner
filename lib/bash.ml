type t = {
  batsh : Parser.t;
  bash_ast : Bash_ast.t;
  bash_ast_expanded : Bash_ast.t;
}

let compile (batsh : Parser.t) : t =
  let bash_ast = Bash_compile.compile batsh in
  let bash_ast_ex
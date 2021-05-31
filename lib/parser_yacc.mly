%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENTIFIER
%token <string> COMMENT
%token TRUE
%token FALSE
%token IF
%token ELSE
%token WHILE
%token FUNCTION
%token GLOBAL
%token RETURN
%token EQUAL
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMICOLON
%token COMMA
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token MODULO
%token CONCAT
%token NOT
%token SEQ
%token SNE
%token AEQ
%token ANE
%token GT
%token LT
%token GE
%token LE
%token EOF

%nonassoc SEQ SNE AEQ ANE
%nonassoc GT LT GE LE
%left CONCAT
%nonassoc NOT
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO

%nonassoc IF
%nonassoc ELSE

%start program
%type <Batsh_ast.t> program

%%

program:
    toplevel_list; EOF;
      { $1 }
  ;

toplevel:
  | statement;
      { Batsh_ast.Statement $1 }
  | FUNCTION; IDENTIFIER; LEFT_PAREN;
      identifier_list; RIGHT_PAREN;
      LEFT_BRACE; statement_list; RIGHT_BRACE;
      { Batsh_ast.Function ($2, $4, $7) }

toplevel_list:
  |   { [] }
  | toplevel; toplevel_list
      { $1 :: $2 }

statement:
  | SEMICOLON;
      { Batsh_ast.Empty }
  | COMMENT;
      { Batsh_ast.Comment $1 }
  | expression; SEMICOLON;
      { Batsh_ast.Expression $1 }
  | LEFT_BRACE; statement_list; RIGHT_BRACE;
      { Batsh_ast.Block $2 }
  | leftvalue; EQUAL; expression; SEMICOLON;
      { Batsh_ast.Assignment ($1, $3) }
  | if_statement
      { $1 }
  | loop_statement
      { $1 }
  | GLOBAL IDENTIFIER; SEMICOLON
      { Batsh_ast.Global $2 }
  | RETURN expression; SEMICOLON
      { Batsh_ast.Return (Some $2)}
  | RETURN SEMICOLON
      { Batsh_ast.Return None}

statement_list:
  |   { [] }
  | statement; statement_list
      { $1 :: $2 }

if_statement:
  | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement      %prec IF
      { Batsh_ast.If ($3, $5) }
  | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement; ELSE; statement
      { Batsh_ast.IfElse ($3, $5, $7) }

loop_statement:
  | WHILE; LEFT_PAREN; expression; RIGHT_PAREN; statement;
      { Batsh_ast.While ($3, $5) }
  ;

expression:
  | leftvalue
      { Batsh_ast.Leftvalue $1 }
  | STRING
      { Batsh_ast.String $1 }
  | INT
      { Batsh_ast.Int $1 }
  | FLOAT
      { Batsh_ast.Float $1 }
  | TRUE
      { Batsh_ast.Bool true }
  | FALSE
      { Batsh_ast.Bool false }
  | LEFT_BRACK; expression_list; RI
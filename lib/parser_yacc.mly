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
      { Batsh_ast
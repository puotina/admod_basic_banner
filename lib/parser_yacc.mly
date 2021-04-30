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
     

// parser.mly

%{ open Parsetree %}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token EQ
%token LESSTHAN
%token PLUS
%token LPAREN RPAREN LCURLY RCURLY LBRACKET RBRACKET
%token COMMA COLON SEMICOLON
%token INT VOID
%token TYPE VAL VAR FUNC
%token IF ELSE FOR WHILE CONTINUE BREAK RETURN
%token EOF

%nonassoc EQ
%nonassoc LESSTHAN
%left PLUS

%start <Parsetree.top_decl list> file

%%

file: EOF                { [] }
  | translation_unit EOF { $1 }
;

translation_unit: top_decl    { [$1] }
  | top_decl translation_unit { $1 :: $2 }
;

top_decl: type_def { TypeDef $1 }
  | val_decl       { ValDecl $1 }
  | func_decl      { FuncDecl $1 }
;

type_def: TYPE IDENTIFIER EQ type_ SEMICOLON { ($2, $4) }
;

val_decl: VAL IDENTIFIER EQ expr SEMICOLON       { ValI ($2, $4) }
  | VAL IDENTIFIER COLON type_ EQ expr SEMICOLON { Val ($2, $4, $6) }
  | VAR IDENTIFIER EQ expr SEMICOLON             { VarI ($2, $4) }
  | VAR IDENTIFIER COLON type_ EQ expr SEMICOLON { Var ($2, $4, $6) }
;

func_decl: FUNC IDENTIFIER
LPAREN argument_list RPAREN COLON type_
compound_statement                             { ($2, $4, $7, $8) }
;

argument_list:                                 { [] }
  | IDENTIFIER COLON type_                     { [($1, $3)] }
  | IDENTIFIER COLON type_ COMMA argument_list { ($1, $3) :: $5 }
;

statement: SEMICOLON              { Empty }
  | val_decl                      { Decl $1 }
  | expr SEMICOLON                { Expr $1 }
  | compound_statement            { $1 }
  | IF expr compound_statement    { IfElse ($2, $3, Empty) }
  | IF expr compound_statement
ELSE compound_statement           { IfElse ($2, $3, $5) }
  | WHILE expr compound_statement { While ($2, $3) }
  | FOR val_decl expr SEMICOLON expr
compound_statement                { For ($2, $3, $5, $6) }
  | CONTINUE SEMICOLON            { Continue }
  | BREAK SEMICOLON               { Break }
  | RETURN expr SEMICOLON         { Return (Some $2) }
  | RETURN SEMICOLON              { Return None }
;

compound_statement: LCURLY RCURLY { Block ([]) }
  | LCURLY block_body RCURLY      { Block ($2) }
;

block_body: statement    { [$1] }
  | statement block_body { $1 :: $2 }
;

// == TODO ==

type_: INT              { Int }
  | VOID                { Void }
  | IDENTIFIER          { NewType $1 }
  | LPAREN type_ RPAREN { $2 }
;

expr: IDENTIFIER                               { Identifier $1 }
  | literal                                    { Literal $1 }
  | LPAREN expr RPAREN                         { $2 }
  | IDENTIFIER EQ expr                         { Assignment ($1, $3) }
  | LPAREN expr RPAREN LBRACKET expr RBRACKET  { Index ($2, $5) }
  | IDENTIFIER LBRACKET expr RBRACKET          { Index (Identifier $1, $3) }
  | IDENTIFIER LPAREN expr_list RPAREN         { FunctionCall (Identifier $1, $3) }
  | LPAREN expr RPAREN LPAREN expr_list RPAREN { FunctionCall ($2, $5) }
  | expr PLUS expr                             { BinOp ($1, Plus, $3) }
  | expr LESSTHAN expr                         { BinOp ($1, LessThan, $3) }
;

expr_list:               { [] }
  | expr                 { [$1] }
  | expr COMMA expr_list { $1 :: $3 }
;

literal: INT_LITERAL { LInt $1 }
;

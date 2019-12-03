
// parser.mly

%{ open Parsetree %}

%token <string> IDENTIFIER
%token <int> I8_LITERAL
%token <int> I16_LITERAL
%token <int> I32_LITERAL
%token <int> I64_LITERAL
%token <int> U8_LITERAL
%token <int> U16_LITERAL
%token <int> U32_LITERAL
%token <int> U64_LITERAL
%token <float> F32_LITERAL
%token <float> F64_LITERAL

%token EQ RSHIFT_ASSIGN LSHIFT_ASSIGN BIT_AND_ASSIGN BIT_OR_ASSIGN BIT_XOR_ASSIGN
ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token RSHIFT LSHIFT BIT_AND BIT_OR BIT_XOR BIT_NOT
%token NOT
%token AND OR
%token EQUAL NOTEQUAL LESSTHAN GREATERTHAN LESSTHANEQUAL GREATERTHANEQUAL
%token PLUS MINUS
%token ASTERISK SLASH PERCENT
%token UMINUS
%token ADDRESSOF
%token DEREF
%token LPAREN RPAREN LCURLY RCURLY LBRACKET RBRACKET
%token COMMA COLON SEMICOLON
%token MUT I8 I16 I32 I64 U8 U16 U32 U64 F32 F64 VOID BOOL TRUE FALSE
%token TYPE VAL VAR FUNC EXTERN
%token IF ELSE FOR WHILE CONTINUE BREAK RETURN
%token EOF


%nonassoc EQ RSHIFT_ASSIGN LSHIFT_ASSIGN BIT_AND_ASSIGN BIT_OR_ASSIGN BIT_XOR_ASSIGN
ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%left RSHIFT LSHIFT BIT_AND BIT_OR BIT_XOR
%nonassoc BIT_NOT
%nonassoc NOT
%left AND OR
%nonassoc EQUAL NOTEQUAL LESSTHAN GREATERTHAN LESSTHANEQUAL GREATERTHANEQUAL
%left PLUS MINUS
%left ASTERISK SLASH PERCENT
%nonassoc UMINUS
%nonassoc ADDRESSOF
%nonassoc DEREF

%start <Parsetree.top_decl list> file

%%

file: EOF                { [] }
  | translation_unit EOF { $1 }
;

translation_unit: _translation_unit { List.rev $1 }
;
_translation_unit: top_decl         { [$1] }
  | _translation_unit top_decl      { $2 :: $1 }
;

top_decl: type_def { TypeDef $1 }
  | val_decl       { ValDecl $1 }
  | func_decl      { FuncDecl $1 }
  | func_fwd_decl  { FuncFwdDecl $1 }
;

type_def: TYPE IDENTIFIER EQ type_ SEMICOLON { ($2, $4) }
;

val_decl: VAL IDENTIFIER EQ expr SEMICOLON       { ValI ($2, $4) }
  | VAL IDENTIFIER COLON type_ EQ expr SEMICOLON { Val ($2, $4, $6) }
  | VAR IDENTIFIER EQ expr SEMICOLON             { VarI ($2, $4) }
  | VAR IDENTIFIER COLON type_ EQ expr SEMICOLON { Var ($2, $4, $6) }
;

func_fwd_decl: FUNC IDENTIFIER
LPAREN argument_list RPAREN COLON type_
SEMICOLON                                       { ($2, $4, $7, false) }
  | EXTERN FUNC IDENTIFIER
LPAREN argument_list RPAREN COLON type_
SEMICOLON                                       { ($3, $5, $8, true) }
;

func_decl: FUNC IDENTIFIER
LPAREN argument_list RPAREN COLON type_
compound_statement                              { ($2, $4, $7, $8) }
;

argument_list: _argument_list                   { List.rev $1 }
;
_argument_list:                                 { [] }
  | IDENTIFIER COLON type_                      { [($1, $3)] }
  | _argument_list COMMA IDENTIFIER COLON type_ { ($3, $5) :: $1 }
;

statement: SEMICOLON              { Empty }
  | val_decl                      { Decl $1 }
  | expr SEMICOLON                { Expr $1 }
  | compound_statement            { $1 }
  | IF expr compound_statement    { IfElse ($2, $3, Block []) }
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

block_body: _block_body   { List.rev $1 }
;
_block_body: statement    { [$1] }
  | _block_body statement { $2 :: $1 }
;

// == TODO ==

type_: base_type        { $1 }
  | pointer_type        { $1 }
  | IDENTIFIER          { NewType $1 }
  | FUNC
LPAREN type_list RPAREN
type_                   { Function ($3, $5) }
;

base_type: I8              { I8 }
  | I16                    { I16 }
  | I32                    { I32 }
  | I64                    { I64 }
  | U8                     { U8 }
  | U16                    { U16 }
  | U32                    { U32 }
  | U64                    { U64 }
  | F32                    { F32 }
  | F64                    { F64 }
  | BOOL                   { Bool }
  | VOID                   { Void }
;

pointer_type: LBRACKET type_ RBRACKET { Pointer $2 }
  | LBRACKET MUT type_ RBRACKET       { MutPointer $3 }

type_list: _type_list      { List.rev $1 }
;
_type_list:                { [] }
  | type_                  { [$1] }
  | _type_list COMMA type_ { $3 :: $1 }
;

cast_type: base_type        { $1 }
  | pointer_type            { $1 }
  | LPAREN cast_type RPAREN { $2 }

expr: IDENTIFIER                               { Identifier $1 }
  | literal                                    { Literal $1 }
  | LPAREN expr RPAREN                         { $2 }
  | LPAREN expr RPAREN LBRACKET expr RBRACKET  { Index ($2, $5) }
  | IDENTIFIER LBRACKET expr RBRACKET          { Index (Identifier $1, $3) }
  | IDENTIFIER LPAREN expr_list RPAREN         { FunctionCall (Identifier $1, $3) }
  | LPAREN expr RPAREN LPAREN expr_list RPAREN { FunctionCall ($2, $5) }
  | cast_type LPAREN expr RPAREN               { TypeCast ($1, $3) }

  | IDENTIFIER EQ expr
    { Assignment ($1, $3) }
  | IDENTIFIER ADD_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, Plus, $3)) }
  | IDENTIFIER SUB_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, Minus, $3)) }
  | IDENTIFIER MUL_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, Times, $3)) }
  | IDENTIFIER DIV_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, Divide, $3)) }
  | IDENTIFIER MOD_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, Modulus, $3)) }
  | IDENTIFIER RSHIFT_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, RShift, $3)) }
  | IDENTIFIER LSHIFT_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, LShift, $3)) }
  | IDENTIFIER BIT_AND_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, BitAnd, $3)) }
  | IDENTIFIER BIT_OR_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, BitOr, $3)) }
  | IDENTIFIER BIT_XOR_ASSIGN expr
    { Assignment ($1, BinOp (Identifier $1, BitXor, $3)) }

  | expr PLUS expr             { BinOp ($1, Plus, $3) }
  | expr MINUS expr            { BinOp ($1, Minus, $3) }
  | expr ASTERISK expr         { BinOp ($1, Times, $3) }
  | expr SLASH expr            { BinOp ($1, Divide, $3) }
  | expr PERCENT expr          { BinOp ($1, Modulus, $3) }
  | expr EQUAL expr            { BinOp ($1, Equal, $3) }
  | expr NOTEQUAL expr         { UnOp  (Not, BinOp ($1, Equal, $3)) }
  | expr LESSTHAN expr         { BinOp ($1, LessThan, $3) }
  | expr GREATERTHAN expr      { BinOp ($1, GreaterThan, $3) }
  | expr LESSTHANEQUAL expr    { UnOp  (Not, BinOp ($1, GreaterThan, $3)) }
  | expr GREATERTHANEQUAL expr { UnOp  (Not, BinOp ($1, LessThan, $3)) }
  | expr RSHIFT expr           { BinOp ($1, RShift, $3) }
  | expr LSHIFT expr           { BinOp ($1, LShift, $3) }
  | expr BIT_AND expr          { BinOp ($1, BitAnd, $3) }
  | expr BIT_OR expr           { BinOp ($1, BitOr, $3) }
  | expr BIT_XOR expr          { BinOp ($1, BitXor, $3) }
  | expr AND expr              { BinOp ($1, And, $3) }
  | expr OR expr               { BinOp ($1, Or, $3) }

  | MINUS expr %prec UMINUS      { UnOp (UMinus, $2) }
  | NOT expr                     { UnOp (Not, $2) }
  | DEREF expr                   { UnOp (Deref, $2) }
  | BIT_NOT expr                 { UnOp (BitNot, $2) }
  | BIT_AND expr %prec ADDRESSOF { UnOp (AddressOf, $2) }
;

expr_list: _expr_list     { List.rev $1 }
;
_expr_list:               { [] }
  | expr                  { [$1] }
  | _expr_list COMMA expr { $3 :: $1 }
;

literal: I8_LITERAL  { LI8 $1 }
  | I16_LITERAL      { LI16 $1 }
  | I32_LITERAL      { LI32 $1 }
  | I64_LITERAL      { LI64 $1 }
  | U8_LITERAL       { LU8 $1 }
  | U16_LITERAL      { LU16 $1 }
  | U32_LITERAL      { LU32 $1 }
  | U64_LITERAL      { LU64 $1 }
  | F32_LITERAL      { LF32 $1 }
  | F64_LITERAL      { LF64 $1 }
  | TRUE             { LBool true }
  | FALSE            { LBool false }
;

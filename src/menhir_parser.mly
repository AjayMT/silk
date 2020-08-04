
// menhir_parser.mly

%{ open Parsetree %}

%token <string> IDENTIFIER
%token <string> TEMPLATE
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
%token <string> STRING_LITERAL

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
%token COMMA DOT COLON SEMICOLON
%token MUT I8 I16 I32 I64 U8 U16 U32 U64 F32 F64 VOID BOOL TRUE FALSE STRUCT PACKED
%token TYPE VAL VAR FUNC EXTERN PRIVATE
%token IF ELSE FOR WHILE CONTINUE BREAK RETURN
%token EOF

%right EQ RSHIFT_ASSIGN LSHIFT_ASSIGN BIT_AND_ASSIGN BIT_OR_ASSIGN BIT_XOR_ASSIGN
ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%left RSHIFT LSHIFT BIT_AND BIT_OR BIT_XOR
%left AND OR
%nonassoc EQUAL NOTEQUAL LESSTHAN GREATERTHAN LESSTHANEQUAL GREATERTHANEQUAL
%left PLUS MINUS
%left ASTERISK SLASH PERCENT
%nonassoc BIT_NOT
%nonassoc NOT
%nonassoc UMINUS
%nonassoc ADDRESSOF

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

template_list: _template_list     { List.rev $1 }
;
_template_list: TEMPLATE          { [$1] }
  | _template_list COMMA TEMPLATE { $3 :: $1 }
;

top_decl: type_def             { TypeDef $1 }
  | template_type_def          { TemplateTypeDef $1 }
  | type_fwd_def               { TypeFwdDef $1 }
  | template_type_fwd_def      { TemplateTypeFwdDef $1 }
  | val_decl                   { ValDecl (true, $1) }
  | PRIVATE val_decl           { ValDecl (false, $2) }
  | func_decl                  { FuncDecl (true, $1) }
  | template_func_decl         { TemplateFuncDecl (true, $1) }
  | PRIVATE func_decl          { FuncDecl (false, $2) }
  | PRIVATE template_func_decl { TemplateFuncDecl (false, $2) }
  | func_fwd_decl              { FuncFwdDecl $1 }
  | template_func_fwd_decl     { TemplateFuncFwdDecl $1 }
;

type_def: TYPE IDENTIFIER EQ type_ SEMICOLON { ($2, $4) }
;
template_type_def: TYPE IDENTIFIER COLON LBRACKET template_list RBRACKET
EQ type_ SEMICOLON { ($5, ($2, $8)) }
;

type_fwd_def: TYPE IDENTIFIER SEMICOLON { $2 }
;
template_type_fwd_def: TYPE IDENTIFIER COLON LBRACKET template_list RBRACKET
SEMICOLON { ($5, $2) }
;


val_decl: VAL IDENTIFIER EQ expr SEMICOLON { ValI ($2, $4) }
  | VAL IDENTIFIER type_ EQ expr SEMICOLON { Val ($2, $3, $5) }
  | VAR IDENTIFIER EQ expr SEMICOLON       { VarI ($2, $4) }
  | VAR IDENTIFIER type_ EQ expr SEMICOLON { Var ($2, $3, $5) }
;

func_fwd_decl: FUNC IDENTIFIER LPAREN argument_list RPAREN type_
SEMICOLON { ($2, $4, $6, false) }
  | FUNC IDENTIFIER LPAREN RPAREN type_
SEMICOLON { ($2, [], $5, false) }
  | EXTERN FUNC IDENTIFIER LPAREN argument_list RPAREN type_
SEMICOLON { ($3, $5, $7, true) }
  | EXTERN FUNC IDENTIFIER LPAREN RPAREN type_
SEMICOLON { ($3, [], $6, true) }
;
template_func_fwd_decl: FUNC IDENTIFIER COLON LBRACKET template_list RBRACKET
LPAREN argument_list RPAREN type_ SEMICOLON { ($5, ($2, $8, $10, false)) }
  | FUNC IDENTIFIER COLON LBRACKET template_list RBRACKET LPAREN RPAREN
type_ SEMICOLON { ($5, ($2, [], $9, false)) }
  | EXTERN FUNC IDENTIFIER COLON LBRACKET template_list RBRACKET
LPAREN argument_list RPAREN type_ SEMICOLON { ($6, ($3, $9, $11, true)) }
  | EXTERN FUNC IDENTIFIER COLON LBRACKET template_list RBRACKET
LPAREN RPAREN type_ SEMICOLON { ($6, ($3, [], $10, true)) }
;

func_decl: FUNC IDENTIFIER LPAREN argument_list RPAREN type_ compound_statement
    { ($2, $4, $6, $7) }
  | FUNC IDENTIFIER LPAREN RPAREN type_ compound_statement
    { ($2, [], $5, $6) }
;
template_func_decl: FUNC IDENTIFIER COLON LBRACKET template_list RBRACKET
LPAREN argument_list RPAREN type_ compound_statement
    { ($5, ($2, $8, $10, $11)) }
  | FUNC IDENTIFIER COLON LBRACKET template_list RBRACKET LPAREN RPAREN
type_ compound_statement { ($5, ($2, [], $9, $10)) }
;

argument_list: _argument_list                   { List.rev $1 }
;
_argument_list: IDENTIFIER type_          { [($1, $2)] }
  | _argument_list COMMA IDENTIFIER type_ { ($3, $4) :: $1 }
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

type_: base_type { $1 }
  | pointer_type { $1 }
  | LBRACKET I32_LITERAL RBRACKET type_ { Array ($2, $4) }
  | struct_type  { $1 }
  | IDENTIFIER   { TypeAlias $1 }
  | TEMPLATE     { Template $1 }
  | IDENTIFIER COLON LBRACKET type_list RBRACKET
    { AliasTemplateInstance ($1, $4) }
  | FUNC LPAREN type_list RPAREN type_  { Function ($3, $5) }
  | FUNC LPAREN RPAREN type_            { Function ([], $4) }
  | LPAREN type_ RPAREN { $2 }
;

base_type: I8 { I8 }
  | I16       { I16 }
  | I32       { I32 }
  | I64       { I64 }
  | U8        { U8 }
  | U16       { U16 }
  | U32       { U32 }
  | U64       { U64 }
  | F32       { F32 }
  | F64       { F64 }
  | BOOL      { Bool }
  | VOID      { Void }
;

pointer_type: ASTERISK type_ { Pointer $2 }
  | MUT ASTERISK type_       { MutPointer $3 }
;

struct_type: STRUCT LPAREN argument_list RPAREN { StructLabeled (false, $3) }
  | STRUCT LPAREN type_list RPAREN              { Struct (false, $3) }
  | PACKED LPAREN argument_list RPAREN          { StructLabeled (true, $3) }
  | PACKED LPAREN type_list RPAREN              { Struct (true, $3) }
;

struct_init: struct_type      { $1 }
  | LPAREN struct_init RPAREN { $2 }
;

type_list: _type_list      { List.rev $1 }
;
_type_list: type_          { [$1] }
  | _type_list COMMA type_ { $3 :: $1 }
;

cast_type: base_type        { $1 }
  | pointer_type            { $1 }
  | TEMPLATE                { Template $1 }
  | LPAREN cast_type RPAREN { $2 }
;

deref_expr: IDENTIFIER { Identifier $1 }
  | literal            { Literal $1 }
  | LPAREN expr RPAREN { $2 }
  | IDENTIFIER COLON LBRACKET type_list RBRACKET { TemplateInstance ($1, $4) }

  | LPAREN struct_expr_list RPAREN             { StructLiteral (false, $2) }
  | LPAREN COLON struct_expr_list COLON RPAREN { StructLiteral (true, $3) }

  | LBRACKET type_ SEMICOLON I32_LITERAL RBRACKET { ArrayInit ($2, $4) }
  | LCURLY expr_list RCURLY                       { ArrayElems $2 }

  | DEREF deref_expr { UnOp (Deref, $2) }
;

non_bin_expr: deref_expr         { $1 }
  | non_bin_expr DOT IDENTIFIER  { StructMemberAccess ($1, $3) }
  | non_bin_expr DOT I32_LITERAL { StructIndexAccess ($1, $3) }

  | non_bin_expr LBRACKET expr RBRACKET { Index ($1, $3) }

  | non_bin_expr LPAREN expr_list RPAREN { FunctionCall ($1, $3) }
  | non_bin_expr LPAREN RPAREN           { FunctionCall ($1, []) }
  | cast_type LPAREN expr RPAREN { TypeCast ($1, $3) }
  | struct_init LPAREN expr_list RPAREN { StructInit ($1, $3) }
;

expr: non_bin_expr { $1 }

  | expr EQ expr
    { Assignment ($1, $3) }
  | expr ADD_ASSIGN expr
    { Assignment ($1, BinOp ($1, Plus, $3)) }
  | expr SUB_ASSIGN expr
    { Assignment ($1, BinOp ($1, Minus, $3)) }
  | expr MUL_ASSIGN expr
    { Assignment ($1, BinOp ($1, Times, $3)) }
  | expr DIV_ASSIGN expr
    { Assignment ($1, BinOp ($1, Divide, $3)) }
  | expr MOD_ASSIGN expr
    { Assignment ($1, BinOp ($1, Modulus, $3)) }
  | expr RSHIFT_ASSIGN expr
    { Assignment ($1, BinOp ($1, RShift, $3)) }
  | expr LSHIFT_ASSIGN expr
    { Assignment ($1, BinOp ($1, LShift, $3)) }
  | expr BIT_AND_ASSIGN expr
    { Assignment ($1, BinOp ($1, BitAnd, $3)) }
  | expr BIT_OR_ASSIGN expr
    { Assignment ($1, BinOp ($1, BitOr, $3)) }
  | expr BIT_XOR_ASSIGN expr
    { Assignment ($1, BinOp ($1, BitXor, $3)) }

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
  | BIT_NOT expr                 { UnOp (BitNot, $2) }
  | BIT_AND expr %prec ADDRESSOF { UnOp (AddressOf, $2) }
;

struct_expr_list: _struct_expr_list     { List.rev $1 }
;
_struct_expr_list: expr COMMA expr { [$3; $1] }
  | _struct_expr_list COMMA expr   { $3 :: $1 }
;


expr_list: _expr_list     { List.rev $1 }
;
_expr_list: expr          { [$1] }
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
  | STRING_LITERAL   { LString $1 }
;

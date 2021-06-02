
{
open Menhir_parser
exception LexError of string

let char_of_string s = (Scanf.unescaped s).[0]
}

let decimalDigit = ['0'-'9']
let hexadecimalDigit = ['0'-'9' 'a'-'f' 'A'-'F']
let octalDigit = ['0'-'7']
let leadingChar = ['a'-'z' 'A'-'Z']
let nonleadingChar = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let whitespace = [' ' '\t']
let newline = "\r\n" | '\r' | '\n'
let escapeSequence = ('\\' (['\'' '"' '?' '\\' 'a' 'b' 'f' 'n' 'r' 't' 'v'] | ['0'-'7']['0'-'7']?['0'-'7']? | 'x'['a'-'f' 'A'-'F' '0'-'9']+))

rule token = parse
| "type"      { TYPE }
| "val"       { VAL }
| "var"       { VAR }
| "extern"    { EXTERN }
| "private"   { PRIVATE }
| "func"      { FUNC }
| "if"        { IF }
| "else"      { ELSE }
| "for"       { FOR }
| "while"     { WHILE }
| "continue"  { CONTINUE }
| "break"     { BREAK }
| "return"    { RETURN }
| "mut"       { MUT }
| "i8"        { I8 }
| "i16"       { I16 }
| "i32"       { I32 }
| "i64"       { I64 }
| "u8"        { U8 }
| "u16"       { U16 }
| "u32"       { U32 }
| "u64"       { U64 }
| "f32"       { F32 }
| "f64"       { F64 }
| "void"      { VOID }
| "bool"      { BOOL }
| "true"      { TRUE }
| "false"     { FALSE }
| "struct"    { STRUCT }
| "packed"    { PACKED }
| "nil"       { NIL }
| leadingChar nonleadingChar* as ident { IDENTIFIER (ident) }
| "$" nonleadingChar+ as template      { TEMPLATE (template) }

| decimalDigit+ "." decimalDigit+ "f" as f32_literal {
F32_LITERAL (float_of_string
(String.sub f32_literal 0 ((String.length f32_literal) - 1)))
}
| decimalDigit+ "." decimalDigit+ as f64_literal {
F64_LITERAL (float_of_string f64_literal)
}

| decimalDigit+ as i32_literal { I32_LITERAL (int_of_string i32_literal) }
| "0x" hexadecimalDigit+ as i32_literal { I32_LITERAL (int_of_string i32_literal) }
| "0o" octalDigit+ as i32_literal { I32_LITERAL (int_of_string i32_literal) }
| "0b" ['0' '1']+ as i32_literal { I32_LITERAL (int_of_string i32_literal) }

| decimalDigit+ "c" as i8_literal {
I8_LITERAL (int_of_string
(String.sub i8_literal 0 ((String.length i8_literal) - 1)))
}
| "0x" hexadecimalDigit+ "c" as i8_literal {
I8_LITERAL (int_of_string
(String.sub i8_literal 0 ((String.length i8_literal) - 1)))
}
| "0o" octalDigit+ "c" as i8_literal {
I8_LITERAL (int_of_string
(String.sub i8_literal 0 ((String.length i8_literal) - 1)))
}
| "0b" ['0' '1']+ "c" as i8_literal {
I8_LITERAL (int_of_string
(String.sub i8_literal 0 ((String.length i8_literal) - 1)))
}
| "'" ([^ '\'' '\\' '\r' '\n'] | escapeSequence)+ "'" as i8_literal {
I8_LITERAL (int_of_char @@
char_of_string @@
String.sub i8_literal 1 @@
(String.length i8_literal) - 2)
}

| decimalDigit+ "h" as i16_literal {
I16_LITERAL (int_of_string
(String.sub i16_literal 0 ((String.length i16_literal) - 1)))
}
| "0x" hexadecimalDigit+ "h" as i16_literal {
I16_LITERAL (int_of_string
(String.sub i16_literal 0 ((String.length i16_literal) - 1)))
}
| "0o" octalDigit+ "h" as i16_literal {
I16_LITERAL (int_of_string
(String.sub i16_literal 0 ((String.length i16_literal) - 1)))
}
| "0b" ['0' '1']+ "h" as i16_literal {
I16_LITERAL (int_of_string
(String.sub i16_literal 0 ((String.length i16_literal) - 1)))
}

| decimalDigit+ "l" as i64_literal {
I64_LITERAL (int_of_string
(String.sub i64_literal 0 ((String.length i64_literal) - 1)))
}
| "0x" hexadecimalDigit+ "l" as i64_literal {
I64_LITERAL (int_of_string
(String.sub i64_literal 0 ((String.length i64_literal) - 1)))
}
| "0o" octalDigit+ "l" as i64_literal {
I64_LITERAL (int_of_string
(String.sub i64_literal 0 ((String.length i64_literal) - 1)))
}
| "0b" ['0' '1']+ "l" as i64_literal {
I64_LITERAL (int_of_string
(String.sub i64_literal 0 ((String.length i64_literal) - 1)))
}

| decimalDigit+ "u" as u32_literal {
U32_LITERAL (int_of_string
(String.sub u32_literal 0 ((String.length u32_literal) - 1)))
}
| "0x" hexadecimalDigit+ "u" as u32_literal {
U32_LITERAL (int_of_string
(String.sub u32_literal 0 ((String.length u32_literal) - 1)))
}
| "0o" octalDigit+ "u" as u32_literal {
U32_LITERAL (int_of_string
(String.sub u32_literal 0 ((String.length u32_literal) - 1)))
}
| "0b" ['0' '1']+ "u" as u32_literal {
U32_LITERAL (int_of_string
(String.sub u32_literal 0 ((String.length u32_literal) - 1)))
}

| decimalDigit+ "uc" as u8_literal {
U8_LITERAL (int_of_string
(String.sub u8_literal 0 ((String.length u8_literal) - 2)))
}
| "0x" hexadecimalDigit+ "uc" as u8_literal {
U8_LITERAL (int_of_string
(String.sub u8_literal 0 ((String.length u8_literal) - 2)))
}
| "0o" octalDigit+ "uc" as u8_literal {
U8_LITERAL (int_of_string
(String.sub u8_literal 0 ((String.length u8_literal) - 2)))
}
| "0b" ['0' '1']+ "uc" as u8_literal {
U8_LITERAL (int_of_string
(String.sub u8_literal 0 ((String.length u8_literal) - 2)))
}

| decimalDigit+ "uh" as u16_literal {
U16_LITERAL (int_of_string
(String.sub u16_literal 0 ((String.length u16_literal) - 2)))
}
| "0x" hexadecimalDigit+ "uh" as u16_literal {
U16_LITERAL (int_of_string
(String.sub u16_literal 0 ((String.length u16_literal) - 2)))
}
| "0o" octalDigit+ "uh" as u16_literal {
U16_LITERAL (int_of_string
(String.sub u16_literal 0 ((String.length u16_literal) - 2)))
}
| "0b" ['0' '1']+ "uh" as u16_literal {
U16_LITERAL (int_of_string
(String.sub u16_literal 0 ((String.length u16_literal) - 2)))
}

| decimalDigit+ "ul" as u64_literal {
U64_LITERAL (int_of_string
(String.sub u64_literal 0 ((String.length u64_literal) - 2)))
}
| "0x" hexadecimalDigit+ "ul" as u64_literal {
U64_LITERAL (int_of_string
(String.sub u64_literal 0 ((String.length u64_literal) - 2)))
}
| "0o" octalDigit+ "ul" as u64_literal {
U64_LITERAL (int_of_string
(String.sub u64_literal 0 ((String.length u64_literal) - 2)))
}
| "0b" ['0' '1']+ "ul" as u64_literal {
U64_LITERAL (int_of_string
(String.sub u64_literal 0 ((String.length u64_literal) - 2)))
}

| "\"" ([^ '"' '\\' '\r' '\n'] | escapeSequence)* "\"" as string_literal {
STRING_LITERAL (Scanf.unescaped @@ String.sub string_literal 1 @@
(String.length string_literal) - 2)
}

| ":"         { COLON }
| ";"         { SEMICOLON }
| ","         { COMMA }
| "."         { DOT }
| "@"         { DEREF }
| ">>"        { RSHIFT }
| "<<"        { LSHIFT }
| "&"         { BIT_AND }
| "|"         { BIT_OR }
| "^"         { BIT_XOR }
| ">>="       { RSHIFT_ASSIGN }
| "<<="       { LSHIFT_ASSIGN }
| "&="        { BIT_AND_ASSIGN }
| "|="        { BIT_OR_ASSIGN }
| "^="        { BIT_XOR_ASSIGN }
| "+"         { PLUS }
| "-"         { MINUS }
| "*"         { ASTERISK }
| "/"         { SLASH }
| "%"         { PERCENT }
| "+="        { ADD_ASSIGN }
| "-="        { SUB_ASSIGN }
| "*="        { MUL_ASSIGN }
| "/="        { DIV_ASSIGN }
| "%="        { MOD_ASSIGN }
| "=="        { EQUAL }
| "!="        { NOTEQUAL }
| "<"         { LESSTHAN }
| ">"         { GREATERTHAN }
| "<="        { LESSTHANEQUAL }
| ">="        { GREATERTHANEQUAL }
| "&&"        { AND }
| "||"        { OR }
| "!"         { NOT }
| "~"         { BIT_NOT }
| "="         { EQ }
| "("         { LPAREN }
| ")"         { RPAREN }
| "{"         { LCURLY }
| "}"         { RCURLY }
| "["         { LBRACKET }
| "]"         { RBRACKET }
| "//" [^ '\r' '\n']* newline { Lexing.new_line lexbuf; token lexbuf }
| "#"  [^ '\r' '\n']* newline { Lexing.new_line lexbuf; token lexbuf }
| whitespace+ { token lexbuf }
| newline     { Lexing.new_line lexbuf; token lexbuf }
| eof         { EOF }
| _           { raise (LexError (string_of_int lexbuf.lex_curr_pos)) }

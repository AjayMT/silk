
{
open Parser
exception SyntaxError of string
}

let decimalDigit = ['0'-'9']
let hexadecimalDigit = ['0'-'9' 'a'-'f']
let octalDigit = ['0'-'7']
let leadingChar = ['a'-'z' 'A'-'Z']
let nonleadingChar = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let whitespace = [' ' '\t' '\n']

rule token = parse
| "type"      { TYPE }
| "val"       { VAL }
| "var"       { VAR }
| "func"      { FUNC }
| "if"        { IF }
| "else"      { ELSE }
| "for"       { FOR }
| "while"     { WHILE }
| "continue"  { CONTINUE }
| "break"     { BREAK }
| "return"    { RETURN }
| "int"       { INT }
| "void"      { VOID }
| leadingChar nonleadingChar* as ident { IDENTIFIER (ident) }
| decimalDigit+ as int_literal { INT_LITERAL (int_of_string int_literal) }
| "0x" hexadecimalDigit+ as int_literal { INT_LITERAL (int_of_string int_literal) }
| "0o" octalDigit+ as int_literal { INT_LITERAL (int_of_string int_literal) }
| "0b" ['0' '1']+ as int_literal { INT_LITERAL (int_of_string int_literal) }
| ":"         { COLON }
| ";"         { SEMICOLON }
| ","         { COMMA }
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
| whitespace+ { token lexbuf }
| eof         { EOF }
| _           { raise (SyntaxError (string_of_int lexbuf.lex_curr_pos)) }


{
open Parser
exception SyntaxError of string
}

let decimalDigit = ['0'-'9']
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
| ":"         { COLON }
| ";"         { SEMICOLON }
| ","         { COMMA }
| "+"         { PLUS }
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


(*
 * Parse tree definitions.
 *)

type literal = LInt of int

type op = Plus | LessThan

type expr = Identifier of string | Literal of literal | Assignment of string * expr
            | FunctionCall of expr * expr list | BinOp of expr * op * expr
            | Index of expr * expr

type type_ = Int | Void | NewType of string

type val_decl = ValI of string * expr | Val of string * type_ * expr
                | VarI of string * expr | Var of string * type_ * expr

type type_def = string * type_

type statement = Empty | Decl of val_decl | Expr of expr | Block of statement list
                 | IfElse of expr * statement * statement
                 | While of expr * statement
                 | For of val_decl * expr * expr * statement
                 | Continue | Break | Return of expr option

type func_decl = string * (string * type_) list * type_ * statement

type top_decl = TypeDef of type_def | ValDecl of val_decl | FuncDecl of func_decl

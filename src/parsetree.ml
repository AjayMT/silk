
(*
 * Parse tree definitions.
 *)

type literal = LI32 of int | LU32 of int
               | LF64 of float
               | LBool of bool

type bin_op = Plus | Minus | Times | Divide | Modulus
              | Equal | LessThan | GreaterThan
              | And | Or
              | RShift | LShift | BitAnd | BitOr | BitXor
type un_op = UMinus | Not | BitNot

type expr = Identifier of string | Literal of literal | Assignment of string * expr
            | FunctionCall of expr * expr list | BinOp of expr * bin_op * expr
            | UnOp of un_op * expr
            | Index of expr * expr

type type_ = I32 | U32 | F64 | Void | Bool | NewType of string

type val_decl = ValI of string * expr | Val of string * type_ * expr
                | VarI of string * expr | Var of string * type_ * expr

type type_def = string * type_

type statement = Empty | Decl of val_decl | Expr of expr | Block of statement list
                 | IfElse of expr * statement * statement
                 | While of expr * statement
                 | For of val_decl * expr * expr * statement
                 | Continue | Break | Return of expr option

type func_fwd_decl = string * (string * type_) list * type_ * bool

type func_decl = string * (string * type_) list * type_ * statement

type top_decl = TypeDef of type_def
              | ValDecl of val_decl
              | FuncDecl of func_decl
              | FuncFwdDecl of func_fwd_decl

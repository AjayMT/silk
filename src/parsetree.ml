
(*
 * Parse tree definitions.
 *)

type type_ = I8 | I16 | I32 | I64
             | U8 | U16 | U32 | U64
             | F32 | F64
             | Function of type_ list * type_
             | Pointer of type_ | MutPointer of type_
             | Array of int * type_
             | Void | Bool | NewType of string

type literal = LI8 of int | LI16 of int | LI32 of int | LI64 of int
               | LU8 of int | LU16 of int | LU32 of int | LU64 of int
               | LF32 of float | LF64 of float
               | LBool of bool

type bin_op = Plus | Minus | Times | Divide | Modulus
              | Equal | LessThan | GreaterThan
              | And | Or
              | RShift | LShift | BitAnd | BitOr | BitXor
type un_op = UMinus | Not | BitNot | AddressOf | Deref

type expr = Identifier of string | Literal of literal | Assignment of expr * expr
            | ArrayElems of expr list
            | ArrayInit of type_ * int
            | FunctionCall of expr * expr list
            | TypeCast of type_ * expr
            | BinOp of expr * bin_op * expr
            | UnOp of un_op * expr
            | Index of expr * expr

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

(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type bop = Add | Mul | Sub | Div | Eq | Lt | And | Or 
and uop = Not

and tprim = Int | Bool

and typ = Type of tprim | TypeFunc of typs * typ (*ARROW*)
and typs = ASTType of typ | ASTTypes of typ * typs

and arg = Argu of string * typ
and args = ASTArg of arg | ASTArgs of arg * args

and expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * exprs
  | ASTBool of bool
  | ASTIf of expr * expr * expr
  | ASTBinary of bop * expr * expr
  | ASTUnary of uop * expr
  | ASTFunc of args * expr
and exprs =
    ASTExpr of expr 
  | ASTExprs of expr * exprs

and stat = 
    ASTEcho of expr
  (* APS1 *)
  | ASTSet of string * expr
  | ASTIfb of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of expr * exprs

and dec = 
    ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTFunRec of string * typ * args * expr
  (* APS1 *)
  | ASTVar of string * typ
  | ASTProc of string * args * block
  | ASTProcRec of string * args * block

(* APS1 *)
and block =
  ASTBlock of cmds

and cmds = 
    ASTStat of stat
  | ASTDec of dec * cmds
  | ASTStatCmd of stat * cmds

type prog = 
  ASTProg of cmds

let bop_of_string bop =
  match bop with
    "add" -> Add
  | "mul" -> Mul
  | "sub" -> Sub
  | "div" -> Div
  | "and" -> And
  | "or" -> Or
  | "eq" -> Eq
  | "lt" -> Lt
  | _ -> failwith "not an operator"

let string_of_bop bop = 
  match bop with
    Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | And -> "and"
  | Or -> "or"
  | Eq -> "eq"
  | Lt -> "lt"

let uop_of_string uop =
  match uop with
    "not" -> Not
  | _ -> failwith "not an operator"

let string_of_uop uop =
  match uop with
    Not -> "not"
    
let string_of_tprim tprim =
(*print a trim in prolog*)
  match tprim with
    Int -> "int"
  | Bool -> "bool"

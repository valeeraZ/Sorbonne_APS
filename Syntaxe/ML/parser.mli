type token =
  | NUM of (int)
  | IDENT of (string)
  | LPAR
  | RPAR

val sexpr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.sexpr

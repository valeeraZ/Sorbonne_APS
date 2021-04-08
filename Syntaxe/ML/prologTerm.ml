(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
  
let rec print_sexpr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTApp(e, es) -> (
	Printf.printf"app(";
	print_sexpr e;
	Printf.printf",[";
	print_sexprs es;
	Printf.printf"])"
      )
and print_sexprs es =
  match es with
      [] -> ()
    | [e] -> print_sexpr e
    | e::es -> (
	print_sexpr e;
	print_char ',';
	print_sexprs es
      )
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let e = Parser.sexpr Lexer.token lexbuf in
      print_sexpr e;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      

open Ast;;

(*N, F, FR*)
(*APS1: A, S, P, PR*)
(*E = ident -> V*)
type value = InN of int | InF of expr * string list * ident list | InFR of value | 
             InA of int | InP of block * string list * ident list | InPR of value | 
             None
(*association id-value*)
and ident = Pair of string * value

let rec in_env id env =
  match env with
    Pair(i,v)::tl -> if (String.equal id i) then true else in_env id tl
  | [] -> false
;;

let rec extract_from_env id env =
  match env with
    Pair(i,v)::tl -> if (String.equal id i) then v else extract_from_env id tl
  | _ -> failwith "error extraction variable"
;;

let rec extract_from_mem address (mem: (int * (value ref)) list) =
  match mem with
    (a,v)::tl -> if (address = a) then v else extract_from_mem address tl
  | _ -> failwith "error reading from memory"

let alloc_indicator = ref 0
let alloc mem =
  let res = (!alloc_indicator, (!alloc_indicator, ref(InN(-1)))::mem ) in
    alloc_indicator := (!alloc_indicator + 1);
    res


let is_bop op = 
  match op with
    Add -> true
  | Mul -> true
  | Sub -> true
  | Div -> true
  | And -> true
  | Or -> true
  | Eq -> true
  | Lt -> true

let is_uop op =
  match op with
    Not -> true

let get_int value = 
  match value with
    InN(n) -> n
  | InF(_,_,_) -> failwith "nothing"
  | _ -> failwith "not N"

let get_ident arg = 
  match arg with
    Argu(id, typ) -> id

let rec make_closure args env = 
  match args with
    ASTArgs(arg, args) -> make_closure args [(get_ident arg)]@env
  | ASTArg(arg) -> [(get_ident arg)]@env

let rec make_closure_proc args =
  match args with
    ASTArg(arg) -> parse_arg arg
  | ASTArgs(arg,args) -> (parse_arg arg)@(make_closure_proc args)
and parse_arg arg =
  match arg with
    | Argu(id, typ) -> id::[]

let eval_bop bop e1 e2 =
  match bop with
    Add -> InN((get_int e1) + (get_int e2))
  | Mul -> InN((get_int e1) * (get_int e2))
  | Sub -> InN((get_int e1) - (get_int e2))
  | Div -> InN((get_int e1) / (get_int e2))
  | And -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) then failwith "not a logical argument"
    else if (get_int e1) = 0 then InN(0) else InN((get_int e2))
  | Or -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) then failwith "not a logical argument"
    else if (get_int e1) = 1 then InN(1) else InN((get_int e2))
  | Eq -> if (get_int e1) = (get_int e2) then InN(1) else InN(0)
  | Lt -> if (get_int e1) < (get_int e2) then InN(1) else InN(0)

let eval_uop op e =
  match op with
    Not -> if (get_int e) = 0 then InN(1) 
    else if (get_int e) = 1 then InN(0) 
    else failwith "not valid argument"

let assoc_val closure env =
  List.map2 (function a -> function e -> Pair(a,e) ) closure env

let rec eval_expr expr env mem = 
  match expr with
  | ASTNum(n) -> InN(n)
  | ASTId(id) -> if in_env id env 
                  then match extract_from_env id env with
                    |InA(a) -> !(extract_from_mem a mem)
                    | v -> v
                  else failwith (id^" not a variable in environment")
  | ASTBinary(op,e1,e2) -> if is_bop op then eval_bop op (eval_expr e1 env mem) (eval_expr e2 env mem) 
    else failwith "not a binary operator"
  | ASTUnary(op,e) -> if is_uop op then eval_uop op (eval_expr e env mem) 
    else failwith "not an unary operator"
  | ASTIf(e1,e2,e3) -> if (eval_expr e1 env mem) = InN(1) then eval_expr e2 env mem
    else if (eval_expr e1 env mem) = InN(0) then (eval_expr e3 env mem) 
    else failwith "not a boolean value"
  | ASTFunc(args,e_prim) -> InF(e_prim, (make_closure args []),env)
  | ASTBool(e) -> if e then InN(1) else InN(0)
  | ASTApp (expr,exprs) -> 
    match (eval_expr expr env mem) with
      InF(e_prim,closure,envi) -> let env_fun = (assoc_val closure (get_eval exprs env mem))@envi in
      eval_expr e_prim env_fun mem
    | InFR(closure) ->( match closure with
          InF(e_prim,f,envi) -> let env_fun = (assoc_val f (get_eval exprs env mem))@env in
          eval_expr (ASTApp(e_prim,exprs)) env_fun mem
        | _ -> failwith "not a recursive function")
    | InN(n) -> InN(n)
    | _ -> failwith "application result not applied yet"
and get_eval exprs env mem =
  match exprs with
    ASTExprs(e,es) -> (eval_expr e env mem)::(get_eval es env mem)
  | ASTExpr(e) -> (eval_expr e env mem)::[]

and eval_dec dec env mem =
  match dec with
  | ASTConst(id,t,expr) -> (Pair(id,(eval_expr expr env mem))::env, mem)
  | ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,make_closure args [],env))::env, mem)
  | ASTFunRec(id,t,args,expr) -> (Pair(id, InFR(InF(expr,make_closure args [],env)))::env, mem)
  | ASTVar(id,t) -> let (adresse, new_mem) = alloc(mem) in (Pair(id, (InA(adresse)))::env, new_mem)
  | ASTProc(id,args,block) -> (Pair(id, InP(block,make_closure_proc args, env))::env, mem)
  | ASTProcRec(id,args,block) -> (Pair(id, InPR(InP(block,make_closure_proc args, env)))::env, mem)

and eval_stat stat env mem output =
  match stat with
  | ASTEcho(n) -> (mem, (eval_expr n env mem)::output)
  | ASTSet(id,e) -> (
      match extract_from_env id env with
        | InA(address) -> 
            let v = (extract_from_mem address mem) and affectation = eval_expr e env mem
              in v := affectation;
              (mem,output)
        | _ -> failwith "variable not in memory"
    )
  | ASTIfb (e,b1,b2) -> if (eval_expr e env mem) = InN(1) 
                          then (eval_block b1 env mem output)
                          else (eval_block b2 env mem output)
  | ASTWhile(e,b) -> if (eval_expr e env mem) = InN(0)
                        then (mem, output)
                        else let (new_mem, new_output) = (eval_block b env mem output) in
                          eval_stat stat env new_mem new_output
  | ASTCall(p,exprs) -> 
    match (eval_expr p env mem) with
      InP(block,closure,envi) -> let env_proc = (assoc_val closure (get_eval exprs env mem))@envi in
      eval_block block env_proc mem output
    | InPR(closure) ->( match closure with
          InP(block,f,envi) -> let env_proc = (assoc_val f (get_eval exprs env mem))@env in
            eval_block block env_proc mem output
        | _ -> failwith "not a recursive function")
    | _ -> failwith "called procedure not implemented yet"



and eval_cmds cmds env mem output =
  match cmds with
    ASTDec(dec,c) -> let (new_env, new_mem) = (eval_dec dec env mem) in eval_cmds c new_env new_mem output
  | ASTStatCmd(stat,c) -> let (new_mem, _) = (eval_stat stat env mem output) in eval_cmds c env new_mem output
  | ASTStat(stat) -> eval_stat stat env mem output

and eval_block block env mem output = 
  match block with
    ASTBlock(cmds) -> eval_cmds cmds env mem output

let print_val value =
  match value with
    InN(n) -> Printf.printf "%d\n" n
  | _ -> failwith "not a printable value"

let rec print_output output =
  List.iter (function x -> print_val x) (List.rev output) 

let eval_prog prog =
  match prog with
    ASTProg(cmds) -> let (_, out) = (eval_cmds cmds [] [] []) in print_output out

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0
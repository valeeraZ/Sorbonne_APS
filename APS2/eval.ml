open Ast;;

(*N, F, FR*)
(*APS1: A, S, P, PR*)
(*E = ident -> V*)
type value = InN of int | InF of expr * string list * ident list | InFR of value | 
             InA of int | InP of block * string list * ident list | InPR of value | 
             (* APS2: bloc de mémoire *)
             InB of int * int |
             None
(*association id-value*)
and ident = Pair of string * value

let print_val value =
  match value with
    InN(n) -> Printf.printf "%d\n" n
  | InB(a, n) -> Printf.printf "B: %d, %d\n" a n
  | _ -> Printf.printf "not implemented print function :) \n"

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

(* allocation d'un bloc de mémoire *)
let allocB mem size =
  if (size <= 0) then failwith "Invalid argument for allocation"
  else
    let address = !alloc_indicator in
        let rec add_to_memory new_mem n =
          match n with
            0 -> new_mem
          | _ -> (
              let tmp = !alloc_indicator in
                alloc_indicator := (!alloc_indicator + 1); 
                add_to_memory ((tmp, ref(InN(-1)))::new_mem) (n - 1)
            ) in
        (address, (add_to_memory mem size))

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
    ASTArgP(arg) -> parse_arg arg
  | ASTArgsP(arg,args) -> (parse_arg arg)@(make_closure_proc args)
and parse_arg arg =
  match arg with
      ArguP(id, typ) -> id::[]
    | ArguPA(id, typ) -> id::[]

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

(*La mémoire sera modifiée donc retourner la nouvelle mémoire*)
let rec eval_expr expr env mem = 
  match expr with
  | ASTNum(n) -> (InN(n), mem)
  | ASTId(id) -> if in_env id env 
                  then match extract_from_env id env with
                    |InA(a) -> (!(extract_from_mem a mem), mem)
                    | v -> (v, mem)
                  else failwith (id^" not a variable in environment")
  | ASTBinary(op,e1,e2) -> if is_bop op then ((eval_bop op (fst (eval_expr e1 env mem)) (fst (eval_expr e2 env mem))),  mem)
    else failwith "not a binary operator"
  | ASTUnary(op,e) -> if is_uop op then (eval_uop op (fst (eval_expr e env mem)), mem) 
    else failwith "not an unary operator"
  | ASTIf(e1,e2,e3) -> if (eval_expr e1 env mem) = (InN(1), mem) then eval_expr e2 env mem
    else if (eval_expr e1 env mem) = (InN(0), mem) then (eval_expr e3 env mem) 
    else failwith "not a boolean value"
  | ASTFunc(args,e_prim) -> (InF(e_prim, (make_closure args []),env), mem)
  | ASTBool(e) -> if e then (InN(1), mem) else (InN(0), mem)
  | ASTApp (expr,exprs) -> (
      let f, new_mem = (eval_expr expr env mem) in
        match f with
          InF(e_prim,closure,envi) -> let env_fun = (assoc_val closure (get_eval exprs env new_mem))@envi in
            eval_expr e_prim env_fun new_mem
        | InFR(closure) ->( match closure with
            InF(e_prim,f,envi) -> let env_fun = (assoc_val f (get_eval exprs env new_mem))@env in
              eval_expr e_prim env_fun new_mem
          | _ -> failwith "not a recursive function")
      | InN(n) -> (InN(n), mem)
      | _ -> failwith "application result not applied yet"
    )
  (* APS2 *)
  | ASTLen(e) -> (
      let v, new_mem = eval_expr e env mem in
        match v with
          InB(a, n) -> (InN(n), new_mem)
        | v -> failwith "Evaluation of len: not a vec"
    )
  | ASTNth(vec, n) -> (
      let (v, mem1) = (eval_expr vec env mem) in
        let (i, mem2) = (eval_expr n env mem1) in
          match i with
            InN(x) -> (
                match v with
                  InB(a, size) -> (
                    if x < size then (!(extract_from_mem (a+x) mem2), mem2) else failwith "Evaluation of nth: index out of range"
                  )
                | v -> (
                  failwith "Evaluation of nth: not a vec"
                )
              )
          | v -> failwith "Evaluation of nth: not an integer"
    )
  | ASTAlloc(n) -> (
      let v = fst (eval_expr n env mem) in
        match v with
          InN(x) -> (
            let (adresse, new_mem) = allocB mem x in
              (* voila ce qui est mise à jour *)
              (InB(adresse, x), new_mem)
          ) 
        | _ -> failwith "Evaluation of alloc: not an integer"
    )
(* APS2 *)
and eval_lval expr env mem =
  match expr with
    ASTLvId(id) -> (
      if in_env id env 
        then match extract_from_env id env with
          | InA(a) -> InA(a)
          | InB(a, n) -> InB(a,n)
          | _ -> failwith "Evaluation of lval error"
        else failwith (id^" not a variable in environment")
    )
  | ASTLval(lv, e) -> 
      let vec = eval_lval lv env mem in
        let e1, mem2 = (eval_expr e env mem) in
          match vec with
            InB(a,n) -> (
              match e1 with
                InN(x) -> (
                  let ad = a + x in
                    match !(extract_from_mem ad mem) with
                      InB(adr, n) -> (
                        InB(adr, n)
                      )
                    | _ ->(
                      InA(ad) 
                    )
                )
              | _ -> failwith "Evaluation of nth: not an integer"
            )
          | _ -> failwith "Evaluation of nth: not a vec"
    
(* APS1A *)
and eval_expr_p expr env mem =
  match expr with
    ASTPr(e) -> eval_expr e env mem
  | ASTPrCall(e) -> (
      match e with
       ASTId(id) -> (
        if in_env id env  
        then match extract_from_env id env with
          |InA(a) -> (InA(a), mem)
          | v -> (v, mem)
        else failwith (id^" not a variable in environment")
      )
      | _ -> failwith ("adresse not a variable in environment")
  )

and get_eval exprs env mem =
  match exprs with
    ASTExprs(e,es) -> (
      let value, new_mem = (eval_expr e env mem) in 
        value::(get_eval es env new_mem)
    )
  | ASTExpr(e) -> [(fst (eval_expr e env mem))]

(* APS1A *)
and get_eval_p exprs env mem = 
  match exprs with
    ASTExprsp(e, es) -> (fst (eval_expr_p e env mem))::(get_eval_p es env mem)
  | ASTExprp(e) -> (fst (eval_expr_p e env mem))::[]

and eval_dec dec env mem =
  match dec with
  | ASTConst(id,t,expr) -> (
    let (v, new_mem) = eval_expr expr env mem in
      (Pair(id, v)::env, new_mem)
  )
  | ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,make_closure args [],env))::env, mem)
  | ASTFunRec(id,t,args,expr) -> (Pair(id, InFR(InF(expr,make_closure args [],env)))::env, mem)
  | ASTVar(id,t) -> let (adresse, new_mem) = alloc(mem) in (Pair(id, (InA(adresse)))::env, new_mem)
  | ASTProc(id,args,block) -> (Pair(id, InP(block,make_closure_proc args, env))::env, mem)
  | ASTProcRec(id,args,block) -> (Pair(id, InPR(InP(block,make_closure_proc args, env)))::env, mem)

and eval_stat stat env mem output =
  match stat with
    ASTEcho(n) -> (
      (mem, (fst (eval_expr n env mem))::output)
    )
  | ASTSet(lval,e) -> (
      match eval_lval lval env mem with
        | InA(a) -> 
            let v = (extract_from_mem a mem) and (affectation, new_mem) = (eval_expr e env mem) 
              in v := affectation;
              (new_mem, output)
        | InB(adr, n) -> failwith "cannot set a vec"
        | _ -> failwith ("address not in memory")
    )
  | ASTIfb (e,b1,b2) -> if (eval_expr e env mem) = (InN(1), mem)
                          then (eval_block b1 env mem output)
                          else (eval_block b2 env mem output)
  | ASTWhile(e,b) -> if (eval_expr e env mem) = (InN(0), mem)
                        then (mem, output)
                        else let (new_mem, new_output) = (eval_block b env mem output) in
                          eval_stat stat env new_mem new_output
  | ASTCall(p,exprs) -> 
    let (v, new_mem) = (eval_expr p env mem) in
      match v with
      InP(block,closure,envi) -> let env_proc = (assoc_val closure (get_eval_p exprs env new_mem))@envi in
        eval_block block env_proc new_mem output
    | InPR(closure) ->( match closure with
          InP(block,f,envi) -> let env_proc = (assoc_val f (get_eval_p exprs env new_mem))@env in
            eval_block block env_proc new_mem output
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
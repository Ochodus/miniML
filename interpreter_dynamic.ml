open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e) -> "Procedure "

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

(* represent the given environment as a string (use this for debugging) *)
let rec string_of_env env = 
	(List.fold_left (fun acc (x,v) -> Printf.sprintf "%s, %s |-> %s" acc x (value2str v)) "{" env) ^ "}" 

(* represent the given memory as a string (use this for debugging) *)
let rec string_of_mem mem = 
	(List.fold_left (fun acc (l,v) -> Printf.sprintf "%s, %d |-> %s" acc l (value2str v)) "{" mem) ^ "}" 
		 

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem -> 
	(match exp with
	| CONST n -> (Int n, mem)
	| VAR s -> (apply_env env s, mem)
	| ADD (e1, e2) -> arthm env mem e1 e2 (+)
	| SUB (e1, e2) -> arthm env mem e1 e2 (-)
	| MUL (e1, e2) -> arthm env mem e1 e2 ( * )
	| DIV (e1, e2) -> arthm env mem e1 e2 (/)
	| EQ (e1, e2) -> bop env mem e1 e2 (=)
	| LT (e1, e2) -> bop env mem e1 e2 (<)
	| ISZERO e -> 
		let (v, mem') = eval e env mem in
		(match v with
		| Int 0 -> (Bool true, mem')
		| Int _ -> (Bool false, mem')
		| _ -> raise (Failure "Type Error: Argument of iszero must be number.")
		)
	| READ -> (Int (read_int ()), mem)
	| IF (e1, e2, e3) -> 
		let (v, mem') = eval e1 env mem in
		(match v with
		| Bool true -> eval e2 env mem'
		| Bool false -> eval e3 env mem'
		| _ -> raise (Failure "Type Error: condition must be Boolean")
		)
	| LET (x, e1, e2) -> 
		let (v, mem') = eval e1 env mem in
		eval e2 (extend_env (x, v) env) mem'
	| LETREC (x1, x2, e1, e2) -> eval e2 (extend_env (x1, Procedure (x2, e1)) env) mem
	| LETMREC (x1, x2, e1, x3, x4, e2, e3) ->
		eval e3 (extend_env (x1, Procedure (x2, e1)) (extend_env (x3, Procedure (x4, e2)) env)) mem
	| PROC (x, e) -> (Procedure(x, e), mem)
	| CALL (e1, e2) ->
		let (v, mem') = eval e1 env mem in
		(match v with
		| Procedure (x, e') -> 
			let (v', mem'') = eval e2 env mem' in
			eval e' (extend_env (x, v') env) mem''
		| _ -> raise (Failure "Error: Not a function.")
		)
  | NEWREF e ->
		let (v, mem') = eval e env mem in
		let l = new_location () in
		(Loc l, extend_mem (l, v) mem')
  | DEREF e ->
		let (v, mem') = eval e env mem in
		(match v with
		| Loc l -> (apply_mem mem' l, mem')
		| _ -> raise (Failure "Error: Not Location")
		)
  | SETREF (e1, e2) ->
		let (v, mem') = eval e1 env mem in
		let (v', mem'') = eval e2 env mem' in
		(match v with
		| Loc l -> (v', extend_mem (l, v') mem'')
		| _ -> raise (Failure "Error: Not Location")
		)
  | SEQ (e1, e2) ->
		let (v1, mem') = eval e1 env mem in
		eval e2 env mem'
  | BEGIN e -> eval e env mem
	)
and arthm : env -> mem -> exp -> exp -> (int -> int -> int) -> (value * mem)
= fun env mem e1 e2 op ->
  let (v1, mem') = eval e1 env mem in
  let (v2, mem'') = eval e2 env mem' in
    (match v1, v2 with 
    | Int n1, Int n2 -> 
			(try (Int (op n1 n2), mem'') with
			| Division_by_zero -> raise UndefinedSemantics
			| _ -> (Int (op n1 n2), mem'')
			)
    | _ -> raise (Failure "Type Error: Cannot arithmetic non-numeric values")
		)
and bop : env -> mem -> exp -> exp -> ('a -> 'a -> bool) -> (value * mem)
= fun env mem e1 e2 bop ->
	let (v1, mem') = eval e1 env mem in
  let (v2, mem'') = eval e2 env mem' in
	(Bool (bop v1 v2), mem'')
	
(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 

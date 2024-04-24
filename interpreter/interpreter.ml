open List;;
open Printf;;

(* Structure of the program *)

type variable = string;;
type symbol = (string * int);;
type signature = symbol list;;
type term = Var of variable | Num of int | Node of symbol * (term list) | Under;;
type atomic = Atom of symbol * (term list);;
type head = Head of atomic;;
type body = Body of atomic list;;
type clause = Fact of head | Rule of head * body;;
type program = clause list;;
type goal = Goal of atomic list;; 
type substitution = (variable * term) list;;

(* exceptions *)

exception NOT_UNIFIABLE;;
exception NOT_FOUND;;
exception INVALID_PROGRAM;;
exception ERROR;;

(* basic functions required *)

let rec mem a l = 
  match l with
    [] -> false
  | h::tl -> (a=h) || (mem a tl)
;;

let rec union l1 l2 =
  match l1 with
    [] -> l2
  | h::tl -> if (mem h l2) then (union tl l2) else h::(union tl l2)
;;

let print_bool b =
  if(b) then print_string ("true.\n")
  else print_string ("false.\n")
;;

let rec occur_check (v : variable) (t : term) : bool = 
  match t with
    Var x -> x = v
  | Node(_, clist) -> exists (occur_check v) clist
  | _ -> false
;;

let rec vars_from_term (t : term) : (variable list) = 
  match t with
    Var(v) -> [v]
  | Node(n , clist) -> fold_left union [] (map vars_from_term clist)
  | _ -> []
;;

let vars_from_atom (Atom(sym, ls) : atomic) : (variable list) = 
  vars_from_term (Node(sym, ls));;
  
let rec vars_from_goal (Goal(g) : goal) : (variable list) = 
  fold_left union [] (map vars_from_atom g);;

let rec substitute (s:substitution) (t:term) : term = 
  match t with
  | Var v -> (
    match s with
      [] -> t
    | s'::sl -> if (fst s') = v then snd s' else substitute sl t 
    )
  | Node (n , clist) ->
    Node (n, map (substitute s) clist)
  | _ -> t
;;

let substitute_atom (s:substitution) (Atom(sym, ls) : atomic) : atomic =
  Atom(sym, map (substitute s) ls)
;;
    
let rec compose_subst (s1: substitution) (s2: substitution) : substitution =
  let rec compose_helper acc = function
  | [] -> acc
  | (var, t) :: rest ->
    let t' = substitute s1 t in
    let acc' = if mem_assoc var s1 then acc else (var, t') :: acc in
    compose_helper acc' rest
  in
  compose_helper s1 s2
;;

let rec mgu_of_term (t:term) (u:term) : substitution =
  match t, u with
  | Var x, Var y ->
    if x=y then []
    else [(x, Var y)]
  | Var x, Node(s, ls) ->
    if occur_check x u then raise NOT_UNIFIABLE
    else [(x, u)]
  | Node(s, ls), Var x ->
    if occur_check x t then raise NOT_UNIFIABLE
    else [(x, t)]
  | (Num(n1), Num(n2)) -> 
    if n1=n2 then [] 
    else raise NOT_UNIFIABLE
  | (Num(_), Var(x)) -> [(x, t)]
  | (Var(x), Num(_)) -> [(x, u)]
  | (Under, _) | (_, Under) -> []
  | Node(s1, ls1) , Node(s2, ls2) when s1 = s2 ->
    let rec helper sub c1 c2 =
      match c1, c2 with
      | [], [] -> sub
      | c1h::c1t, c2h::c2t ->
        let s = compose_subst sub (mgu_of_term (substitute sub c1h) (substitute sub c2h)) in
        helper s c1t c2t
      | _, _ -> raise NOT_UNIFIABLE
    in 
    helper [] ls1 ls2
  | _, _ -> raise NOT_UNIFIABLE
;;

let mgu_of_atom (Atom(sym1, ls1) : atomic) (Atom(sym2, ls2) : atomic) : substitution =
  mgu_of_term(Node(sym1, ls1)) (Node(sym2, ls2))
;;

(* print helpers *)

let rec print_list (t : term list) : unit =
  match t with
    [] -> print_string ("")
  | [t] -> print_term t
  | hd::tl -> 
    print_term hd;
    print_string (",");
    print_list tl;

and print_listb (t : term) : unit = 
  match t with
    Node(("_emptylist", 0), []) -> print_string("")
  | Node(("_list", 2), [t'; Node(("_emptylist", 0), [])]) -> print_term t'
  | Node(("_list", 2), [t1; t2]) -> 
    print_term t1;
    print_string (",");
    print_term t2;
  | _ -> raise ERROR

and print_term (t:term) : unit =
  match t with
    Var(x) -> print_string (" " ^ x ^ " ")
  | Node(("_emptylist", 0), []) -> print_string (" [] ")
  | Node(s, []) -> print_string (" " ^ (fst s) ^ " ")
  | Node(("_list", 2), _) -> 
    print_string (" [");
    print_listb t;
    print_string ("] ");
  | Node(s, l) -> 
    print_string (" " ^ (fst s) ^ "( ");
    print_list l;
    print_string (" ) ");
  | Num(n) -> print_string ( " " ^ (string_of_int n) ^ " ")
  | Under -> print_string (" _ ")
;;

let rec print_solution (sub : substitution) : unit = 
  match sub with
  [] -> print_string ("true. ")
  | [(v, t)] ->
    print_string (v ^ " = ");
    print_term t;
  | (v, t)::tl -> 
    print_string (v ^ " = ");
    print_term t;
    print_string (", ");
    print_solution tl;
;;

(* program validating function *)

let rec validate_program (p : program) : bool =
  match p with
    [] -> true
  | (Fact(Head(x)))::xl | (Rule(Head(x), _))::xl -> 
    match x with
      Atom(("=", _), _) | Atom(("<>", _), _) | Atom(("!", _), _) | Atom((">", _), _) | Atom(("<", _), _) -> raise INVALID_PROGRAM
    | _ -> validate_program xl
;;

(* processing the program attributes *)

let rec process_term (t : term) : term = 
  match t with
    Var(x) -> Var(x)
  | Node(sym, l) -> Node(sym, map (process_term) l)
  | _ -> t
;;

let rec process_atomic (a : atomic) : atomic =
  match a with
    Atom(sym, l) -> Atom(sym, map (process_term ) l)
;;

let rec process_clause (c : clause) : clause = 
  match c with
    Fact(Head(h)) -> Fact(Head(process_atomic h))
  | Rule(Head(h), Body(b)) -> Rule(Head(process_atomic h), Body(map (process_atomic) b))
;;

let rec preprocess_program (p : program) : program =
  match p with
    [] -> []
  | c::clist -> (process_clause c)::(preprocess_program clist)
;;

let rec process_program (p : program) (Atom(sym ,l) : atomic) : program =
  match p with
    [] -> []
  | c::clist -> 
    match c with
      Fact(Head(Atom(sym', _))) | Rule(Head(Atom(sym', _)), _) ->
        if sym = sym' then (process_clause c)::(process_program clist (Atom(sym, []))) else c::(process_program clist (Atom(sym, [])))
;;

let extractchar () : char = 
  let stdin_channel = Unix.in_channel_of_descr Unix.stdin in
  let old_flags = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { old_flags with Unix.c_icanon = false } in
  let res = input_char stdin_channel in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN old_flags;
  res
;;

let rec bind (sub : substitution) (v : variable list) =
  match v with
    [] -> []
  | hd::tl -> 
    let rec find l = 
      match l with
        [] -> raise NOT_FOUND
      | a::b -> if(fst a) = hd then a
                else find b
    in 
    try (find sub)::(bind sub tl) 
    with NOT_FOUND -> (bind sub tl)
;;

let bind_terms (t1 : term) (t2 : term) (sub : substitution) : substitution =
  compose_subst sub (mgu_of_term (substitute sub t1) (substitute sub t2))
;;

let bind_atomics (a1 : atomic) (a2 : atomic) (sub : substitution) : substitution = 
  compose_subst sub (mgu_of_atom (substitute_atom sub a1) (substitute_atom sub a2))
;;

let rec eval_terms (t : term) : term =
  match t with
  | Num(_) -> t
  | Node(("+", 2), [t1; t2]) -> (
      match ((eval_terms t1), (eval_terms t2)) with
      (Num(n1), Num(n2)) -> Num(n1 + n2)
    | _ -> raise NOT_UNIFIABLE
    )
  | Node(("-", 2), [t1; t2]) -> (
      match ((eval_terms t1), (eval_terms t2)) with
      (Num(n1), Num(n2)) -> Num(n1 - n2)
    | _ -> raise NOT_UNIFIABLE
    )
  | Node(("*", 2), [t1; t2]) -> (
      match ((eval_terms t1), (eval_terms t2)) with
      (Num(n1), Num(n2)) -> Num(n1 * n2)
    | _ -> raise NOT_UNIFIABLE
    )
  | Node(("/", 2), [t1; t2]) -> (
      match ((eval_terms t1), (eval_terms t2)) with
      (Num(n1), Num(n2)) -> Num(n1 / n2)
    | _ -> raise NOT_UNIFIABLE
    )
  | _ -> t
;;
        
let eval_atoms (a : atomic) (sub : substitution) : substitution =
  match a with
    Atom(("=", 2), [t1; t2])
  | Atom(("<>", 2), [t1; t2]) -> 
    compose_subst sub (mgu_of_term (eval_terms (substitute sub t1)) (eval_terms (substitute sub t2)))
  | Atom((">", 2), [t1; t2]) -> (
    match (eval_terms (substitute sub t1), eval_terms (substitute sub t2)) with
      (Num(n1), Num(n2)) -> if n1 > n2 then sub 
    else raise NOT_UNIFIABLE
    | _ -> raise NOT_UNIFIABLE
  )
  | Atom(("<", 2), [t1; t2]) -> (
    match (eval_terms (substitute sub t1), eval_terms (substitute sub t2)) with
      (Num(n1), Num(n2)) -> if n1 < n2 then sub 
    else raise NOT_UNIFIABLE
    | _ -> raise NOT_UNIFIABLE
  )
  | _ -> sub
;;

let rec eval_goal (p : program) (g : goal) (sub : substitution) (v : variable list) : (bool * substitution) = 
  match g with
  | Goal([]) -> (
    print_solution (bind sub v);
    flush stdout;
    let c = ref (extractchar ()) in
    while (!c <> '.' && !c <> ';') do
      print_string ("\nInvalid Action : ");
      print_char(!c);
      print_string ("\nAction? ");
      print_string ("");
      flush stdout;
      c := extractchar ();
    done;
    print_string("\n");
    if !c = '.' then (true, [])
    else (false, [])
  )
  | Goal(at::atlist) ->
    match at with
      Atom(("=", 2), _) | Atom((">", 2), _) | Atom(("<", 2), _) -> (
        try eval_goal p (Goal(atlist)) (eval_atoms at sub) v with
        NOT_UNIFIABLE -> (false, []) 
      )
    | Atom(("<>", 2), _) -> (
      try (false, eval_atoms at sub) with
      NOT_UNIFIABLE -> eval_goal p (Goal(atlist)) sub v
    )
    | Atom(("!", 0), _) -> let _ = eval_goal p (Goal(atlist)) sub v in (true, [])
    | _ ->
      let temp = process_program p at in
      let rec iter p' = match p' with
        [] -> (false, [])
      | cl::clist -> match cl with 
          Fact(Head(a)) -> (
            try
              let u = (bind_atomics a at sub) in
              match (eval_goal temp (Goal(atlist)) u v) with
                (true, u') -> (true, u')
              | _ -> iter clist
              with NOT_UNIFIABLE -> iter clist
          )
        | Rule(Head(hd), Body(bd)) -> (
            try 
              let u = (bind_atomics hd at sub) in
              match (eval_goal temp (Goal(bd @ atlist)) u v) with
                (true, u') -> (true, u')
              | _ -> iter clist
            with NOT_UNIFIABLE -> iter clist
        )
      in iter p
;;

let interpret (p : program) (g : goal) = 
  eval_goal p g [] (vars_from_goal g)
;;

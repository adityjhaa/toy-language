type term =
  | Variable of string
  | Number of int
  | Under
  | Node of string * (term list)
  | Tuple of term * term
;;

type atomic =
  | Atom of string * (term list)
;;

type  head = Head of atomic;;
type body = Body of atomic list;;

type clause =
  | Fact of head 
  | Rule of head * body 
;;

type goal = Goal of atomic list;;

type program = clause list;;


let rec string_of_term term =
  match term with
  | Variable v -> "\n\t\t\t\t\tVariable : " ^ v
  | Under -> "\n\t\t\t\t\t _ "
  | Number n -> "\n\t\t\t\t\tNumber : " ^ string_of_int n
  | Node (label, []) -> "\n\t\t\t\t\tConstant : " ^ label
  | Node (label, args) ->
      let args_str = String.concat "," (List.map string_of_term args) in
      "\n\t\t\t\t\tlabel : " ^ label ^ "(" ^ args_str ^ ")"
  | Tuple (t1,t2) -> "\n\t\t\t\t\tTuple : " ^ (string_of_term t1) ^ ", " ^ (string_of_term t2)
;;

let rec string_of_atomic atomic =
  match atomic with
  | Atom (pred, []) -> pred
  | Atom (pred, args) ->
      let args_str = String.concat "," (List.map string_of_term args) in
      "atom : \n\t\t\t\tlabel : "^pred ^ "(" ^ args_str ^ ")"
;;


let rec string_of_clause clause =
  match clause with
  | Fact (Head(atomic)) -> "Clause(Fact):\n\t\tHead: \n\t\t\t" ^ string_of_atomic atomic 
  | Rule (Head(atomic), Body(alist)) ->
    "Clause(Rule):\n\t\tHead: \n\t\t\t" ^ string_of_atomic atomic ^ "\n\t\tBody: \n\t\t\t" ^ String.concat "\n\t\t\t" (List.map string_of_atomic alist)
;;

  
let string_of_goal (Goal atoms) =
  String.concat ", " (List.map string_of_atomic atoms)
;;

let string_of_program program =
  let p = String.concat "\n\t" (List.map string_of_clause program) in
  "Program : \n\t" ^ p ^ "\nEOF\n"
;;



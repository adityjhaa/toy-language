open Printf
open Token
open String

let generate_token_list lexbuf =
  let rec helper a =
    match Lexer.token lexbuf with
    | EOF -> a
    | t -> helper (t::a)
  in List.rev (helper [])
;;

let print_tokens = function
  | PLUS -> "+ : arithmetic operator (PLUS)"
  | TIMES -> "* : arithmetic operator (TIMES)"
  | MINUS -> "- : arithmetic operator (MINUS)"
  | BY -> "/ : arithmetic operator (BY)"
  | MOD -> "% : arithmetic operator (MOD)"
  | ASSIGN -> "= : assignment operator (ASSIGN)"
  | EQUAL -> "== : comparison operator (EQUAL)"
  | GREATER -> "> : comparison operator (GREATER)"
  | LESS -> "< : comparison operator (LESS)"
  | GTE -> ">= : comparison operator (GTE)"
  | LTE -> "<= : comparison operator (LTE)"
  | UNEQUAL -> "<> : comparison operator (UNEQUAL)"
  | AND -> "boolean operator (AND)"
  | OR -> "boolean operator (OR)"
  | NOT -> "boolean operator (NOT)"
  | DOT -> ". : DOT operator"
  | INCREMENT -> "++ : increment operator"
  | DECREMENT -> "-- : decrement operator"
  | CONCATENATE -> "con : CONCATENATE operator"
  | SUBSTR -> "substr : substring operator"
  | LEN -> "len : string length"
  | INV -> "inv : string inverse"
  | IDENTIFIER i -> sprintf "%s : IDENTIFIER" i
  | KEYWORD v -> sprintf "%s : KEYWORD" v
  | BOOLEAN b -> sprintf "%b : BOOLEAN" b
  | INT n -> sprintf "%d : INTEGER" n
  | LITERAL s -> sprintf "%s : STRING LITERAL" s
  | OPEN_PARENTHESIS -> "OPEN_PARENTHESIS"
  | CLOSE_PARENTHESIS -> "CLOSE_PARENTHESIS"
  | OPEN_BRACE -> "OPEN_BRACE"
  | CLOSE_BRACE -> "CLOSE_BRACE"
  | OPEN_SQUARE -> "OPEN_SQUARE"
  | CLOSE_SQUARE -> "CLOSE_SQUARE"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | EOF -> "EOF"
  | CAPERROR c -> sprintf "%s : ERROR" c
  | ERROR -> "ERROR!"
;;

let main =
  let input = read_line() in
  let s = sub input 1 (length input -2) in
  let lexbuf = Lexing.from_string s in
  let tokens_list = generate_token_list lexbuf in
  List.map print_tokens tokens_list |> List.iter (printf "%s\n")
;;


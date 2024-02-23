type token = 
| IDENTIFIER of string
| KEYWORD of string
| BOOLEAN of bool
| LITERAL of string
| INT of int
| PLUS
| MINUS
| TIMES
| BY
| MOD
| ASSIGN
| EQUAL
| GREATER
| LESS
| GTE
| LTE
| UNEQUAL
| AND
| OR
| NOT
| DOT
| INCREMENT
| DECREMENT
| CONCATENATE
| SUBSTR
| LEN
| INV
| OPEN_PARENTHESIS
| CLOSE_PARENTHESIS
| OPEN_BRACE
| CLOSE_BRACE
| OPEN_SQUARE
| CLOSE_SQUARE
| SEMICOLON
| COMMA
| EOF
| CAPERROR of string
| ERROR
;;

let keywords = ["let"; "int"; "bool"; "double"; "string"; "char"; "if"; "then"; "else"; "pair"; "first"; "second"; "for"; "each"; "while"; "print"; "in"; "typedef"; "rec"; "return"];;

let is_key s = List.mem s keywords;;

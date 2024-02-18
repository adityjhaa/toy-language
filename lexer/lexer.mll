{
    open Token
}


rule token = parse
    [' ' '\t' '\n']
        { token lexbuf }
    | ['+']
        { PLUS }
    | ['-']
        { MINUS }
    | ['*']
        { TIMES }
    | ['/']
        { BY }
    | ['%']
        { MOD }
    | ['=']
        { ASSIGN }
    | "=="
        { EQUAL }
    | ['>']
        { GREATER }
    | ['<']
        { LESS }
    | ">="
        { GTE }
    | "<="
        { LTE }
    | "<>"
        { UNEQUAL }
    | "true"
        { BOOLEAN true }
    | "false"
        { BOOLEAN false }
    | ['0'-'9']+ as n
        { INT (int_of_string n) }
    | ['\"'] ['!' '#'-'~' ' ']* ['\"'] as s
        { LITERAL s }
    | "and"
        { AND }
    | "&&"
        { AND }
    | "||"
        { OR }
    | "or"
        { OR }
    | "not"
        { NOT }
    | ['!']
        { NOT }
    | ['.']
        { DOT }
    | "con"
        { CONCATENATE }
    | "substr"
        { SUBSTR }
    | "len"
        { LEN }
    | "inv"
        { INV }
    | ['(']
        { OPEN_PARENTHESIS }
    | [')']
        { CLOSE_PARENTHESIS }
    | ['{']
        { OPEN_BRACE }
    | ['}']
        { CLOSE_BRACE }
    | [']']
        { OPEN_SQUARE }
    | ['[']
        { CLOSE_SQUARE }
    | [',']
        { COMMA }
    | [';']
        { SEMICOLON }
    | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as v
        { if is_key v then KEYWORD v else IDENTIFIER v }
    | eof 
        { EOF }
    | ['A'-'Z' '\'']+ as c
        { CAPERROR c }
    | _ 
        { ERROR }


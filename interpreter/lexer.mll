{
    open Parser;;
    exception InvalidToken;;
}

rule tokenize = parse
    | [' ' '\t' '\n']
        { tokenize lexbuf }
    | ['(']
        { LPAREN }
    | [')']
        { RPAREN }
    | ['[']
        { LBRACKET }
    | [']']
        { RBRACKET }
    | [',']
        { COMMA }
    | ['.']
        { DOT }
    | ":-"
        { COND }
    | ['!']
        { CUT }
    | ['|']
        { PIPE }
    | ['_']
        { UNDERLINE }
    | "not"
        { NOT }
    | ['=']
        { EQ }
    | "<>"
        { N_EQ }
    | ['+']
        { PLUS }
    | ['-']
        { MINUS }
    | ['*']
        { TIMES }
    | ['/']
        { DIV }
    | ['>']
        { GT }
    | ['<']
        { LT }
    | ['0'] | ['1'-'9']['0'-'9']* as n
        { INT(int_of_string n)}
    | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as v
        { VAR(v) }
    | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as c
        { CONST(c) }
    | "//"
        { line_comment lexbuf }
    | "/*"
        { all_comment lexbuf }
    | eof
        { EOF }
    | _
        { raise (InvalidToken) }

and line_comment = parse
      eof 
        { EOF }
    | ['\n']
        { tokenize lexbuf }
    | _ { line_comment lexbuf }

and all_comment = parse
      eof
        { failwith "Syntax error : /* not closed with */" }
    | "*/"
        { tokenize lexbuf }
    | _
        { all_comment lexbuf }

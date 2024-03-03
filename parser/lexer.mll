{
    open Parser;;
    exception InvalidToken of char;;
}

rule token = parse
      [' ' '\t' '\n']
        { token lexbuf }
    | ['(']
        { LPAREN }
    | [')']
        { RPAREN }
    | ['[']
        { LBRACKET }
    | [']']
        { RBRACKET }
    | ['+']
        { ADD }
    | ['-']
        { SUB }
    | ['*']
        { MUL }
    | ['/']
        { DIV }
    | ['=']
        { EQUAL}
    | ['>']
        { GT }
    | ['<']
        { LT }
    | "<>"
        { NOT_EQUAL }
    | "\+"
        { NOT }
    | [',']
        { COMMA }
    | ['!']
        { CUT }
    | "->"
        { ARROW }
    | ['|']
        { PIPE }
    | ['_']
        { UNDERSCORE }
    | ['.']
        { DOT }
    | ['0'] | ['1'-'9']['0'-'9']* as n
        { INT(int_of_string n)}
    | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as v
        { VAR(v) }
    | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* | ['"'] ['!' '#'-'~' ' ']+ ['"'] as c
        { CONST(c) }
    | ['%'] | "//"
        { line_comment lexbuf }
    | "/*"
        { all_comment lexbuf }
    | eof
        { EOF }
    | _ as s
        { raise (InvalidToken s) }

and line_comment = parse
      eof 
        { EOF }
    | ['\n']
        { token lexbuf }
    | _ { line_comment lexbuf }

and all_comment = parse
      eof
        { failwith "Syntax error : /* not closed with */" }
    | "*/"
        { token lexbuf }
    | _
        { all_comment lexbuf }

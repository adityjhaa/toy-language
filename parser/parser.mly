%{
    open Ast;;
%}

%token <int> INT
%token <string> VAR CONST
%token LPAREN RPAREN LBRACKET RBRACKET ADD SUB MUL DIV EQUAL NOT_EQUAL GT LT NOT COMMA CUT ARROW DOT EOF PIPE UNDERSCORE

%left COMMA
%nonassoc EQUAL LT GT PIPE NOT
%left ADD SUB MUL DIV
%nonassoc DOT

%start program goal
%type <Ast.program> program
%type <Ast.goal> goal
%%

program:
    | EOF
        { [] }
    | clause_list EOF
        { $1 }
;

clause_list:
    | clause
        { [$1] }
    | clause clause_list
        { $1::$2 }
;

clause:
    | atomic DOT
        { Fact(Head($1)) }
    | atomic ARROW atomic_list DOT 
        { Rule(Head($1), Body($3)) }
;

goal:
    | atomic_list DOT 
        {Goal($1)}
;

atomic_list:
    | atomic
        { [$1] }
    | atomic COMMA atomic_list
        { $1::$3 }
;

atomic:
    | CONST
        { Atom($1, []) }
    | CONST LPAREN term_list RPAREN
        { Atom($1, $3) }
    | term EQUAL term
        { Atom("=", [$1; $3]) }
    | term NOT_EQUAL term
        { Atom("<>", [$1; $3]) }
    | term LT term
        { Atom("<", [$1; $3]) }
    | term GT term
        { Atom(">", [$1; $3]) }
    | NOT term
        { Atom("not", [$2]) }
    | CUT
        { Atom("!", []) }
;

term_list:
    | term
        { [$1] }
    | term COMMA term_list
        { ($1)::$3 }
;

term:
    | LPAREN term RPAREN
        { $2 }
    | LPAREN term COMMA term RPAREN
        { Tuple($2,$4) }
    | VAR
        { Variable($1) }
    | UNDERSCORE
        { Under }
    | CONST
        { Node($1, []) }
    | INT
        { Number($1) }
    | CONST LPAREN term_list RPAREN
        { Node($1, $3) }
    | term ADD term
        { Node("+", [$1; $3]) }
    | term SUB term
        { Node("-", [$1; $3]) }
    | term MUL term
        { Node("*", [$1; $3]) }
    | term DIV term
        { Node("/", [$1; $3]) }
    | list
        { $1 }
;

list:
    | LBRACKET RBRACKET
        { Node("emptylist", []) }
    | LBRACKET list_body RBRACKET
        { $2 }
;

list_body:
    | term
        { Node("listitem", [$1]) }
    | term COMMA list_body
        { Node("listitem", [$1; $3]) }
    | term PIPE term
        { Node("listitem", [$1; $3]) }
;

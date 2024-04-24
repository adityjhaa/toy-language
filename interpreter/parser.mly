%{
    open Interpreter;;
%}

%token <int> INT
%token <string> VAR CONST
%token LPAREN RPAREN LBRACKET RBRACKET COMMA DOT COND CUT PIPE UNDERLINE NOT EQ N_EQ PLUS MINUS TIMES DIV GT LT EOF

%left COMMA
%nonassoc PIPE EQ GT LT
%left PLUS MINUS
%left TIMES DIV
%nonassoc DOT

%start program goal
%type <Interpreter.program> program
%type <Interpreter.goal> goal
%%

program:
    | EOF
        {[]}
    | clause_list EOF
        { $1 }
;

clause_list:
    | clause
        { [$1] }
    | clause clause_list
        { ($1)::$2 }
;

clause:
    | atomic DOT
        { Fact(Head($1)) }
    | atomic COND atomic_list DOT 
        { Rule(Head($1), Body($3)) }
;

goal:
    | atomic_list DOT
        { Goal($1) }
;

atomic_list:
    | atomic
        { [$1] }
    | atomic COMMA atomic_list
        { ($1)::$3 }
;

atomic:
    | CONST
        { Atom(($1, 0), []) }
    | CONST LPAREN term_list RPAREN
        { Atom(($1, 1), $3) }
    | term EQ term
        { Atom(("=", 2), [$1; $3]) }
    | term N_EQ term
        { Atom(("<>", 2), [$1; $3]) }
    | term LT term
        { Atom(("<", 2), [$1; $3]) }
    | term GT term
        { Atom((">", 2), [$1; $3]) }
    | NOT term
        { Atom(("not", 1), [$2]) }
    | CUT
        { Atom(("!", 0), []) }
;

term_list:
    | term
        { [$1] }
    | term COMMA term_list
        { ($1)::$3 }
;

term:
    | LPAREN term LPAREN
        { $2 }
    | UNDERLINE
        { Under }
    | VAR
        { Var($1) }
    | CONST
        { Node(($1, 0),[]) }
    | INT
        { Num($1) }
    | CONST LPAREN term_list RPAREN
        { Node(($1, 1),$3) }
    | term PLUS term
        { Node(("+", 2), [$1; $3]) }
    | term MINUS term
        { Node(("-", 2), [$1; $3]) }
    | term TIMES term
        { Node(("*", 2), [$1; $3]) }
    | term DIV term
        { Node(("/", 2), [$1; $3]) }
    | list
        { $1 }
;

list:
    | LBRACKET RBRACKET
        { Node(("_emptylist", 0), []) }
    | LBRACKET list_body RBRACKET
        { $2 }
;

list_body:
    | term
        { Node(("_list", 2), [$1; Node(("_emptylist", 0), [])]) }
    | term COMMA list_body
        { Node(("_list", 2), [$1; $3]) }
    | term PIPE term
        { Node(("_list", 2), [$1; $3]) }
;

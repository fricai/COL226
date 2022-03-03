open AST
%%
%name While

%term PROGRAM
    | VAR
    | INT
    | BOOL
    | READ
    | WRITE
    | IF
    | THEN
    | ELSE
    | ENDIF
    | WHILE
    | DO
    | ENDWH
    | TT
    | FF
    | NOT
    | AND
    | OR
    | LT
    | LEQ
    | EQ
    | NEQ
    | GT
    | GEQ
    | PLUS
    | MINUS
    | NEGATIVE
    | TIMES
    | DIV
    | MOD
    | ASSIGN
    | COMMA
    | SEMICOLON
    | DOUBLECOLON
    | COLON
    | IDENTIFIER      of string
    | ILLCH
    | INTCONST        of int
    | ADDOP
    | MULOP
    | BOOLOP
    | RELOP
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | EOF

%nonterm begin        of Prog
       | block        of Blk
       | declarationseq of Dec list
       | declaration  of Dec list
       | varlist      of Var list
       | variable     of Var
       | commandseq   of Cmd list
       | commandseqend of Cmd list
       | command      of Cmd
       | expression   of Exp
       | addop        of (Exp * Exp -> Exp)
       | mulop        of (Exp * Exp -> Exp)
       | relop        of (Exp * Exp -> Exp)
       | boolop       of (Exp * Exp -> Exp)

%pos int

%eop EOF
%noshift EOF

%left     BOOLOP
%nonassoc RELOP
%left     ADDOP
%left     MULOP
%right    NEGATIVE NOT

%nodefault
%verbose
%arg (fileName) : string

%%
begin:
PROGRAM IDENTIFIER DOUBLECOLON block    (PROG(IDENTIFIER, block))

block:
declarationseq commandseq               (BLK(declarationseq, commandseq))

declarationseq:
  declaration declarationseq            (declaration @ declarationseq)
|                                       ([]) 

declaration:
  VAR varlist COLON INT SEMICOLON       (map INT  varlist)
| VAR varlist COLON BOOL SEMICOLON      (map BOOL varlist)

varlist:
  variable COMMA varlist                 (variable::varlist)
| variable                               ([variable])

variable: IDENTIFIER                    (IDENTIFIER)

commandseq:
  LBRACE commandseqend                  (commandseqend)

commandseqend:
  command SEMICOLON commandseqend       (command::commandseqend)
| RBRACE                                ([])

command:
  variable ASSIGN expression            (SET(variable, expression))
| READ variable                         (READ(variable))
| WRITE expression                      (WRITE(expression))
| IF expression THEN commandseq ELSE commandseq
                                        (ITE(expression, commandseq1, commandseq2))
| WHILE expression DO commandseq ENDWH  (WH(expression, commandseq))

expression:
  variable                                              (VAR(variable))
| INTCONST                                              (INTVAL(INTCONST))
| TT                                                    (BOOLVAL(true))
| FF                                                    (BOOLVAL(false))

addop:  PLUS (PLUS) | MINUS (MINUS)
mulop:  TIMES (TIMES) | DIV (DIV) | MOD (MOD)
boolop: AND (AND) | OR (OR)
relop:  LT (LT) | LEQ (LEQ) | EQ (EQ) | GT (GT) | GEQ (GEQ) | NEQ (NEQ)

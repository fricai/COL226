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

%keyword PROGRAM VAR INT BOOL READ WRITE IF THEN ELSE ENDIF WHILE DO ENDWH TT FF

%pos int
%start begin

%eop EOF
%noshift EOF

%left     OR
%left     AND
%nonassoc RELOP LT LEQ EQ NEQ GT GEQ
%left     ADDOP PLUS MINUS
%left     MULOP TIMES DIV MOD
%right    NEGATIVE NOT

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
| IF expression THEN commandseq ELSE commandseq ENDIF
                                        (ITE(expression, commandseq1, commandseq2))
| WHILE expression DO commandseq ENDWH  (WH(expression, commandseq))

expression:
  expression addop expression            %prec ADDOP    (addop(expression1, expression2))
| expression mulop expression            %prec MULOP    (mulop(expression1, expression2))
| NEGATIVE expression                    %prec NEGATIVE (NEGATIVE(expression))
| INTCONST                                              (INTVAL(INTCONST))
| expression AND expression              %prec AND      (AND(expression1, expression2))
| expression OR expression               %prec OR       (OR(expression1, expression2))
| NOT expression                         %prec NOT      (NOT(expression))
| LPAREN expression RPAREN                              (expression)
| variable                                              (VAR(variable))
| TT                                                    (BOOLVAL(true))
| FF                                                    (BOOLVAL(false))
| expression relop expression            %prec RELOP    (relop(expression1, expression2))

addop:  PLUS (PLUS) | MINUS (MINUS)
mulop:  TIMES (TIMES) | DIV (DIV) | MOD (MOD)
relop:  LT (LT) | LEQ (LEQ) | EQ (EQ) | GT (GT) | GEQ (GEQ) | NEQ (NEQ)
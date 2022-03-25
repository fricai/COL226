open AST

exception TypeMismatch
exception InvalidVariable

val printError : string * string * int * int -> unit = fn (fileName, msg, line, col) =>
     print (fileName ^ "[" ^ Int.toString line ^ ":" ^ Int.toString col ^ "] " ^ msg ^ "\n");

type binOpPayload = (Exp * Exp -> Exp) * int * int

val binOpParse : string * binOpPayload * (Exp * Exp -> bool) * Exp * Exp -> Exp  =
    fn (fileName, oper, checkFn, exp1, exp2) =>
        let
            val (f, lin, col) = oper
        in
            if (checkFn(exp1, exp2))
            then f(exp1, exp2)
            else (
                printError(fileName, "Expression type mismatch", lin, col);
                raise TypeMismatch
            )
        end

type unaryOpPayload = (Exp -> Exp) * int * int
val unaryOpParse : string * unaryOpPayload * (Exp -> bool) * Exp -> Exp  =
    fn (fileName, oper, checkFn, exp) =>
        let
            val (f, lin, col) = oper
        in
            if (checkFn(exp))
            then f(exp)
            else (
                printError(fileName, "Expression type mismatch", lin, col);
                raise TypeMismatch
            )
        end

structure VarTable : 
sig
    val find : Var -> bool option
    val insert : Var * bool -> unit
    val inDomain : Var -> bool
    val clear : unit -> unit
end 
=
struct
    val TableSize = 422 (* 211 *)
    val ht : (string, bool) HashTable.hash_table =
        HashTable.mkTable (HashString.hashString, op=) (TableSize, Fail "Variable not found")
    val insert    = HashTable.insert   ht
    val find      = HashTable.find     ht
    val inDomain  = HashTable.inDomain ht
    val clear     = fn () => HashTable.clear ht
end

val dupCheck : string * Var * int * int -> unit =
    fn (fileName, x, lin, col) =>
        if (VarTable.inDomain x)
        then (
            printError (fileName, "Variable " ^ x ^ " already exists", lin, col);
            raise InvalidVariable
        ) else ()


val isBool : Exp -> bool =
    fn x => case x of
          AND _     => true
        | OR  _     => true
        | NOT _     => true
        | LT  _     => true
        | LEQ _     => true
        | EQ  _     => true
        | GT  _     => true
        | GEQ _     => true
        | NEQ _     => true
        | BOOLVAL _ => true
        | VAR a     => valOf (VarTable.find a) (* look up in symbol table *)
        | _         => false

val isInt : Exp -> bool = fn x => not (isBool x)
val bothBool : Exp * Exp -> bool = fn (x, y) => (isBool x) andalso (isBool y)
val bothInt  : Exp * Exp -> bool = fn (x, y) => (isInt  x) andalso (isInt  y)
val bothEq   : Exp * Exp -> bool = fn (x, y) => (isBool x) = (isBool y)

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
    | IDENTIFIER  of string
    | ILLCH
    | INTCONST    of int
    | ADDOP
    | MULOP
    | RELOP
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | EOF

%nonterm begin          of Prog
       | block          of Blk
       | declarationseq of Dec list
       | declaration    of Dec list
       | declvarlist    of Var list
       | declvar        of Var
       | variable       of Var
       | commandseq     of Cmd list
       | commandseqend  of Cmd list
       | command        of Cmd
       | expression     of Exp
       | addop          of binOpPayload
       | mulop          of binOpPayload
       | relop          of binOpPayload
       | boolop         of binOpPayload

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
PROGRAM IDENTIFIER DOUBLECOLON block    ((VarTable.clear(); PROG(IDENTIFIER, block)))

block:
declarationseq commandseq               (BLK(declarationseq, commandseq))

declarationseq:
  declaration declarationseq            (declaration @ declarationseq)
|                                       ([]) 

declaration:
  VAR declvarlist COLON INT SEMICOLON       (map (fn x => (dupCheck (fileName, x, VARleft, VARright); VarTable.insert (x, false); INT x)) declvarlist)
| VAR declvarlist COLON BOOL SEMICOLON      (map (fn x => (dupCheck (fileName, x, VARleft, VARright); VarTable.insert (x, true); BOOL x)) declvarlist)

declvarlist:
  declvar COMMA declvarlist             (declvar::declvarlist)
| declvar                               ([declvar])

declvar: IDENTIFIER                     (IDENTIFIER)
variable: IDENTIFIER                    (if VarTable.inDomain(IDENTIFIER)
                                         then IDENTIFIER
                                         else (
                                            printError(fileName, IDENTIFIER ^ " not declared", IDENTIFIERleft, IDENTIFIERright);
                                            raise InvalidVariable
                                         ))

commandseq:
  LBRACE commandseqend                  (commandseqend)

commandseqend:
  command SEMICOLON commandseqend       (command::commandseqend)
| RBRACE                                ([])

command:
  variable ASSIGN expression            (if bothEq(VAR(variable), expression)
                                         then SET(variable, expression)
                                         else (
                                            printError(fileName, "Expression type mismatch", ASSIGNleft, ASSIGNright);
                                            raise TypeMismatch                           
                                         ))
| READ variable                         (READ(variable))
| WRITE expression                      (WRITE(expression))
| IF expression THEN commandseq ELSE commandseq ENDIF
                                        (if isBool(expression)
                                         then ITE(expression, commandseq1, commandseq2)
                                         else (
                                            printError(fileName, "Boolean expression expected", IFleft, IFright);
                                            raise TypeMismatch
                                         ))
| WHILE expression DO commandseq ENDWH  (if isBool(expression)
                                         then WH(expression, commandseq)
                                         else (
                                            printError(fileName, "Boolean expression expected", WHILEleft, WHILEright);
                                            raise TypeMismatch
                                         ))

expression:
  expression addop expression            %prec ADDOP    (binOpParse(fileName, addop, bothInt, expression1, expression2))
| expression mulop expression            %prec MULOP    (binOpParse(fileName, mulop, bothInt, expression1, expression2))
| NEGATIVE expression                    %prec NEGATIVE (unaryOpParse(fileName, (NEGATIVE, NEGATIVEleft, NEGATIVEright), isInt, expression))
| INTCONST                                              (INTVAL(INTCONST))
| expression AND expression              %prec AND      (binOpParse(fileName, (AND, ANDleft, ANDright), bothBool, expression1, expression2))
| expression OR expression               %prec OR       (binOpParse(fileName, (OR,  ORleft,  ORright ), bothBool, expression1, expression2))
| NOT expression                         %prec NOT      (unaryOpParse(fileName, (NOT, NOTleft, NOTright), isBool, expression))
| LPAREN expression RPAREN                              (expression)
| variable                                              (VAR(variable))
| TT                                                    (BOOLVAL(true))
| FF                                                    (BOOLVAL(false))
| expression relop expression            %prec RELOP    (binOpParse(fileName, relop, bothEq, expression1, expression2))

addop:
  PLUS  ((PLUS , PLUSleft , PLUSright))
| MINUS ((MINUS, MINUSleft, MINUSright))

mulop:
  TIMES ((TIMES, TIMESleft, TIMESright))
| DIV   ((DIV  , DIVleft  , DIVright))
| MOD   ((MOD  , MODleft  , MODright))

relop:
  LT    ((LT , LTleft , LTright))
| LEQ   ((LEQ, LEQleft, LEQright))
| EQ    ((EQ , EQleft , EQright))
| GT    ((GT , GTleft , GTright))
| GEQ   ((GEQ, GEQleft, GEQright))
| NEQ   ((NEQ, NEQleft, NEQright))

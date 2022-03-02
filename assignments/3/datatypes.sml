structure AST =
struct
  type Var = string
  datatype
      Prog   = PROG of string * Blk
  and Blk    = BLK of (Dec list) * (Cmd list)
  and Dec    = INT     of Var
             | BOOL    of Var
  and Cmd    = SET     of Var * Exp
             | READ    of Var
             | WRITE   of Exp
             | ITE     of Exp * (Cmd list) * (Cmd list)
             | WH      of Exp * (Cmd list)
  and Exp    = (* boolean operations *)
               AND     of Exp * Exp
             | OR      of Exp * Exp
             | NOT     of Exp
             (* relational operations *)
             | LT      of Exp * Exp
             | LEQ     of Exp * Exp
             | EQ      of Exp * Exp
             | GT      of Exp * Exp
             | GEQ     of Exp * Exp
             | NEQ     of Exp * Exp
             (* integer operations *)
             | PLUS    of Exp * Exp
             | MINUS   of Exp * Exp
             | TIMES   of Exp * Exp
             | DIV     of Exp * Exp
             | MOD     of Exp * Exp
             (* terminals *)
             | INTVAR  of string
             | INTVAL  of Var
             | BOOLVAR of string
             | BOOLVAL of Var
end;

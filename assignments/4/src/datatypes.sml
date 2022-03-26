structure AST =
struct
  type Var = string
  datatype
      Prog   = PROG     of string * Blk
  and Blk    = BLK      of (Dec list) * (Cmd list)
  and Dec    = INT      of Var
             | BOOL     of Var
  and Cmd    = SET      of Var * Exp
             | READ     of Var
             | WRITE    of Exp
             | ITE      of Exp * (Cmd list) * (Cmd list)
             | WH       of Exp * (Cmd list)
  and Exp    = (* boolean operators *)
               AND      of Exp * Exp
             | OR       of Exp * Exp
             | NOT      of Exp
             (* relational operators *)
             | LT       of Exp * Exp
             | LEQ      of Exp * Exp
             | EQ       of Exp * Exp
             | GT       of Exp * Exp
             | GEQ      of Exp * Exp
             | NEQ      of Exp * Exp
             (* integer operators *)
             | PLUS     of Exp * Exp
             | MINUS    of Exp * Exp
             | TIMES    of Exp * Exp
             | DIV      of Exp * Exp
             | MOD      of Exp * Exp
             | NEGATIVE of Exp
             (* terminals *)
             | VAR      of Var
             | INTVAL   of int
             | BOOLVAL  of bool
end

structure StackElement =
struct
  datatype StackElement = VAL of int    | VAR of int
                        | SET   | READ  | WRITE | ITE | WH  | SEQ
                        | AND   | OR    | NOT
                        | LT    | LEQ   | EQ    | GT  | GEQ | NEQ
                        | PLUS  | MINUS | TIMES | DIV | MOD | NEGATIVE
                        | EMPTY | EOS

  fun toString (VAL x) = "VAL " ^ (Int.toString x)
    | toString (VAR x) = "VAR " ^ (Int.toString x)
    | toString z =
        case z of
             SET   => "SET"   | READ   => "READ"  | WRITE  => "WRITE" | ITE  => "ITE"
           | WH    => "WH"    | SEQ    => "SEQ"   | AND    => "AND"   | OR   => "OR"
           | NOT   => "NOT"   | LT     => "LT"    | LEQ    => "LEQ"   | EQ   => "EQ"
           | GT    => "GT"    | GEQ    => "GEQ"   | NEQ    => "NEQ"   | PLUS => "PLUS"
           | MINUS => "MINUS" | TIMES  => "TIMES" | DIV    => "DIV"   | MOD  => "MOD"
           | EMPTY => "EMPTY" | EOS    => "EOS"   | NEGATIVE => "NEGATIVE"
           | _ => ""
end

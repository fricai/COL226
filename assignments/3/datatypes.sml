(* datatypes.sml
   Data structures used in User's Guide to ML-Lex and ML-Yacc
   Copyright (C) 2004 Roger Price

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, 
   Boston, MA  02111-1307, USA.
 *)

signature AST =
sig datatype
      Prog   = PROG of string * Blk
  and BoolOp = NOT | AND | OR
  and RelOp  = LT | LEQ | EQ | GT | GEQ | NEQ
  and IntOp  = PLUS | MINUS | TIMES | DIV | MOD
  and Blk    = BLK of (Dec list) * (Cmd list)
  and Dec    = INT     of Var
             | BOOL    of Var
  and Cmd    = SET     of Var * Exp
             | READ    of Var
             | WRITE   of Exp
             | ITE     of Exp * (Cmd list) * (Cmd list)
             | WH      of Exp * (Cmd list)
  and Var    = string
  and Exp    = BOOLOP  of Exp * BoolOp * Exp
             | INTOP   of Exp * IntOp  * Exp
             | RELOP   of Exp * RelOp  * Exp
             | INTVAR  of Var
             | INTVAL  of int
             | BOOLVAR of Var
             | BOOLVAL of bool
end;


(*
signature DATATYPES = 
sig datatype A     = A of Pat * Proc
    and      Pi    = Pi of Proc list
    and      Pat   = Pat of Path
    and      Path  = Name of string * string * int * int
    and      Proc  = New of Path * Proc
                   | Output of Path * V
                   | Input of Path * A
                   | Parallel of Proc list
    and      V     = V of Path

end;

structure DataTypes : DATATYPES =
struct
  datatype  A     = A of Pat*Proc
  and       Pi    = Pi of Proc list
  and       Pat   = Pat of Path
  and       Path  = Name of string * string * int * int
  and       Proc  = New of Path * Proc
                  | Output of Path * V 
                  | Input of Path * A
                  | Parallel of Proc list
  and       V     = V of Path
end;
 *)

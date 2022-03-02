(* pi.yacc
   Grammar used in User's Guide to ML-Lex and ML-Yacc
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

open DataTypes
%%
%name Pi
%term CARET
    | DVBAR
    | EOF
    | EQUALS
    | IDE        of string
    | ILLCH
    | INPUT
    | LPAR
    | NEW
    | OUTPUT
    | RPAR
%nonterm abs          of A
       | begin        of Pi
       | procList     of Proc list
       | parallel     of Proc list
       | pat          of Pat
       | path         of Path
       | pi           of Proc list
       | proc         of Proc
       | value        of V
%pos int
%eop EOF
%noshift EOF
%nonassoc DVBAR EOF EQUALS ILLCH INPUT LPAR NEW OUTPUT RPAR
%nodefault
%verbose
%keyword NEW
%arg (fileName) : string

%%
begin: procList         ((Pi procList))

abs: pat EQUALS proc    ((A (pat,proc)))

procList: proc procList ((proc::procList))
|                       ([])

parallel: 
  proc DVBAR parallel   ((proc::parallel))
| proc RPAR             ([proc])
| RPAR                  ([])

pat: path               ((Pat path))
path: IDE               ((Name (IDE,fileName,IDEleft,IDEright)))

proc:
  NEW path proc         ((New (path,proc)))
| path OUTPUT value     ((Output (path,value)))
| path INPUT abs        ((Input (path,abs)))
| LPAR parallel         ((Parallel parallel))

value: path             ((V path))

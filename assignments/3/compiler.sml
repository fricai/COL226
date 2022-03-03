(* compiler.sml
   Toy compiler used in User's Guide to ML-Lex and ML-Yacc
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

structure While:
sig val compile : string -> While.Pi
end =
struct
exception PiError;

fun compile (fileName) = 
    let val inStream =  TextIO.openIn fileName;
        val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream 
                 then ""
                 else TextIO.inputN (inStream,n);

        val printError : string * int * int -> unit = fn 
            (msg,line,col) =>
             print (fileName^"["^Int.toString line^":"
                                ^Int.toString col^"] "^msg^"\n");

        val _ = Compiler.Control.Print.printDepth:=12;
        val (tree,rem) = PiParser.parse 
                     (15,
                     (PiParser.makeLexer grab fileName),
                     printError,
                     fileName)
            handle PiParser.ParseError => raise PiError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in tree
    end
end; (* of structure Compile *)

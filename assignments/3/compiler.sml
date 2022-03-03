structure While :
sig val compile : string -> AST.Prog
end =
struct
exception WhileError;

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
        val (tree,rem) = WhileParser.parse 
                     (15,
                     (WhileParser.makeLexer grab fileName),
                     printError,
                     fileName)
            handle WhileParser.ParseError => raise WhileError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in tree
    end
end; (* of structure Compile *)

structure T = Tokens;

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn 
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["^Int.toString line
                 ^"."^Int.toString col^"] Invalid character \""
                 ^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

structure KeyWord : 
sig
    val find : string -> (int * int -> (svalue,int) token) option
end 
=
struct
 val TableSize =  422 (* 211 *)
 val HashFactor = 5

 val hash = fn 
     s => List.foldr (fn (c,v) => (v * HashFactor + (ord c)) mod TableSize) 0 (explode s)

val HashTable = Array.array(TableSize,nil) :
       (string * (int * int -> (svalue, int) token)) list Array.array

 val add = fn 
     (s,v) => let val i = hash s
              in Array.update(HashTable, i, (s,v) 
                 :: (Array.sub(HashTable, i)))
              end

 val find = fn 
     s => let val i = hash s
              fun f ((key,v)::r) = if s=key then SOME v else f r
                | f nil = NONE
          in  f (Array.sub(HashTable, i))
          end

 val _ = (List.app add [
      ("program", T.PROGRAM),
      ("var",     T.VAR),
      ("int",     T.INT),
      ("bool",    T.BOOL),
      ("read",    T.READ),
      ("write",   T.WRITE),
      ("if",      T.IF),
      ("then",    T.THEN),
      ("else",    T.ELSE),
      ("endif",   T.ENDIF),
      ("while",   T.WHILE),
      ("do",      T.DO),
      ("endwh",   T.ENDWH),
      ("tt",      T.TT),
      ("ff",      T.FF),
      ("!",       T.NOT),
      ("&&",      T.AND),
      ("||",      T.OR),
      ("<",       T.LT),
      ("<=",      T.LEQ),
      ("=",       T.EQ),
      (">",       T.GT),
      (">=",      T.GEQ),
      ("<>",      T.NEQ),
      ("+",       T.PLUS),
      ("-",       T.MINUS),
      ("~",       T.NEGATIVE),
      ("*",       T.TIMES),
      ("/",       T.DIV),
      ("%",       T.MOD),
      (":=",      T.ASSIGN)
      (";",       T.SEMICOLON),
      ("::",      T.DOUBLECOLON),
      (":",       T.COLON),
      ("(",       T.LPAREN),
      (")",       T.RPAREN),
      ("{",       T.LBRACE),
      ("}",       T.RBRACE)
     ])
  end
open KeyWord

%%
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName: string);
alpha         = [A-Za-z];
digit         = [0-9];
alphanum      = ({alpha}|{digit});
ws            = [\ \t];
eol           = ("\013\010"|"\010"|"\013");

%%
{ws}*      => (continue());
{eol}      => (lin := (!lin) + 1; eolpos := yypos + size yytext; continue());
{digit}+ => (
                     col := yypos - (!eolpos);
                     T.INTCONST(valOf(Int.fromString yytext), !lin, !col)
              );
{alpha}{alphanum}*   => (
                     col := yypos - (!eolpos);
		     case find yytext of
		          SOME key => key(!lin, !col) (* for keywords *)
			  | _      => T.IDENTIFIER(yytext, !lin, !col) (* is a variable name *)
	      );
.           => (
		col := yypos - (!eolpos);
		case find yytext of
		     SOME op => op(!lin, !col)      (* for operators *)
		     | _      => (badCh(fileName, yytex, !lin, !col); T.ILLCH(!lin, !col))
              );

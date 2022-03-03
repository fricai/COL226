structure T = Tokens;

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badChar : string * string * int * int -> unit = fn 
    (fileName, bad, line, col) =>
    TextIO.output(TextIO.stdOut,fileName ^ "[" ^ Int.toString line
                  ^ "." ^ Int.toString col ^ "] Invalid character \""
                  ^ bad ^ "\"\n");
val overflowError : string * string * int * int -> unit = fn 
    (fileName, bad, line, col) =>
    TextIO.output(TextIO.stdOut,fileName ^ "[" ^ Int.toString line
                  ^ "." ^ Int.toString col ^ "] Integer overflows \""
                  ^ bad ^ "\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

structure KeyWord : 
sig
    val find : string -> (int * int -> (svalue,int) token) option
end 
=
struct
 val keywords = [
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
      ("ff",      T.FF)
 ]

 val find = fn s =>
                 case List.find (fn (x, v) => x = s) keywords of
		        SOME (_, v) => SOME v
		      | NONE        => NONE
end

%%
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName: string);
alpha         = [A-Za-z];
digit         = [0-9];
alphanum      = ({alpha}|{digit});
ws            = [\ \t];
eol           = ("\013\010"|"\010"|"\013");

%%
{ws}*          => (continue());
{eol}          => (lin := (!lin) + 1; eolpos := yypos + size yytext; continue());
{digit}+       => (
                     col := yypos - (!eolpos);
                     T.INTCONST(valOf(
		                Int.fromString yytext
				handle Overflow => (
					overflowError(fileName, yytext, !lin, !col);
					raise Overflow)
				),
				!lin, !col)
              );
{alpha}{alphanum}* => (
                     col := yypos - (!eolpos);
		     case KeyWord.find yytext of
		            SOME key => key(!lin, !col) (* for keywords *)
			  | _      => T.IDENTIFIER(yytext, !lin, !col) (* is a variable name *)
	      );
":"  => (col := yypos - (!eolpos); T.COLON(!lin, !col));
"::" => (col := yypos - (!eolpos); T.DOUBLECOLON(!lin, !col));
"!"  => (col := yypos - (!eolpos); T.NOT(!lin, !col));
"&&" => (col := yypos - (!eolpos); T.AND(!lin, !col));
"||" => (col := yypos - (!eolpos); T.OR(!lin, !col));
"<"  => (col := yypos - (!eolpos); T.LT(!lin, !col));
"<=" => (col := yypos - (!eolpos); T.LEQ(!lin, !col));
"="  => (col := yypos - (!eolpos); T.EQ(!lin, !col));
">"  => (col := yypos - (!eolpos); T.GT(!lin, !col));
">=" => (col := yypos - (!eolpos); T.GEQ(!lin, !col));
"<>" => (col := yypos - (!eolpos); T.NEQ(!lin, !col));
"+"  => (col := yypos - (!eolpos); T.PLUS(!lin, !col));
"-"  => (col := yypos - (!eolpos); T.MINUS(!lin, !col));
"~"  => (col := yypos - (!eolpos); T.NEGATIVE(!lin, !col));
"*"  => (col := yypos - (!eolpos); T.TIMES(!lin, !col));
"/"  => (col := yypos - (!eolpos); T.DIV(!lin, !col));
"%"  => (col := yypos - (!eolpos); T.MOD(!lin, !col));
":=" => (col := yypos - (!eolpos); T.ASSIGN(!lin, !col));
";"  => (col := yypos - (!eolpos); T.SEMICOLON(!lin, !col));
"("  => (col := yypos - (!eolpos); T.LPAREN(!lin, !col));
")"  => (col := yypos - (!eolpos); T.RPAREN(!lin, !col));
"{"  => (col := yypos - (!eolpos); T.LBRACE(!lin, !col));
"}"  => (col := yypos - (!eolpos); T.RBRACE(!lin, !col));
","  => (col := yypos - (!eolpos); T.COMMA(!lin, !col));
.    => (badChar(fileName, yytext, !lin, !col); T.ILLCH(!lin, !col));


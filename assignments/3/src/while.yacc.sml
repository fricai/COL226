functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\003\000\000\000\
\\001\000\003\000\042\000\004\000\041\000\000\000\
\\001\000\005\000\024\000\006\000\023\000\007\000\022\000\011\000\021\000\
\\036\000\020\000\045\000\019\000\000\000\
\\001\000\008\000\110\000\012\000\110\000\017\000\110\000\018\000\110\000\
\\025\000\052\000\026\000\051\000\028\000\050\000\029\000\049\000\
\\030\000\048\000\033\000\110\000\043\000\110\000\000\000\
\\001\000\008\000\065\000\017\000\060\000\018\000\059\000\019\000\058\000\
\\020\000\057\000\021\000\056\000\022\000\055\000\023\000\054\000\
\\024\000\053\000\025\000\052\000\026\000\051\000\028\000\050\000\
\\029\000\049\000\030\000\048\000\000\000\
\\001\000\009\000\077\000\000\000\
\\001\000\010\000\079\000\000\000\
\\001\000\012\000\061\000\017\000\060\000\018\000\059\000\019\000\058\000\
\\020\000\057\000\021\000\056\000\022\000\055\000\023\000\054\000\
\\024\000\053\000\025\000\052\000\026\000\051\000\028\000\050\000\
\\029\000\049\000\030\000\048\000\000\000\
\\001\000\013\000\076\000\000\000\
\\001\000\014\000\036\000\015\000\035\000\016\000\034\000\027\000\033\000\
\\036\000\020\000\038\000\032\000\042\000\031\000\000\000\
\\001\000\017\000\060\000\018\000\059\000\019\000\058\000\020\000\057\000\
\\021\000\056\000\022\000\055\000\023\000\054\000\024\000\053\000\
\\025\000\052\000\026\000\051\000\028\000\050\000\029\000\049\000\
\\030\000\048\000\043\000\074\000\000\000\
\\001\000\031\000\028\000\000\000\
\\001\000\033\000\027\000\000\000\
\\001\000\033\000\066\000\000\000\
\\001\000\033\000\067\000\000\000\
\\001\000\034\000\005\000\000\000\
\\001\000\035\000\026\000\000\000\
\\001\000\036\000\004\000\000\000\
\\001\000\036\000\015\000\000\000\
\\001\000\036\000\020\000\000\000\
\\001\000\044\000\012\000\000\000\
\\001\000\046\000\000\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\002\000\009\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\032\000\025\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\017\000\060\000\018\000\059\000\019\000\058\000\020\000\057\000\
\\021\000\056\000\022\000\055\000\023\000\054\000\024\000\053\000\
\\025\000\052\000\026\000\051\000\028\000\050\000\029\000\049\000\
\\030\000\048\000\000\000\
\\095\000\000\000\
\\096\000\017\000\060\000\018\000\059\000\019\000\058\000\020\000\057\000\
\\021\000\056\000\022\000\055\000\023\000\054\000\024\000\053\000\
\\025\000\052\000\026\000\051\000\028\000\050\000\029\000\049\000\
\\030\000\048\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\028\000\050\000\029\000\049\000\030\000\048\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\019\000\058\000\020\000\057\000\021\000\056\000\022\000\055\000\
\\023\000\054\000\024\000\053\000\025\000\052\000\026\000\051\000\
\\028\000\050\000\029\000\049\000\030\000\048\000\000\000\
\\104\000\017\000\060\000\019\000\058\000\020\000\057\000\021\000\056\000\
\\022\000\055\000\023\000\054\000\024\000\053\000\025\000\052\000\
\\026\000\051\000\028\000\050\000\029\000\049\000\030\000\048\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\"
val actionRowNumbers =
"\000\000\017\000\015\000\025\000\
\\025\000\020\000\022\000\018\000\
\\024\000\023\000\002\000\029\000\
\\016\000\030\000\012\000\032\000\
\\011\000\034\000\031\000\009\000\
\\009\000\009\000\019\000\018\000\
\\001\000\002\000\009\000\007\000\
\\048\000\009\000\043\000\009\000\
\\009\000\050\000\049\000\004\000\
\\037\000\036\000\028\000\013\000\
\\014\000\033\000\035\000\009\000\
\\009\000\009\000\055\000\054\000\
\\053\000\052\000\051\000\060\000\
\\059\000\061\000\058\000\057\000\
\\056\000\009\000\009\000\020\000\
\\010\000\042\000\046\000\020\000\
\\027\000\026\000\003\000\041\000\
\\040\000\045\000\044\000\008\000\
\\047\000\005\000\039\000\020\000\
\\006\000\038\000\021\000"
val gotoT =
"\
\\001\000\078\000\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\005\000\004\000\004\000\000\000\
\\003\000\008\000\004\000\004\000\000\000\
\\008\000\009\000\000\000\
\\000\000\
\\005\000\012\000\006\000\011\000\000\000\
\\000\000\
\\000\000\
\\007\000\016\000\009\000\015\000\010\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\028\000\011\000\027\000\000\000\
\\007\000\028\000\011\000\035\000\000\000\
\\007\000\028\000\011\000\036\000\000\000\
\\007\000\037\000\000\000\
\\005\000\038\000\006\000\011\000\000\000\
\\000\000\
\\007\000\016\000\009\000\041\000\010\000\014\000\000\000\
\\007\000\028\000\011\000\042\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\000\000\
\\007\000\028\000\011\000\060\000\000\000\
\\000\000\
\\007\000\028\000\011\000\061\000\000\000\
\\007\000\028\000\011\000\062\000\000\000\
\\000\000\
\\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\007\000\028\000\011\000\066\000\000\000\
\\007\000\028\000\011\000\067\000\000\000\
\\007\000\028\000\011\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\028\000\011\000\069\000\000\000\
\\007\000\028\000\011\000\070\000\000\000\
\\008\000\071\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\008\000\073\000\000\000\
\\000\000\
\\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\012\000\045\000\013\000\044\000\014\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 79
val numrules = 41
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INTCONST of unit ->  (int) | IDENTIFIER of unit ->  (string)
 | boolop of unit ->  (binOpPayload)
 | relop of unit ->  (binOpPayload) | mulop of unit ->  (binOpPayload)
 | addop of unit ->  (binOpPayload) | expression of unit ->  (Exp)
 | command of unit ->  (Cmd) | commandseqend of unit ->  (Cmd list)
 | commandseq of unit ->  (Cmd list) | variable of unit ->  (Var)
 | declvar of unit ->  (Var) | declvarlist of unit ->  (Var list)
 | declaration of unit ->  (Dec list)
 | declarationseq of unit ->  (Dec list) | block of unit ->  (Blk)
 | begin of unit ->  (Prog)
end
type svalue = MlyValue.svalue
type result = Prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 0) => true | (T 1) => true | (T 2) => true | (T 3) => true | (T 
4) => true | (T 5) => true | (T 6) => true | (T 7) => true | (T 8)
 => true | (T 9) => true | (T 10) => true | (T 11) => true | (T 12)
 => true | (T 13) => true | (T 14) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 45) => true | _ => false
val showTerminal =
fn (T 0) => "PROGRAM"
  | (T 1) => "VAR"
  | (T 2) => "INT"
  | (T 3) => "BOOL"
  | (T 4) => "READ"
  | (T 5) => "WRITE"
  | (T 6) => "IF"
  | (T 7) => "THEN"
  | (T 8) => "ELSE"
  | (T 9) => "ENDIF"
  | (T 10) => "WHILE"
  | (T 11) => "DO"
  | (T 12) => "ENDWH"
  | (T 13) => "TT"
  | (T 14) => "FF"
  | (T 15) => "NOT"
  | (T 16) => "AND"
  | (T 17) => "OR"
  | (T 18) => "LT"
  | (T 19) => "LEQ"
  | (T 20) => "EQ"
  | (T 21) => "NEQ"
  | (T 22) => "GT"
  | (T 23) => "GEQ"
  | (T 24) => "PLUS"
  | (T 25) => "MINUS"
  | (T 26) => "NEGATIVE"
  | (T 27) => "TIMES"
  | (T 28) => "DIV"
  | (T 29) => "MOD"
  | (T 30) => "ASSIGN"
  | (T 31) => "COMMA"
  | (T 32) => "SEMICOLON"
  | (T 33) => "DOUBLECOLON"
  | (T 34) => "COLON"
  | (T 35) => "IDENTIFIER"
  | (T 36) => "ILLCH"
  | (T 37) => "INTCONST"
  | (T 38) => "ADDOP"
  | (T 39) => "MULOP"
  | (T 40) => "RELOP"
  | (T 41) => "LPAREN"
  | (T 42) => "RPAREN"
  | (T 43) => "LBRACE"
  | (T 44) => "RBRACE"
  | (T 45) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 36) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, _, block1right)) :: _ :: ( _,
 ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROGRAM1left,
 _)) :: rest671)) => let val  result = MlyValue.begin (fn _ => let
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (block as block1) = block1 ()
 in ((VarTable.clear(); PROG(IDENTIFIER, block)))
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.commandseq commandseq1, _, commandseq1right)
) :: ( _, ( MlyValue.declarationseq declarationseq1, 
declarationseq1left, _)) :: rest671)) => let val  result = 
MlyValue.block (fn _ => let val  (declarationseq as declarationseq1) =
 declarationseq1 ()
 val  (commandseq as commandseq1) = commandseq1 ()
 in (BLK(declarationseq, commandseq))
end)
 in ( LrTable.NT 1, ( result, declarationseq1left, commandseq1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.declarationseq declarationseq1, _, 
declarationseq1right)) :: ( _, ( MlyValue.declaration declaration1, 
declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.declarationseq (fn _ => let val  (declaration as declaration1
) = declaration1 ()
 val  (declarationseq as declarationseq1) = declarationseq1 ()
 in (declaration @ declarationseq)
end)
 in ( LrTable.NT 2, ( result, declaration1left, declarationseq1right),
 rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.declarationseq (fn _
 => ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.declvarlist declvarlist1, _, _)) :: ( _, ( _, (VARleft as 
VAR1left), VARright)) :: rest671)) => let val  result = 
MlyValue.declaration (fn _ => let val  (declvarlist as declvarlist1) =
 declvarlist1 ()
 in (
map (fn x => (dupCheck (fileName, x, VARleft, VARright); VarTable.insert (x, false); INT x)) declvarlist
)
end)
 in ( LrTable.NT 3, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 5, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.declvarlist declvarlist1, _, _)) :: ( _, ( _, (VARleft as 
VAR1left), VARright)) :: rest671)) => let val  result = 
MlyValue.declaration (fn _ => let val  (declvarlist as declvarlist1) =
 declvarlist1 ()
 in (
map (fn x => (dupCheck (fileName, x, VARleft, VARright); VarTable.insert (x, true); BOOL x)) declvarlist
)
end)
 in ( LrTable.NT 3, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.declvarlist declvarlist1, _, 
declvarlist1right)) :: _ :: ( _, ( MlyValue.declvar declvar1, 
declvar1left, _)) :: rest671)) => let val  result = 
MlyValue.declvarlist (fn _ => let val  (declvar as declvar1) = 
declvar1 ()
 val  (declvarlist as declvarlist1) = declvarlist1 ()
 in (declvar::declvarlist)
end)
 in ( LrTable.NT 4, ( result, declvar1left, declvarlist1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.declvar declvar1, declvar1left, 
declvar1right)) :: rest671)) => let val  result = MlyValue.declvarlist
 (fn _ => let val  (declvar as declvar1) = declvar1 ()
 in ([declvar])
end)
 in ( LrTable.NT 4, ( result, declvar1left, declvar1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.declvar
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (IDENTIFIER)
end)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, (IDENTIFIERleft as 
IDENTIFIER1left), (IDENTIFIERright as IDENTIFIER1right))) :: rest671))
 => let val  result = MlyValue.variable (fn _ => let val  (IDENTIFIER
 as IDENTIFIER1) = IDENTIFIER1 ()
 in (
if VarTable.inDomain(IDENTIFIER)
                                         then IDENTIFIER
                                         else (
                                            printError(fileName, IDENTIFIER ^ " not declared", IDENTIFIERleft, IDENTIFIERright);
                                            raise InvalidVariable
                                         )
)
end)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.commandseqend commandseqend1, _, 
commandseqend1right)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) =>
 let val  result = MlyValue.commandseq (fn _ => let val  (
commandseqend as commandseqend1) = commandseqend1 ()
 in (commandseqend)
end)
 in ( LrTable.NT 7, ( result, LBRACE1left, commandseqend1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.commandseqend commandseqend1, _, 
commandseqend1right)) :: _ :: ( _, ( MlyValue.command command1, 
command1left, _)) :: rest671)) => let val  result = 
MlyValue.commandseqend (fn _ => let val  (command as command1) = 
command1 ()
 val  (commandseqend as commandseqend1) = commandseqend1 ()
 in (command::commandseqend)
end)
 in ( LrTable.NT 8, ( result, command1left, commandseqend1right), 
rest671)
end
|  ( 12, ( ( _, ( _, RBRACE1left, RBRACE1right)) :: rest671)) => let
 val  result = MlyValue.commandseqend (fn _ => ([]))
 in ( LrTable.NT 8, ( result, RBRACE1left, RBRACE1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, ASSIGNleft, ASSIGNright)) :: ( _, ( MlyValue.variable 
variable1, variable1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (variable as variable1) = variable1
 ()
 val  (expression as expression1) = expression1 ()
 in (
if bothEq(VAR(variable), expression)
                                         then SET(variable, expression)
                                         else (
                                            printError(fileName, "Expression type mismatch", ASSIGNleft, ASSIGNright);
                                            raise TypeMismatch                           
                                         )
)
end)
 in ( LrTable.NT 9, ( result, variable1left, expression1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.variable variable1, _, variable1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (variable as variable1) = variable1
 ()
 in (READ(variable))
end)
 in ( LrTable.NT 9, ( result, READ1left, variable1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (WRITE(expression))
end)
 in ( LrTable.NT 9, ( result, WRITE1left, expression1right), rest671)

end
|  ( 16, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.commandseq 
commandseq2, _, _)) :: _ :: ( _, ( MlyValue.commandseq commandseq1, _,
 _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _)) :: ( _, (
 _, (IFleft as IF1left), IFright)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  commandseq1 = commandseq1 ()
 val  commandseq2 = commandseq2 ()
 in (
if isBool(expression)
                                         then ITE(expression, commandseq1, commandseq2)
                                         else (
                                            printError(fileName, "Boolean expression expected", IFleft, IFright);
                                            raise TypeMismatch
                                         )
)
end)
 in ( LrTable.NT 9, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 17, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.commandseq 
commandseq1, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _,
 _)) :: ( _, ( _, (WHILEleft as WHILE1left), WHILEright)) :: rest671))
 => let val  result = MlyValue.command (fn _ => let val  (expression
 as expression1) = expression1 ()
 val  (commandseq as commandseq1) = commandseq1 ()
 in (
if isBool(expression)
                                         then WH(expression, commandseq)
                                         else (
                                            printError(fileName, "Boolean expression expected", WHILEleft, WHILEright);
                                            raise TypeMismatch
                                         )
)
end)
 in ( LrTable.NT 9, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: ( _, ( MlyValue.addop addop1, _, _)) :: ( _, ( 
MlyValue.expression expression1, expression1left, _)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  expression1 =
 expression1 ()
 val  (addop as addop1) = addop1 ()
 val  expression2 = expression2 ()
 in (binOpParse(fileName, addop, bothInt, expression1, expression2))

end)
 in ( LrTable.NT 10, ( result, expression1left, expression2right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: ( _, ( MlyValue.mulop mulop1, _, _)) :: ( _, ( 
MlyValue.expression expression1, expression1left, _)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  expression1 =
 expression1 ()
 val  (mulop as mulop1) = mulop1 ()
 val  expression2 = expression2 ()
 in (binOpParse(fileName, mulop, bothInt, expression1, expression2))

end)
 in ( LrTable.NT 10, ( result, expression1left, expression2right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, (NEGATIVEleft as NEGATIVE1left), NEGATIVEright)) :: 
rest671)) => let val  result = MlyValue.expression (fn _ => let val  (
expression as expression1) = expression1 ()
 in (
unaryOpParse(fileName, (NEGATIVE, NEGATIVEleft, NEGATIVEright), isInt, expression)
)
end)
 in ( LrTable.NT 10, ( result, NEGATIVE1left, expression1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.INTCONST INTCONST1, INTCONST1left, 
INTCONST1right)) :: rest671)) => let val  result = MlyValue.expression
 (fn _ => let val  (INTCONST as INTCONST1) = INTCONST1 ()
 in (INTVAL(INTCONST))
end)
 in ( LrTable.NT 10, ( result, INTCONST1left, INTCONST1right), rest671
)
end
|  ( 22, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: ( _, ( _, ANDleft, ANDright)) :: ( _, ( MlyValue.expression 
expression1, expression1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
binOpParse(fileName, (AND, ANDleft, ANDright), bothBool, expression1, expression2)
)
end)
 in ( LrTable.NT 10, ( result, expression1left, expression2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: ( _, ( _, ORleft, ORright)) :: ( _, ( MlyValue.expression 
expression1, expression1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
binOpParse(fileName, (OR,  ORleft,  ORright ), bothBool, expression1, expression2)
)
end)
 in ( LrTable.NT 10, ( result, expression1left, expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, (NOTleft as NOT1left), NOTright)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (
unaryOpParse(fileName, (NOT, NOTleft, NOTright), isBool, expression))

end)
 in ( LrTable.NT 10, ( result, NOT1left, expression1right), rest671)

end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (expression)
end)
 in ( LrTable.NT 10, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.variable variable1, variable1left, 
variable1right)) :: rest671)) => let val  result = MlyValue.expression
 (fn _ => let val  (variable as variable1) = variable1 ()
 in (VAR(variable))
end)
 in ( LrTable.NT 10, ( result, variable1left, variable1right), rest671
)
end
|  ( 27, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => (BOOLVAL(true)))
 in ( LrTable.NT 10, ( result, TT1left, TT1right), rest671)
end
|  ( 28, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => (BOOLVAL(false)))
 in ( LrTable.NT 10, ( result, FF1left, FF1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: ( _, ( MlyValue.relop relop1, _, _)) :: ( _, ( 
MlyValue.expression expression1, expression1left, _)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  expression1 =
 expression1 ()
 val  (relop as relop1) = relop1 ()
 val  expression2 = expression2 ()
 in (binOpParse(fileName, relop, bothEq, expression1, expression2))

end)
 in ( LrTable.NT 10, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( _, (PLUSleft as PLUS1left), (PLUSright as PLUS1right
))) :: rest671)) => let val  result = MlyValue.addop (fn _ => (
(PLUS , PLUSleft , PLUSright)))
 in ( LrTable.NT 11, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 31, ( ( _, ( _, (MINUSleft as MINUS1left), (MINUSright as 
MINUS1right))) :: rest671)) => let val  result = MlyValue.addop (fn _
 => ((MINUS, MINUSleft, MINUSright)))
 in ( LrTable.NT 11, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 32, ( ( _, ( _, (TIMESleft as TIMES1left), (TIMESright as 
TIMES1right))) :: rest671)) => let val  result = MlyValue.mulop (fn _
 => ((TIMES, TIMESleft, TIMESright)))
 in ( LrTable.NT 12, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 33, ( ( _, ( _, (DIVleft as DIV1left), (DIVright as DIV1right)))
 :: rest671)) => let val  result = MlyValue.mulop (fn _ => (
(DIV  , DIVleft  , DIVright)))
 in ( LrTable.NT 12, ( result, DIV1left, DIV1right), rest671)
end
|  ( 34, ( ( _, ( _, (MODleft as MOD1left), (MODright as MOD1right)))
 :: rest671)) => let val  result = MlyValue.mulop (fn _ => (
(MOD  , MODleft  , MODright)))
 in ( LrTable.NT 12, ( result, MOD1left, MOD1right), rest671)
end
|  ( 35, ( ( _, ( _, (LTleft as LT1left), (LTright as LT1right))) :: 
rest671)) => let val  result = MlyValue.relop (fn _ => (
(LT , LTleft , LTright)))
 in ( LrTable.NT 13, ( result, LT1left, LT1right), rest671)
end
|  ( 36, ( ( _, ( _, (LEQleft as LEQ1left), (LEQright as LEQ1right)))
 :: rest671)) => let val  result = MlyValue.relop (fn _ => (
(LEQ, LEQleft, LEQright)))
 in ( LrTable.NT 13, ( result, LEQ1left, LEQ1right), rest671)
end
|  ( 37, ( ( _, ( _, (EQleft as EQ1left), (EQright as EQ1right))) :: 
rest671)) => let val  result = MlyValue.relop (fn _ => (
(EQ , EQleft , EQright)))
 in ( LrTable.NT 13, ( result, EQ1left, EQ1right), rest671)
end
|  ( 38, ( ( _, ( _, (GTleft as GT1left), (GTright as GT1right))) :: 
rest671)) => let val  result = MlyValue.relop (fn _ => (
(GT , GTleft , GTright)))
 in ( LrTable.NT 13, ( result, GT1left, GT1right), rest671)
end
|  ( 39, ( ( _, ( _, (GEQleft as GEQ1left), (GEQright as GEQ1right)))
 :: rest671)) => let val  result = MlyValue.relop (fn _ => (
(GEQ, GEQleft, GEQright)))
 in ( LrTable.NT 13, ( result, GEQ1left, GEQ1right), rest671)
end
|  ( 40, ( ( _, ( _, (NEQleft as NEQ1left), (NEQright as NEQ1right)))
 :: rest671)) => let val  result = MlyValue.relop (fn _ => (
(NEQ, NEQleft, NEQright)))
 in ( LrTable.NT 13, ( result, NEQ1left, NEQ1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.begin x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLECOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun INTCONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.INTCONST (fn () => i),p1,p2))
fun ADDOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun MULOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun RELOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end

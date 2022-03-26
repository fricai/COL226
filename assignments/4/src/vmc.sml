(*
signature VMC =
sig
  type states
  val init : AST.Prog -> states
  val toString : states -> (string list) * (string list) * (string list)
  val rules : states -> states
end
 *)

structure Vmc =
struct
  open StackElement
  val err = FunStack.Error

  type states = (StackElement FunStack.Stack) * (int Array.array)
              * (StackElement FunStack.Stack)

  fun init ast =
    let val AST.PROG (_, AST.BLK(decl, _)) = ast
    in
      (FunStack.create(), Array.array(length decl, 0), PostFix.postfix ast)
    end

  fun toString (V, M, C) = (
      FunStack.stack2list (FunStack.map StackElement.toString V),
      Array.foldr (fn (x, y) => (Int.toString x)::y) [] M,
      FunStack.stack2list (FunStack.map StackElement.toString C)
    )

  fun composition (f, n) = (* computes f^n *)
    if n > 0 then
      f o (composition (f, n - 1))
    else
      (fn x => x)

  datatype CmdType = BINOP | UNOP | DATA | CEMPTY | CEOS | CITE | CWH | CSET
                   | IO | CSEQ

  fun getCmdType x = case x of
                          VAR _ => DATA   | VAL _  => DATA
                        | AND   => BINOP  | OR     => BINOP
                        | LT    => BINOP  | LEQ    => BINOP | EQ  => BINOP
                        | GT    => BINOP  | GEQ    => BINOP | NEQ => BINOP
                        | PLUS  => BINOP  | MINUS  => BINOP | DIV => BINOP
                        | MOD   => BINOP  | TIMES  => BINOP
                        | EMPTY => CEMPTY | EOS  => CEOS
                        | NOT   => UNOP   | NEGATIVE => UNOP
                        | ITE   => CITE   | SET    => CSET  | WH  => CWH 
                        | READ  => IO     | WRITE  => IO    | SEQ => CSEQ

  fun arity x = case (getCmdType x) of
                     BINOP => 2 | UNOP   => 1
                   | DATA  => 0 | CEMPTY => 0 | CEOS => 0
                   | CITE  => 3 | CWH    => 2 | CSET   => 2
                   | IO    => 1 | CSEQ  => 2

  val push = FunStack.push
    (* move a single element from V to C *)

  fun emptyC2V (V, C) =
    (* move everything from C onto V, or until you encounter a delimiter *)
    let
      val x = FunStack.poptop C
    in
      case x of
           NONE                            => (V, C)
         | SOME (StackElement.EOS, rest) => (V, C)
         | SOME (top, rest)                => emptyC2V (push (top, V), rest)
    end

  fun C2V f (V, C) =
    let
      val (x, rest) = valOf (FunStack.poptop C)
    in
      (push (f x, V), rest)
    end

  fun V2C f (V, C) =
    let
      val (x, rest) = valOf (FunStack.poptop V)
    in
      (rest, push (f x, C))
    end

  fun moveTreeV2C (V, C) = 
    let
      val (top, rest) = valOf (FunStack.poptop V)
    in
      (composition (moveTreeV2C, arity top)) (rest, push (top, C))
    end

  fun getVal (M, StackElement.VAL x):int = x
    | getVal (M, StackElement.VAR x):int = Array.sub(M, x)
    (* handle Subscript => raise err "Variable doesn't exist" *)
    | getVal (M, _)     = raise err "Variable or value expected"

  fun unOp x = 
    case x of
         NEGATIVE => (fn z => ~z)
       | NOT      => (fn z => 1 - z)
       | _        => raise err "Unary operator expected"

  fun b2i false = 0
    | b2i true  = 1

  fun binOp x:(int * int -> int) =
    case x of
         AND     => (fn (a, b) => a * b)
       | OR      => (fn (a, b) => 1 - (1 - a) * (1 - b))
       | LT      => b2i o op< 
       | LEQ     => b2i o op<=
       | EQ      => b2i o op=
       | GT      => b2i o op>
       | GEQ     => b2i o op>=
       | NEQ     => b2i o op<>
       | PLUS    => op+
       | MINUS   => op-
       | TIMES   => op*
       | DIV     => (op div)
       | MOD     => (op mod)
       | _       => raise err "Binary operator expected"

  val stackPrint = FunStack.toString StackElement.toString

  (*
   * evaluate expression until you hit a EOS in C,
   * evaluated expression is stored on top of V
   *)
   (*
  fun evalExpr (V, M, C) = 
    let
      val (top, rest) = (valOf o FunStack.poptop) C
    in
    end
    *)

  (*
  fun evalExpr (V, M, C) =
    let
      val (top, rest) = (valOf o FunStack.poptop) V
      val deg = arity top
      val (V0, _) = (composition (evalExpr, deg)) (rest, M)
    in
      case deg of
           0 => (V, M)
         | 1 =>
             let
               val (x, V1) = (valOf o FunStack.poptop) V0
               val VAL xu = x
             in
               (push (VAL ((unOp top) xu), V1), M)
             end
         | 2 =>
             let
               val (x, V1) = (valOf o FunStack.poptop) V0
               val (y, V2) = (valOf o FunStack.poptop) V1
               val VAL xu = x
               val VAL yu = y
             in
               (push (VAL ((binOp top) (yu, xu)), V2), M)
             end
         | _ => raise err "Invalid expression"
    end
     *)

  fun rules (V, M, C) =
    if (FunStack.empty) C then (V, M, C)
    else if ((FunStack.top C) = EOS) then (V, M, FunStack.pop C)
    (* EOS.C -> C *)
    else let
      val (V0, C0) = emptyC2V (V, C)
      val (cmd, V1) = (valOf o FunStack.poptop) V0
    in
      case cmd of
           SEQ => (* <V, M, c.d.SEQ.C> -> <V, M, c.EOS.d.C> *)
           let
             val (V2, C2) = moveTreeV2C (V1, C0)
             val C3 = push (EOS, C2)
             val (V4, C4) = moveTreeV2C (V2, C3)
           in
             (V4, M, C4)
           end
         | READ => (* <V, M, x.READ.C> -> <V, M{x <- input}, C> *)
             let
               val (VAR idx, V2) = (valOf o FunStack.poptop) V1
             in
               (
               Array.update(M, idx,
               valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn))));
               (V2, M, C0)
               )
             end
         (* | WRITE => *) (*
         <V, M, e.WRITE.C> -> <V, M, e.EOS.C> -> <V, M, e0.EOS.C> -> <V, M, C>,
          write to console
          the 2nd transition is done by evalExpr
         *)
         | _ => raise err "I wasn't programmed for this, welp"
    end
end

signature VMC =
sig
  type states = (StackElement.StackElement FunStack.Stack) * (int Array.array)
              * (StackElement.StackElement FunStack.Stack)
  val init : AST.Prog -> states
  val toString : states -> (string list) * (string list) * (string list)
  val rules : states -> states
end

structure Vmc:> VMC =
struct
  open StackElement
  val err = FunStack.Error
  val poptop = FunStack.poptop
  val push = FunStack.push
  val pop = FunStack.pop

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

  fun compose (f, n) = (* computes f^n *)
    if n > 0 then
      f o (compose (f, n - 1))
    else
      (fn x => x)

  datatype expType = BINOP | UNOP | DATA | NOTEXP

  fun getExpType x = case x of
                          VAR _ => DATA   | VAL _  => DATA
                        | AND   => BINOP  | OR     => BINOP
                        | LT    => BINOP  | LEQ    => BINOP | EQ  => BINOP
                        | GT    => BINOP  | GEQ    => BINOP | NEQ => BINOP
                        | PLUS  => BINOP  | MINUS  => BINOP | DIV => BINOP
                        | MOD   => BINOP  | TIMES  => BINOP
                        | NOT   => UNOP   | NEGATIVE => UNOP
                        | _     => NOTEXP

  fun arity x = case x of
                     EMPTY => 0 | EOS  => 0 | ITE   => 3 | WH  => 2
                   | SET   => 2 | READ => 1 | WRITE => 1 | SEQ => 2
                   | _ => (case (getExpType x) of
                               BINOP => 2 | UNOP => 2 | DATA => 0
                             | _     => raise err "Impossible command type")

  fun emptyC2V (V, C) =
    (* move everything from C onto V, or until you encounter a delimiter *)
    let
      val x = poptop C
    in
      case x of
           NONE                          => (V, C)
         | SOME (StackElement.EOS, rest) => (V, C)
         | SOME (top, rest)              => emptyC2V (push (top, V), rest)
    end

  fun moveTreeV2C (V, C) = 
    let
      val (top, rest) = valOf (poptop V)
    in
      (compose (moveTreeV2C, arity top)) (rest, push (top, C))
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

  fun binOp x:(int * int -> int) =
    let
      fun b2i false = 0
        | b2i true  = 1
    in
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
       | DIV     => op div
       | MOD     => op mod
       | _       => raise err "Binary operator expected"
    end

  val stackPrint = FunStack.toString StackElement.toString

  (*
   * evaluate expression until you hit a EOS in C,
   * evaluated expression is stored on top of V
   *)
  fun evalExpr (V, M, C) = 
    (* <V, M, exp.EOS.C'> <eval(exp).V, M, C'> *)
    let
      val (top, C0) = (valOf o poptop) C
    in
      if top = EOS then (V, M, C0)
      else 
        evalExpr (
        case getExpType top of
             DATA => (push (VAL (getVal(M, top)), V), M, C0)
           | UNOP =>
               let
                 val (VAL x, V0) = (valOf o poptop) V
               in
                 (push (VAL ((unOp top) x), V0), M, C0)
               end
           | BINOP =>
               let
                 val (VAL x, V0) = (valOf o poptop) V
                 val (VAL y, V1) = (valOf o poptop) V0
               in
                 (push (VAL ((binOp top) (y, x)), V1), M, C0)
               end
           | NOTEXP => raise err "Invalid expression"
           )
    end

  fun emptyStack st = (* empty stack until (and including) EOS *)
    let
      val x = poptop st
    in
      case x of
           NONE => st
         | SOME(EOS, res) => res
         | SOME(_, res) => emptyStack res
    end
  
  fun pushEOSonC (x, y) = (x, push (EOS, y))

  fun copyStack (0, _, C) = C (* copy first top len elements from V to C *)
    | copyStack (len, from, to) =
    (* here we implicitly created a copy of from *)
    (* use n-th instead? *)
      let
        val (top, rest) = (valOf o poptop) from
      in
        copyStack (len - 1, rest, push (top, to))
      end


  fun rules (V, M, C) =
    if (FunStack.empty) C then (V, M, C)
    else if ((FunStack.top C) = EOS) then (V, M, FunStack.pop C)
    (* EOS.C -> C *)
    else let
      val len_V = FunStack.depth V
      val (V0, C0) = emptyC2V (V, C)
      val (cmd, V1) = (valOf o poptop) V0
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
               val (VAR idx, V2) = (valOf o poptop) V1
             in
               (
                 Array.update(M, idx,
                   (print "Input: ";
                   (valOf o Int.fromString o valOf o TextIO.inputLine) TextIO.stdIn)
                 );
                 (V2, M, C0)
               )
             end
         | WRITE =>
             (*
              * <V, M, e.WRITE.C> -> <V, M, e.EOS.C> -> <e0.V, M, C> -> <V, M, C>,
              * write to console, the 2nd transition is done by evalExpr
              *
              * currently, state is
              * <e^R.V, M, C0> -> <V, M, e.EOS.C> -> <e0.V, M, C>
              * V1 = e^R.V
              * V2 = V2, C2 = e.EOS.C
              * V3 = e0.V, C3 = C
              *)
              let
                val (V2, C2) = moveTreeV2C (V1, push (EOS, C0))
                val (V3, _, C3) = evalExpr (V2, M, C2)
                val VAL e0 = FunStack.top V3
              in
                (
                  print ("Output: " ^ (Int.toString e0) ^ "\n");
                  (V, M, C3)
                )
              end
         | ITE =>
             (*
              *    <V, M, b.c.d.ITE.C>
              * -> <d^r.c^r.b^r.V, M, C>
              * -> <c^r.b^r.V, M, d.EOS.C>
              * -> <b^r.V, M, c.EOS.d.EOS.C>
              * -> <V, M, b.EOS.c.EOS.d.EOS.C>
              * -> <b0.V, M, c.EOS.d.EOS.C>
              *
              * transition 2,3,4 are just
              *   moveTreeV2C (V, M, push (EOS, C))
              *   = (moveTreeV2C o pushEOSonC)^3
              *
              * <0.V, M, c.EOS.d.EOS.C> -> <V, M, d.EOS.C>
              * <1.V, M, c.EOS.d.EOS.C> -> <c^r.V, M, EOS.d.EOS.C>
              *  -> <c^r.V, M, C> -> <V, M, c.EOS.C>
              *)
              let
                val (V2, C2) = (compose (moveTreeV2C o pushEOSonC, 3)) (V1, C0)
                val (V3, _, C3) = evalExpr (V2, M, C2)
                val (VAL b0, V4) = (valOf o poptop) V3
              in
                if (b0 = 0) then (V4, M, emptyStack C3)
                else
                  let
                    val (V5, C5) = emptyC2V (V4, C3)
                    val C6 = (emptyStack o pop) C5
                    val (V7, C7) = (moveTreeV2C o pushEOSonC) (V5, C6)
                  in
                    (V7, M, C7)
                  end
              end
         | EMPTY => (V1, M, C0) (* <V, M, EMPTY.C> -> <V, M, C> *)
         | SET   => 
             (*
              *    <V, M, x.e.SET.C>
              * -> <e^R.x.V, M, C>      V1, C0
              * -> <x.V, M, e.EOS.C>    V2, C2
              * -> <e0.x.V, M, C>       V3, C3
              * -> <x.V, M, C>          V4, C3
              * -> <V, M{x <- e0}, C>   V5, C3
              *)
              let
                val (V2, C2) = (moveTreeV2C o pushEOSonC) (V1, C0)
                val (V3, _, C3) = evalExpr (V2, M, C2)
                val (VAL e0, V4) = (valOf o poptop) V3
                val (VAR x, V5) = (valOf o poptop) V4
              in
                (Array.update(M, x, e0); (V5, M, C3))
              end
         | WH =>
             (*
              * |b| + |c| = |V1| - |V|
              * |c| = |V1| - |V2|
              *
              *
              *    <V, M, b.c.WH.C>
              * -> <c^R.b^R.V, M, C>           V1, C0
              * -> <b^R.V, M, c.WH.C>          V2, C2
              * -> <b^R.V, M, b.c.WH.C>        V2, C3
              * -> <V, M, b.EOS.b.c.WH.C>      V4, C4
              * -> <b0.V, M, b.c.WH.C>         V5, C5
              *
              *    <0.V, M, b.c.WH.C>          V5, C5
              * -> <V, M, C>                   V6,  
              *
              *    <1.V, M, b.c.WH.C>        1.V6, C5
              *  -> <c^R.b^R.V, M, b.c.WH.C>   V7, C5
              *  -> <b^R.V, M, c.EOS.b.c.WH.C> V8, C8
              *  -> <V, M, c.EOS.b.c.WH.c>     V9, C8
              *)
              let
                val len_V1 = FunStack.depth V1
                val (V2, C2) = moveTreeV2C (V1, push (WH, C0))
                val len_V2 = FunStack.depth V2
                val len_c = len_V1 - len_V2
                val len_b = len_V1 - len_V - len_c
                val C3 = copyStack (len_b, V2, C2)
                val (V4, C4) = (moveTreeV2C o pushEOSonC) (V2, C3)
                val (V5, _, C5) = evalExpr (V4, M, C4)
                val (VAL b0, V6) = (valOf o poptop) V5 
              in
                if (b0 = 0)
                then (V6, M, FunStack.drop(C5, len_b + len_c + 1))
                else 
                  let
                    val V7 = copyStack (len_b + len_c, C5, V6)
                    val (V8, C8) = (moveTreeV2C o pushEOSonC) (V7, C5)
                    val V9 = FunStack.drop (V8, len_b)
                  in
                    (V9, M, C8)
                  end
              end
         | _ => raise err "I wasn't programmed for this, welp"
    end
end

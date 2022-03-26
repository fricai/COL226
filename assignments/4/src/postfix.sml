signature POSTFIX =
sig
  val postfix : AST.Prog -> StackElement.StackElement FunStack.Stack
  val ast2vmc : AST.Prog -> Vmc.states
end

structure PostFix :> POSTFIX =
struct

structure SymbolTable:
sig
  val add : AST.Dec list -> unit 
  val find : string -> int option
end =
struct
  val TableSize = 422 (* 211 *)
  val ht : (string, int) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (TableSize, Fail "Variable not found")
  val insert = HashTable.insert ht

  val find = HashTable.find ht

  fun f (x::xs, n) = (insert (case x of AST.BOOL y => y | AST.INT y => y, n); f(xs, n + 1))
    | f ([], _) = ()
  fun add x = f (x, 0)
end

fun postfixExp exp =
  case exp of
       AST.AND (exp1, exp2)      => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.AND]
     | AST.OR  (exp1, exp2)      => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.OR]
     | AST.NOT exp               => (postfixExp exp)  @ [StackElement.NOT]

     | AST.LT (exp1, exp2)       => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.LT]
     | AST.LEQ (exp1, exp2)      => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.LEQ]
     | AST.EQ (exp1, exp2)       => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.EQ]
     | AST.GT (exp1, exp2)       => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.GT]
     | AST.GEQ (exp1, exp2)      => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.GEQ]
     | AST.NEQ (exp1, exp2)      => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.NEQ]

     | AST.PLUS (exp1, exp2)     => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.PLUS]
     | AST.MINUS (exp1, exp2)    => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.MINUS]
     | AST.TIMES (exp1, exp2)    => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.TIMES]
     | AST.DIV (exp1, exp2)      => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.DIV]
     | AST.MOD (exp1, exp2)      => (postfixExp exp1) @ (postfixExp exp2) @ [StackElement.MOD]
     | AST.NEGATIVE exp          => (postfixExp exp)  @ [StackElement.NEGATIVE]

     | AST.VAR var               => [StackElement.VAR (valOf(SymbolTable.find var))]
     | AST.INTVAL num            => [StackElement.VAL num]
     | AST.BOOLVAL b             => [StackElement.VAL (if b then 1 else 0)]

fun postfixCmd cmd = 
  case cmd of
       AST.SET (var, exp)        => (StackElement.VAR (valOf(SymbolTable.find var)))::((postfixExp exp) @ [StackElement.SET])
     | AST.READ var              => [StackElement.VAR (valOf(SymbolTable.find var)), StackElement.READ]
     | AST.WRITE exp             => (postfixExp exp) @ [StackElement.WRITE]
     | AST.ITE (exp, cmd1, cmd2) => (postfixExp exp) @ (postfixCmdList cmd1) @ (postfixCmdList cmd2) @ [StackElement.ITE]
     | AST.WH (exp, cmd)         => (postfixExp exp) @ (postfixCmdList cmd)  @ [StackElement.WH]
  and postfixCmdList cmdlist =
    if null cmdlist then [StackElement.EMPTY]
    else (List.concat (map postfixCmd cmdlist)) @ (
            List.tabulate ((length cmdlist) - 1, (fn _ => StackElement.SEQ))
          )

fun postfix ast =
  let
    val AST.PROG (_, AST.BLK (decllist, cmdlist)) = ast
  in (
      SymbolTable.add decllist;
      (FunStack.list2stack o postfixCmdList) cmdlist
    )
  end

fun ast2vmc ast =
  let
    val AST.PROG (_, AST.BLK(decl, _)) = ast
  in
    (FunStack.create(), Array.array(length decl, 0), postfix ast)
  end
end

CM.make "src/while.cm";

fun run x = (Vmc.execute o PostFix.ast2vmc o While.compile) x
  handle FunStack.Error str => (print str; raise FunStack.Error str)

val printStack = app (fn x => print (x ^ "."))
fun debugExecute (V, M, C) =
  let
    val (sV, sM, sC) = Vmc.toString (V, M, C)
  in
  (
    print "V: "; printStack sV; print "\n";
    print "M: "; printStack sM; print "\n";
    print "C: "; printStack sC; print "\n";
    print "\n";
     if FunStack.empty C then (V, M, C)
     else (debugExecute o Vmc.rules) (V, M, C)
  )
  end
fun debugRun x = (debugExecute o PostFix.ast2vmc o While.compile) x
  handle FunStack.Error str => (print str; raise FunStack.Error str)

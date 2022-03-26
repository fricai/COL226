CM.make "src/while.cm";

fun execute (V, M, C) =
  if FunStack.empty C then (V, M, C)
  else execute (Vmc.rules (V, M, C))

val run = execute o Vmc.init o While.compile

(*
val printList = app (fn x => print (x ^ "."))
fun debugExecute (V, M, C) =
  let
    val (sV, sM, sC) = Vmc.toString (V, M, C)
  in
  (
    print "V: "; printList sV; print "\n";
    print "M: "; printList sM; print "\n";
    print "C: "; printList sC; print "\n";
    print "\n";
     if FunStack.empty C then (V, M, C)
     else debugExecute (Vmc.rules (V, M, C))
  )
  end

val ast = While.compile "tests/loop.wh"
val _ = (debugExecute o Vmc.init) ast
 *)

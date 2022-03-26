CM.make "src/while.cm";
val ast = While.compile "tests/eval.wh";
val S = (Vmc.rules o Vmc.rules o Vmc.rules o Vmc.rules) (Vmc.init ast)
val (V, M, C) = Vmc.toString S

val printList = app (fn x => print (x ^ "."))
val _ = (print "V: "; printList V; print "\n")
val _ = (print "M: "; printList M; print "\n")
val _ = (print "C: "; printList C; print "\n")

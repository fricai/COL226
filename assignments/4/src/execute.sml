fun execute (V, M, C) =
  if FunStack.empty C
  then (V, M, C)
  else execute (Vmc.rules (V, M, C))

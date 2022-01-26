pushback(X, [], [X]).
pushback(X, [Head|Tail], L):- % NL is L added to the end of L
	pushback(X, Tail, NL),
	L = [Head|NL].

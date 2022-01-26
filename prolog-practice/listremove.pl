remove(E, [E|Tail], L):-
	remove(E, Tail, L).
remove(E, [Head|Tail], L):-
	Head \= E,
	remove(E, Tail, NL),
	L = [Head|NL].
remove(_, [], []).

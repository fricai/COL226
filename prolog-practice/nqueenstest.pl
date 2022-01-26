queens(0, []).
queens(N, L):-
	N > 0,
	partqueens(N, N, L).

partqueens(_, 0, []).
partqueens(N, M, [(X, Y) | L]) :-
	M > 0, N >= M,
	Mminus1 is M - 1,
	partqueens(N, Mminus1, L),
	X is M - 1,
	Nminus1 is N - 1,
	between(0, Nminus1, Y),
	safe((X, Y), L).

safe((_, _),[]).
safe((X, Y), [(X1, Y1)|Rest]) :-
	X =\= X1, Y =\= Y1, X - X1 =\= Y - Y1, X - X1 =\= Y1 - Y,
	safe((X, Y), Rest).

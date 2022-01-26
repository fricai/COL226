queens(0, []).
queens(N, L):-
	N > 0,
	partqueens(N, N, L).

partqueens(_, 0, []).
partqueens(N, M, [(X, Y) | L]) :-
	M > 0, N >= M,
	Mminus1 is M - 1,
	partqueens(N, Mminus1, L),
	numlist(N, NL),
	X is M - 1,
	chooseSafe(NL, [(X, Y)|L]).

numlist(N, L) :-
	clopenint(0, N, L).

clopenint(L, U, []) :-
	integer(L), integer(U), L >= U.
clopenint(L, U, [L|Tail]) :-
	integer(L), integer(U), U > L, M is L + 1, clopenint(M, U, Tail).

chooseSafe([Y|_], [(X, Y)|L]) :-
	safe((X, Y), L).
chooseSafe([_|R], [(X, Y)|L]) :-
	chooseSafe(R, [(X, Y)|L]).

safe((_,_),[]).
safe((X, Y), [(X1, Y1)|Rest]) :-
	X =\= X1, Y =\= Y1, X - X1 =\= Y - Y1, X - X1 =\= Y1 - Y,
	safe((X, Y), Rest).

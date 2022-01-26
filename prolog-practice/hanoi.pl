hanoi(N) :-
	hanoi(N, a, b, c).
hanoi(0, _, _, _).
hanoi(N, FromPin, ToPin, UsingPin) :-
	M is N - 1,
	hanoi(M, FromPin, UsingPin, ToPin),
	move(FromPin, ToPin),
	hanoi(M, UsingPin, ToPin, FromPin).
move(From, To) :-
	write([move, disk, from, pin, From, to, pin, To]),
	nl.

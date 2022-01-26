ibt(empty).
ibt(node(N, L, R)) :- integer(N), ibt(L), ibt(R).

size(empty, 0).
size(node(_, L, R), N) :- size(L, X), size(R, Y), N is 1 + X + Y.

height(empty, 0).
height(node(_, L, R), H) :- height(L, X), height(R, Y), H is 1 + max(X, Y).

preorder(empty, []).
preorder(node(N, L, R), [N | Tail]) :-
	preorder(L, X), preorder(R, Y),
	append(X, Y, Tail).

inorder(empty, []).
inorder(node(N, L, R), List) :-
	inorder(L, X), inorder(R, Y),
	append([X, [N], Y], List).

postorder(empty, []).
postorder(node(N, L, R), List) :-
	postorder(L, X), postorder(R, Y),
	append([X, Y, [N]], List).

eulerTour(empty, []).
eulerTour(node(N, L, R), List) :-
	eulerTour(L, X), eulerTour(R, Y),
	append([[N], X, [N], Y, [N]], List).

% Convert to String
toString(empty, "()").
toString(node(N, L, R), S) :-
	toString(L, LBT), toString(R, RBT),
	atomics_to_string(["(", N, ", ", LBT, ", ", RBT, ")"], S).

% Balancedness check
isBalanced(empty).
isBalanced(node(N, L, R)) :- % make this O(N)
	integer(N),
	isBalanced(L), isBalanced(R),
	height(L, X), height(R, Y),
	Del is Y - X,
	Del > -2, Del < 2.

% BST Check
isBST(empty).
isBST(node(N, L, R)) :- isBST(node(N, L, R), _, _).

isBST(node(N, L, R), Min, Max) :- % Max, Min are the max, min key in the tree
	integer(N), isBST(L, Min, LeftMax), isBST(R, RightMin, Max),
	LeftMax < N, N < RightMin.
isBST(node(N, L, empty), Min, N) :-
	integer(N), isBST(L, Min, LeftMax), LeftMax < N.
isBST(node(N, empty, R), N, Max) :-
	integer(N), isBST(R, RightMin, Max), N < RightMin.
isBST(node(N, empty, empty), N, N) :- integer(N).

% Lookup
lookup(N, node(N, _, _)).
lookup(Target, node(N, L, _)):- Target < N, lookup(Target, L).
lookup(Target, node(N, _, R)):- Target > N, lookup(Target, R).

% Insertion
insert(Target, empty, node(Target, empty, empty)). % if the tree is empty, it is the only node
insert(Target, node(N, L, R), T2) :- 
	Target < N, insert(Target, L, L2),
	T2 = node(N, L2, R).
insert(Target, node(N, empty, R), T2) :-
	% found a node such that left child is empty
	%  and value on node is less than value to be inserted
	%  so Target is the left child of this node.
	Target < N,
	T2 = node(N, node(Target, empty, empty), R).

insert(Target, node(N, L, R), T2) :-
	Target > N, insert(Target, R, R2),
	T2 = node(N, L, R2).
insert(Target, node(N, L, empty), T2) :-
	% found a node such that right child is empty
	%  and value on node is less than value to be inserted
	%  so Target is the right child of this node.
	Target > N,
	T2 = node(N, L, node(Target, empty, empty)).

% MakeBST, sort and split it in half
makeBST(List, BT):-
	sort(List, SortedList),
	makeSortedBST(SortedList, BT).

splitInThree(List, A, X, B) :-
	length(List, N),
	L is (N - 1) // 2,
	length(A, L),
	append([A, [X], B], List).

makeSortedBST([], empty).
makeSortedBST(List, node(N, L, R)) :-
	splitInThree(List, Left, N, Right),
	makeSortedBST(Left, L),
	makeSortedBST(Right, R).

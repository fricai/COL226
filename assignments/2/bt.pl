ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

size(empty, 0).
size(node(_, L, R), N):- size(L, X), size(R, Y), N is 1 + X + Y.

height(empty, 0).
height(node(_, L, R), H):- height(L, X), height(R, Y), H is 1 + max(X, Y).

% Non-tail recursive
preorder(empty, []).
preorder(node(N, L, R), [N | Tail]):-
	preorder(L, X), preorder(R, Y),
	append(X, Y, Tail).

inorder(empty, []).
inorder(node(N, L, R), List):-
	inorder(L, X), inorder(R, Y),
	append(X, [N | Y], List).

postorder(empty, []).
postorder(node(N, L, R), List):-
	postorder(L, X), postorder(R, Y),
	append([X, Y, [N]], List).

% Tail-recursive
% 	Add exposition here
trPreorder(BT, List):- trPreorderAcc(BT, [], List).
trPreorderAcc(node(N, L, R), Acc, List):-
	List = [N | List1],
	trPreorderAcc(L, Acc1, List1),
	trPreorderAcc(R, Acc, Acc1).
trPreorderAcc(empty, List, List).

trInorder(BT, List):- trInorderAcc(BT, [], List).
trInorderAcc(node(N, L, R), Acc, List):-
	trInorderAcc(L, Acc1, List),
	Acc1 = [N | List2],
	trInorderAcc(R, Acc, List2).
trInorderAcc(empty, List, List).

trPostorder(BT, List):- trPostorderAcc(BT, [], List).
trPostorderAcc(node(N, L, R), Acc, List):-
	trPostorderAcc(L, Acc1, List),
	trPostorderAcc(R, Acc2, Acc1),
	Acc2 = [N | Acc].
trPostorderAcc(empty, List, List).

% Euler Tour
eulerTour(empty, []).
eulerTour(node(N, L, R), List):-
	eulerTour(L, X), eulerTour(R, Y),
	append([[N], X, [N], Y, [N]], List).

preET(BT, L):- eulerTour(BT, ET), preETList(ET, [], L).

% Explain the logic here
preETList([], [], []).
preETList([X | Tail], [], [X | L]):-
	preETList(Tail, [(X, l)], L).
preETList([X | Tail], [(Z, D) | Rest], [X | L]):-
	X =\= Z, preETList(Tail, [(X, l) | [(Z, D) | Rest]], L).
preETList([X | Tail], [(X, l) | Rest], L):-
	preETList(Tail, [(X, r) | Rest], L).
preETList([X | Tail], [(X, r) | Rest], L):-
	preETList(Tail, Rest, L).

inET(BT, L):- eulerTour(BT, ET), inETList(ET, [], L).
inETList([], [], []).
inETList([X | Tail], [], L):-
	inETList(Tail, [(X, l)], L).
inETList([X | Tail], [(Z, D) | Rest], L):-
	X =\= Z, inETList(Tail, [(X, l) | [(Z, D) | Rest]], L).
inETList([X | Tail], [(X, l) | Rest], [X | L]):-
	inETList(Tail, [(X, r) | Rest], L).
inETList([X | Tail], [(X, r) | Rest], L):-
	inETList(Tail, Rest, L).

postET(BT, L):- eulerTour(BT, ET), postETList(ET, [], L).
postETList([], [], []).
postETList([X | Tail], [], L):-
	postETList(Tail, [(X, l)], L).
postETList([X | Tail], [(Z, D) | Rest], L):-
	X =\= Z, postETList(Tail, [(X, l) | [(Z, D) | Rest]], L).
postETList([X | Tail], [(X, l) | Rest], L):-
	postETList(Tail, [(X, r) | Rest], L).
postETList([X | Tail], [(X, r) | Rest], [X | L]):-
	postETList(Tail, Rest, L).

% Convert to String
toString(empty, "()").
toString(node(N, L, R), S):-
	toString(L, LBT), toString(R, RBT),
	atomics_to_string(["(", N, ", ", LBT, ", ", RBT, ")"], S).

/* Balancedness check, we use AVL definition
 *
 *  A tree is balanced iff for every node,
 *  the difference in height of the two subtrees <= 1
 */
isBalanced(empty).
isBalanced(node(N, L, R)):- % make this O(N)
	integer(N),
	isBalanced(L), isBalanced(R),
	height(L, X), height(R, Y),
	Del is Y - X, % Difference in height
	Del > -2, Del < 2.

% BST Check
isBST(empty).
isBST(node(N, L, R)) :- isBST(node(N, L, R), _, _).

/* A tree is a BST if for for all nodes,
 * all values in left subtree < the value in node < all values in right subtree
 * equivalently,
 * max value in left subtree < the value in node < min value in right subtree.
 */
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

/* MakeBST, sort and split it in half
 *
 * We build the tree by sorting the input list,
 * placing the median on the current node
 * and recurring on the left and right half of the list.
 */
makeBST(List, BT):-
	sort(List, SortedList),
	makeSortedBST(SortedList, BT).

% Extracts the middle element of the list and splits it into left and right halves
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

/* Delete BST = node(N, L, R) after deleting Target
 *
 * It deletes by first finding Target, finding its 
 *  inorder successor, placing the inorder successor
 *  there and then recursively deleting that
 */
delete(Target, node(N, L, R), BST):-
	BST = node(N, X, R),
	Target < N, delete(Target, L, X).
delete(Target, node(N, L, R), BST):-
	BST = node(N, L, X),
	Target > N, delete(Target, R, X).
delete(_, empty, empty).
delete(Target, node(Target, L, empty), L).
delete(Target, node(Target, empty, R), R).
delete(Target, node(Target, L, R), BST):-
	getMin(R, Succ), % Succ is the inorder successor to Target
	delete(Succ, R, X),
	BST = node(Succ, L, X).

% X is the minimum element in the BST
getMin(node(_, L, _), X):- getMin(L, X).
getMin(node(X, empty, _), X).

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
/*
 * Implemented using accumulators
 */
trPreorder(BT, List):- trPreorderAcc(BT, [], List).
trPreorderAcc(node(N, L, R), Acc, List):-
	List = [N | Left],
	trPreorderAcc(L, Right, Left),
	trPreorderAcc(R, Acc, Right).
trPreorderAcc(empty, List, List).

trInorder(BT, List):- trInorderAcc(BT, [], List).
trInorderAcc(node(N, L, R), Acc, List):-
	trInorderAcc(L, Acc1, List),
	Acc1 = [N | Right],
	trInorderAcc(R, Acc, Right).
trInorderAcc(empty, List, List).

trPostorder(BT, List):- trPostorderAcc(BT, [], List).
trPostorderAcc(node(N, L, R), Acc, List):-
	trPostorderAcc(L, Right, List),
	trPostorderAcc(R, Acc1, Right),
	Acc1 = [N | Acc].
trPostorderAcc(empty, List, List).

% Euler Tour
% eulerTour(empty, []).
% eulerTour(node(N, L, R), List):-
%	eulerTour(L, X), eulerTour(R, Y),
%	append([[N], X, [N], Y, [N]], List).

% Implemented using accumulator:
eulerTour(empty, []).
eulerTour(BT, List):- eulerTour(BT, [], List).
eulerTour(node(N, L, R), Acc, List):-
	List = [N | List2],
	eulerTour(L, Acc2, List2),
	Acc2 = [N | List1],
	eulerTour(R, Acc1, List1),
	Acc1 = [N | Acc].
eulerTour(empty, List, List).

% Pre/In/Post-order from Euler Tour
/*
 * These work in O(N^2) when all the values are distinct
 * however, they find all pre/in/post order given the euler tour
 *
 * Note that values should be distinct for pre/in/post order to be
 * uniquely determined from euler tour, the following trees have
 * the same euler tour but different pre and inorder
 *
 * node(1, node(1, empty, node(2, empty, empty)), node(1, empty, empty))
 * node(1, node(1, node(1, node(2, empty, empty), empty), empty), empty)
 *
 */

/*
preET(BT, L):- eulerTour(BT, ET), preETList(ET, L).
preETList(ET, L):-
	append([[N], Left, [N], Right, [N]], ET),
	preETList(Left, LLeft),
	preETList(Right, LRight),
	append([N | LLeft], LRight, L).
preETList([], []).

inET(BT, L):- eulerTour(BT, ET), inETList(ET, L).
inETList(ET, L):-
	append([[N], Left, [N], Right, [N]], ET),
	inETList(Left, LLeft),
	inETList(Right, LRight),
	append(LLeft, [N | LRight], L).
inETList([], []).

postET(BT, L):- eulerTour(BT, ET), postETList(ET, L).
postETList(ET, L):-
	append([[N], Left, [N], Right, [N]], ET),
	postETList(Left, LLeft),
	postETList(Right, LRight),
	append([LLeft, LRight, [N]], L).
postETList([], []).
*/

% These work in O(N) but don't work when values aren't distinct
/*
 * They work as follows, iterate from start to end
 *  when you encounter a label X for the first time
 *   place (X, l) into a stack, until the next occurrence
 *   of X, this will indicate that we are currently in
 *   the left subtree of X.
 *
 *  When you encounter the next X, the top of the stack
 *   must be (X, l), pop this and place (X, r) onto the stack.
 *   This will indicate that you are currently in the right
 *   subtree of X.
 *
 *  Finally, when the third occurrence of X occurs, the top of
 *   the stack will have (X, r), pop this.
 *   This indicates that you have completed the subtree of X.
 *  
 *  Stacks are implemented as accumulators and tail recursion
 *   is used.
 *
 * A formal proof of correctness would require using the interval
 * structure of DFS times in a tree.
 *
 */

preET(BT, L):- eulerTour(BT, ET), preETList(ET, [], L).
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
isBalanced(BT):- isBalanced(BT, _).

isBalanced(empty, 0).
isBalanced(node(N, L, R), H):- % H is the height of the true
	integer(N),
	isBalanced(L, HL), isBalanced(R, HR),
	H is 1 + max(HL, HR),
	Del is HR - HL, % Difference in height
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
isBST(node(N, L, empty), Min, N) :- % N = Max if no right child
	integer(N), isBST(L, Min, LeftMax), LeftMax < N.
isBST(node(N, empty, R), N, Max) :- % N = Min if no left child
	integer(N), isBST(R, RightMin, Max), N < RightMin.
isBST(node(N, empty, empty), N, N) :- integer(N).

% Lookup
/*
 * If Target < Value in node, then Target lies in the left subtree
 * If Target > Value in node, then Target lies in the right subtree
 * If Target = Value, then Target lies in the current node.
 */
lookup(Target, node(Target, _, _)).
lookup(Target, node(Val, L, _)):- Target < Val, lookup(Target, L).
lookup(Target, node(Val, _, R)):- Target > Val, lookup(Target, R).

% Insertion
insert(Target, empty, node(Target, empty, empty)). % if the tree is empty, it is the only node
insert(Target, node(Val, L, R), T2) :- 
	Target < Val, insert(Target, L, L2),
	T2 = node(Val, L2, R).
insert(Target, node(Val, empty, R), T2) :-
	% found a node such that left child is empty
	%  and value on node is less than value to be inserted
	%  so Target is the left child of this node.
	Target < Val,
	T2 = node(Val, node(Target, empty, empty), R).

insert(Target, node(Val, L, R), T2) :-
	Target > Val, insert(Target, R, R2),
	T2 = node(Val, L, R2).
insert(Target, node(Val, L, empty), T2) :-
	% found a node such that right child is empty
	%  and value on node is less than value to be inserted
	%  so Target is the right child of this node.
	Target > Val,
	T2 = node(Val, L, node(Target, empty, empty)).

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
delete(Target, node(Target, L, empty), L).
delete(Target, node(Target, empty, R), R).
delete(Target, node(Target, L, R), BST):-
	getMin(R, Succ), % Succ is the inorder successor to Target
	delete(Succ, R, X),
	BST = node(Succ, L, X).

% X is the minimum element in the BST
getMin(node(_, L, _), X):- getMin(L, X).
getMin(node(X, empty, _), X).

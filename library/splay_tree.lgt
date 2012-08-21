
:- object(splay_tree).

	:- info([
		version is 1.0,
		author is 'Vijay Saraswat; adapted to Logtalk by Paulo Moura.',
		author is 'Paulo Moura',
		date is 2009/7/5,
		comment is 'Splay trees implementation.']).

	:- public(access/5).

	:- public(insert/4).

	:- public(delete/3).
	:- mode(delete(?list, ?list, ?list), zero_or_more).
	:- info(delete/3, [
		comment is 'Delete item Item from tree Tree, assuming that it is present.',
		argnames is ['Item', 'Tree', 'NewTree']]).

	:- public(init/1).

	:- public(split/5).

	% Date: Sun 22 Mar 87 03:40:22-EST
	% >From: vijay <Vijay.Saraswat@C.CS.CMU.EDU>
	% Subject: Splay trees in LP languages.
 
	% There have hardly been any interesting programs in this Digest for a
	% long while now. Here is something which may stir the slothful among
	% you!  I present Prolog programs for implementing self-adjusting binary
	% search trees, using splaying. These programs should be among the most
	% efficient Prolog programs for maintaining binary search trees, with
	% dynamic insertion and deletion.
 
	% The algorithm is taken from: "Self-adjusting Binary Search Trees",
	% D.D. Sleator and R.E. Tarjan, JACM, vol. 32, No.3, July 1985, p. 668.
	% (See Tarjan's Turing Award lecture in this month's CACM for a more
	% informal introduction).  
	% -----------------------------------------
 
	% The operations provided by the program are:
 
	% 1. access(i,t):  (implemented by the call access(V, I, T, New))
	%   "If item i is in tree t, return a pointer to its location;
	%   otherwise return a pointer to the null node."
	%   In our implementation, in the call access(V, I, T, New),
	%   V is unifies with `null' if the item is not there, else
	%   with  `true' if it is there, in which case I is also
	%   unified with that item.
 
	% 2. insert(i,t):  (implemented by the call insert(I, T, New))
	%   "Insert item i in tree t, assuming that it is not there already."
	%   (In our implementation, i is not inserted if it is already
	%   there: rather it is unified with the item already in the tree.)
 
	% 3. delete(i,t):  (implemented by the call del(I, T, New))
	%   "Delete item i from tree t, assuming that it is present."
	%   (In our implementation, the call fails if the item is not in
	%   the tree.)
 
	% 4. join(t1,t2):  (Implemented by the call join(T1, T2, New))
	%    "Combine trees t1 and t2 into a single tree containing
	%     all items from both trees, and return the resulting
	%     tree. This operation assumes that all items in t1 are
	%     less than all those in t2 and destroys both t1 and t2."
 
	% 5. split(i,t): (implemented by the call split(I, T, Left, Right))
	%    "Construct and return two trees t1 and t2, where t1
	%     contains all items in t less than i, and t2 contains all
	%     items in t greater than i. This operations destroys t."
 
	% The basic workhorse is the routine bst(Op, Item, Tree, NewTree), which
	% returns in NewTree a binary search tree obtained by searching for Item
	% in Tree and splaying. OP controls what must happen if Item is not
	% found in the Tree.  If Op = access(V), then V is unified with null if
	% the item is not found in the tree, and with true if it is; in the
	% latter case Item is also unified with the item found in the tree. In
	% the first case, splaying is done at the node at which the discovery
	% was made that Item was not in the tree, and in the second case
	% splaying is done at the node at which Item is found. If Op=insert,
	% then Item is inserted in the tree if it is not found, and splaying is
	% done at the new node; if the item is found, then splaying is done at
	% the node at which it is found.

	% A node is simply an n/3 structure: n(NodeValue, LeftSon, RightSon).
	% NodeValue could be as simple as an integer, or it could be a (Key,
	% Value) pair.
 
	% Here are the top-level axioms. The algorithm for del/3 is the first
	% algorithm mentioned in the JACM paper: namely, first access the
	% element to be deleted, thus bringing it to the root, and then join its
	% sons. (join/4 is discussed later.)
 
	access(V, Item, Val, Tree, NewTree) :-
		bst(access(V), Item, Val, Tree, NewTree).

	insert(Item, Val,Tree, NewTree) :-
		bst(insert, Item, Val, Tree, NewTree).

	delete(Item, Tree, NewTree) :-
		bst(access(true), Item, Val, Tree, n(Item, Val, Left, Right)),
		join(Left, Right, NewTree).

	join(Left, Right, New) :-
		join(L-L, Left, Right, New).

	split(Item, Val, Tree, Left, Right) :-
		bst(access(true), Item, Val, Tree, n(Item, Val, Left, Right)).

	% We now consider the definition of bst. We use the notion of
	% `difference-bsts'. There are two types of difference-bsts, a left one
	% and a right one. The left one is of the form T-L where T is a bst and
	% L is the *right* son of the node with the largest value in T. The
	% right one is of the form T-R where T is a binary search tree and R is
	% the *left* son of the node with the smallest value in T. An empty bst
	% is denoted by a variable. Hence L-L denotes the empty left (as well as
	% right) difference bst.
 
	% As discussed in the JACM paper, we start with a notion of a left
	% fragment and a right fragment of the new bst to be constructed.
	% Intially, the two fragments are empty.
 
	bst(Op, Item, Val, Tree, NewTree) :-
		bst(Op, Item, Val, L-L, Tree, R-R, NewTree).

	% We now consider the base cases. The empty tree is a variable: hence it
	% will unify with the atom null. (A non-empty tree is a n/3 structure,
	% which will not unify with null). If Item was being *access*ed, then it
	% was not found in the tree, and hence Null=null. On the other hand, if
	% the Item is found at the root, then the call terminates, with the New
	% Tree being set up appropriately.
 
	% The base clauses are:
 
	bst(access(Null), Item, _, L, null, R, Tree) :-
		!, Null = null.
	bst(access(true), Item, Val, Left-A, n(Item0, Val0, A, B), Right-B, n(Item, Val, Left, Right)) :-
		Item == Item0, !, Val = Val0.
	bst(insert, Item, Val, Left-A, T, Right-B, n(Item0, Val, Left, Right)) :-
		(var(T) ; T = n(Item0, Val0, A, B), Item == Item0), !, Item = Item0.
	% We now consider the zig case, namely that we have reached a node such
	% that the required Item is either to the left of the current node and
	% the current node is a leaf, or the required item is the left son of
	% the current node. Depending upon the Op, the appropriate action is
	% taken:
	bst(access(Null), Item, _, Left-L, n(X, VX, null, B), Right-B, n(X, VX, Left, Right)) :-
		Item @< X, !, Null = null.
	bst(Op, Item, Val, Left, n(X, VX, n(Item, Val, A1, A2), B), R-n(X, VX, NR,B), New) :-
		Item @< X, !,
		bst(Op, Item, Val, Left, n(Item, Val, A1, A2), R-NR, New).
	% The recursive cases are straightforward:
	% Zig-Zig:
	bst(Op, Item, Val, Left, n(X, VX, n(Y, VY, Z, B), C), R-n(Y, VY, NR, n(X, VX, B, C)), New) :-
		Item @< X, Item @< Y, !,
		bst(Op, Item, Val, Left, Z, R-NR, New).
	% Zig-Zag:
	bst(Op, Item, Val, L-n(Y, VY, A, NL), n(X, VX, n(Y, VY, A, Z), C), R-n(X, NX, NR, C), New) :-
		Item @< X, Y @< Item, !,
		bst(Op, Item, Val, L-NL, Z, R-NR, New).
	% The symmetric cases for the right sons of the current node
	% are straightforward too:

	% Zag
	bst(access(Null), Item, _, Left-B, n(X, VX, B, null), Right-R, n(X, VX, Left, Right)) :-
		X @< Item, !, Null = null.  % end of the road.
	bst(Op, Item, Val, L-n(X, VX, B, NL), n(X, VX, B, n(Item, Val, A1, A2)), Right, New) :-
		X @< Item, !,
		bst(Op, Item, Val, L-NL, n(Item, Val, A1, A2), Right, New).
	% Zag-Zag
	bst(Op, Item, Val, L-n(Y, VY, n(X, VX, C, B), NL), n(X, VX, C, n(Y, VY, B, Z)), Right, New) :-
		X @< Item, Y @<Item, !,
		bst(Op, Item, Val, L-NL, Z, Right, New).
	% Zag-Zig
	bst(Op, Item, Val, L-n(X, VX, A, NL), n(X, VX, A, n(Y, VY, Z, C)), R-n(Y, VY, NR, C), New) :-
		X @< Item, Item @< Y, !,
		bst(Op, Item, Val, L-NL, Z, R-NR, New).

	% We now consider the definition of join.  To join Left to Right, it is
	% sufficient to splay at the rightmost vertex in Left, and make Right
	% its Right son. To build NewTree, we initially start with an empty left
	join(Left-A, n(X, VX, A, var), Right, n(X, VX, Left, Right)) :-
		!.
	join(Left-n(X, VX, A, B), n(X, VX, A, n(Y, VY, B, var)), Right,	n(Y, VY, Left, Right)) :-
		!.
	join(Left-n(Y, VY, n(X, VX, C, B), NL), n(X, VX, C, n(Y, VY, B, n(Z, VZ, A1, A2))), Right, New) :-
		join(Left-NL, n(Z, VZ,A1, A2), Right, New).
 

	init(_).

:- end_object.

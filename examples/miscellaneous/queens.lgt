%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%   example adapted form:

%   File   : Queens.Pl
%   Author : Richard A O'Keefe
%   Updated: 8 Feb 84
%   Purpose: Solve the N queens problem in Prolog.


:- object(queens).


	:- uses(list, [keysort/2, member/2]).


	:- public(queens/1).
	:- mode(queens(+integer), one).


	:- private(forbidden/4).
	:- mode(forbidden(+integer, +integer, +integer, +integer), zero_or_one).

	:- private(least_room_to_move/4).
	:- mode(least_room_to_move(+list, -integer, -integer, -list), zero_or_more).

	:- private(lr2m/6).
	:- mode(lr2m(+list, +integer, +integer, -integer, -integer, -list), zero_or_more).

	:- private(make_initial_table/2).
	:- mode(make_initial_table(+integer, -list), zero_or_one).

	:- private(make_initial_table/3).
	:- mode(make_initial_table(+integer, +list, -list), zero_or_one).

	:- private(number_list/2).
	:- mode(number_list(+integer, -list), one).

	:- private(place/2).
	:- mode(place(+list, -list), zero_or_more).

	:- private(prune/4).
	:- mode(prune(+list, +integer, +integer, -list), zero_or_more).

	:- private(prune/5).
	:- mode(prune(+list, +integer, +integer, +integer, -list), zero_or_more).

	:- private(shorter/2).
	:- mode(shorter(+list, +list), zero_or_one).


/*  The N-queens problem is to place N queens on an NxN chessboard so
    that no two queens attack each other.  Suppose we have a queen in
    (Row1,Col1) and a queen in (Row2,Col2).  They attack each other if
	1.  Same rank:		Row1 = Row2.
	2.  Same file:		Col1 = Col2.
	3.  Same NW-SE diagonal Row2 - Row1 = Col2 - Col1.
	4.  Same SW-NE diagonal Row2 - Row1 = Col1 - Col2.
    We can express 3 and 4 another way:
	3.			Row2 - Col2 = Row1 - Col1.
	4.			Row2 + Col2 = Row1 + Col1.
    So each of the N queens has four numbers associated with it,
    (Row,Col,Dif,Sum), and any two queens must have different numbers
    in all these positions.  The possible values are
	Row : 1 .. N
	Col : 1 .. N
	Sum : 1 .. 2N
	Dif : 1-N .. N-1

    The first question is, how shall we represent the board?
    It is sufficient to have a table indexed by rows, whose elements
    are the columns in which the corresponding queen is placed.  For
    example, with rows horizontal and columns vertical
		  1   2   3   4	   -column /  row
		+---+---+---+---+
		|   | Q |   |   |		1
		+---+---+---+---+
		|   |   |   | Q |		2
		+---+---+---+---+
		| Q |   |   |   |		3
		+---+---+---+---+
		|   |   | Q |   |		4
		+---+---+---+---+

    could be represented by board(2,4,1,3).  

    How are we going to place the queens?  The first idea that springs
    to mind is to place them one after another, but we can do better than
    that.  Let us maintain a table of (QueenRow/PossibleColumns), and at
    each step pick the queen with the fewest possible columns, place it,
    and prune the remaining sets.

    Let's start by writing a predicate to generate this table.  For N=4
    it will look like [1/[1,2,3,4], 2/[1,2,3,4], 3/[1,2,3,4], 4/[1,2,3,4]].
*/

	make_initial_table(N, Table) :-
		number_list(N, PossibleColumns), 	% set of all possible columns
		make_initial_table(N, PossibleColumns, Table).


	make_initial_table(0, _, []) :- !.
	make_initial_table(N, PossibleColumns, [N/PossibleColumns|Table]) :-
		M is N-1,
		%   in C-Prolog we could write succ(M, N) which would eliminate
		%   the need for the cut in the previous clause
		make_initial_table(M, PossibleColumns, Table).


	number_list(0, []) :- !.
	number_list(N, [N| List]) :-
		M is N-1,	%  see previous comment
		number_list(M, List).


/*  This actually generates the reverse of what I said, so we'd get
	[4/[4,3,2,1], 3/[4,3,2,1], 2/[4,3,2,1], 1/[4,3,2,1]],
    but since it was to be a set of number/set pairs, that's ok.
    We shall only be operating on these sets an element at a time,
    so it the order doesn't matter.

    Now the problem is solved if there are no queens left to place.
    Otherwise, we pick the queen with the fewest possible columns,
    backtrack over those possible columns, prune the possible columns
    sets of the remaining queens, and recur.

    We are given a set of Row/PossCols pairs for the unplaced queens,
    and we are to return a set of Row-Col pairs saying where we put
    the queens.
*/

	place([], []).
	place(UnplacedQueens, [Queen-Col|Placement]) :-
		least_room_to_move(UnplacedQueens, Queen, Columns, OtherQueens),
		member(Col, Columns),	% backtrack over possible places
		prune(OtherQueens, Queen, Col, RemainingQueens),
		place(RemainingQueens, Placement).


/*  If you haven't done this sort of thing before, least_room_to_move
    can be quite tricky.  The idea is the we wander down the list of
    pairs, keeping the current best pair apart, and when we find that
    there is a better pair we have to put the current pair back in the list.
    But because these are sets, it doesn't matter *where* the pairs go in
    the list.  Note that we don't need a cut in the first clause of place/2
    because least_room_to_move will fail on an empty list.
*/

	least_room_to_move([Q/C|Table], Qbest, Cbest, Rest) :-
		lr2m(Table, Q, C, Qbest, Cbest, Rest).

/*  This uses accumulator passing.  I really have to explain this program
    to you in person.
*/

	lr2m([], Q, C, Q, C, []).
	lr2m([NewQ/NewC|Table], OldQ, OldC, MinQ, MinC, [OldQ/OldC|Rest]) :-
		shorter(NewC, OldC),
		!,
		lr2m(Table, NewQ, NewC, MinQ, MinC, Rest).
	lr2m([Pair|Table], OldQ, OldC, MinQ, MinC, [Pair|Rest]) :-
		lr2m(Table, OldQ, OldC, MinQ, MinC, Rest).


/*  shorter(L1, L2) is true when the list L1 is strictly shorter than
    the list L2
*/
	shorter([], [_|_]).
	shorter([_|L1], [_|L2]) :-
		shorter(L1, L2).


/*  Now we have to code prune.  To prune all the queens, we prune each
    queen in turn.
*/
	prune([], _, _, []).
	prune([Queen/Columns|Queens], Row, Col, [Queen/Pruned|RestPruned]) :-
		prune(Columns, Queen, Row, Col, Pruned),
		prune(Queens, Row, Col, RestPruned).

/*  To prune a single queen, we have to eliminate all the positions
    forbidden by the queen wee have just placed.
*/

	prune([], _, _, _, []).
	prune([Col2|Cols], Row2, Row1, Col1, Permitted) :-
		forbidden(Row1, Col1, Row2, Col2),
		!,
		prune(Cols, Row2, Row1, Col1, Permitted).
	prune([Col2|Cols], Row2, Row1, Col1, [Col2|Permitted]) :-
		prune(Cols, Row2, Row1, Col1, Permitted).


/*  Finally, since we have ensured that two queens are automatically in
    different rows, we have only to check rules 2, 3, and 4.
*/
	forbidden(_, Col, _, Col).
	forbidden(Row1, Col1, Row2, Col2) :-
		Row2 - Col2 =:= Row1 - Col1.
	forbidden(Row1, Col1, Row2, Col2) :-
		Row2 + Col2 =:= Row1 + Col1.


/*  The last thing left for us to do is to write the top level predicate
    that ties all the pieces together.  Because the 'place' predicate
    may place the queens in any order, we keysort the list to make it more
    readable.  I'm afraid I had that in mind when I decided that it would
    be a list of Row-Col pairs.  I seem to recommend sorting for everything.
    Well, it IS a panacea.
*/
	queens(N) :-
		make_initial_table(N, Table),
		place(Table, Placement),
		keysort(Placement, DisplayForm),
		write(DisplayForm), nl.


:- end_object.

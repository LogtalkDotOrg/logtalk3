%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the examples
% distributed with SICStus Prolog 4.0.2 (November 2010)

/* Solitaire Battleship puzzles, a.k.a. Torpedo.
 *
 * Problem 14 at http://www.csplib.org/
 *
 * Author	: Mats Carlsson
 *
 */

:- object(torpedo).

	:- public(torpedo/1).

	:- use_module(lists, [
		keys_and_values/3, keyclumped/2, length/2, member/2
	]).
	:- use_module(clpfd, [
		disjoint2/1, fdset_parts/4, fdset_subtract/3, global_cardinality/2,
		in/2, in_set/2, lex_chain/2, list_to_fdset/2, labeling/2,
		(#=)/2, (#=<)/2, (#<=>)/2, (#\)/2,
		op(700, xfx, #=), op(700, xfx, #=<), op(760, yfx, #<=>), op(700, xfx, #\)
	]).

	torpedo(ID) :-
		data(ID, PerRowData, PerColData, ShipData),
		length(PerRowData, NRows),
		length(PerColData, NCols),
		% set up domains for the battleship variables
		% a zero tally => that row or column must be empty
		zero_positions(PerColData, 0, NoCols1, []),
		list_to_fdset(NoCols1, NoCols2),
		fdset_parts(XDom1, 1, NCols, []),
		fdset_subtract(XDom1, NoCols2, XDom2),
		zero_positions(PerRowData, 0, NoRows1, []),
		list_to_fdset(NoRows1, NoRows2),
		fdset_parts(YDom1, 1, NRows, []),
		fdset_subtract(YDom1, NoRows2, YDom2),
		ships(ShipData, NCols, NRows, XDom2, YDom2, TShips1, [], Coords, []),
		% constrain wrt. the row and column tallies
		keys_and_values(Coords, Xs, Ys),
		tag(PerColData, 1, XC),
		tag(PerRowData, 1, YC),
		global_cardinality(Xs, XC),
		global_cardinality(Ys, YC),
		% constrain ships to be non-adjacent
		keys_and_values(TShips1, _, Ships),
		disjoint2(Ships),
		% break symmetries: lex order ships of the same class
		keysort(TShips1, TShips2),
		keyclumped(TShips2, Groups),
		order_groups(Groups),
		% search: largest ship first
		ships_vars(Ships, Vars, []),
		labeling([bisect], Vars),
		% display solution
		draw(Ships, NRows, NCols).

	zero_positions([], _) --> [].
	zero_positions([0|Xs], I) --> !, [J],
		{J is I+1},
		zero_positions(Xs, J).
	zero_positions([_|Xs], I) -->
		{J is I+1},
		zero_positions(Xs, J).

	ships([], _, _, _, _, S, S, C, C).
	ships([X|Xs], NCols, NRows, XDom2, YDom2, S0, S, C0, C) :-
		ship(X, NCols, NRows, XDom2, YDom2, S0, S1, C0, C1),
		ships(Xs, NCols, NRows, XDom2, YDom2, S1, S, C1, C).

	ship(1, _NCols, _NRows, XDom2, YDom2, [1-ship(X,2,Y,2)|Ships], Ships, [X-Y|Coords], Coords) :-
		X in_set XDom2,
		Y in_set YDom2.
	ship(2, NCols, NRows, XDom2, YDom2, [2-ship(X,W,Y,H)|Ships], Ships, [X-Y,X1-Y1|Coords], Coords) :-
		X in_set XDom2,
		Y in_set YDom2,
		W in {2,3},
		H in {2,3},
		X+W #=< NCols+2,
		Y+H #=< NRows+2,
		H #= 2 #<=> Horiz,
		W #= 2 #<=> Vert,
		Horiz #\ Vert,
		X1 #= X+Horiz,
		Y1 #= Y+Vert.
	ship(3, NCols, NRows, XDom2, YDom2, [3-ship(X,W,Y,H)|Ships], Ships, [X-Y,X1-Y1,X2-Y2|Coords], Coords) :-
		X in_set XDom2,
		Y in_set YDom2,
		W in {2,4},
		H in {2,4},
		X+W #=< NCols+2,
		Y+H #=< NRows+2,
		H #= 2 #<=> Horiz,
		W #= 2 #<=> Vert,
		Horiz #\ Vert,
		X1 #= X+Horiz,
		Y1 #= Y+Vert,
		X2 #= X1+Horiz,
		Y2 #= Y1+Vert.
	ship(4, NCols, NRows, XDom2, YDom2, [4-ship(X,W,Y,H)|Ships], Ships, [X-Y,X1-Y1,X2-Y2,X3-Y3|Coords], Coords) :-
		X in_set XDom2,
		Y in_set YDom2,
		W in {2,5},
		H in {2,5},
		X+W #=< NCols+2,
		Y+H #=< NRows+2,
		H #= 2 #<=> Horiz,
		W #= 2 #<=> Vert,
		Horiz #\ Vert,
		X1 #= X+Horiz,
		Y1 #= Y+Vert,
		X2 #= X1+Horiz,
		Y2 #= Y1+Vert,
		X3 #= X2+Horiz,
		Y3 #= Y2+Vert.

	tag([], _, []).
	tag([X|Xs], T, [T-X|Ys]) :-
		U is T+1,
		tag(Xs, U, Ys).

	order_groups([]).
	order_groups([_-Group|Groups]) :-
		origins(Group, Origs),
		lex_chain(Origs, [op(#<)]),
		order_groups(Groups).

	origins([], []).
	origins([ship(X,_,Y,_)|Ships], [[X,Y]|Origs]) :-
		origins(Ships, Origs).

	ships_vars([]) --> [].
	ships_vars([ship(X,W,Y,H)|Ships]) --> [W,H,X,Y],
		ships_vars(Ships).

	draw(Ships, NRows, NCols) :-
		format('+~*c+\n', [NCols,0'-]),
		draw_lines(0, NRows, NCols, Ships),
		format('+~*c+\n', [NCols,0'-]).

	draw_lines(NR, NR, _, _) :- !.
	draw_lines(I,  NR, NC, Ships) :-
		R is I+1,
		ascii_line(0, NC, R, Ships, String, "|\n"),
		format([0'||String], []),
		draw_lines(R, NR, NC, Ships).

	ascii_line(NC, NC, _, _) --> !.
	ascii_line(I, NC, R, Ships) -->
		{J is I+1},
		ascii_cell(R, J, Ships),
		ascii_line(J, NC, R, Ships).

	ascii_cell(R, J, Ships) -->
		{Template = ship(X,W,Y,H),
		 member(Template, Ships),
		 X=<J, J<X+W-1,
		 Y=<R, R<Y+H-1}, !,
		"#".
	ascii_cell(_, _, _) --> " ".

	% A couple of items from Moshe Rubin's
	% "List of unsolvable Solitaire Battleship boards"

	:- public(data/3).
	:- dynamic(data/3).

	data(id113, % said to have 70 solutions
		 [2,4,3,3,2,4,1,1,0,0],
		 [0,5,0,2,2,3,1,3,2,2],
		 [4,3,3,2,2,2,1,1,1,1]).
	data(id1337, % said to have 49874 solutions
		 [1,3,2,2,2,2,2,3,1,2],
		 [3,0,4,0,3,0,3,1,2,4],
		 [4,3,3,2,2,2,1,1,1,1]).
	data(id2794, % said to have 1 solution
		 [0,0,1,4,1,5,2,1,6,0],
		 [1,3,3,1,1,4,0,1,1,5],
		 [4,3,3,2,2,2,1,1,1,1]).

:- end_object.

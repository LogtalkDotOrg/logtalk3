%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(meta,
	implements(metap)).

	:- info([
		version is 4.0,
		date is 2011/01/18,
		author is 'Paulo Moura',
		comment is 'Some useful meta-predicates.'
	]).

	:- alias(metap, map/2, succeeds/2).
	:- alias(metap, include/3, filter/3).
	:- alias(metap, fold_left/4, foldl/4).
	:- alias(metap, fold_right/4, foldr/4).
	:- alias(metap, scan_left/4, scanl/4).
	:- alias(metap, scan_right/4, scanr/4).

	:- meta_predicate(include_(*, 1, *)).
	include_([], _, []).
	include_([Arg| Args], Closure, Included) :-
		(	call(Closure, Arg) ->
			Included = [Arg| Rest]
		;	Included = Rest
		),
		include_(Args, Closure, Rest).

	:- meta_predicate(include(1, *, *)).
	include(Closure, List, Included) :-
		include_(List, Closure, Included).

	:- meta_predicate(filter(1, *, *)).
	filter(Closure, List, Included) :-
		include_(List, Closure, Included).

	:- meta_predicate(exclude_(*, 1, *)).
	exclude_([], _, []).
	exclude_([Arg| Args], Closure, Excluded) :-
		(	call(Closure, Arg) ->
			Excluded = Rest
		;	Excluded = [Arg| Rest]
		),
		exclude_(Args, Closure, Rest).

	:- meta_predicate(exclude(1, *, *)).
	exclude(Closure, List, Excluded) :-
		exclude_(List, Closure, Excluded).

	:- meta_predicate(findall_member_(*, *, 0, *)).
	findall_member_([], _, _, []).
	findall_member_([Head| Tail], Member, Test, Result) :-
		\+ (Head = Member, call(Test)),
		!,
	    findall_member_(Tail, Member, Test, Result).
	findall_member_([Head| Tail], Member, Test, [Head| Result]) :-
	    findall_member_(Tail, Member, Test, Result).

	:- meta_predicate(findall_member(*, *, 0, *)).
	findall_member(Member, List, Test, Result) :-
		findall_member_(List, Member, Test, Result).

	:- meta_predicate(findall_member_(*, *, 0, *, *)).
	findall_member_([], _, _, Result, Result).
	findall_member_([Head| Tail], Member, Test, Result0, Result) :-
	    \+ (Head = Member, call(Test)),
	    !,
	    findall_member_(Tail, Member, Test, Result0, Result).
	findall_member_([Head| Tail], Member, Test, [Head| Result0], Result) :-
	    findall_member_(Tail, Member, Test, Result0, Result).

	:- meta_predicate(findall_member(*, *, 0, *, *)).
	findall_member(Member, List, Test, Result, Tail) :-
		findall_member_(List, Member, Test, Result, Tail).

	:- meta_predicate(partition_(*, 1, *, *)).
	partition_([], _, [], []).
	partition_([Arg| Args], Closure, Included, Excluded) :-
		(   call(Closure, Arg) ->
			Included = [Arg| RestIncluded],
			Excluded = RestExcluded
		;	Included = RestIncluded,
			Excluded = [Arg| RestExcluded]
		),
		partition_(Args, Closure, RestIncluded, RestExcluded).

	:- meta_predicate(partition(1, *, *, *)).
	partition(Closure, List, Included, Excluded) :-
		partition_(List, Closure, Included, Excluded).

	:- meta_predicate(partition(*, 3, *, *, *, *)).
	partition_([], _, _, [], [], []).
	partition_([X| Xs], Closure, Y, Less, Equal, Greater) :-
		call(Closure, Order, X, Y),
		partition_(Order, X, Xs, Closure, Y, Less, Equal, Greater).

	partition_(<, X, Xs, Closure, Y, [X| Less], Equal, Greater) :-
		partition_(Xs, Closure, Y, Less, Equal, Greater).
	partition_(=, X, Xs, Closure, Y, Less, [X| Equal], Greater) :-
		partition_(Xs, Closure, Y, Less, Equal, Greater).
	partition_(>, X, Xs, Closure, Y, Less, Equal, [X| Greater]) :-
		partition_(Xs, Closure, Y, Less, Equal, Greater).

	:- meta_predicate(partition(3, *, *, *, *, *)).
	partition(Closure, List, Value, Less, Equal, Greater) :-
		partition_(List, Closure, Value, Less, Equal, Greater).

	:- meta_predicate(fold_left_(*, 3, *, *)).
	fold_left_([], _, Result, Result).
	fold_left_([Arg| Args], Closure, Acc, Result) :-
		call(Closure, Acc, Arg, Acc2),
		fold_left_(Args, Closure, Acc2, Result).

	:- meta_predicate(fold_left(3, *, *, *)).
	fold_left(Closure, Acc, List, Result) :-
		fold_left_(List, Closure, Acc, Result).

	:- meta_predicate(foldl(3, *, *, *)).
	foldl(Closure, Acc, List, Result) :-
		fold_left_(List, Closure, Acc, Result).

	:- meta_predicate(scan_left_(*, 3, *, *)).
	scan_left_([], _, Result, [Result]).
	scan_left_([Arg| Args], Closure, Acc, [Acc| Results]) :-
		call(Closure, Acc, Arg, Acc2),
		scan_left_(Args, Closure, Acc2, Results).

	:- meta_predicate(scan_left(3, *, *, *)).
	scan_left(Closure, Acc, List, Results) :-
		scan_left_(List, Closure, Acc, Results).

	:- meta_predicate(scanl(3, *, *, *)).
	scanl(Closure, Acc, List, Results) :-
		scan_left_(List, Closure, Acc, Results).

	:- meta_predicate(fold_right_(*, 3, *, *)).
	fold_right_([], _, Result, Result).
	fold_right_([Arg| Args], Closure, Acc, Result) :-
		fold_right_(Args, Closure, Acc, Acc2),
		call(Closure, Arg, Acc2, Result).

	:- meta_predicate(fold_right(3, *, *, *)).
	fold_right(Closure, Acc, List, Result) :-
		fold_right_(List, Closure, Acc, Result).

	:- meta_predicate(foldr(3, *, *, *)).
	foldr(Closure, Acc, List, Result) :-
		fold_right_(List, Closure, Acc, Result).

	:- meta_predicate(scan_right_(*, 3, *, *)).
	scan_right_([], _, Result, [Result]).
	scan_right_([Arg| Args], Closure, Acc, [Result, Acc2| Results]) :-
		scan_right_(Args, Closure, Acc, [Acc2| Results]),
		call(Closure, Arg, Acc2, Result).

	:- meta_predicate(scan_right(3, *, *, *)).
	scan_right(Closure, Acc, List, Results) :-
		scan_right_(List, Closure, Acc, Results).

	:- meta_predicate(scanr(3, *, *, *)).
	scanr(Closure, Acc, List, Results) :-
		scan_right_(List, Closure, Acc, Results).

	:- meta_predicate(map_(*, 1)).
	map_([], _).
	map_([Head| Tail], Closure) :-
		call(Closure, Head),
		map_(Tail, Closure).

	:- meta_predicate(map(1, *)).
	map(Closure, List) :-
		map_(List, Closure).

	:- meta_predicate(succeeds(1, *)).
	succeeds(Closure, List) :-
		map_(List, Closure).

	:- meta_predicate(map_(*, 2, *)).
	map_([], _, []).
	map_([A| As], Closure, [B| Bs]) :-
		call(Closure, A, B),
		map_(As, Closure, Bs).

	:- meta_predicate(map(2, *, *)).
	map(Closure, As, Bs) :-
		map_(As, Closure, Bs).

	:- meta_predicate(map_(*, 3, *, *)).
	map_([], _, [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs]) :-
		call(Closure, A, B, C),
		map_(As, Closure, Bs, Cs).

	:- meta_predicate(map(3, *, *, *)).
	map(Closure, As, Bs, Cs) :-
		map_(As, Closure, Bs, Cs).

	:- meta_predicate(map_(*, 4, *, *, *)).
	map_([], _, [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds]) :-
		call(Closure, A, B, C, D),
		map_(As, Closure, Bs, Cs, Ds).

	:- meta_predicate(map(4, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds) :-
		map_(As, Closure, Bs, Cs, Ds).

	:- meta_predicate(map_(*, 5, *, *, *, *)).
	map_([], _, [], [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds], [E| Es]) :-
		call(Closure, A, B, C, D, E),
		map_(As, Closure, Bs, Cs, Ds, Es).

	:- meta_predicate(map(5, *, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds, Es) :-
		map_(As, Closure, Bs, Cs, Ds, Es).

	:- meta_predicate(map_(*, 6, *, *, *, *, *)).
	map_([], _, [], [], [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds], [E| Es], [F| Fs]) :-
		call(Closure, A, B, C, D, E, F),
		map_(As, Closure, Bs, Cs, Ds, Es, Fs).

	:- meta_predicate(map(6, *, *, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds, Es, Fs) :-
		map_(As, Closure, Bs, Cs, Ds, Es, Fs).

	:- meta_predicate(map_(*, 7, *, *, *, *, *, *)).
	map_([], _, [], [], [], [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds], [E| Es], [F| Fs], [G| Gs]) :-
		call(Closure, A, B, C, D, E, F, G),
		map_(As, Closure, Bs, Cs, Ds, Es, Fs, Gs).

	:- meta_predicate(map(7, *, *, *, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds, Es, Fs, Gs) :-
		map_(As, Closure, Bs, Cs, Ds, Es, Fs, Gs).

	:- meta_predicate(map_reduce_(*, 2, 3, *, *)).
	map_reduce_([], _, _, Result, Result).
	map_reduce_([Arg| Args], Map, Reduce, Acc, Result) :-
		call(Map, Arg, Arg2),
		call(Reduce, Acc, Arg2, Acc2),
		map_reduce_(Args, Map, Reduce, Acc2, Result).

	:- meta_predicate(map_reduce(2, 3, *, *, *)).
	map_reduce(Map, Reduce, Acc, List, Result) :-
		map_reduce_(List, Map, Reduce, Acc, Result).

:- end_object.

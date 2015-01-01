%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
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



:- object(loop,
	implements(loopp)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/2/16,
		comment is 'Loop control structures predicates.'
	]).

	:- meta_predicate(whiledo(0, 0)).
	whiledo(Condition, Action) :-
		(	call(Condition) ->
			\+ \+ call(Action),
			whiledo(Condition, Action)
		;	true
		).

	:- meta_predicate(dowhile(0, 0)).
	dowhile(Action, Condition) :-
		\+ \+ call(Action),
		whiledo(Condition, Action).

	:- meta_predicate(foreach(*, *, 0)).
	foreach(Count, List, Goal) :-
		foreach_inv(List, Count, Goal).

	foreach_inv([], _, _).
	foreach_inv([Element| List], Count, Goal) :-
		\+ \+ (Count = Element, call(Goal)),
		foreach_inv(List, Count, Goal).

	:- meta_predicate(forto_aux(*, *, *, *, 0)).
	forto_aux(Count, First, Last, Increment, Goal) :-
		(	First =< Last ->
			\+ \+ (Count = First, call(Goal)),
			Next is First + Increment,
			forto_aux(Count, Next, Last, Increment, Goal)
		;	true
		).

	:- meta_predicate(forto(*, *, 0)).
	forto(FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		forto_aux(_, First, Last, 1, Goal).

	:- meta_predicate(forto(*, *, *, 0)).
	forto(Count, FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		forto_aux(Count, First, Last, 1, Goal).

	:- meta_predicate(forto(*, *, *, *, 0)).
	forto(Count, FirstExp, LastExp, IncrementExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		Increment is abs(IncrementExp),
		forto_aux(Count, First, Last, Increment, Goal).

	:- meta_predicate(fordownto_aux(*, *, *, *, 0)).
	fordownto_aux(Count, First, Last, Decrement, Goal) :-
		(	First >= Last ->
			\+ \+ (Count = First, call(Goal)),
			Next is First - Decrement,
			fordownto_aux(Count, Next, Last, Decrement, Goal)
		;	true
		).

	:- meta_predicate(fordownto(*, *, 0)).
	fordownto(FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		fordownto_aux(_, First, Last, 1, Goal).

	:- meta_predicate(fordownto(*, *, *, 0)).
	fordownto(Count, FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		fordownto_aux(Count, First, Last, 1, Goal).

	:- meta_predicate(fordownto(*, *, *, *, 0)).
	fordownto(Count, FirstExp, LastExp, DecrementExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		Decrement is abs(DecrementExp),
		fordownto_aux(Count, First, Last, Decrement, Goal).

:- end_object.

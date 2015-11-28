%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(countries).

	:- info([
		version is 1.0,
		date is 2009/11/28,
		author is 'Paulo Moura',
		comment is 'Simple example of using lambda expressions to simplify setof/3 and similar calls.'
	]).

	:- public(currencies_wrong/1).
	currencies_wrong(Currencies) :-
		setof(Currency, country(_, _, _, Currency), Currencies).

	:- public(currencies_no_lambda/1).
	currencies_no_lambda(Currencies) :-
		setof(Currency, Country^Capital^Population^country(Country, Capital, Population, Currency), Currencies).

	% adapted from a Ulrich Neumerkel's lambda proposal example
	:- public(currencies_lambda/1).
	currencies_lambda(Currencies) :-
		setof(Currency, {Currency}/country(_, _, _, Currency), Currencies).

	% country(Country, Capital, Population, Currency)
	country(portugal, lisbon, 10, euro).
	country(spain, madrid, 46, euro).
	country(england, london, 51, pound_sterling).
	country(malaysia, kuala_lumpur, 28, ringgit).
	country(iraq, baghdad, 31, dinar).
	country(tunisia, tunis, 10, dinar).

:- end_object.



:- object(sigma).

	:- info([
		version is 1.0,
		author is 'Artur Miguel Dias',
		date is 2009/12/04,
		comment is 'Generic sum predicate for testing lambda expressions.'
	]).

	% note that this is simple an example of using lambda expressions; Logtalk
	% already includes a fold_left/4 predicate in its library that could
	% easily be used for performing the same computations

	:- public(sum/4).
	:- meta_predicate(sum(2, *, *, *)).
	:- meta_predicate(sum(2, *, *, *, *)).

	sum(Closure, Inf, Sup, Result) :-
		sum(Closure, Inf, Sup, 0, Result).

	sum(Closure, Inf, Sup, Acc, Result) :-
		(	Inf =< Sup ->
			call(Closure, Inf, Sum),
			Acc2 is Acc + Sum,
			Next is Inf + 1,
			sum(Closure, Next, Sup, Acc2, Result)
		;	Result = Acc
		).

:- end_object.



:- object(misc).

	:- info([
		version is 1.1,
		date is 2009/12/5,
		author is 'Paulo Moura',
		comment is 'Some miscellaneous tests for lambda expressions collected from public forums.'
	]).

	% adapted from a Richard O'Keefe example
	:- public(common_prefix/3).
	common_prefix(Front, Xs, Ys) :-
		meta::map({Front}/(list::append(Front)), Xs, Ys).

	% an alternative definition
	%
	%:- uses(meta, [map/3]).
	%:- uses(list, [append/3]).
	%
	%common_prefix(Front, Xs, Ys) :-
	%	map({Front}/append(Front), Xs, Ys).

	% adapted from a Ulrich Neumerkel's lambda proposal example
	:- public(call_n/0).
	call_n :-
		f(X, Y),
		write('This test should print '), write(f(X, Y)), write(' in all lines:'), nl,
		call(f, A1, A2), write(f(A1, A2)), nl,
		call([X]>>f(X), B1, B2), write(f(B1, B2)), nl,
		call([X,Y]>>f(X,Y), C1, C2), write(f(C1, C2)), nl,
		call([X]>>({X}/[Y]>>f(X,Y)), D1, D2), write(f(D1, D2)), nl,
		call(call(f, E1), E2), write(f(E1, E2)), nl,
		call(f(F1), F2), write(f(F1, F2)), nl.

	f(x, y).

	:- public((local)/0).	% use ()'s in order to avoid a XSB parser bug
	local :-
		integer::sequence(1, 100, List),
		meta::map([X]>>less(0,X),List).

	less(X, Y) :-
		X < Y.

:- end_object.



:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == b; Dialect == swi; Dialect == xsb; Dialect == yap)
)).

	:- object(lambda_benchmarks).

		:- public([bench1/0, bench2/0]).

		% the time/1 predicate is a library predicate in SWI-Prolog
		% and a built-in predicate in B-Prolog, XSB and YAP
		:- if(current_logtalk_flag(prolog_dialect, swi)).
			:- use_module(prolog_statistics, [time/1]).
		:- endif.

		:- uses(integer, [between/3, sequence/3]).
		:- uses(meta, [map/2, map/3]).

		bench1 :-
			sequence(1, 100000, List),
			write('Using map/2 with a closure for testing less(0, X) with X in [1..100000]: '), nl,
			time(map(less(0), List)),
			write('Using map/2 with a lambda for testing less(0, X) with X in [1..100000]:  '), nl,
			time(map([X]>>less(0,X), List)).

		less(X, Y) :-
			X < Y.

		% the second benchmark is based on code posted by Jan Wielemaker in the SWI-Prolog mailing list:

		bench2 :-
			sequence(1, 100000, List),
			(   (   write('Adding 1 to every integer in the list [1..100000] using a local add1/2 predicate:'), nl,
					time(add1(List, _))
				;	write('Adding 1 to every integer in the list [1..100000] using map/3 with the integer::plus/3 predicate:'), nl,
					time(sum(List, _))
				;   write('Adding 1 to every integer in the list [1..100000] using map/3 with a lambda argument with a is/2 goal:'), nl,
					time(map([X,Y]>>{Y is X+1}, List, _))
				),
				fail
			;   true
		).

		sum(List1, List2) :-
			map(integer::plus(1), List1, List2).

		add1([], []).
		add1([H0| T0], [H| T]) :-
			H is H0 + 1,
			add1(T0, T).

	:- end_object.

:- endif.



:- object(warnings).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/04/15,
		comment is 'Example for illustrating lambda compilation warnings.']).

	% lambda expression with unclassified variables
	foo(C1, C2, C3) :-
		g(Z),
		call([X,Y]>>f(X,Y,Z), C1, C2, C3).

	% lambda expression with mixed-up variables
	bar(C1, C2, C3) :-
		call({Z}/[X,Y,Z]>>f(X,Y,Z), C1, C2, C3).

	f(1, 2, 3).

	g(4).

:- end_object.

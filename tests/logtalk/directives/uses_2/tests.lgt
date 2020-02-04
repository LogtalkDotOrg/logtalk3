%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(uses_2_test_object_1).

	:- public(p/1).
	p(1).

	:- public(q/1).
	q(2).

	:- public(r/2).
	r(1, one).
	r(2, two).

	:- public(s/2).
	s(1, a). s(1, b). s(1, c).
	s(2, x). s(2, y). s(2, z).

:- end_object.



% test object for calling user-defined predicates in "user"
% it requires a backend Prolog compiler that supports the
% definition of meta-predicates in plain Prolog

:- object(uses_2_test_object_2).

	:- uses(user, [foo/1]).

	:- public(p/1).
	p(X) :-
		foo(X).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == eclipse; Dialect == sicstus; Dialect = swi; Dialect = yap)
	)).

	:- uses(user, [bar/1]).

	:- public(mp/1).
	mp(X) :-
		bar(l(X)).

	l(2).

	:- endif.

:- end_object.



% test entities for using a parametric variable in the directive

:- protocol(foo).

	:- public([q/1, g//1]).

:- end_protocol.


:- object(bar,
	implements(foo)).

	q(bar).

	g(X) --> [X].

:- end_object.


:- object(baz,
	implements(foo)).

	q(baz).

	g(Y) --> [X], {Y is X * 2}.

:- end_object.


:- object(foo(_Object_)).

	:- uses(_Object_, [q/1, g//1]).

	:- public(p/1).
	p(X) :- q(X).

	:- public(r/1).
	r(X) :- phrase(g(X), [1]).

:- end_object.


:- object(bar(_Object_)).

	:- uses(_Object_, [r/1]).

	:- public(q/1).
	q(X) :- r(X).


:- end_object.


:- object(baz(_Object_)).

	:- uses(bar(_Object_), [q/1]).

	:- public(p/1).
	p(X) :- q(X).

:- end_object.


% tests

:- object(tests(_Index_),
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2020-01-23,
		comment is 'Unit tests for the uses/2 built-in directive.'
	]).

	:- uses(uses_2_test_object_1, [
		p/1, q/1::qq/1, r(1, Atom) as r(Atom), s(_Index_, Value) as s(Value)
	]).

	test(uses_2_01, true(X == 1)) :-
		p(X).

	test(uses_2_02, true(X == 2)) :-
		qq(X).

	test(uses_2_03, true(Xs == [one])) :-
		findall(X, r(X), Xs).

	test(uses_2_04, true(Xs == [x,y,z])) :-
		findall(X, s(X), Xs).

	test(uses_2_05, true(X == 1)) :-
		uses_2_test_object_2::p(X).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == eclipse; Dialect == sicstus; Dialect = swi; Dialect = yap)
	)).

		test(uses_2_06, true(X == 2)) :-
			uses_2_test_object_2::mp(X).

	:- else.

		- test(uses_2_06, true).

	:- endif.

	test(uses_2_07, true(X == bar)) :-
		foo(bar)::p(X).

	test(uses_2_08, true(X == baz)) :-
		foo(baz)::p(X).

	test(uses_2_09, true(X == 1)) :-
		foo(bar)::r(X).

	test(uses_2_10, true(X == 2)) :-
		foo(baz)::r(X).

	test(uses_2_11, true(L == [1,2,3])) :-
		findall(X, baz(user)::p(X), L).

:- end_object.

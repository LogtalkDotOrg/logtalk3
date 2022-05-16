%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		(	Dialect == eclipse; Dialect == lvm; Dialect == sicstus;
			Dialect = swi; Dialect = trealla; Dialect = yap
		)
	)).

	:- uses(user, [bar/1]).

	:- public(mp/1).
	mp(X) :-
		bar(l(X)).

	l(2).

	:- endif.

:- end_object.


% test entity for the database and reflection built-in methods

:- object(dyn).

	:- public(d1/1).
	:- dynamic(d1/1).
	d1(0).

	:- public(d2/1).
	:- dynamic(d2/1).
	d2(0).

	:- public(d3/1).
	:- dynamic(d3/1).
	d3(0).

:- end_object.


% test object for handling dynamic predicates listed
% in uses/2 directives with runtime resolving

:- object(uses_2_test_object_3(_Object_)).

	:- uses(_Object_, [d1/1, d2/1, d3/1]).

	:- public(cp/1).
	cp(X) :-
		current_predicate(X).

	:- public(pp/2).
	pp(X, P) :-
		predicate_property(X, P).

	:- public(p/1).
	p(X) :-
		clause(X, _).

	:- public(q/1).
	q(X) :-
		assertz(X).

	:- public(r/1).
	r(X) :-
		retract(X).

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

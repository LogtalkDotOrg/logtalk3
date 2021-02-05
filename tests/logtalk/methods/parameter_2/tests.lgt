%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(parameter_test_object_1(_)).

	:- public(p/1).
	p(X) :-
		parameter(1, X).

	:- public(p/0).
	p :-
		parameter(1, a).

:- end_object.


:- object(parameter_test_object_2(_)).

	:- public(q/1).
	q(X) :-
		parameter(1, X).

:- end_object.


:- object(parameter_test_object_12(X, Y),
	extends((parameter_test_object_1(X), parameter_test_object_2(Y)))).

	:- public(r/2).
	r(X, Y) :-
		parameter(1, X),
		parameter(2, Y).

:- end_object.


:- category(parameter_test_category(_)).

	:- public(s/1).
	s(X) :-
		parameter(1, X).

:- end_category.


:- object(parameter_test_object(X),
	imports(parameter_test_category(X))).

	:- public(t/1).
	t(X) :-
		parameter(1, X).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2014-08-06,
		comment is 'Unit tests for the parameter/2 built-in method.'
	]).

	test(parameter_2_1) :-
		parameter_test_object_1(_)::p(X),
		var(X).

	test(parameter_2_2) :-
		parameter_test_object_1(1)::p(X),
		X == 1.

	test(parameter_2_3) :-
		parameter_test_object_1(X)::p,
		X == a.

	test(parameter_2_4) :-
		\+ parameter_test_object_1(b)::p.

	test(parameter_2_5) :-
		parameter_test_object_12(X, Y)::p(a),
		parameter_test_object_12(X, Y)::q(b),
		parameter_test_object_12(X, Y)::r(P1, P2),
		X == a, Y == b,
		P1 == a, P2 == b.

	test(parameter_2_6) :-
		parameter_test_object(X)::s(c),
		parameter_test_object(X)::t(P),
		X == c,
		P == c.

	test(parameter_2_7) :-
		parameter_2_multifile_test_object_1(foo)::p(Parameter),
		Parameter == 2.

:- end_object.

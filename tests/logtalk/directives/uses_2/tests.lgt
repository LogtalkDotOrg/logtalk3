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


:- object(uses_2_test_object_1).

	:- public(p/1).
	p(1).

	:- public(q/1).
	q(2).

:- end_object.



% test object for calling user-defined predicates in "user"
% it requires a back-end Prolog compiler that supports the
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



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/28,
		comment is 'Unit tests for the uses/2 built-in directive.'
	]).

	:- uses(uses_2_test_object_1, [p/1, q/1::r/1]).

	test(uses_2_1) :-
		p(X),
		X == 1.

	test(uses_2_2) :-
		r(X),
		X == 2.

	test(uses_2_3) :-
		uses_2_test_object_2::p(X),
		X == 1.

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == eclipse; Dialect == sicstus; Dialect = swi; Dialect = yap)
	)).

	test(uses_2_4) :-
		uses_2_test_object_2::mp(X),
		X == 2.

	:- endif.

:- end_object.

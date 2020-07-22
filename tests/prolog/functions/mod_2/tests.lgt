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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-07-22,
		comment is 'Unit tests for the ISO Prolog standard mod/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.1.7

	test(iso_mod_2_01, true(X == 1)) :-
		{X is mod(5, 2)}.

	test(iso_mod_2_02, true(X == 0)) :-
		{X is mod(0, 3+11)}.

	test(iso_mod_2_03, true(X == -1)) :-
		{X is mod(5,-2)}.

	test(iso_mod_2_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is mod(77, N)}.

	test(iso_mod_2_05, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is mod(Foo, 77)}.

	test(iso_mod_2_06, error(type_error(integer,7.5))) :-
		% try to delay the expected error to runtime
		{G = (_X is mod(7.5, 2)), call(G)}.

	test(iso_mod_2_07, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is mod(7, 0)), call(G)}.

	% tests from the Logtalk portability work

	test(lgt_mod_2_08, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is abs(Foo)}.

	test(lgt_mod_2_09, true(X == 1)) :-
		{X is mod(-5, 2)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

:- end_object.

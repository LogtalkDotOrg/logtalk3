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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2022-03-25,
		comment is 'Unit tests for the ISO Prolog standard (-)/1 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_minus_1_01, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is - N}.

	test(lgt_minus_1_02, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is - Foo}.

	test(lgt_minus_1_03, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is - Foo}.

	test(lgt_minus_1_04, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is - Foo}.

	test(lgt_minus_1_05, true, [condition(current_prolog_flag(bounded,true))]) :-
		% try to delay any error to runtime
		current_prolog_flag(max_integer, Max),
		{catch(X is - Max, error(Error, _), Error == evaluation_error(int_overflow))},
		(	var(Error) ->
			X < 0
		;	true
		).

	test(lgt_minus_1_06, true((N == 42, M == -42))) :-
		{N = 42, M is - N}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.

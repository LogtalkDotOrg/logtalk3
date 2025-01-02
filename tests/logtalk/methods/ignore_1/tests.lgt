%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-10-20,
		comment is 'Unit tests for the ignore/1 built-in method.'
	]).

	% ignore/1 calls are expanded and thus the error term is for call/1

	test(ignore_1_01, error(instantiation_error)) :-
		ignore(_).

	test(ignore_1_02, error(type_error(callable,1))) :-
		Goal = 1,
		ignore(Goal).

	% it's not always possible to decompile the actual call

	test(ignore_1_03, error(existence_error(procedure,_))) :-
		Goal = p(_),
		ignore(Goal).

	test(ignore_1_04, true) :-
		ignore(true).

	test(ignore_1_05, true) :-
		ignore(fail).

	% ignore/1 is opaque to cuts

	test(ignore_1_06, true(L == [1, 2, 3])) :-
		findall(X, ((X = 1; X =2; X = 3), ignore(!)), L).

	test(ignore_1_07, true(L == [1, 2, 3])) :-
		findall(X, ((X = 1; X =2; X = 3), ignore((true,!))), L).

	test(ignore_1_08, true(L == [1, 2, 3])) :-
		findall(X, ((X = 1; X =2; X = 3), ignore((true;!))), L).

	test(ignore_1_09, true(L == [1, 2, 3])) :-
		findall(X, ((X = 1; X =2; X = 3), ignore((fail;!))), L).

:- end_object.

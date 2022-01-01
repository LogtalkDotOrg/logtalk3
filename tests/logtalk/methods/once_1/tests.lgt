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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2020-10-20,
		comment is 'Unit tests for the once/1 built-in method.'
	]).

	% once/1 calls are expanded and thus the error term is for call/1

	test(once_1_01, error(instantiation_error)) :-
		once(_).

	test(once_1_02, error(type_error(callable,1))) :-
		Goal = 1,
		once(Goal).

	% it's not always possible to decompile the actual call

	test(once_1_03, error(existence_error(procedure,_))) :-
		Goal = p(_),
		once(Goal).

	test(once_1_04, true) :-
		once(!).

	test(once_1_05, true(Xs == [1])) :-
		findall(X, once(a(X)), Xs).

	test(once_1_06, true(Xs == [1, 0])) :-
		findall(X, (once(a(X)); X = 0), Xs).

	test(once_1_07, variant(Xs, [_, 1, 2, 3])) :-
		findall(X, (once(!); a(X)), Xs).

	test(once_1_08, true(Xs == [1, 2, 3])) :-
		findall(X, (once((!, fail)); a(X)), Xs).

	test(once_1_09, false) :-
		% avoid a warning about a no matching clause for goal a(4)
		% by delaying the argument instantiation to runtime
		N = 4,
		once(a(N)).

	test(once_1_10, false) :-
		once(fail).

	% once/1 is opaque to cuts

	test(once_1_11, true(L == [1, 2, 3])) :-
		findall(X, ((X = 1; X =2; X = 3), once(!)), L).

	test(once_1_12, true(L == [1, 2, 3])) :-
		findall(X, ((X = 1; X =2; X = 3), once((true,!))), L).

	test(once_1_13, true(L == [1, 2, 3])) :-
		findall(X, ((X = 1; X =2; X = 3), once((true;!))), L).

	% auxiliary predicates

	a(1).
	a(2).
	a(3).

:- end_object.

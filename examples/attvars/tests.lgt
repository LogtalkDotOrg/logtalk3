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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2021-12-08,
		comment is 'Unit tests for the "attvars" example.'
	]).

	:- if(current_object(domain)).

		test(attvars_01, false) :-
			domain::domain(X, [a,b]),
			X = c.

		test(attvars_02, true(X == a)) :-
			domain::domain(X, [a,b]),
			domain::domain(X, [a,c]).

		test(attvars_03, true(List == [a,c])) :-
			domain::domain(X, [a,b,c]),
			domain::domain(X, [a,c]),
			domain::domain(X, List).

	:- endif.

	:- if(current_object(domain(_))).

		test(attvars_04, false) :-
			domain(atom)::domain(X, [a,b]),
			X = c.

		test(attvars_05, true(X == 1)) :-
			domain(integer)::domain(X, [1,2]),
			domain(integer)::domain(X, [1,3]).

		test(attvars_06, true(List == [1,3])) :-
			domain(integer)::domain(X, [1,2,3]),
			domain(integer)::domain(X, [1,3]),
			domain(integer)::domain(X, List).

		test(attvars_07, true(Type-List == atom-[a, c])) :-
			domain(atom)::domain(X, [a,b,c]),
			domain(atom)::domain(X, [a,c]),
			domain(Type)::domain(X, List).

		test(attvars_08, true(Type-List == integer-[1, 3])) :-
			domain(integer)::domain(X, [1,2,3]),
			domain(integer)::domain(X, [1,3]),
			domain(Type)::domain(X, List).

	:- endif.

:- end_object.

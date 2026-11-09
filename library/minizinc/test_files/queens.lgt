%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(minizinc_queens,
	implements(minizinc_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'N-queens fixture exercising arrays, ``alldifferent``, and array comprehensions.'
	]).

	:- include('alldifferent.mzn').

	:- par(int, n, int(8)).

	:- array_var(range(int(1), id(n)), [range(int(1), id(n))], q).

	:- constraint(call(alldifferent, [id(q)])).
	:- constraint(call(alldifferent, [
		comprehension(array, op('+', index(id(q), [id(i)]), id(i)), [
			generator([i], range(int(1), id(n)), none)
		])
	])).
	:- constraint(call(alldifferent, [
		comprehension(array, op('-', index(id(q), [id(i)]), id(i)), [
			generator([i], range(int(1), id(n)), none)
		])
	])).

	:- solve(satisfy).

:- end_object.

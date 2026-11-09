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


:- object(minizinc_knapsack,
	implements(minizinc_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Knapsack optimization fixture exercising optimization, ``sum``, and an externally supplied parameter.'
	]).

	% the capacity parameter is declared without a value: it must be supplied
	% via the data(Path) or json_data(Pairs) solve/3 option
	:- par(int, capacity, none).

	:- array_par(int, [range(int(1), int(5))], weights, array([int(2), int(3), int(4), int(5), int(9)])).
	:- array_par(int, [range(int(1), int(5))], values, array([int(3), int(4), int(5), int(8), int(10)])).

	:- array_var(range(int(0), int(1)), [range(int(1), int(5))], take).

	:- constraint(op('<=',
		call(sum, [
			comprehension(array, op('*', index(id(weights), [id(i)]), index(id(take), [id(i)])), [
				generator([i], range(int(1), int(5)), none)
			])
		]),
		id(capacity)
	)).

	:- solve(maximize(call(sum, [
		comprehension(array, op('*', index(id(values), [id(i)]), index(id(take), [id(i)])), [
			generator([i], range(int(1), int(5)), none)
		])
	]))).

:- end_object.

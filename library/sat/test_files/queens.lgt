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


% Four-queens CNF encoding using queen(Row, Column) as the propositional
% variable identity. One "at least one per row" clause per row, and "at
% most one" clauses for every row, column, and diagonal pair.

:- object(sat_queens,
	implements(sat_problem_protocol)).

	:- comment('four-queens: at least one queen per row').
	clause([queen(1,1), queen(1,2), queen(1,3), queen(1,4)]).
	clause([queen(2,1), queen(2,2), queen(2,3), queen(2,4)]).
	clause([queen(3,1), queen(3,2), queen(3,3), queen(3,4)]).
	clause([queen(4,1), queen(4,2), queen(4,3), queen(4,4)]).

	:- comment('at most one queen per row').
	clause([neg(queen(1,1)), neg(queen(1,2))]).
	clause([neg(queen(1,1)), neg(queen(1,3))]).
	clause([neg(queen(1,1)), neg(queen(1,4))]).
	clause([neg(queen(1,2)), neg(queen(1,3))]).
	clause([neg(queen(1,2)), neg(queen(1,4))]).
	clause([neg(queen(1,3)), neg(queen(1,4))]).
	clause([neg(queen(2,1)), neg(queen(2,2))]).
	clause([neg(queen(2,1)), neg(queen(2,3))]).
	clause([neg(queen(2,1)), neg(queen(2,4))]).
	clause([neg(queen(2,2)), neg(queen(2,3))]).
	clause([neg(queen(2,2)), neg(queen(2,4))]).
	clause([neg(queen(2,3)), neg(queen(2,4))]).
	clause([neg(queen(3,1)), neg(queen(3,2))]).
	clause([neg(queen(3,1)), neg(queen(3,3))]).
	clause([neg(queen(3,1)), neg(queen(3,4))]).
	clause([neg(queen(3,2)), neg(queen(3,3))]).
	clause([neg(queen(3,2)), neg(queen(3,4))]).
	clause([neg(queen(3,3)), neg(queen(3,4))]).
	clause([neg(queen(4,1)), neg(queen(4,2))]).
	clause([neg(queen(4,1)), neg(queen(4,3))]).
	clause([neg(queen(4,1)), neg(queen(4,4))]).
	clause([neg(queen(4,2)), neg(queen(4,3))]).
	clause([neg(queen(4,2)), neg(queen(4,4))]).
	clause([neg(queen(4,3)), neg(queen(4,4))]).

	:- comment('at most one queen per column').
	clause([neg(queen(1,1)), neg(queen(2,1))]).
	clause([neg(queen(1,1)), neg(queen(3,1))]).
	clause([neg(queen(1,1)), neg(queen(4,1))]).
	clause([neg(queen(2,1)), neg(queen(3,1))]).
	clause([neg(queen(2,1)), neg(queen(4,1))]).
	clause([neg(queen(3,1)), neg(queen(4,1))]).
	clause([neg(queen(1,2)), neg(queen(2,2))]).
	clause([neg(queen(1,2)), neg(queen(3,2))]).
	clause([neg(queen(1,2)), neg(queen(4,2))]).
	clause([neg(queen(2,2)), neg(queen(3,2))]).
	clause([neg(queen(2,2)), neg(queen(4,2))]).
	clause([neg(queen(3,2)), neg(queen(4,2))]).
	clause([neg(queen(1,3)), neg(queen(2,3))]).
	clause([neg(queen(1,3)), neg(queen(3,3))]).
	clause([neg(queen(1,3)), neg(queen(4,3))]).
	clause([neg(queen(2,3)), neg(queen(3,3))]).
	clause([neg(queen(2,3)), neg(queen(4,3))]).
	clause([neg(queen(3,3)), neg(queen(4,3))]).
	clause([neg(queen(1,4)), neg(queen(2,4))]).
	clause([neg(queen(1,4)), neg(queen(3,4))]).
	clause([neg(queen(1,4)), neg(queen(4,4))]).
	clause([neg(queen(2,4)), neg(queen(3,4))]).
	clause([neg(queen(2,4)), neg(queen(4,4))]).
	clause([neg(queen(3,4)), neg(queen(4,4))]).

	:- comment('at most one queen per diagonal').
	clause([neg(queen(1,1)), neg(queen(2,2))]).
	clause([neg(queen(1,1)), neg(queen(3,3))]).
	clause([neg(queen(1,1)), neg(queen(4,4))]).
	clause([neg(queen(1,2)), neg(queen(2,1))]).
	clause([neg(queen(1,2)), neg(queen(2,3))]).
	clause([neg(queen(1,2)), neg(queen(3,4))]).
	clause([neg(queen(1,3)), neg(queen(2,2))]).
	clause([neg(queen(1,3)), neg(queen(2,4))]).
	clause([neg(queen(1,3)), neg(queen(3,1))]).
	clause([neg(queen(1,4)), neg(queen(2,3))]).
	clause([neg(queen(1,4)), neg(queen(3,2))]).
	clause([neg(queen(1,4)), neg(queen(4,1))]).
	clause([neg(queen(2,1)), neg(queen(3,2))]).
	clause([neg(queen(2,1)), neg(queen(4,3))]).
	clause([neg(queen(2,2)), neg(queen(3,1))]).
	clause([neg(queen(2,2)), neg(queen(3,3))]).
	clause([neg(queen(2,2)), neg(queen(4,4))]).
	clause([neg(queen(2,3)), neg(queen(3,2))]).
	clause([neg(queen(2,3)), neg(queen(3,4))]).
	clause([neg(queen(2,3)), neg(queen(4,1))]).
	clause([neg(queen(2,4)), neg(queen(3,3))]).
	clause([neg(queen(2,4)), neg(queen(4,2))]).
	clause([neg(queen(3,1)), neg(queen(4,2))]).
	clause([neg(queen(3,2)), neg(queen(4,1))]).
	clause([neg(queen(3,2)), neg(queen(4,3))]).
	clause([neg(queen(3,3)), neg(queen(4,2))]).
	clause([neg(queen(3,3)), neg(queen(4,4))]).
	clause([neg(queen(3,4)), neg(queen(4,3))]).

:- end_object.

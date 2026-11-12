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


:- object(problog_annotated,
	implements(problog_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'ProbLog sample code adapted to Logtalk by Paulo Moura',
		date is 2026-11-12,
		comment is 'A coin annotated disjunction with shared variables in rules and queries.'
	]).

	0.3::coin(heads); 0.7::coin(tails).

	same_side(Side, Side) :-
		coin(Side).

	:- query(coin(heads)).
	:- query(same_side(Side, Side)).

:- end_object.

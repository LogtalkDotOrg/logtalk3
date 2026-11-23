%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(max_sat_example,
	implements(max_sat_problem_protocol)).

	% hard: exactly one of x1 and x2; soft: prefer x1 (3), x2 (2), x3 (1)
	% the optimum cost is 2 with x1 and x3 true and x2 false
	x1 ; x2.
	-x1 ; -x2.
	soft(3, x1).
	soft(2, x2).
	soft(1, x3).

:- end_object.


:- object(max_sat_vertex_cover,
	implements(max_sat_problem_protocol)).

	% weighted minimum vertex cover of a triangle: every edge must be covered
	% (hard clauses) and selecting a vertex costs its weight (soft clauses);
	% the optimum selects b and c at a cost of 3
	in(a) ; in(b).
	in(b) ; in(c).
	in(a) ; in(c).
	soft(3, -in(a)).
	soft(1, -in(b)).
	soft(2, -in(c)).

:- end_object.


:- object(max_sat_hard_unsat,
	implements(max_sat_problem_protocol)).

	p.
	-p.
	soft(1, q).

:- end_object.


:- object(max_sat_all_soft,
	implements(max_sat_problem_protocol)).

	% no hard clauses; the cost of the optimum is 1
	soft(2, p).
	soft(1, -p).

:- end_object.


:- object(max_sat_empty_clause_fixture,
	implements(max_sat_problem_protocol)).

	p.
	false.
	soft(2, false).

:- end_object.


:- object(max_sat_raw_fixture,
	implements(max_sat_problem_protocol)).

	:- raw('c trusted raw comment').
	:- comment('a readable comment').

:- end_object.


:- object(max_sat_unrelated_fixture).

	:- public(value/1).
	value(ordinary).

:- end_object.

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


:- object(wmc_example,
	implements(wmc_problem_protocol)).

	% weighted model count: 0.41
	x1 ; x2.
	-x1 ; x3.
	probability(x1, 0.3).
	probability(x2, 0.5).
	probability(x3, 0.2).

:- end_object.


:- object(wmc_independent_sets,
	implements(wmc_problem_protocol)).

	% unweighted model count: 5 independent sets of a path with three vertices
	-in(a) ; -in(b).
	-in(b) ; -in(c).

:- end_object.


:- object(wmc_alarm,
	implements(wmc_problem_protocol)).

	% the alarm sounds if there is a burglary or an earthquake;
	% the weighted model count is the total probability: 1.0
	-burglary ; alarm.
	-earthquake ; alarm.
	burglary ; earthquake ; -alarm.
	probability(burglary, 0.1).
	probability(earthquake, 0.2).

:- end_object.


:- object(wmc_alarm_evidence,
	implements(wmc_problem_protocol)).

	% same as wmc_alarm with evidence that the alarm sounded:
	% the weighted model count is the probability of the alarm: 1 - 0.9 * 0.8 = 0.28
	-burglary ; alarm.
	-earthquake ; alarm.
	burglary ; earthquake ; -alarm.
	alarm.
	probability(burglary, 0.1).
	probability(earthquake, 0.2).

:- end_object.


:- object(wmc_unsatisfiable,
	implements(wmc_problem_protocol)).

	p.
	-p.

:- end_object.


:- object(wmc_free_variables,
	implements(wmc_problem_protocol)).

	% variables that occur only in weights are free: (0.25 + 0.75) * (2 + 3) = 5.0
	weight(+a, 2).
	weight(-a, 3).
	probability(b, 0.25).

:- end_object.


:- object(wmc_raw_fixture,
	implements(wmc_problem_protocol)).

	:- raw('c trusted raw comment').
	:- comment('a readable comment').

:- end_object.


:- object(wmc_unrelated_fixture).

	:- public(value/1).
	value(ordinary).

:- end_object.

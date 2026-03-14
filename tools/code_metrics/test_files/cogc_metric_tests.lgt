%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(cogc_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-14,
		comment is 'Unit tests for the cogc_metric object.'
	]).

	:- uses(cogc_metric, [
		entity_score/2
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(cogc_metric).

	% cogc_obj_1: p/0 has 1 clause, no recursion => 0
	test(cogc_metric_01, true(Score == 0)) :-
		entity_score(cogc_obj_1, Score).

	% cogc_obj_2: p/1 has 3 clauses, no recursion => 3-1 = 2
	test(cogc_metric_02, true(Score == 2)) :-
		entity_score(cogc_obj_2, Score).

	% cogc_obj_3: p/1 has 2 clauses + direct recursion => (2-1)+1 = 2
	test(cogc_metric_03, true(Score == 2)) :-
		entity_score(cogc_obj_3, Score).

	% cogc_obj_4: a/0 (2 clauses, not recursive) => 1; b/0 (1 clause, recursive) => 1; total = 2
	test(cogc_metric_04, true(Score == 2)) :-
		entity_score(cogc_obj_4, Score).

	% cogc_cat_1: f/1 has 2 clauses, no recursion => 1
	test(cogc_metric_05, true(Score == 1)) :-
		entity_score(cogc_cat_1, Score).

	% protocol: entity_score/2 must fail
	test(cogc_metric_protocol, fail) :-
		entity_score(prot_a, _).

:- end_object.

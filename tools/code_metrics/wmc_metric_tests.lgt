%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
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


:- object(wmc_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Unit tests for the wmc_metric object.'
	]).

	:- uses(wmc_metric, [
		entity_score/2
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(wmc_metric).

	% lcom_obj_1 defines p/0, q/0, r/0 => 3
	test(wmc_metric_01, true(Score == 3)) :-
		entity_score(lcom_obj_1, Score).

	% lcom_obj_2 defines p/0, q/0, r/0, s/0 => 4
	test(wmc_metric_02, true(Score == 4)) :-
		entity_score(lcom_obj_2, Score).

	% lcom_obj_3 defines p/0 => 1
	test(wmc_metric_03, true(Score == 1)) :-
		entity_score(lcom_obj_3, Score).

	% lcom_cat_1 defines f/0, g/0 => 2
	test(wmc_metric_04, true(Score == 2)) :-
		entity_score(lcom_cat_1, Score).

	% expert_system defines begin/0, diagnose/1, ask/2, clear_facts/0 => 4
	test(wmc_metric_05, true(Score == 4)) :-
		entity_score(expert_system, Score).

	% protocol: entity_score/2 must fail
	test(wmc_metric_protocol, fail) :-
		entity_score(prot_a, _).

:- end_object.

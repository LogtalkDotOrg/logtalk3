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


:- object(lcom_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Unit tests for the lcom_metric object.'
	]).

	:- uses(lcom_metric, [
		entity_score/2
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(lcom_metric).

	% three predicates, all connected: p->q->r => 1 component
	test(lcom_metric_obj1, true(Score == lcom(1,3))) :-
		entity_score(lcom_obj_1, Score).

	% four predicates, two disjoint groups {p,q} and {r,s} => 2 components
	test(lcom_metric_obj2, true(Score == lcom(2,4))) :-
		entity_score(lcom_obj_2, Score).

	% single predicate, no calls => 1 component
	test(lcom_metric_obj3, true(Score == lcom(1,1))) :-
		entity_score(lcom_obj_3, Score).

	% category: f->g => 1 component, 2 predicates
	test(lcom_metric_cat1, true(Score == lcom(1,2))) :-
		entity_score(lcom_cat_1, Score).

	% protocol: entity_score/2 must fail
	test(lcom_metric_protocol, fail) :-
		entity_score(prot_a, _).

:- end_object.

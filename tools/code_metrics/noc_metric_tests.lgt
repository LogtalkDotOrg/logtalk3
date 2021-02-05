%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(noc_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0:11:1,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2020-04-13,
		comment is 'Unit tests for the entity number of clauses metric.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(noc_metric).

	:- uses(noc_metric, [entity_score/2]).

	test(noc_cat_a) :-
		entity_score(cat_a, Score),
		Score == number_of_clauses(0, 0).

	test(noc_cat_b) :-
		entity_score(cat_b, Score),
		Score == number_of_clauses(1, 1).

	test(noc_cat_c) :-
		entity_score(cat_c, Score),
		Score == number_of_clauses(1, 1).

	test(noc_cat_d) :-
		entity_score(cat_d, Score),
		Score == number_of_clauses(0, 0).

	test(noc_obj_e_wrong_clause) :-
		entity_score(obj_e, Score),
		Score \== number_of_clauses(2, 2).

	test(noc_obj_e) :-
		entity_score(obj_e, Score),
		Score == number_of_clauses(1, 1).

	test(noc_obj_d) :-
		entity_score(obj_d, Score),
		Score == number_of_clauses(2, 2).

	test(noc_obj_a) :-
		entity_score(obj_a, Score),
		Score == number_of_clauses(4, 4).

	test(noc_obj_b) :-
		entity_score(obj_b, Score),
		Score == number_of_clauses(1, 1).

	test(noc_obj_c) :-
		entity_score(obj_c, Score),
		Score == number_of_clauses(1, 1).

	test(noc_prot_a) :-
		entity_score(prot_a, Score),
		Score == number_of_clauses(0, 0).

	test(noc_prot_b) :-
		entity_score(prot_b, Score),
		Score == number_of_clauses(0, 0).

	test(noc_car) :-
		entity_score(car, Score),
		Score == number_of_clauses(0, 0).

	test(noc_vehicle) :-
		entity_score(vehicle, Score),
		Score == number_of_clauses(0, 0).

	test(noc_meta_vehicle) :-
		entity_score(meta_vehicle, Score),
		Score == number_of_clauses(0, 0).

	test(noc_herring) :-
		entity_score(herring, Score),
		Score == number_of_clauses(0, 0).

	test(noc_bird) :-
		entity_score(bird, Score),
		Score == number_of_clauses(0, 0).

:- end_object.

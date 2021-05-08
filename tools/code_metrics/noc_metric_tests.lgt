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
		version is 0:12:0,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2021-05-08,
		comment is 'Unit tests for the entity number of clauses metric.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(noc_metric).

	:- uses(noc_metric, [entity_score/2]).

	test(noc_cat_a, true(Score == number_of_clauses(0, 0))) :-
		entity_score(cat_a, Score).

	test(noc_cat_b, true(Score == number_of_clauses(1, 1))) :-
		entity_score(cat_b, Score).

	test(noc_cat_c, true(Score == number_of_clauses(1, 1))) :-
		entity_score(cat_c, Score).

	test(noc_cat_d, true(Score == number_of_clauses(0, 0))) :-
		entity_score(cat_d, Score).

	test(noc_obj_e_wrong_clause, true(Score \== number_of_clauses(2, 2))) :-
		entity_score(obj_e, Score).

	test(noc_obj_e, true(Score == number_of_clauses(1, 1))) :-
		entity_score(obj_e, Score).

	test(noc_obj_d, true(Score == number_of_clauses(2, 2))) :-
		entity_score(obj_d, Score).

	test(noc_obj_a, true(Score == number_of_clauses(4, 4))) :-
		entity_score(obj_a, Score).

	test(noc_obj_b, true(Score == number_of_clauses(1, 1))) :-
		entity_score(obj_b, Score).

	test(noc_obj_c, true(Score == number_of_clauses(1, 1))) :-
		entity_score(obj_c, Score).

	test(noc_prot_a, true(Score == number_of_clauses(0, 0))) :-
		entity_score(prot_a, Score).

	test(noc_prot_b, true(Score == number_of_clauses(0, 0))) :-
		entity_score(prot_b, Score).

	test(noc_car, true(Score == number_of_clauses(0, 0))) :-
		entity_score(car, Score).

	test(noc_vehicle, true(Score == number_of_clauses(0, 0))) :-
		entity_score(vehicle, Score).

	test(noc_meta_vehicle, true(Score == number_of_clauses(0, 0))) :-
		entity_score(meta_vehicle, Score).

	test(noc_herring, true(Score == number_of_clauses(0, 0))) :-
		entity_score(herring, Score).

	test(noc_bird, true(Score == number_of_clauses(0, 0))) :-
		entity_score(bird, Score).

:- end_object.

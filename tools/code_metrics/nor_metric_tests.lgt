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


:- object(nor_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0:3:0,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2021-05-08,
		comment is 'Unit tests for the entity number of rules metric.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(nor_metric).

	:- uses(nor_metric, [entity_score/2]).

	test(nor_cat_a, true(Score == number_of_rules(0, 0))) :-
		entity_score(cat_a, Score).

	test(nor_cat_b, true(Score == number_of_rules(1, 1))) :-
		entity_score(cat_b, Score).

	test(nor_cat_c, true(Score == number_of_rules(1, 1))) :-
		entity_score(cat_c, Score).

	test(nor_cat_d, true(Score == number_of_rules(0, 0))) :-
		entity_score(cat_d, Score).

	test(nor_obj_e_wrong_clause, true(Score \== number_of_rules(2, 2))) :-
		entity_score(obj_e, Score).

	test(nor_obj_e, true(Score == number_of_rules(1, 1))) :-
		entity_score(obj_e, Score).

	test(nor_obj_d, true(Score == number_of_rules(2, 2))) :-
		entity_score(obj_d, Score).

	test(nor_obj_a, true(Score == number_of_rules(3, 3))) :-
		entity_score(obj_a, Score).

	test(nor_obj_b, true(Score == number_of_rules(1, 1))) :-
		entity_score(obj_b, Score).

	test(nor_obj_c, true(Score == number_of_rules(1, 1))) :-
		entity_score(obj_c, Score).

	test(nor_prot_a, true(Score == number_of_rules(0, 0))) :-
		entity_score(prot_a, Score).

	test(nor_prot_b, true(Score == number_of_rules(0, 0))) :-
		entity_score(prot_b, Score).

	test(nor_car, true(Score == number_of_rules(0, 0))) :-
		entity_score(car, Score).

	test(nor_vehicle, true(Score == number_of_rules(0, 0))) :-
		entity_score(vehicle, Score).

	test(nor_meta_vehicle, true(Score == number_of_rules(0, 0))) :-
		entity_score(meta_vehicle, Score).

	test(nor_herring, true(Score == number_of_rules(0, 0))) :-
		entity_score(herring, Score).

	test(nor_bird, true(Score == number_of_rules(0, 0))) :-
		entity_score(bird, Score).

:- end_object.

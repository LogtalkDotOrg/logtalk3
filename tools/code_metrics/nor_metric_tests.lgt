%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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
		version is 0.1,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2018/07/16,
		comment is 'Unit tests for the entity number of rules metric.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(nor_metric).

	:- uses(nor_metric, [entity_score/2]).

	test(nor_cat_a) :-
		entity_score(cat_a, Score), 
		Score == number_of_rules(0, 0).

	test(nor_cat_b) :-
		entity_score(cat_b, Score),
		Score == number_of_rules(1, 1).

	test(nor_cat_c) :-
		entity_score(cat_c, Score),
		Score == number_of_rules(1, 1).

	test(nor_cat_d) :-
		entity_score(cat_d, Score),
		Score == number_of_rules(0, 0).

	test(nor_obj_e_wrong_clause) :-
		entity_score(obj_e, Score),
		\+ Score == number_of_rules(2, 2).

	test(nor_obj_e) :-
		entity_score(obj_e, Score),
		Score == number_of_rules(1, 1).

	test(nor_obj_d) :-
		entity_score(obj_d, Score),
		Score == number_of_rules(2, 2).

	test(nor_obj_a) :-
		entity_score(obj_a, Score),
		Score == number_of_rules(3, 3).

	test(nor_obj_b) :-
		entity_score(obj_b, Score),
		Score == number_of_rules(1, 1).

	test(nor_obj_c) :-
		entity_score(obj_c, Score),
		Score == number_of_rules(1, 1).

	test(nor_prot_a) :-
		entity_score(prot_a, Score),
		Score == number_of_rules(0, 0).

	test(nor_prot_b) :-
		entity_score(prot_b, Score),
		Score == number_of_rules(0, 0).

	test(nor_car) :-
		entity_score(car, Score),
		Score == number_of_rules(0, 0).

	test(nor_vehicle) :-
		entity_score(vehicle, Score),
		Score == number_of_rules(0, 0).

	test(nor_meta_vehicle) :-
		entity_score(meta_vehicle, Score),
		Score == number_of_rules(0, 0).

	test(nor_herring) :-
		entity_score(herring, Score),
		Score == number_of_rules(0, 0).

	test(nor_bird) :-
		entity_score(bird, Score),
		Score == number_of_rules(0, 0).

	% suppress all messages from the "code_metrics"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, code_metrics, _Tokens).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(dit_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0:5:1,
		author is 'Ebrahim Azarisooreh',
		date is 2020-04-13,
		comment is 'Unit tests for the depth of inheritance code metric.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(dit_metric).

	test(dit_reflexive_obj) :-
		depth_is(object, 2).

	test(dit_reflexive_class) :-
		depth_is(class, 1).

	test(dit_reflexive_abstract_class) :-
		depth_is(abstract_class, 3).

	test(dit_obj_a) :-
		depth_is(obj_a, 3).

	test(dit_obj_b) :-
		depth_is(obj_b, 4).

	test(dit_obj_c) :-
		depth_is(obj_c, 5).

	test(dit_obj_d) :-
		depth_is(obj_d, 1).

	test(dit_obj_e) :-
		depth_is(obj_e, 1).

	test(dit_obj_c_wrong_output) :-
		\+ depth_is(obj_c, 7).

	test(dit_cat_a) :-
		depth_is(cat_a, 2).

	test(dit_cat_b) :-
		depth_is(cat_b, 3).

	test(dit_cat_c) :-
		depth_is(cat_c, 2).

	test(dit_cat_d) :-
		depth_is(cat_d, 1).

	test(dit_prot_a) :-
		depth_is(prot_a, 1).

	test(dit_prot_b) :-
		depth_is(prot_b, 2).

	test(dit_prot_c_wrong_output) :-
		\+ depth_is(prot_a, 0).

	test(dit_herring) :-
		depth_is(herring, 2).

	test(dit_car) :-
		depth_is(car, 3).

	test(dit_meta_vehicle) :-
		depth_is(meta_vehicle, 1).

	test(dit_vehicle) :-
		depth_is(vehicle, 2).

	% auxiliary predicates

	depth_is(Entity, N) :-
		findall(D, dit_metric::entity_score(Entity, D), Depths),
		Depths == [N].

:- end_object.

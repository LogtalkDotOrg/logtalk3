%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
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


:- object(dit_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0.4,
		author is 'Ebrahim Azarisooreh',
		date is 2017/06/11,
		comment is 'Unit tests for code metrics framework.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(dit_metric).

	:- uses(dit_metric, [
		all/0,
		rlibrary/1,
		library/1,
		rdirectory/1,
		directory/1,
		file/1,
		entity/1
	]).

	:- uses(lgtunit, [
		deterministic/1
	]).

	% DIT tests

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

	test(wrong_output(dit_obj_c)) :-
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

	test(wrong_output(dit_prot_c)) :-
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

	% suppress all messages from the "code_metrics"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, code_metrics, _Tokens).

:- end_object.

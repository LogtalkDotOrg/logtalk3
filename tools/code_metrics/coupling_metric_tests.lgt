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


:- object(coupling_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0.5,
		author is 'Ebrahim Azarisooreh',
		date is 2017/12/28,
		comment is 'Unit tests for entity coupling code metric.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(coupling_metric).

	test(coupling_obj_a) :-
		coupling_is(obj_a, 3).

	test(coupling_obj_b) :-
		coupling_is(obj_b, 2).

	test(coupling_obj_c) :-
		coupling_is(obj_c, 2).

	test(coupling_obj_d) :-
		coupling_is(obj_d, 1).

	test(coupling_obj_e) :-
		coupling_is(obj_e, 0).

	test(coupling_obj_c_wrong_output) :-
		\+ coupling_is(obj_c, 10).

	test(coupling_cat_a) :-
		coupling_is(cat_a, 1).

	test(coupling_cat_b) :-
		coupling_is(cat_b, 2).

	test(coupling_cat_c) :-
		coupling_is(cat_c, 2).

	test(coupling_cat_d) :-
		coupling_is(cat_d, 0).

	test(coupling_cat_c_wrong_output) :-
		\+ coupling_is(obj_c, 4).

	test(coupling_prot_a) :-
		coupling_is(prot_a, 0).

	test(coupling_prot_b) :-
		coupling_is(prot_b, 1).

	test(coupling_herring) :-
		coupling_is(herring, 1).

	test(coupling_car) :-
		coupling_is(car, 1).

	test(coupling_meta_vehicle) :-
		coupling_is(meta_vehicle, 0).

	test(coupling_vehicle) :-
		coupling_is(vehicle, 1).

	% auxiliary predicates

	coupling_is(Entity, N) :-
		findall(C, coupling_metric::entity_score(Entity, C), Couplings),
		Couplings == [N].

	% suppress all messages from the "code_metrics"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, code_metrics, _Tokens).

:- end_object.

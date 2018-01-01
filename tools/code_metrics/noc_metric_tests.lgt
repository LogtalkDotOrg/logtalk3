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


:- object(noc_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0.6,
		author is 'Ebrahim Azarisooreh',
		date is 2018/01/01,
		comment is 'Unit tests for entity number of clauses metric.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(noc_metric).

	:- uses(noc_metric, [entity_score/2]).

	test(noc_cat_a) :-
		entity_score(cat_a, Nocs), 
		Nocs == [].

	test(noc_cat_b) :-
		entity_score(cat_b, Nocs),
		Nocs == [foo/0-1].

	test(noc_cat_c) :-
		entity_score(cat_c, Nocs),
		Nocs == [foo/0-1].

	test(noc_cat_d) :-
		entity_score(cat_d, Nocs),
		Nocs == [].

	test(noc_obj_e_wrong_clause) :-
		entity_score(obj_e, Nocs),
		\+ Nocs == [foo/0-2].

	test(noc_obj_e) :-
		entity_score(obj_e, Nocs),
		Nocs == [foo/0-1, fact/1-0].

	test(noc_obj_d) :-
		entity_score(obj_d, Nocs),
		Nocs == [bar/0-1, foo/0-1].

	test(noc_obj_a) :-
		entity_score(obj_a, Nocs),
		Nocs == [foo/0-1, bar/0-1, baz/1-2].

	test(noc_obj_b) :-
		entity_score(obj_b, Nocs),
		Nocs == [foo/0-1].

	test(noc_obj_c) :-
		entity_score(obj_c, Nocs),
		Nocs == [foo/0-1, fact/1-0].

	test(noc_prot_a) :-
		\+ entity_score(prot_a, _).

	test(noc_prot_b) :-
		\+ entity_score(prot_b, _).

	test(noc_car) :-
		entity_score(car, Nocs),
		Nocs == [].

	test(noc_vehicle) :-
		entity_score(vehicle, Nocs),
		Nocs == [].

	test(noc_meta_vehicle) :-
		entity_score(meta_vehicle, Nocs),
		Nocs == [].

	test(noc_herring) :-
		entity_score(herring, Nocs),
		Nocs == [].

	test(noc_bird) :-
		entity_score(bird, Nocs),
		Nocs == [].

	% suppress all messages from the "code_metrics"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, code_metrics, _Tokens).

:- end_object.

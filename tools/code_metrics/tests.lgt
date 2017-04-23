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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.3,
		author is 'Ebrahim Azarisooreh',
		date is 2017/04/23,
		comment is 'Unit tests for code metrics framework.'
	]).

	cover(code_metrics).
	cover(coupling_metric).
	cover(dit_metric).
	cover(noc_metric).

	:- uses(code_metrics, [
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

	% coupling tests

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

	test(wrong_output(coupling_obj_c)) :-
		\+ coupling_is(obj_c, 10).

	test(coupling_cat_a) :-
		coupling_is(cat_a, 1).

	test(coupling_cat_b) :-
		coupling_is(cat_b, 2).

	test(coupling_cat_c) :-
		coupling_is(cat_c, 2).

	test(coupling_cat_d) :-
		coupling_is(cat_d, 0).

	test(wrong_output(coupling_cat_c)) :-
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

	% noc tests

	test(noc_cat_a) :-
		\+ noc_metric::entity_score(cat_a, _).

	test(noc_cat_b) :-
		nocs_are(cat_b, Nocs),
		Nocs == [foo/0-1].

	test(noc_cat_c) :-
		nocs_are(cat_c, Nocs),
		Nocs == [foo/0-1].

	test(noc_cat_d) :-
		\+ noc_metric::entity_score(cat_d, _).

	test(wrong_clause(noc_obj_e)) :-
		nocs_are(obj_e, Nocs),
		\+ Nocs == [foo/0-2].

	test(noc_obj_e) :-
		nocs_are(obj_e, Nocs),
		Nocs == [foo/0-1].

	test(noc_obj_d) :-
		nocs_are(obj_d, Nocs),
		Nocs == [bar/0-1, foo/0-1].

	test(noc_obj_a) :-
		nocs_are(obj_a, Nocs),
		Nocs == [foo/0-1, bar/0-1, baz/1-2].

	test(noc_obj_b) :-
		nocs_are(obj_b, Nocs),
		Nocs == [foo/0-1].

	test(noc_obj_c) :-
		nocs_are(obj_c, Nocs),
		Nocs == [foo/0-1].

	test(noc_prot_a) :-
		\+ noc_metric::entity_score(prot_a, _).

	test(noc_prot_b) :-
		\+ noc_metric::entity_score(prot_b, _).

	test(noc_car) :-
		\+ noc_metric::entity_score(car, _).

	test(noc_vehicle) :-
		\+ noc_metric::entity_score(vehicle, _).

	test(noc_meta_vehicle) :-
		\+ noc_metric::entity_score(meta_vehicle, _).

	test(noc_herring) :-
		\+ noc_metric::entity_score(herring, _).

	test(noc_bird) :-
		\+ noc_metric::entity_score(bird, _).

	% main interface tests

	test(code_metrics_entity) :-
		deterministic(entity(obj_c)).

	test(code_metrics_file) :-
		object_property(lgtunit, file(File)),
		deterministic(file(File)).

	test(code_metrics_library) :-
		deterministic(library(lgtunit)).

	test(code_metrics_rlibrary) :-
		deterministic(rlibrary(lgtunit)).

	test(code_metrics_directory) :-
		logtalk::expand_library_path(lgtunit, Directory),
		deterministic(directory(Directory)).

	test(code_metrics_rdirectory) :-
		logtalk::expand_library_path(lgtunit, Directory),
		deterministic(rdirectory(Directory)).

	test(code_metrics_all) :-
		deterministic(all).

	% convenience

	coupling_is(Entity, N) :-
		findall(C, coupling_metric::entity_score(Entity, C), Couplings),
		Couplings == [N].

	depth_is(Entity, N) :-
		findall(D, dit_metric::entity_score(Entity, D), Depths),
		Depths == [N].

	nocs_are(Entity, Nocs) :-
		findall(
			Predicate-Noc,
			noc_metric::entity_score(Entity, predicate_noc(Predicate, Noc)),
			Nocs
		).

	% suppress all messages from the "code_metrics"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, code_metrics, _Tokens).

:- end_object.

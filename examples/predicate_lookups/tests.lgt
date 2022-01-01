%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-27,
		comment is 'Unit tests for the "predicate_lookups" example.'
	]).

	test(predicate_lookups_prototypes_01, true(Material == aluminum)) :-
		bike::frame(Material).

	test(predicate_lookups_prototypes_02, true(Where == land)) :-
		bike::where(Where).

	test(predicate_lookups_prototypes_03, true(Material == carbon)) :-
		mountain_bike::frame(Material).

	test(predicate_lookups_prototypes_04, true) :-
		mountain_bike::crewed.

	test(predicate_lookups_prototypes_05, true(Predicates == [crewed/0,frame/1,where/1])) :-
		findall(Predicate, mountain_bike::current_predicate(Predicate), Predicates).

	test(predicate_lookups_prototypes_06, true(Declaration == bike)) :-
		mountain_bike::predicate_property(frame(_), declared_in(Declaration)).

	test(predicate_lookups_prototypes_07, true(Definition == mountain_bike)) :-
		mountain_bike::predicate_property(frame(_), defined_in(Definition)).

	test(predicate_lookups_prototypes_08, true(Redefined == bike)) :-
		mountain_bike::predicate_property(frame(_), redefined_from(Redefined)).

	test(predicate_lookups_instances_01, true(Structure == soft)) :-
		paraglider::structure(Structure).

	test(predicate_lookups_instances_02, true(Purpose == fun)) :-
		sailplane::purpose(Purpose).

	test(predicate_lookups_instances_03, true(Structure == rigid)) :-
		sailplane::structure(Structure).

	test(predicate_lookups_instances_04, true(Predicates == [purpose/1,structure/1])) :-
		findall(Predicate, sailplane::current_predicate(Predicate), Predicates).

	test(predicate_lookups_instances_05, true(Declaration == artificial)) :-
		sailplane::predicate_property(purpose(_), declared_in(Declaration)).

	test(predicate_lookups_instances_06, true(Definition == sailplane)) :-
		sailplane::predicate_property(purpose(_), defined_in(Definition)).

	test(predicate_lookups_instances_07, true(Redefined == aircraft)) :-
		sailplane::predicate_property(purpose(_), redefined_from(Redefined)).

:- end_object.

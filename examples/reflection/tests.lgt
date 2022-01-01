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

	:- set_logtalk_flag(unknown_entities, silent).

	:- info([
		version is 1:4:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-06-11,
		comment is 'Unit tests for the "reflection" example.'
	]).

	cover(object).
	cover(class).
	cover(abstract_class).

	test(reflection_01, true(Predicates == [abstract_class/0,delete/1,instances/1,metaclass/0,new/1,print/0,strict_instance/0])) :-
		setof(Predicate, object::current_predicate(Predicate), Predicates).

	test(reflection_02, true(Predicates == [abstract_class/0,delete/1,instances/1,metaclass/0,new/1,print/0,strict_instance/0])) :-
		setof(Predicate, abstract_class::current_predicate(Predicate), Predicates).

	test(reflection_03, true(Predicates == [abstract_class/0,delete/1,instances/1,metaclass/0,new/1,print/0,strict_instance/0])) :-
		setof(Predicate, class::current_predicate(Predicate), Predicates).

	test(reflection_04, true(InstancesSorted == [abstract_class, class, object])) :-
		class::instances(Instances),
		class::metaclass,
		list::msort(Instances, InstancesSorted).

	test(reflection_05, true(Predicates == [abstract_class/0,metaclass/0,print/0,strict_instance/0])) :-
		abstract_class::new(ac),
		ac::abstract_class,
		setof(Predicate, ac::current_predicate(Predicate), Predicates).

	test(reflection_06, error(existence_error(predicate_declaration,new/1))) :-
		ac::new(i).

	test(reflection_07, true(Instances == [i])) :-
		class::new(c),
		c::new(i),
		c::instances(Instances).

	test(reflection_08, false) :-
		i::current_predicate(_Predicate).

	test(reflection_09, true(Predicates == [print/0,strict_instance/0])) :-
		object::new(j),
		setof(Predicate, j::current_predicate(Predicate), Predicates).

	test(reflection_10, true) :-
		c::delete(i),
		class::delete(c),
		abstract_class::delete(ac),
		object::delete(j).

:- end_object.

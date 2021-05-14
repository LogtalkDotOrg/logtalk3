%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:8:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-05-14,
		comment is 'Unit tests for the "complements/allow" example.'
	]).

	cover(employee).
	cover(add_on).

	test(complements_allow_01, true(Categories == [add_on, dynamic_patch])) :-
		setof(Category, complements_object(Category, employee), Categories).

	test(complements_allow_02, true(Protocol == monitoring)) :-
		conforms_to_protocol(employee, Protocol).

	test(complements_allow_03, true(Protocol-Scope == monitoring-(public))) :-
		conforms_to_protocol(employee, Protocol, Scope).

	test(complements_allow_04, true(Name == john)) :-
		^^suppress_text_output,
		employee::name(Name).

	test(complements_allow_05, true(PredicatesSorted == [after/3,age/1,before/3,income/1,name/1,predicates/1,salary/1])) :-
		^^suppress_text_output,
		employee::predicates(Predicates),
		list::msort(Predicates, PredicatesSorted).

	test(complements_allow_06, true(list::subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		findall(Property, employee::predicate_property(predicates(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, declared_in(add_on), defined_in(add_on), scope(public)], PropertiesSorted).

	test(complements_allow_07, true(list::subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		findall(Property, employee::predicate_property(income(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, alias_of(salary(_)), declared_in(employee), defined_in(dynamic_patch), scope(public)], PropertiesSorted).

	test(complements_allow_08, true(Salary == 42000)) :-
		^^suppress_text_output,
		employee::salary(Salary).

	test(complements_allow_09, true(Salary == 23500)) :-
		^^suppress_text_output,
		abolish_category(dynamic_patch),
		employee::salary(Salary).

:- end_object.

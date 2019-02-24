%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.6,
		author is 'Parker Jones and Paulo Moura',
		date is 2018/03/26,
		comment is 'Unit tests for the "complements/allow" example.'
	]).

	cover(employee).
	cover(add_on).

	test(complements_allow_1) :-
		complements_object(Category, Object),
		Category == add_on, Object == employee.

	test(complements_allow_2) :-
		conforms_to_protocol(employee, Protocol),
		Protocol == monitoring.

	test(complements_allow_3) :-
		conforms_to_protocol(employee, Protocol, Scope),
		Protocol == monitoring,
		Scope == (public).

	test(complements_allow_4) :-
		^^suppress_text_output,
		employee::name(Name),
		Name == john.

	test(complements_allow_5) :-
		^^suppress_text_output,
		employee::predicates(Predicates),
		list::msort(Predicates, PredicatesSorted),
		PredicatesSorted = [after/3, age/1, before/3, income/1, name/1, predicates/1, salary/1].

	test(complements_allow_6) :-
		findall(Property, employee::predicate_property(predicates(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, declared_in(add_on), defined_in(add_on), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(complements_allow_7) :-
		findall(Property, employee::predicate_property(income(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, alias_of(salary(_)), declared_in(employee), defined_in(dynamic_patch), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(complements_allow_8) :-
		^^suppress_text_output,
		employee::salary(Salary),
		Salary == 42000.

:- end_object.

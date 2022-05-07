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
		version is 1:8:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2022-05-07,
		comment is 'Unit tests for the "aliases" example.'
	]).

	:- uses(list, [
		msort/2, subsequence/3
	]).

	cover(rectangle(_, _)).
	cover(square(_)).
	cover(square1).

	cover(ellipse(_, _)).
	cover(circle(_)).

	test(aliases_01, true(PredicatesSorted == [area/1, height/1, side/1, width/1])) :-
		findall(Predicate, square(_)::current_predicate(Predicate), PredicatesUnsorted),
		msort(PredicatesUnsorted, PredicatesSorted).

	test(aliases_02, true(Side == 2)) :-
		square(2)::side(Side).

	test(aliases_03, true(Area == 4)) :-
		square(2)::area(Area).

	test(aliases_04, true(Width == 2)) :-
		square(2)::width(Width).

	test(aliases_05, true(Height == 2)) :-
		square(2)::height(Height).

	test(aliases_06, true(Side == 1)) :-
		square1::side(Side).

	test(aliases_07, true(Area == 1)) :-
		square1::area(Area).

	test(aliases_08, true(Width == 1)) :-
		square1::width(Width).

	test(aliases_09, true(Height == 1)) :-
		square1::height(Height).

	test(aliases_10, true(subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		findall(Property, square(_)::predicate_property(side(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([alias_of(width(_)), alias_declared_in(square(_),_), logtalk, public, static, declared_in(rectangle(_,_)), defined_in(rectangle(_,_)), scope(public), number_of_clauses(1)], PropertiesSorted).

	test(aliases_11, true(subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		findall(Property, square(_)::predicate_property(width(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([logtalk, public, static, declared_in(rectangle(_,_)), defined_in(rectangle(_,_)), scope(public)], PropertiesSorted).

	test(aliases_12, true(PredicatesSorted == [area/1, r/1, rx/1, ry/1])) :-
		findall(Predicate, circle(_)::current_predicate(Predicate), PredicatesUnsorted),
		msort(PredicatesUnsorted, PredicatesSorted).

	test(aliases_13, true(Radius == 3)) :-
		circle(3)::r(Radius).

	test(aliases_14, true(subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		findall(Property, circle(3)::predicate_property(r(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([alias_of(rx(_)), logtalk, public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), scope(public)], PropertiesSorted).

	test(aliases_15, true(subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		findall(Property, circle(3)::predicate_property(rx(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([logtalk, public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), scope(public)], PropertiesSorted).

	test(aliases_16, true(subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		object_property(square(_), alias(side/1, AllProperties)),
		msort(AllProperties, AllPropertiesSorted),
		msort([for(width/1), from(rectangle(_,_)), line_count(_)], PropertiesSorted).

	test(aliases_17, true(subsequence(AllPropertiesSorted, PropertiesSorted, _))) :-
		object_property(circle(_), alias(r/1, AllProperties)),
		msort(AllProperties, AllPropertiesSorted),
		msort([for(rx/1), from(ellipse(_,_)), line_count(_)], PropertiesSorted).

:- end_object.

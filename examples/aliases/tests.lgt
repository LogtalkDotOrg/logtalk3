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
		version is 1.7,
		author is 'Parker Jones and Paulo Moura',
		date is 2019/01/01,
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

	test(aliases_01) :-
		findall(Predicate, square(_)::current_predicate(Predicate), PredicatesUnsorted),
		msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted == [area/1, height/1, side/1, width/1].

	test(aliases_02) :-
		square(2)::side(Side),
		Side == 2.

	test(aliases_03) :-
		square(2)::area(Area),
		Area == 4.

	test(aliases_04) :-
		square(2)::width(Width),
		Width == 2.

	test(aliases_05) :-
		square(2)::height(Height),
		Height == 2.

	test(aliases_06) :-
		square1::side(Side),
		Side == 1.

	test(aliases_07) :-
		square1::area(Area),
		Area == 1.

	test(aliases_08) :-
		square1::width(Width),
		Width == 1.

	test(aliases_09) :-
		square1::height(Height),
		Height == 1.

	test(aliases_10) :-
		findall(Property, square(_)::predicate_property(side(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([alias_of(width(_)), alias_declared_in(square(_),_), logtalk, public, static, declared_in(rectangle(_,_)), defined_in(rectangle(_,_)), scope(public), number_of_clauses(1)], PropertiesSorted),
		subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_11) :-
		findall(Property, square(_)::predicate_property(width(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([logtalk, public, static, declared_in(rectangle(_,_)), defined_in(rectangle(_,_)), scope(public)], PropertiesSorted),
		subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_12) :-
		findall(Predicate, circle(_)::current_predicate(Predicate), PredicatesUnsorted),
		msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted == [area/1, r/1, rx/1, ry/1].

	test(aliases_13) :-
		circle(3)::r(Radius),
		Radius == 3.

	test(aliases_14) :-
		findall(Property, circle(3)::predicate_property(r(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([alias_of(rx(_)), logtalk, public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), scope(public)], PropertiesSorted),
		subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_15) :-
		findall(Property, circle(3)::predicate_property(rx(_), Property), AllProperties),
		msort(AllProperties, AllPropertiesSorted),
		msort([logtalk, public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), scope(public)], PropertiesSorted),
		subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_16) :-
		object_property(square(_), alias(side/1, AllProperties)),
		msort(AllProperties, AllPropertiesSorted),
		msort([for(width/1), from(rectangle(_,_)), line_count(_)], PropertiesSorted),
		subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_17) :-
		object_property(circle(_), alias(r/1, AllProperties)),
		msort(AllProperties, AllPropertiesSorted),
		msort([for(rx/1), from(ellipse(_,_)), line_count(_)], PropertiesSorted),
		subsequence(AllPropertiesSorted, PropertiesSorted, _).

:- end_object.

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
		version is 1:12:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-02-02,
		comment is 'Unit tests for the "sicstus" example.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2,
		assertion/1, variant/2
	]).

	:- uses(list, [
		msort/2, nth1/3
	]).

	test(sicstus_01, true(Sorted == [1/8, 2/9, 2/7, 1/3, 6/5])) :-
		sort(rational)::sort([1/8, 2/7, 6/5, 2/9, 1/3], Sorted).

	test(sicstus_02, true(Sorted == [red, orange, yellow, green, blue, indigo, violet])) :-
		sort(colours)::sort([orange, indigo, red, yellow, violet, blue, green], Sorted).

	test(sicstus_03, true(Sorted == [1, 2, 3, 4, 9])) :-
		sort(user)::sort([3, 1, 4, 2, 9], Sorted).

	test(sicstus_04, true(Color == red)) :-
		red_circle(3)::color(Color).

	test(sicstus_05, true(Area =~= 28.2743338823081)) :-
		red_circle(3)::area(Area).

	test(sicstus_06, true(As == [circle(3,red), ellipse(3,3,red)])) :-
		red_circle(3)::ancestors(As).

	test(sicstus_07, true(Side == 2)) :-
		square(2)::side(Side).

	test(sicstus_08, true(Width == 2)) :-
		square(2)::width(Width).

	test(sicstus_09, true(Height == 2)) :-
		square(2)::height(Height).

	test(sicstus_10, true(Area == 4)) :-
		square(2)::area(Area).

	test(sicstus_11, true(PredSorted == [area/1, height/1, side/1, width/1])) :-
		findall(Pred, square(2)::current_predicate(Pred), Preds),
		msort(Preds,PredSorted).

	test(sicstus_12, true) :-
		findall(Prop, square(_)::predicate_property(side(_), Prop), Props),
		msort(Props,PropsSorted),
		nth1(1, PropsSorted, Logtalk),    assertion(Logtalk == logtalk),
		nth1(2, PropsSorted, Public),     assertion(Public == public),
		nth1(3, PropsSorted, Static),     assertion(Static == static),
		nth1(4, PropsSorted, DeclaredIn), variant(DeclaredIn, declared_in(square(_))),
		nth1(5, PropsSorted, DefinedIn),  variant(DefinedIn, defined_in(square(_))).

:- end_object.

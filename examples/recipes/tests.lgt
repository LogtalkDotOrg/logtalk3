%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2023-06-04,
		comment is 'Unit tests for the "recipes" example.'
	]).

	test(recipes_01, true(Ingredients == [peas,cream,water,oil])) :-
		green_soup::ingredients(Ingredients).

	test(recipes_02, true(Steps == [1-'Boil the peas.',2-'Mash the peas',3-'Add salt and mix.'])) :-
		mashed_peas::steps(Steps).

	test(recipes_03, true(Recipes == [green_soup,mashed_peas])) :-
		setof(Recipe, ({recipe(Recipe)}, Recipe::ingredient(peas)), Recipes).

	test(recipes_04, true(Level == hard)) :-
		green_soup::level(Level).

	test(recipes_05, true(Level == easy)) :-
		mashed_peas::level(Level).

	test(recipes_06, true(Level == easy)) :-
		{recipe(_,_,_)}::level(Level).

	test(recipes_07, true(Name == 'Berries and cream')) :-
		{recipe(Recipe)},
		Recipe::ingredient(chocolate),
		Recipe::cooking_time(CookingTime),
		Recipe::name(Name),
		CookingTime < 35.

:- end_object.

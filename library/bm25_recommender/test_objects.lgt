%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(bm25_dataset(_Ratings_, _Items_, _Contents_),
	implements([rating_dataset_protocol, item_content_dataset_protocol])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Parametric rating and content fixtures for the BM25 recommender tests.',
		parnames is ['Rating', 'Items', 'Contents']
	]).

	:- uses(list, [
		member/2, length/2
	]).

	rating(User, Item, Rating) :-
		member(rating(User,Item,Rating), _Ratings_).

	rating_count(Count) :-
		length(_Ratings_, Count).

	rating_scale(_, _) :-
		fail.

	item(Item) :-
		member(Item, _Items_).

	item_content(Item, Content) :-
		member(Item-Content, _Contents_).

:- end_object.


:- object(bm25_scale_fixture(_Min_, _Max_),
	extends(bm25_dataset([rating(u,x,1)], [x], [x-vector([a-2])]))).

	rating_scale(_Min_, _Max_).

:- end_object.


:- object(bm25_validation_counter,
	extends(bm25_recommender)).

	:- public([
		reset_validation_count/0, validation_count/1,
		reset_profile_count/0, profile_count/1
	]).

	:- private(validations/1).
	:- dynamic(validations/1).

	:- private(profile_builds/1).
	:- dynamic(profile_builds/1).

	reset_profile_count :-
		retractall(profile_builds(_)),
		assertz(profile_builds(0)).

	profile_count(Count) :-
		once(profile_builds(Count)).

	user_profile(User, Ratings, Documents, Options, Profile) :-
		(	retract(profile_builds(Count)) ->
			Next is Count + 1,
			assertz(profile_builds(Next))
		;	true
		),
		^^user_profile(User, Ratings, Documents, Options, Profile).

	reset_validation_count :-
		retractall(validations(_)),
		assertz(validations(0)).

	validation_count(Count) :-
		once(validations(Count)).

	recommender_valid_data(Model) :-
		retract(validations(Count)),
		!,
		Next is Count + 1,
		assertz(validations(Next)),
		^^recommender_valid_data(Model).

:- end_object.

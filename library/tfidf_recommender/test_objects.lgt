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


:- object(tfidf_dataset(_, _, _),
	implements([rating_dataset_protocol, item_content_dataset_protocol])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Parametric rating, catalog, and content fixtures for the TF-IDF recommender tests.'
	]).

	:- uses(list, [length/2, member/2]).

	rating(User, Item, Rating) :-
		parameter(1, Ratings), member(rating(User, Item, Rating), Ratings).
	rating_count(Count) :- parameter(1, Ratings), length(Ratings, Count).
	rating_scale(_, _) :- fail.
	item(Item) :- parameter(2, Items), member(Item, Items).
	item_content(Item, Content) :- parameter(3, Contents), member(Item-Content, Contents).

:- end_object.


:- object(tfidf_scale_fixture(_, _),
	extends(tfidf_dataset([rating(u, x, 1)], [x], [x-vector([x-1])]))).

	rating_scale(Min, Max) :- parameter(1, Min), parameter(2, Max).

:- end_object.


:- object(tfidf_count_fixture,
	extends(tfidf_dataset([rating(u, x, 1)], [x], [x-vector([x-1])]))).

	rating_count(9).

:- end_object.

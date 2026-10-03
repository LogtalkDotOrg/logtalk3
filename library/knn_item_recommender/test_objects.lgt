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


:- object(item_knn_ratings,
	implements(rating_dataset_protocol)).

	rating(u, a, 1).
	rating(u, b, 3).
	rating(v, a, 2).
	rating(v, b, 4).
	rating(v, c, 5).

	rating_count(5).

	rating_scale(1, 5).

:- end_object.


:- object(item_knn_metric(_),
	implements(similarity_metric_protocol)).

	similarity(_, _, Score) :-
		parameter(1, Score).

:- end_object.


:- object(item_knn_filtered_ratings,
	implements(rating_dataset_protocol)).

	rating(User, Item, Rating) :-
		item_knn_ratings::rating(User, Item, Rating).
	rating(v, d, 5).

	rating_count(6).

	rating_scale(1, 5).

:- end_object.


:- object(item_knn_scaled_ratings(_, _),
	implements(rating_dataset_protocol)).

	rating(User, Item, Rating) :-
		item_knn_ratings::rating(User, Item, Rating).

	rating_count(5).

	rating_scale(Min, Max) :-
		parameter(1, Min), parameter(2, Max).

:- end_object.


:- object(item_knn_unscaled_ratings,
	implements(rating_dataset_protocol)).

	rating(User, Item, Rating) :-
		item_knn_ratings::rating(User, Item, Rating).

	rating_count(5).

	rating_scale(_, _) :-
		fail.

:- end_object.


:- object(item_knn_multiple_metric,
	implements(similarity_metric_protocol)).

	similarity(_, _, 0.5).
	similarity(_, _, 1.0).

:- end_object.


:- object(item_knn_failing_metric,
	implements(similarity_metric_protocol)).

	similarity(_, _, _) :-
		fail.

:- end_object.

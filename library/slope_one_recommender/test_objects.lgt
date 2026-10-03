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


:- object(slope_ratings,
	implements(rating_dataset_protocol)).

	rating(u, a, 1).
	rating(u, b, 3).
	rating(v, a, 2).
	rating(v, b, 4).
	rating(v, c, 5).

	rating_count(5).

	rating_scale(1, 5).

:- end_object.


:- object(slope_weighted_ratings,
	implements(rating_dataset_protocol)).

	rating(User, Item, Rating) :-
		slope_ratings::rating(User, Item, Rating).
	rating(w, a, 1).
	rating(w, c, 3).

	rating_count(7).

	rating_scale(1, 5).

:- end_object.


:- object(slope_disconnected_ratings,
	implements(rating_dataset_protocol)).

	rating(u, a, 1).
	rating(v, b, 5).

	rating_count(2).

	rating_scale(_, _) :-
		fail.

:- end_object.


:- object(slope_clipped_ratings,
	implements(rating_dataset_protocol)).

	rating(u, a, 5).
	rating(v, a, 1).
	rating(v, b, 5).

	rating_count(3).

	rating_scale(1, 5).

:- end_object.

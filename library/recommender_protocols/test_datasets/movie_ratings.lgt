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


:- object(movie_ratings,
	implements(rating_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
		comment is 'Small MovieLens-style dataset: six users rating six movies on a 1-5 scale. Movies m1-m3 are action films and m4-m6 are romance films; alice, bob, and carol mostly rate action films highly, while dave, erin, and frank mostly rate romance films highly, with a few cross-genre ratings, giving two recognizable but imperfect taste clusters.'
	]).

	rating(alice, m1, 5).
	rating(alice, m2, 4).
	rating(alice, m3, 5).
	rating(alice, m4, 1).
	rating(bob, m1, 4).
	rating(bob, m2, 5).
	rating(bob, m3, 4).
	rating(bob, m5, 2).
	rating(carol, m1, 5).
	rating(carol, m3, 5).
	rating(carol, m4, 2).
	rating(carol, m6, 1).
	rating(dave, m1, 1).
	rating(dave, m4, 5).
	rating(dave, m5, 4).
	rating(dave, m6, 5).
	rating(erin, m2, 2).
	rating(erin, m4, 4).
	rating(erin, m5, 5).
	rating(erin, m6, 4).
	rating(frank, m3, 1).
	rating(frank, m4, 4).
	rating(frank, m5, 5).
	rating(frank, m6, 5).

	rating_count(24).

	rating_scale(1, 5).

:- end_object.

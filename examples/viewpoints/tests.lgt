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
		version is 1:2:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-24,
		comment is 'Unit tests for the "viewpoints" example.'
	]).

	cover(joe_person).
	cover(joe_sportsman).
	cover(joe_chess_player).
	cover(joe_film_enthusiast).
	cover(joe_employee).

	test(viewpoints_01, true(Age == 30)) :-
		joe_person::age(Age).

	test(viewpoints_02, true(Age == 30)) :-
		joe_sportsman::age(Age).

	test(viewpoints_03, true(Age == 31)) :-
		joe_person::grow_older,
		joe_chess_player::age(Age).

	test(viewpoints_04, true(Age == 32)) :-
		joe_employee::grow_older,
		joe_person::age(Age).

	test(viewpoints_05, true(Score == 0)) :-
		joe_person::score(Score).

	test(viewpoints_06, true(Score == 0)) :-
		joe_employee::score(Score).

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(viewpoints_07, true(Score == 2200)) :-
		joe_chess_player::set_score(2200), joe_chess_player::score(Score).

	test(viewpoints_08, true(Score == 0)) :-
		joe_person::score(Score).

	test(viewpoints_09, true(Score == 0)) :-
		joe_sportsman::score(Score).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "viewpoints" example.'
	]).

	cover(joe_person).
	cover(joe_sportsman).
	cover(joe_chess_player).
	cover(joe_film_enthusiast).
	cover(joe_employee).

	test(viewpoints_1) :-
		joe_person::age(Age),
		Age == 30.

	test(viewpoints_2) :-
		joe_sportsman::age(Age),
		Age == 30.

	test(viewpoints_3) :-
		joe_person::grow_older,
		joe_chess_player::age(Age),
		Age == 31.

	test(viewpoints_4) :-
		joe_employee::grow_older,
		joe_person::age(Age),
		Age == 32.

	test(viewpoints_5) :-
		joe_person::score(Score),
		Score == 0.

	test(viewpoints_6) :-
		joe_employee::score(Score),
		Score == 0.

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(viewpoints_7) :-
		joe_chess_player::set_score(2200), joe_chess_player::score(Score),
		Score == 2200.

	test(viewpoints_8) :-
		joe_person::score(Score),
		Score == 0.

	test(viewpoints_9) :-
		joe_sportsman::score(Score),
		Score == 0.

:- end_object.

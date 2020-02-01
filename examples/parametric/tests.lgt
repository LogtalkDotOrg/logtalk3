%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		date is 2017-11-19,
		comment is 'Unit tests for the "parametric" example.'
	]).

	cover([_| _]).
	cover('[]').
	cover(date(_, _, _)).
	cover(time(_, _, _)).
	cover(rectangle(_, _, _, _)).
	cover(person(_, _)).
	cover(employee(_, _, _)).
	cover(dress(_)).
	cover(speech(_)).
	cover(speech(_, _)).

	test(parametric_01) :-
		findall(X, [1, 2, 3]::member(X), Solutions),
		Solutions == [1, 2, 3].

	test(parametric_02) :-
		findall(X, [1, 2, 3]::last(X), Solutions),
		Solutions == [3].

	test(parametric_03) :-
		findall(X, [1, 2, 3]::nextto(2,X), Solutions),
		Solutions == [3].

	test(parametric_04) :-
		\+ '[]'::member(_).

	test(parametric_05) :-
		rectangle(W, H, X, Y)::init, rectangle(W, H, X, Y)::move(3, 4, NR), NR::position(X2, Y2),
		W  == 2,
		H  == 1,
		X  == 0,
		Y  == 0,
		NR == rectangle(2, 1, 3, 4),
		X2 == 3,
		Y2 == 4.

	test(parametric_06) :-
		person(sally, 20)::grow_older(NewId),
		NewId == person(sally, 21).

	test(parametric_07) :-
		employee(sally, 21, 1200)::give_raise(250, NewId),
		NewId == employee(sally, 21, 1450).

	test(parametric_08) :-
		speech(winter, wedding)::advice(Clothes, Speech),
		Clothes == [pants, sleeves, heavy],
		Speech == [happy, jokes].

	test(parametric_09) :-
		date(2017, 11, 19) :: (year(Year), month(Month), day(Day)),
		Year == 2017, Month == 11, Day == 19.

	test(parametric_10) :-
		time(21, 33, 42) :: (hours(Hours), mins(Mins), secs(Secs)),
		Hours == 21, Mins == 33, Secs == 42.

:- end_object.

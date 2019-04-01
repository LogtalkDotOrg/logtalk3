%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.3,
		author is 'Parker Jones and Paulo Moura',
		date is 2019/04/01,
		comment is 'Unit tests for the "proxies" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	cover(circle(_, _, _)).

	test(proxies_01) :-
		circle(one, 7, red)::id(Id),
		Id == one.

	test(proxies_02) :-
		circle(one, 7, red)::radius(Radius),
		Radius == 7.

	test(proxies_03) :-
		circle(one, 7, red)::color(Color),
		Color == red.

	test(proxies_04) :-
		{circle('#2', Radius, Color)}::print,
		Radius =~= 3.71,
		Color == yellow.

	test(proxies_05) :-
		findall(Area, {circle(_, _, _)}::area(Area), Areas),
		Areas = [Area1, Area2, Area3, Area4, Area5],
		Area1 =~= 4.75291552561599,
		Area2 =~= 43.2411954432752,
		Area3 =~= 0.477836242611007,
		Area4 =~= 103.507938113415,
		Area5 =~= 217.468583303854.

:- end_object.

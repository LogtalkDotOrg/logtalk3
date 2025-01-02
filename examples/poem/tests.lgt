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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-11-21,
		comment is 'Unit tests for the "poem" example.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(point(_, _)).
	cover(line(_, _)).
	cover(ellipse(_, _, _)).
	cover(circle(_, _)).

	test(poem_point_distance, true(Distance =~= 2.23606797749979)) :-
		point(4, 5)::distance(point(6,6), Distance).

	test(poem_points_identical, true) :-
		point(4, 5)::identical(point(4,5)).

	test(poem_points_not_identical, false) :-
		point(0, 0)::identical(point(6,6)).

	test(poem_ellipse_area, true(Area =~= 47.1294)) :-
		ellipse(point(5,6), 3, 5)::area(Area).

	test(poem_circle_area, true(Area =~= 28.277639999999998)) :-
		circle(point(2,2), 3)::area(Area).

	test(poem_circle_circumference, true(Circumference =~= 18.85176)) :-
		circle(point(2,2), 3)::circumference(Circumference).

	test(poem_line_distance, true(Distance =~= 0.2229882438741499)) :-
		line(point(-4,-3), point(6,6))::distance(point(3,3), Distance).

	test(poem_lines_intersect, true) :-
		line(point(0,0), point(4,5))::intersects(line(point(-4,-3), point(6,6))).

	test(poem_lines_dont_intersect, false) :-
		line(point(-4,-3), point(4,5))::intersects(line(point(0,0), point(6,6))).

	test(poem_line_length, true(Lenght =~= 2.8284271247461903)) :-
		line(point(0,0), point(2,2))::length(Lenght).

:- end_object.

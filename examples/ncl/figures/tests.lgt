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
		date is 2020-06-08,
		comment is 'Unit tests for the NCL "figures" example.'
	]).

	:- uses([
		figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) as figures,
		figures_split(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) as figures_split
	]).

	cover(figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
	cover(figures_split(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).

	test(figures_01, true(Classes == [four_side_figure, parallelogram])) :-
		findall(
			Class,
			figures::(
				edge(1, 2,  0, 20),
				edge(2, 3, 45, 30),
				edge(3, 4,  0, 20),
				edge(4, 1, 45, 30),
				class(Class)
			),
			Classes0
		),
		sort(Classes0, Classes).

	test(figures_02, true(Classes == [four_side_figure, parallelogram, rhombus])) :-
		findall(
			Class,
			figures::(
				edge(a, b, 45, 10),
				edge(b, c,  0, 10),
				edge(c, d, 45, 10),
				edge(d, a,  0, 10),
				class(Class)
			),
			Classes0
		),
		sort(Classes0, Classes).

	test(figures_split_01, true(Classes == [four_side_figure, rectangular, square])) :-
		findall(
			Class,
			figures_split::(
				edge(1, 2,  0, 20),
				edge(2, 3, 90, 20),
				edge(3, 4,  0, 20),
				edge(4, 1, 90, 20),
				perpendicular,
				class(Class)
			),
			Classes0
		),
		sort(Classes0, Classes).

	test(figures_split_02, true(Classes == [four_side_figure, parallelogram, rhombus])) :-
		findall(
			Class,
			figures_split::(
				edge(a, b, 45, 10),
				edge(b, c,  0, 10),
				edge(c, d, 45, 10),
				edge(d, a,  0, 10),
				perpendicular,
				class(Class)
			),
			Classes0
		),
		sort(Classes0, Classes).

:- end_object.

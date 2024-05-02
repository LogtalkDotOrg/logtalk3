%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


% an object with no hierarchy relations with other
% objects is always compiled as a prototype

:- object(shape).

	:- info([
		author is 'Paulo Moura',
		version is 1:0:1,
		date is 2024-05-02,
		comment is 'Generic geometric shape.'
	]).

	:- public(color/1).
	:- mode(color(?atom), zero_or_one).
	:- info(color/1, [
		comment is 'Shape color.',
		argnames is ['Color']
	]).

	:- public(position/2).
	:- mode(position(?number, ?number), zero_or_one).
	:- info(position/2, [
		comment is 'Shape position.',
		argnames is ['X', 'Y']
	]).

	% default shape color
	color(red).

	% default shape position
	position(0, 0).

:- end_object.


% an object that extends another object is also compiled as a prototype

:- object(polygon,
	extends(shape)).

	:- info([
		author is 'Paulo Moura',
		version is 1:1:1,
		date is 2024-05-02,
		comment is 'Generic polygon.'
	]).

	:- public(nsides/1).
	:- mode(nsides(?integer), zero_or_one).
	:- info(nsides/1, [
		comment is 'Polygon number of sides.',
		argnames is ['Number']
	]).

	:- public(area/1).
	:- mode(area(-number), zero_or_one).
	:- info(area/1, [
		comment is 'Polygon area.',
		argnames is ['Area']
	]).

	:- public(perimeter/1).
	:- mode(perimeter(?number), zero_or_one).
	:- info(perimeter/1, [
		comment is 'Polygon perimeter.',
		argnames is ['Perimeter']
	]).

:- end_object.


:- object(regular_polygon,
	extends(polygon)).

	:- info([
		author is 'Paulo Moura',
		version is 1:1:1,
		date is 2024-05-02,
		comment is 'Generic regular polygon.'
	]).

	:- public(side/1).
	:- mode(side(?number), zero_or_one).
	:- info(side/1, [
		comment is 'Regular polygon side length.',
		argnames is ['Length']
	]).

	% default side length
	side(1).

	perimeter(Perimeter) :-
		::nsides(Number),
		::side(Side),
		Perimeter is Number*Side.

:- end_object.


:- object(square,
	extends(regular_polygon)).

	:- info([
		author is 'Paulo Moura',
		version is 1:0:0,
		date is 2003-2-3,
		comment is 'Geometric square.'
	]).

	nsides(4).

	area(Area) :-
		::side(Side),
		Area is Side*Side.

:- end_object.


:- object(q1,
	extends(square)).

:- end_object.


:- object(q2,
	extends(square)).

	position(2, 3).

	color(blue).

	side(3).

:- end_object.

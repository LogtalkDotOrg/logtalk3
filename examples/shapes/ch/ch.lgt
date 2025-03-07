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


% this example reuses the "roots" example, which provides three (objects
% playing the role of) classes that are used here:
%
% - abstract_class
%	default metaclass for all abstract classes
% - class
%	default metaclass for all classes
% - object
%	root class for class-based hierarchies
%
% see the documentation of the "roots" example for more details


% "shape" abstract class

:- object(shape,
	instantiates(abstract_class),
	specializes(object)).

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


% "polygon" abstract class

:- object(polygon,
	instantiates(abstract_class),
	specializes(shape)).

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


% "regular_polygon" abstract class

:- object(regular_polygon,
	instantiates(abstract_class),
	specializes(polygon)).

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

	% the perimeter depends on the number of sides of the specific type
	% of regular polygon, defined in descendant classes, and the length
	% of the side, which can be redefined from the default value here in
	% descendant classes and instances; thus, we must sent the messages
	% to _self_ (i.e., the instance that receives the perimeter/1 message)
	% using the ::/1 control construct
	perimeter(Perimeter) :-
		::nsides(Number),
		::side(Side),
		Perimeter is Number*Side.

:- end_object.


% "square" instantiable class

:- object(square,
	instantiates(class),
	specializes(regular_polygon)).

	:- info([
		author is 'Paulo Moura',
		version is 1:0:0,
		date is 2003-02-03,
		comment is 'Geometric square.'
	]).

	% as all squares have 4 sides, we can define the nsides/1 predicate here
	% instead of repeating the definition in all descendants of this class
	nsides(4).

	% the area depends on the length of the side, which can be redefined
	% from the inherited default value in descendant classes and instances;
	% thus, we must sent the side/1 message to _self_ (i.e., the instance that
	% receives the perimeter/1 message) using the ::/1 control construct
	area(Area) :-
		::side(Side),
		Area is Side*Side.

:- end_object.


% "q1" static instance of "square"

:- object(q1,
	instantiates(square)).

	% inherits default definitions for position/2, color/1, and side/1

:- end_object.


% "q2" static instance of "square"

:- object(q2,
	instantiates(square)).

	% overrides inherited default definitions for position/2, color/1, and side/1

	position(2, 3).

	color(blue).

	side(3).

:- end_object.

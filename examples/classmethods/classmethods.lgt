%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(metacircle,					% avoid infinite metaclass regression by
	instantiates(metacircle)).			% making "metacircle" its own metaclass

	:- public(new/4).
	:- mode(new(+float, +float, +float, -object_identifier), one).
	:- info(new/4, [
		comment is 'Creates a new circle in a given position and with a given radius.',
		argnames is ['X', 'Y', 'Radius', 'Circle']
	]).

	new(Radius, X, Y, Circle) :-		% this would be a "constructor" in other languages
		self(Self),						% we may be instantiating a subclass of "circle"
		create_object(Circle, [instantiates(Self)], [], [position(X, Y), radius(Radius)]).

	:- public(area/2).					% this would be an utility class method, usable without
	:- mode(area(+float, -float), one).	% being necessary to first instantiate the "circle" class
	:- info(area/2, [
		comment is 'Calculates the area of a circle given its radius.',
		argnames is ['Radius', 'Area']
	]).

	area(Radius, Area) :-
		Area is 4*atan(1.0)*Radius*Radius.

:- end_object.


:- object(circle,						% "circle" is an instantiable class as it accepts
	instantiates(metacircle)).			% messages for creating new objects (declared and
										% defined in its metaclass, "metacircle")
	:- public(position/2).
	:- mode(position(?float, ?float), zero_or_one).
	:- info(position/2, [
		comment is 'Circle position.',
		argnames is ['X', 'Y']
	]).

	% default position
	position(0.0, 0.0).

	:- public(radius/1).
	:- mode(radius(?float), zero_or_one).
	:- info(radius/1, [
		comment is 'Circle radius.',
		argnames is ['Radius']
	]).

	% default radius
	radius(1.0).

	:- public(area/1).
	:- mode(area(-float), one).
	:- info(area/1, [
		comment is 'Circle area.',
		argnames is ['Area']
	]).

	area(Area) :-
		% ask the circle's instance that received the area/1 message its radius
		::radius(Radius),
		Area is 4*atan(1.0)*Radius*Radius.

:- end_object.


:- object(c42,							% a static instance of "circle"; of course, you
	instantiates(circle)).				% can also create dynamic instances at runtime
										% by sending the new/4 message to "circle"
	position(3.7, 4.5).
	radius(2.8).

:- end_object.

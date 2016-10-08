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


:- object(rectangle(_Width, _Height, _Position)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/08/02,
		comment is 'A simple implementation of a geometric rectangle using assignable variables and parametric objects.',
		parnames is ['Width', 'Height', 'Position']
	]).

	:- uses(assignvars, [
		assignable/2, (=>)/2, (<=)/2, op(100, xfx, '=>'), op(100, xfx, '<=')
	]).

	:- public(init/0).
	:- mode(init, one).
	:- info(init/0, [
		comment is 'Initialize rectangle position.'
	]).

	:- public(area/1).
	:- mode(area(-integer), one).
	:- info(area/1, [
		comment is 'Rectangle area.',
		argnames is ['Area']
	]).

	:- public(move/2).
	:- mode(move(+integer, +integer), one).
	:- info(move/2, [
		comment is 'Moves a rectangle to a new position.',
		argnames is ['X', 'Y']
	]).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Rectangle current position.',
		argnames is ['X', 'Y']
	]).

	init :-
		parameter(3, Position),
		assignable(Position, (0, 0)).

	area(Area) :-
		parameter(1, Width),
		parameter(2, Height),
		Area is Width*Height.

	move(X, Y) :-
		parameter(3, Position),
		Position <= (X, Y).

	position(X, Y) :-
		parameter(3, Position),
		Position => (X, Y).

:- end_object.

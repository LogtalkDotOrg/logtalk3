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


:- object(rectangle(_Width_, _Height_, _Position_)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-02-07,
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
		assignable(_Position_, (0, 0)).

	area(Area) :-
		Area is _Width_ * _Height_.

	move(X, Y) :-
		_Position_ <= (X, Y).

	position(X, Y) :-
		_Position_ => (X, Y).

:- end_object.

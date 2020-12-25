%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(rectangle(_Width_, _Height_)).

	:- info([
		author is 'Paulo Moura',
		version is 1:1:0,
		date is 2019-05-02,
		comment is 'Parametric object for representing geometric rectangles.',
		parnames is ['Width', 'Height']
	]).

	:- public(width/1).
	:- mode(width(?number), zero_or_one).
	:- info(width/1, [
		comment is 'Rectangle width.',
		argnames is ['Width']
	]).

	:- public(height/1).
	:- mode(height(?number), zero_or_one).
	:- info(height/1, [
		comment is 'Rectangle height.',
		argnames is ['Height']
	]).

	:- public(area/1).
	:- mode(area(-number), one).
	:- info(area/1, [
		comment is 'Rectangle area.',
		argnames is ['Area']
	]).

	width(_Width_).

	height(_Height_).

	area(Area) :-
		% thanks to parameter passing between entities,
		% there's no need of using ::/1 messages
		Area is _Width_ * _Height_.

:- end_object.


:- object(square(_Side_),
	extends(rectangle(_Side_, _Side_))).

	:- info([
		author is 'Paulo Moura',
		version is 1:1:0,
		date is 2019-05-02,
		comment is 'Parametric object for representing geometric squares.',
		parnames is ['Side']
	]).

	:- public(side/1).
	:- mode(side(?number), zero_or_one).
	:- info(side/1, [
		comment is 'Square side.',
		argnames is ['Side']
	]).

	side(_Side_).

:- end_object.

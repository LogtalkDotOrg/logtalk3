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


% general information about Joe:

:- object(joe_person).

	:- public([
		grow_older/0, address/1, age/1, name/1,
		phone/1, score/1, set_score/1
	]).

	:- dynamic([
		age/1, score/1
	]).

	% this predicate uses property sharing, i.e. the property and its
	% value are shared by all descendant prototypes/viewpoints; changes
	% are shared no matter which viewpoint receives the grow_older/1 message
	grow_older :-
		retract(age(Old)),
		New is Old + 1,
		asserta(age(New)).

	address('8 Octave Street').

	age(30).

	name('John').

	phone(11-11-11-11).

	% default value for the score/1 property, shared
	% by all descendant prototypes/viewpoints
	score(0).

	% changing the default value results in in a local value stored in
	% the descendant prototype that received the set_score/1 message
	set_score(Score) :-
		::retractall(score(_)),
		::asserta(score(Score)).

:- end_object.


% information on Joe as an employee:

:- object(joe_employee,
	extends(joe_person)).

	:- public([
		works_for/1, salary/1, give_raise/1
	]).

	:- dynamic(salary/1).

	works_for('ToonTown').

	salary(1500).

	% another example of property sharing
	give_raise(Raise) :-
		retract(salary(Old)),
		New is Old + Raise,
		asserta(salary(New)).

:- end_object.


% information on Joe as an chess player:

:- object(joe_chess_player,
	extends(joe_person)).

	:- public(category/1).

	category('National Master').

:- end_object.


% information on Joe as a movies fan:

:- object(joe_film_enthusiast,
	extends(joe_person)).

	:- public([
		favorite_actor/1, favorite_film/1, favorite_director/1
	]).

	favorite_actor('Fred Filistone').

	favorite_film('The Wizard of Oz').

	favorite_director('Krzystof Kieslowski').

:- end_object.


% information on Joe as a sportsman:

:- object(joe_sportsman,
	extends(joe_person)).

	:- public([
		sport/1, stamina/1, weight/1
	]).

	sport(snowboard).

	stamina(30).

	weight(111).

:- end_object.

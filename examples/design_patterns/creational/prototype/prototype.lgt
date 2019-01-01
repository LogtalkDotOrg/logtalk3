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


% our example uses cars, with two prototype instances, one for a diesel
% powered car and one for a gasoline powered car

:- object(metacar,
	instantiates(metacar)).

	% the clone operation creates a new object with the
	% same state as the currently configured prototype
	:- public(clone/1).
	clone(Clone) :-
		self(Self),
		::prototype_(Prototype),
		::state(Prototype, State),
		create_object(Clone, [instantiates(Self)], [], State).

	% allow changing the configured prototype
	:- public(set_prototype/1).
	set_prototype(Prototype) :-
		::retractall(prototype_(_)),
		::assertz(prototype_(Prototype)).

	:- private(prototype_/1).
	:- dynamic(prototype_/1).

	% predicate for retrieving the prototype state;
	% to be defined in the class
	:- private(state/2).

:- end_object.


:- object(car,
	instantiates(metacar)).

	:- public(describe/0).
	describe :-
		::motor(Motor),
		::doors_(Doors),
		::color_(Color),
		write('Motor: '), write(Motor), nl,
		write('Doors: '), write(Doors), nl,
		write('Color: '), write(Color), nl.

	% car state predicates, one static and two dynamic
	:- private([
		motor/1, doors_/1, color_/1
	]).
	:- dynamic([
		doors_/1, color_/1
	]).

	state(Prototype, [motor(Motor), doors_(Doors), color_(Color)]) :-
		Prototype::motor(Motor),
		Prototype::doors_(Doors),
		Prototype::color_(Color).

:- end_object.


:- object(diesel_car_prototype,
	instantiates(car)).

	motor(diesel).
	doors_(4).
	color_(blue).

:- end_object.


:- object(gasoline_car_prototype,
	instantiates(car)).

	motor(gasoline).
	doors_(2).
	color_(red).

:- end_object.

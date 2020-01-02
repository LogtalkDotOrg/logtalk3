%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Builder design pattern:
%
% https://en.wikipedia.org/wiki/Builder_pattern


% in this example, the products to be built are cars, simplified to
% three build options: wheels, seats, and colors


% we start by defining two protocols, one for cars and one builders

:- protocol(car_protocol).

	:- public([
		wheels/1, seats/1, color/1
	]).

:- end_protocol.


:- protocol(builder_protocol).

	:- public([
		set_wheels/1, set_seats/1, set_color/1, get_result/1
	]).

:- end_protocol.


% the first alternative solution uses a parametric object to represent
% cars with the parameters being used to represent the build options

:- object(car(_Wheels_,_Seats_,_Color_),
	implements(car_protocol)).

	wheels(_Wheels_).
	seats(_Seats_).
	color(_Color_).

:- end_object.


% in this case, the builder, also a parametric object, accepts build
% options and returns an identifier for the car parametric object

:- object(builder(_Car_),
	implements(builder_protocol)).

	:- public(set_wheels/1).
	set_wheels(Wheels) :-
		_Car_ = car(Wheels, _, _).

	:- public(set_seats/1).
	set_seats(Seats) :-
		_Car_ = car(_, Seats, _).

	:- public(set_color/1).
	set_color(Color) :-
		_Car_ = car(_, _, Color).

	:- public(get_result/1).
	get_result(_Car_).

:- end_object.


% the second alternative solution is heavier and less declarative, using
% objects with dynamic state for representing cars and builders

:- object(car,
	implements(car_protocol)).

	:- public(new/1).
	new(Car) :-
		self(Self),
		% create a new car with a default state
		create_object(Car, [extends(Self)], [], [wheels(4),seats(4),color(black)]).

	:- public(set_wheels/1).
	set_wheels(Wheels) :-
		::retractall(wheels(_)),
		::assertz(wheels(Wheels)).

	:- public(set_seats/1).
	set_seats(Seats) :-
		::retractall(seats(_)),
		::assertz(seats(Seats)).

	:- public(set_color/1).
	set_color(Color) :-
		::retractall(color(_)),
		::assertz(color(Color)).

:- end_object.


% building a car requires creating a new builder (in this case, a
% derived prototype)

:- object(builder,
	implements(builder_protocol)).

	:- public(new/1).
	new(Builder) :-
		self(Self),
		% create a new car with a default state
		car::new(Car),
		create_object(Builder, [extends(Self)], [], [car(Car)]).

	:- private(car/1).

	set_wheels(Wheels) :-
		::car(Car),
		Car::set_wheels(Wheels).

	set_seats(Seats) :-
		::car(Car),
		Car::set_seats(Seats).

	set_color(Color) :-
		::car(Car),
		Car::set_color(Color).

	get_result(Car) :-
		::car(Car).

:- end_object.

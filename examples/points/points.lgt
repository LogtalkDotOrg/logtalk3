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


:- object(point,
	instantiates(class),
	specializes(object)).

	:- info([
		version is 1.2,
		date is 2012/10/25,
		author is 'Paulo Moura',
		comment is 'Two dimensional point class.',
		source is 'Example adapted from the SICStus Objects documentation.'
	]).

	:- public(move/2).
	:- mode(move(+integer, +integer), zero_or_one).

	:- public(print/0).
	:- mode(print, one).

	:- public(position/2).
	:- mode(position(?integer, ?integer), one).

	:- private(position_/2).
	:- dynamic(position_/2).
	:- mode(position_(?integer, ?integer), one).

	move(X, Y) :-
		::retractall(position_(_, _)),
		::assertz(position_(X, Y)).

	position(X, Y) :-
		::position_(X, Y).

	print :-
		self(Self),
		::position_(X, Y),
		writeq(Self), write(' @ '), write((X, Y)), nl.

	default_init_option(position-(0, 0)).
	default_init_option(Default) :-
		^^default_init_option(Default).

	process_init_option(position-(X, Y)) :-
		::assertz(position_(X, Y)).
	process_init_option(Option) :-
		^^process_init_option(Option).

	instance_base_name(p).

:- end_object.



:- category(bounded_coordinate).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Point coordinate bounds management predicates.',
		source is 'Example adapted from the SICStus Objects documentation.'
	]).

	:- public(set_bounds/3).
	:- mode(set_bounds(+atom, +integer, +integer), one).

	:- public(clear_bounds/1).
	:- mode(clear_bounds(+atom), one).

	:- public(bounds/3).
	:- mode(bounds(?atom, ?integer, ?integer), zero_or_more).

	:- public(check_bounds/2).
	:- mode(check_bounds(+atom, +integer), zero_or_one).

	:- public(print_bounds/1).
	:- mode(print_bounds(?atom), zero_or_more).

	:- public(valid_value/2).
	:- mode(valid_value(+atom, +integer), zero_or_one).

	:- private(bounds_/3).
	:- dynamic(bounds_/3).
	:- mode(bounds_(?atom, ?integer, ?integer), zero_or_more).

	set_bounds(Coordinate, Min, Max) :-
		::retractall(bounds_(Coordinate, _, _)),
		::assertz(bounds_(Coordinate, Min, Max)).

	clear_bounds(Coordinate) :-
		::retractall(bounds_(Coordinate, _, _)).

	bounds(Coordinate, Min, Max) :-
		::bounds_(Coordinate, Min, Max).

	check_bounds(Coordinate, Value) :-
		::bounds_(Coordinate, Min, Max),
		Value >= Min,
		Value =< Max.

	print_bounds(Coordinate) :-
		::bounds_(Coordinate, Min, Max),
		writeq(bounds(Coordinate)),
		write(' : '),
		write((Min, Max)),
		nl.

	valid_value(Coordinate, Value) :-
		(	::bounds_(Coordinate, Min, Max) ->
			Value >= Min, Value =< Max
		;	true
		).

:- end_category.



:- object(bounded_point,
	imports(bounded_coordinate),
	instantiates(class),
	specializes(point)).

	:- info([
		version is 1.1,
		date is 2000/10/31,
		author is 'Paulo Moura',
		comment is 'Two dimensional point moving in a constrained area.',
		source is 'Example adapted from the SICStus Objects documentation.'
	]).

	move(X, Y) :-
		::check_bounds(x, X),
		::check_bounds(y, Y),
		^^move(X, Y).

	print :-
		::print_bounds(x),
		::print_bounds(y),
		^^print.

	instance_base_name(bp).

	default_init_option(bounds(x)-(-10, 10)).
	default_init_option(bounds(y)-(-10, 10)).
	default_init_option(Default) :-
		^^default_init_option(Default).

	process_init_option(bounds(Coordinate)-(Min, Max)) :-
		::set_bounds(Coordinate, Min, Max).
	process_init_option(Option) :-
		^^process_init_option(Option).

:- end_object.



:- category(point_history).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Point position history management predicates.',
		source is 'Example adapted from the SICStus Objects documentation.'
	]).

	:- public(add_to_history/1).
	:- mode(add_to_history(+nonvar), one).

	:- public(init_history/1).
	:- mode(init_history(+list), one).

	:- public(history/1).
	:- mode(history(-list), zero_or_one).

	:- public(print_history/0).
	:- mode(print_history, zero_or_one).

	:- private(history_/1).
	:- dynamic(history_/1).
	:- mode(history_(-list), zero_or_one).

	add_to_history(Location) :-
		::retract(history_(History)),
		::assertz(history_([Location| History])).

	init_history(History) :-
		::retractall(history_(_)),
		::assertz(history_(History)).

	history(History) :-
		::history_(History).

	print_history :-
		::history_(History),
		write('location history: '),
		write(History),
		nl.

:- end_category.



:- object(history_point,
	imports(point_history),
	instantiates(class),
	specializes(point)).

	:- info([
		version is 1.1,
		date is 2000/10/31,
		author is 'Paulo Moura',
		comment is 'Two dimensional point remembering past positions.',
		source is 'Example adapted from the SICStus Objects documentation.'
	]).

	move(X, Y) :-
		::position(OldX, OldY),
		^^move(X, Y),
		::add_to_history((OldX, OldY)).

	print :-
		::print_history,
		^^print.

	instance_base_name(hp).

	default_init_option(history-[]).
	default_init_option(Default) :-
		^^default_init_option(Default).

	process_init_option(history-History) :-
		::init_history(History).
	process_init_option(Option) :-
		^^process_init_option(Option).

:- end_object.



:- object(bounded_history_point,
	imports((bounded_coordinate, point_history)),
	instantiates(class),
	specializes(point)).

	:- info([
		version is 1.1,
		date is 2000/10/31,
		author is 'Paulo Moura',
		comment is 'Two dimensional point moving in a constrained area and remembering past point positions.',
		source is 'Example adapted from the SICStus Objects documentation.'
	]).

	move(X, Y) :-
		::check_bounds(x, X),
		::check_bounds(y, Y),
		::position(OldX, OldY),
		^^move(X, Y),
		::add_to_history((OldX, OldY)).

	print :-
		::print_bounds(x),
		::print_bounds(y),
		::print_history,
		^^print.

	instance_base_name(bhp).

	default_init_option(history-[]).
	default_init_option(bounds(x)-(-10, 10)).
	default_init_option(bounds(y)-(-10, 10)).
	default_init_option(Default) :-
		^^default_init_option(Default).

	process_init_option(history-History) :-
		::init_history(History).
	process_init_option(bounds(Coordinate)-(Min, Max)) :-
		::set_bounds(Coordinate, Min, Max).
	process_init_option(Option) :-
		^^process_init_option(Option).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(polygon,
	instantiates(abstract_class),
	specializes(object)).

	:- info([
		version is 1.2,
		date is 2005/8/15,
		author is 'Paulo Moura',
		comment is 'Polygon predicates.'
	]).

	:- public(move/2).
	:- mode(move(+integer, +integer), one).

	:- public(trans_x/2).
	:- mode(trans_x(+integer), one).

	:- public(trans_y/2).
	:- mode(trans_y(+integer), one).

	:- public(number_of_sides/1).
	:- mode(number_of_sides(?integer), zero_or_one).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).

	:- private(position_/2).
	:- dynamic(position_/2).
	:- mode(position_(?integer, ?integer), zero_or_one).

	position(X, Y) :-
		::position_(X, Y).

	move(X, Y) :-
		::retractall(position_(_, _)),
		::assertz(position_(X, Y)).

	trans_x(X) :-
		::retractall(position_(_, Y)),
		::assertz(position_(X, Y)).

	trans_y(Y) :-
		::retractall(position_(X, _)),
		::assertz(position_(X, Y)).

	default_init_option(position-(0, 0)).
	default_init_option(Default) :-
		^^default_init_option(Default).

	process_init_option(position-(X, Y)) :-
		::move(X, Y).
	process_init_option(Option) :-
		^^process_init_option(Option).

:- end_object.



:- object(triangle,
	instantiates(class),
	specializes(polygon)).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Triangle class.'
	]).

	number_of_sides(3).

	instance_base_name(tri).

:- end_object.



:- object(square,
	instantiates(class),
	specializes(polygon)).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Square class.'
	]).

	number_of_sides(4).

	instance_base_name(sq).

:- end_object.



:- object(pentagon,
	instantiates(class),
	specializes(polygon)).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Pentagon class.'
	]).

	number_of_sides(5).

	instance_base_name(pen).

:- end_object.



:- object(hexagon,
	instantiates(class),
	specializes(polygon)).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Hexagon class.'
	]).

	number_of_sides(6).

	instance_base_name(hex).

:- end_object.



:- object(concentric,
	instantiates(constrained_relation)).

	:- info([
		version is 1.1,
		date is 2004/8/15,
		author is 'Paulo Moura',
		comment is 'Concentric polygons as a constrained binary relation.'
	]).

	:- uses(list,
		[member/2, select/3]).

	descriptor_([x1, x2]).

	domain_(x1, polygon).
	domain_(x2, polygon).

	key_([x1, x2]).

	cardinality_(x1, 0, n).
	cardinality_(x2, 0, n).

	delete_option_(x1, cascade).
	delete_option_(x2, cascade).

	add_tuple([Polygon| Polygons]) :-
		Polygon::position(X, Y),
		forall(member(Polygon2, Polygons), {Polygon2::move(X, Y)}),
		^^add_tuple([Polygon| Polygons]).

	activ_points_(x1, before, []).
	activ_points_(x1, after, [move(_, _), trans_x(_), trans_y(_)]).

	activ_points_(x2, before, []).
	activ_points_(x2, after, [move(_, _), trans_x(_), trans_y(_)]).

	propagate(after, move(X, Y), Polygon, _, Tuple) :-
		select(Polygon, Tuple, Polygons),
		!,
		forall(
			(member(Polygon2, Polygons),\+ Polygon2::position(X, Y)),
			{Polygon2::move(X, Y)}).

	propagate(after, trans_x(X), Polygon, _, Tuple) :-
		select(Polygon, Tuple, Polygons),
		!,
		forall(
			(member(Polygon2, Polygons), \+ Polygon2::position(X, _)),
			{Polygon2::trans_x(X)}).

	propagate(after, trans_y(Y), Polygon, _, Tuple) :-
		select(Polygon, Tuple, Polygons),
		!,
		forall(
			(member(Polygon2, Polygons), \+ Polygon2::position(_, Y)),
			{Polygon2::trans_y(Y)}).

:- end_object.

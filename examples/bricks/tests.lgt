%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- set_logtalk_flag(unknown_entities, silent).
	:- set_logtalk_flag(events, allow).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "bricks" example.'
	]).

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(bricks_1) :-
		brick::new(a, [position-(8, 1)]),
		brick::new(b, [position-(6, 1)]),
		brick::new(c, [position-(4, 1)]),
		brick::new(d, [position-(2, 1)]),
		brick_stack::add_tuple([c,d]),
		brick_stack::add_tuple([b,c]),
		brick_stack::add_tuple([a,b]),
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [b,c], [c,d]].

	test(bricks_2) :-
		d::move(9, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 9,
		Xb == 9,
		Xc == 9,
		Xd == 9,
		Ya == 4,
		Yb == 3,
		Yc == 2,
		Yd == 1.

	test(bricks_3) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [b,c], [c,d]].

	test(bricks_4) :-
		b::move(3, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 3,
		Xb == 3,
		Xc == 9,
		Xd == 9,
		Ya == 2,
		Yb == 1,
		Yc == 2,
		Yd == 1.

	test(bricks_5) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [c,d]].

	test(bricks_6) :-
		brick_stack::add_tuple([d, a]),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 3,
		Xb == 3,
		Xc == 3,
		Xd == 3,
		Ya == 2,
		Yb == 1,
		Yc == 4,
		Yd == 3.

	test(bricks_7) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [c,d], [d,a]].

	test(bricks_8) :-
		b::move(5, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 5,
		Xb == 5,
		Xc == 5,
		Xd == 5,
		Ya == 2,
		Yb == 1,
		Yc == 4,
		Yd == 3.

	test(bricks_9) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [c,d], [d,a]].

:- end_object.

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


:- object(tests,
	extends(lgtunit)).

	:- set_logtalk_flag(unknown_entities, silent).
	:- set_logtalk_flag(events, allow).

	:- info([
		version is 1:1:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-06-02,
		comment is 'Unit tests for the "bricks" example.'
	]).

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(bricks_01, true) :-
		brick::new(a, [position-(8, 1)]),
		brick::new(b, [position-(6, 1)]),
		brick::new(c, [position-(4, 1)]),
		brick::new(d, [position-(2, 1)]).

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(bricks_02, true(TuplesSorted == [[a,b], [b,c], [c,d]])) :-
		brick_stack::add_tuple([c,d]),
		brick_stack::add_tuple([b,c]),
		brick_stack::add_tuple([a,b]),
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted).

	test(bricks_03, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [9-4,9-3,9-2,9-1])) :-
		d::move(9, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(bricks_04, true(TuplesSorted == [[a,b], [b,c], [c,d]])) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted).

	test(bricks_05, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [3-2,3-1,9-2,9-1])) :-
		b::move(3, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(bricks_06, true(TuplesSorted == [[a,b], [c,d]])) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted).

	test(bricks_07, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [3-2,3-1,3-4,3-3])) :-
		brick_stack::add_tuple([d, a]),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(bricks_08, true(TuplesSorted == [[a,b], [c,d], [d,a]])) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted).

	test(bricks_09, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [5-2,5-1,5-4,5-3])) :-
		b::move(5, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(bricks_10, true(TuplesSorted == [[a,b], [c,d], [d,a]])) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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

	% avoid warnings as the tests create at runtime new objects with
	% fixed identifiers that are used as references in other tests
	:- set_logtalk_flag(unknown_entities, silent).

	% generate events for all messages so that the
	% "block_stack" object can perform its magic
	:- set_logtalk_flag(events, allow).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-06-02,
		comment is 'Unit tests for the "blocks" example.'
	]).

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(blocks_01, true) :-
		block::new(a, [position-(8, 1)]),
		block::new(b, [position-(6, 1)]),
		block::new(c, [position-(4, 1)]),
		block::new(d, [position-(2, 1)]).

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(blocks_02, true(TuplesSorted == [a-b, b-c, c-d])) :-
		block_stack::add_tuple(c-d),
		block_stack::add_tuple(b-c),
		block_stack::add_tuple(a-b),
		block_stack::tuples(Tuples),
		list::msort(Tuples, TuplesSorted).

	test(blocks_03, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [9-4,9-3,9-2,9-1])) :-
		d::move(9, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(blocks_04, true(TuplesSorted == [a-b, b-c, c-d])) :-
		block_stack::tuples(Tuples),
		list::msort(Tuples, TuplesSorted).

	test(blocks_05, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [3-2,3-1,9-2,9-1])) :-
		b::move(3, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(blocks_06, true(TuplesSorted == [a-b, c-d])) :-
		block_stack::tuples(Tuples),
		list::msort(Tuples, TuplesSorted).

	test(blocks_07, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [3-2,3-1,3-4,3-3])) :-
		block_stack::add_tuple(d-a),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(blocks_08, true(TuplesSorted == [a-b, c-d, d-a])) :-
		block_stack::tuples(Tuples),
		list::msort(Tuples, TuplesSorted).

	test(blocks_09, true([Xa-Ya,Xb-Yb,Xc-Yc,Xd-Yd] == [5-2,5-1,5-4,5-3])) :-
		b::move(5, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).

	test(blocks_10, true(TuplesSorted == [a-b, c-d, d-a])) :-
		block_stack::tuples(Tuples),
		list::msort(Tuples, TuplesSorted).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-09,
		comment is 'Unit tests for the "deques" library.'
	]).

	:- uses(deque, [
		new/1, empty/1, push_front/3, push_back/3, pop_front/3, pop_back/3, peek_front/2, peek_back/2,
		length/2, as_list/2, as_deque/2, valid/1, map/2, map/3
	]).

	cover(deque).

	% new/1 tests

	test(deque_new_1_01, deterministic) :-
		new(Deque),
		valid(Deque).

	test(deque_new_1_02, deterministic(N == 0)) :-
		new(Deque),
		length(Deque, N).

	% empty/1 tests

	test(deque_empty_1_01, deterministic) :-
		new(Deque),
		empty(Deque).

	test(deque_empty_1_02, deterministic) :-
		new(Deque0),
		push_front(1, Deque0, Deque),
		\+ empty(Deque).

	test(deque_empty_1_03, deterministic) :-
		new(Deque0),
		push_back(1, Deque0, Deque),
		\+ empty(Deque).

	% push_front/3

	test(deque_head_2_01, deterministic(Element == 1)) :-
		new(Deque0),
		push_front(1, Deque0, Deque),
		peek_front(Deque, Element).

	% push_back/3 tests

	test(deque_push_back_3_01, deterministic(Element == 1)) :-
		new(Deque0),
		push_back(1, Deque0, Deque),
		peek_back(Deque, Element).

	test(deque_push_back_3_02, deterministic(Element == 3)) :-
		new(Deque0),
		push_back(1, Deque0, Deque1),
		push_back(2, Deque1, Deque2),
		push_back(3, Deque2, Deque),
		peek_back(Deque, Element).

	% pop_front/3 tests

	test(deque_pop_front_3_01, false) :-
		new(Deque),
		pop_front(Deque, _, _).

	test(deque_pop_front_3_02, deterministic(Element-List == 1-[2,3])) :-
		new(Deque0),
		push_back(1, Deque0, Deque1),
		push_back(2, Deque1, Deque2),
		push_back(3, Deque2, Deque3),
		pop_front(Deque3, Element, Deque),
		as_list(Deque, List).

	test(deque_pop_front_3_03, deterministic(Element-List == 4-[5,6])) :-
		new(Deque0),
		push_front(6, Deque0, Deque1),
		push_front(5, Deque1, Deque2),
		push_front(4, Deque2, Deque3),
		pop_front(Deque3, Element, Deque),
		as_list(Deque, List).

	% pop_back/3 tests

	test(deque_pop_back_3_01, false) :-
		new(Deque),
		pop_back(Deque, _, _).

	test(deque_pop_back_3_02, deterministic(Element-List == 3-[1,2])) :-
		new(Deque0),
		push_front(3, Deque0, Deque1),
		push_front(2, Deque1, Deque2),
		push_front(1, Deque2, Deque3),
		pop_back(Deque3, Element, Deque),
		as_list(Deque, List).

	test(deque_pop_back_3_03, deterministic(Element-List == 6-[4,5])) :-
		new(Deque0),
		push_back(4, Deque0, Deque1),
		push_back(5, Deque1, Deque2),
		push_back(6, Deque2, Deque3),
		pop_back(Deque3, Element, Deque),
		as_list(Deque, List).

	% peek_front/2 tests

	test(deque_peek_front_2_01, false) :-
		new(Deque),
		peek_front(Deque, _).

	test(deque_peek_front_2_02, deterministic(Element == 3)) :-
		new(Deque0),
		push_front(1, Deque0, Deque1),
		push_front(2, Deque1, Deque2),
		push_front(3, Deque2, Deque),
		peek_front(Deque, Element).

	test(deque_peek_front_2_03, deterministic(Element == 1)) :-
		new(Deque0),
		push_back(1, Deque0, Deque1),
		push_back(2, Deque1, Deque2),
		push_back(3, Deque2, Deque),
		peek_front(Deque, Element).

	% peek_back/2 tests

	test(deque_peek_back_2_01, false) :-
		new(Deque),
		peek_back(Deque, _).

	test(deque_peek_back_2_02, deterministic(Element == 3)) :-
		new(Deque0),
		push_back(1, Deque0, Deque1),
		push_back(2, Deque1, Deque2),
		push_back(3, Deque2, Deque),
		peek_back(Deque, Element).

	test(deque_peek_back_2_03, deterministic(Element == 1)) :-
		new(Deque0),
		push_front(1, Deque0, Deque1),
		push_front(2, Deque1, Deque2),
		push_front(3, Deque2, Deque),
		peek_back(Deque, Element).

	% length/2 tests

	test(deque_length_2_01, deterministic(Length == 0)) :-
		new(Deque),
		length(Deque, Length).

	test(deque_length_2_02, deterministic(Length == 2)) :-
		new(Deque0),
		push_front(1, Deque0, Deque1),
		push_back(2, Deque1, Deque),
		length(Deque, Length).

	% as_list/2 tests

	test(deque_as_list_2_01, deterministic(List == [])) :-
		new(Deque),
		as_list(Deque, List).

	test(deque_as_list_2_02, deterministic(List == [1,2])) :-
		new(Deque0),
		push_front(1, Deque0, Deque1),
		push_back(2, Deque1, Deque),
		as_list(Deque, List).

	% as_deque/2 tests

	test(deque_as_deque_2_01, deterministic(List == [])) :-
		as_deque([], Deque),
		as_list(Deque, List).

	test(deque_as_deque_2_02, deterministic(List == [1,2,3])) :-
		as_deque([1,2,3], Deque),
		as_list(Deque, List).

	% map/2 tests

	test(deque_map_2_01, true) :-
		new(Deque),
		map(integer, Deque).

	test(deque_map_2_02, true) :-
		as_deque([1,2,3], Deque),
		map(integer, Deque).

	test(deque_map_2_03, false) :-
		as_deque([1,2,3], Deque),
		map(atom, Deque).

	% map/3 tests

	test(deque_map_3_01, true(List == [])) :-
		new(Deque0),
		map(integer, Deque0, Deque),
		as_list(Deque, List).

	test(deque_map_3_02, true(List == [97,98,99])) :-
		as_deque([a,b,c], Deque0),
		map(char_code, Deque0, Deque),
		as_list(Deque, List).

	% valid/1 tests

	test(deque_valid_1_01, deterministic) :-
		new(Deque),
		valid(Deque).

	test(deque_valid_1_02, deterministic) :-
		as_deque([1,2,3], Deque),
		valid(Deque).

:- end_object.

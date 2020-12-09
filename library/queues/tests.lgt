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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-12-09,
		comment is 'Unit tests for the "queues" library.'
	]).

	:- uses(queue, [
		as_list/2,
		empty/1, head/2, serve/3, length/2,
		join/3, join_all/3,
		jump/3, jump_all/3, jump_all_block/3,
		append/3,
		map/2, map/3,
		new/1, valid/1
	]).

	cover(queue).

	% new/1 tests

	test(queue_new_1_01, deterministic) :-
		new(EmptyQueue),
		valid(EmptyQueue).

	% empty/1 tests

	test(queue_empty_1_01, deterministic) :-
		new(EmptyQueue),
		empty(EmptyQueue).

	test(queue_empty_1_02, deterministic) :-
		new(EmptyQueue),
		join(1, EmptyQueue, Queue),
		\+ empty(Queue).

	% head/2 tests

	test(queue_head_2_01, deterministic) :-
		new(EmptyQueue),
		\+ head(EmptyQueue, _).

	% join/3 tests

	test(queue_join_3_01, deterministic(Head == 1)) :-
		new(Queue),
		join(1, Queue, NewQueue),
		head(NewQueue, Head).

	test(queue_join_3_02, deterministic(Head == 1)) :-
		new(EmptyQueue),
		join(1, EmptyQueue, Queue0),
		join(2, Queue0, Queue1),
		join(3, Queue1, Queue),
		head(Queue, Head).

	% join_all/3 tests

	test(queue_join_all_3_01, deterministic(EmptyQueue == Queue)) :-
		new(EmptyQueue),
		join_all([], EmptyQueue, Queue).

	test(queue_join_all_3_02, deterministic(Head == 1)) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		head(Queue, Head).

	% jump/3 tests

	test(queue_jump_3_01, deterministic(Head == 0)) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		jump(0, Queue, NewQueue),
		head(NewQueue, Head).

	% jump_all/3 tests

	test(queue_jump_all_3_01, deterministic(Queue == NewQueue)) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		jump_all([], Queue, NewQueue).

	test(queue_jump_all_3_02, deterministic(Head == c)) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		jump_all([a,b,c], Queue, NewQueue),
		head(NewQueue, Head).

	% jump_all_block/3 tests

	test(queue_jump_all_block_3_01, deterministic(Queue == NewQueue)) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		jump_all_block([], Queue, NewQueue).

	test(queue_jump_all_block_3_02, deterministic(Head == a)) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		jump_all_block([a,b,c], Queue, NewQueue),
		head(NewQueue, Head).

	% append/3 tests

	test(queue_append_3_01, deterministic(Queue2 == NewQueue)) :-
		new(EmptyQueue1),
		new(EmptyQueue2),
		join_all([1,2,3], EmptyQueue2, Queue2),
		append(EmptyQueue1, Queue2, NewQueue).

	test(queue_append_3_02, deterministic(Queue1 == NewQueue)) :-
		new(EmptyQueue1),
		join_all([1,2,3], EmptyQueue1, Queue1),
		new(EmptyQueue2),
		append(Queue1, EmptyQueue2, NewQueue).

	test(queue_append_3_03, deterministic(List == [1,2,3,4,5,6])) :-
		new(EmptyQueue1),
		join_all([1,2,3], EmptyQueue1, Queue1),
		new(EmptyQueue2),
		join_all([4,5,6], EmptyQueue2, Queue2),
		append(Queue1, Queue2, NewQueue),
		as_list(NewQueue, List).

	% length/2 tests

	test(queue_length_2_01, deterministic(Length == 0)) :-
		new(EmptyQueue),
		length(EmptyQueue, Length).

	test(queue_length_2_02, deterministic(Length == 3)) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		length(Queue, Length).

	% map/2 tests

	test(queue_map_2_01, true) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		map(integer, Queue).

	test(queue_map_2_02, false) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		map(atom, Queue).

	% map/3 tests

	test(queue_map_3_01, true(List == [97,98,99])) :-
		new(EmptyQueue),
		join_all([a,b,c], EmptyQueue, Queue),
		map(char_code, Queue, NewQueue),
		as_list(NewQueue, List).

	% serve/3 tests

	test(queue_serve_3_01, deterministic) :-
		new(EmptyQueue),
		\+ serve(EmptyQueue, _, _).

	test(queue_serve_3_02, deterministic) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		serve(Queue, Head, NewQueue),
		^^assertion(head, Head == 1),
		as_list(NewQueue, List),
		^^assertion(list, List == [2,3]).

	% as_list/2 tests

	test(queue_as_list_2_01, deterministic(List == [])) :-
		new(EmptyQueue),
		as_list(EmptyQueue, List).

	test(queue_as_list_2_02, deterministic(List == [1,2,3])) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		as_list(Queue, List).

	% valid/1 tests

	test(queue_valid_1_01, deterministic) :-
		new(EmptyQueue),
		join_all([], EmptyQueue, Queue),
		valid(Queue).

	test(queue_valid_1_02, deterministic) :-
		new(EmptyQueue),
		join_all([1,2,3], EmptyQueue, Queue),
		valid(Queue).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:5:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2024-11-06,
		comment is 'Unit tests for the "searching" example.'
	]).

	cover(state_space).
	cover(heuristic_state_space).
	cover(search_strategy).
	cover(blind_search(_)).
	cover(heuristic_search(_)).
	cover(breadth_first(_)).
	cover(depth_first(_)).
	cover(best_first(_)).
	cover(hill_climbing(_)).

	% generate all solutions then check this path is one of them
	test(searching_01) :-
		farmer::initial_state(Initial),
		findall(Path, depth_first(8)::solve(farmer, Initial, Path), Solutions),
		ground(Solutions),
		Answer = [
			s(north,north,north,north),s(north,south,north,south),s(north,south,north,north),
			s(south,south,north,south),s(south,north,north,north),s(south,north,south,south),
			s(south,north,south,north),s(south,south,south,south)],
		list::memberchk(Answer, Solutions).

	% generate all solutions then check this path is one of them
	test(searching_02) :-
		miss_cann::initial_state(Initial),
		findall(Cost-Path, hill_climbing(15)::solve(miss_cann, Initial, Path, Cost), Solutions),
		ground(Solutions),
		Answer = 15-[
			s((3,3),left,(0,0)),s((3,1),right,(0,2)),s((3,2),left,(0,1)),s((3,0),right,(0,3)),
			s((3,1),left,(0,2)),s((1,1),right,(2,2)),s((2,2),left,(1,1)),s((0,2),right,(3,1)),
			s((0,3),left,(3,0)),s((0,1),right,(3,2)),s((0,2),left,(3,1)),s((0,0),right,(3,3))
		],
		list::memberchk(Answer, Solutions).

	% generate all solutions then check this path is one of them
	test(searching_03) :-
		miss_cann::initial_state(Initial),
		best_first(16)::solve(miss_cann, Initial, Path, Cost),
		Path == [
			s((3,3),left,(0,0)),s((3,1),right,(0,2)),s((3,2),left,(0,1)),s((3,0),right,(0,3)),
			s((3,1),left,(0,2)),s((1,1),right,(2,2)),s((2,2),left,(1,1)),s((0,2),right,(3,1)),
			s((0,3),left,(3,0)),s((0,1),right,(3,2)),s((1,1),left,(2,2)),s((0,0),right,(3,3))
		],
		Cost == 15.

	test(searching_04) :-
		bridge::initial_state(Initial),
		hill_climbing(30)::solve(bridge, Initial, Path, Cost),
		Path == [
			s([],right,[1,3,6,8,12]),s([1,3],left,[6,8,12]),s([3],right,[1,6,8,12]),
			s([1,3,6],left,[8,12]),s([3,6],right,[1,8,12]),s([3,6,8,12],left,[1]),
			s([6,8,12],right,[1,3]),s([1,3,6,8,12],left,[])
		],
		Cost == 29.

	% generate all solutions then check this path is one of them
	test(searching_05) :-
		water_jug::initial_state(Initial),
		findall(Path, breadth_first(5)::solve(water_jug, Initial, Path), Solutions),
		ground(Solutions),
		Answer = [(0,0),(0,3),(3,0),(3,3),(4,2),(0,2)],
		list::memberchk(Answer, Solutions).

	% generate all solutions then check this path is one of them
	test(searching_06) :-
		water_jug::initial_state(Initial),
		findall(Path, depth_first(7)::solve(water_jug, Initial, Path), Solutions),
		ground(Solutions),
		Answer = [(0,0),(4,0),(4,3),(0,3),(3,0),(3,3),(4,2),(0,2)],
		list::memberchk(Answer, Solutions).

	test(searching_07) :-
		salt(100, 500, 200)::initial_state(Initial),
		breadth_first(6)::solve(salt(100, 500, 200), Initial, Path),
		Path == [
			s(0,0,0,all_empty),s(0,500,0,fill(m1)),s(0,300,200,transfer(m1,m2)),
			s(0,300,0,empty(m2)),s(0,100,200,transfer(m1,m2)),s(100,0,200,transfer(m1,acc))
		].

	% generate all solutions then check this path is one of them
	test(searching_08) :-
		eight_puzzle::initial_state(five_steps, Initial),
		findall(Cost-Path, hill_climbing(5)::solve(eight_puzzle, Initial, Path, Cost), Solutions),
		ground(Solutions),
		Answer = 5-[
			[2-1,1-2,1-3,3-3,3-2,3-1,2-2,1-1,2-3],
			[2-2,1-2,1-3,3-3,3-2,3-1,2-1,1-1,2-3],
			[2-3,1-2,1-3,3-3,3-2,3-1,2-1,1-1,2-2],
			[1-3,1-2,2-3,3-3,3-2,3-1,2-1,1-1,2-2],
			[1-2,1-3,2-3,3-3,3-2,3-1,2-1,1-1,2-2],
			[2-2,1-3,2-3,3-3,3-2,3-1,2-1,1-1,1-2]
		],
		list::memberchk(Answer, Solutions).

:- end_object.

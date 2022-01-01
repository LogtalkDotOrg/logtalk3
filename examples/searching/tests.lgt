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

	:- info([
		version is 1:4:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-02-03,
		comment is 'Unit tests for the "searching" example.'
	]).

	test(searching_1) :-
		farmer::initial_state(Initial),
		Initial == (north,north,north,north),
		findall(Path, depth_first(8)::solve(farmer, Initial, Path), Solutions),
		ground(Solutions),
		Answer = [
			(north,north,north,north),(north,south,north,south),(north,south,north,north),
			(south,south,north,south),(south,north,north,north),(south,north,south,south),
			(south,north,south,north),(south,south,south,south)],
		list::memberchk(Answer, Solutions).

	% test 2.  % generate all solutions then check this path is one of them
	test(searching_2) :-
		miss_cann::initial_state(Initial),
		findall(Cost-Path, hill_climbing(15)::solve(miss_cann, Initial, Path, Cost), Solutions),
		ground(Solutions),
		Answer = 15-[((3,3),left,(0,0)),((3,1),right,(0,2)),((3,2),left,(0,1)),((3,0),right,(0,3)),((3,1),left,(0,2)),((1,1),right,(2,2)),((2,2),left,(1,1)),((0,2),right,(3,1)),((0,3),left,(3,0)),((0,1),right,(3,2)),((0,2),left,(3,1)),((0,0),right,(3,3))],
		list::memberchk(Answer, Solutions).

	test(searching_3) :-
		bridge::initial_state(Initial),
		hill_climbing(30)::solve(bridge, Initial, Path, Cost),
		Path == [s([],right,[1,3,6,8,12]),s([1,3],left,[6,8,12]),s([3],right,[1,6,8,12]),s([1,3,6],left,[8,12]),s([3,6],right,[1,8,12]),s([3,6,8,12],left,[1]),s([6,8,12],right,[1,3]),s([1,3,6,8,12],left,[])],
		Cost == 29.

	% test 4.  % generate all solutions then check this path is one of them
	test(searching_4) :-
		water_jug::initial_state(Initial),
		findall(Path, breadth_first(5)::solve(water_jug, Initial, Path), Solutions),
		ground(Solutions),
		Answer = [(0,0),(0,3),(3,0),(3,3),(4,2),(0,2)],
		list::memberchk(Answer, Solutions).

	% test 5.  % generate all solutions then check this path is one of them
	test(searching_5) :-
		water_jug::initial_state(Initial),
		findall(Path, depth_first(7)::solve(water_jug, Initial, Path), Solutions),
		ground(Solutions),
		Answer = [(0,0),(4,0),(4,3),(0,3),(3,0),(3,3),(4,2),(0,2)],
		list::memberchk(Answer, Solutions).

	% test 6.  % generate all solutions then check this path is one of them
	test(searching_6) :-
		salt(100, 500, 200)::initial_state(Initial),
		breadth_first(6)::solve(salt(100, 500, 200), Initial, Path),
		Path == [s(0,0,0,all_empty),s(0,500,0,fill(m1)),s(0,300,200,transfer(m1,m2)),s(0,300,0,empty(m2)),s(0,100,200,transfer(m1,m2)),s(100,0,200,transfer(m1,acc))].

	% test 7.  % generate all solutions then check this path is one of them
	test(searching_7) :-
		eight_puzzle::initial_state(five_steps, Initial),
		findall(Cost-Path, hill_climbing(5)::solve(eight_puzzle, Initial, Path, Cost), Solutions),
		ground(Solutions),
		Answer = 5-[[2-1,1-2,1-3,3-3,3-2,3-1,2-2,1-1,2-3],[2-2,1-2,1-3,3-3,3-2,3-1,2-1,1-1,2-3],[2-3,1-2,1-3,3-3,3-2,3-1,2-1,1-1,2-2],[1-3,1-2,2-3,3-3,3-2,3-1,2-1,1-1,2-2],[1-2,1-3,2-3,3-3,3-2,3-1,2-1,1-1,2-2],[2-2,1-3,2-3,3-3,3-2,3-1,2-1,1-1,1-2]],
		list::memberchk(Answer, Solutions).

:- end_object.

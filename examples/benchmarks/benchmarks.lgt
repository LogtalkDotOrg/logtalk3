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


:- object(benchmarks).

	:- info([
		version is 5:7:0,
		author is 'Paulo Moura',
		date is 2021-05-24,
		comment is 'Benchmark utility predicates and standard set of benchmarks.'
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs all benchmarks the default number of times.'
	]).

	:- public(run/1).
	:- mode(run(+integer), one).
	:- info(run/1, [
		comment is 'Runs all benchmarks the specified number of times.',
		argnames is ['N']
	]).

	:- public(run/2).
	:- mode(run(+atom, +integer), one).
	:- info(run/2, [
		comment is 'Runs a specific benchmark the specified number of times.',
		argnames is ['Id', 'N']
	]).

	:- public(benchmark/2).
	:- mode(benchmark(?atom, -callable), zero_or_more).
	:- info(benchmark/2, [
		comment is 'Table of benchmark identifiers and benchmark goals.',
		argnames is ['Id', 'Goal']
	]).

	:- uses(os, [cpu_time/1]).

	% run all benchmarks the default number of times:
	run :-
		run(100000).

	% run all benchmark tests N times:
	run(N) :-
		empty_loop_time(N, LoopTime),
		benchmark(Id, Goal),
			run(Id, N, LoopTime, GoalTime, Average, Speed),
			report(Id, Goal, N, LoopTime, GoalTime, Average, Speed),
		fail.
	run(_).

	% run a specific benchmark test:
	run(Id, N) :-
		benchmark(Id, Goal),
		empty_loop_time(N, LoopTime),
		run(Id, N, LoopTime, GoalTime, Average, Speed),
		report(Id, Goal, N, LoopTime, GoalTime, Average, Speed).

	empty_loop_time(N, LoopTime) :-
		cpu_time(Begin),
		do_benchmark(empty_loop, N),
		cpu_time(End),
		LoopTime is End - Begin.

	run(Id, N, LoopTime, GoalTime, Average, Speed) :-
		cpu_time(Begin),
		do_benchmark(Id, N),
		cpu_time(End),
		GoalTime is End - Begin,
		Average is (GoalTime - LoopTime)/N,
		catch(Speed is round(1.0/Average), _, Speed = 'n/a').

	report(Id, Goal, N, LoopTime, GoalTime, Average, Speed) :-
		write(Id), write(' goal: '),
		writeq(Goal), nl,
		write('Number of repetitions: '), write(N), nl,
		write('Empty loop time: '), write(LoopTime), nl,
		write('Goal loop time:  '), write(GoalTime), nl,
		write('Average goal time: '), write(Average), nl,
		write('Number of goals per second: '), write(Speed), nl,
		nl.

	% some benchmark tests for static code:
	benchmark(s11, my_length(List, _)) :-
		{generate_list(30, List)}.
	:- if(current_logtalk_flag(modules, supported)).
	benchmark(s12, ':'(module, mod_length(List, _))) :-
		{generate_list(30, List)}.
	:- endif.
	benchmark(s13, object::length(List, _)) :-
		{generate_list(30, List)}.

	benchmark(s21, my_nrev(List, _)) :-
		{generate_list(30, List)}.
	:- if(current_logtalk_flag(modules, supported)).
	benchmark(s22, ':'(module, mod_nrev(List, _))) :-
		{generate_list(30, List)}.
	:- endif.
	benchmark(s23, object::nrev(List, _)) :-
		{generate_list(30, List)}.

	benchmark(s31, maze_solve(1, 7, _)).
	:- if(current_logtalk_flag(modules, supported)).
	benchmark(s32, ':'(module, mod_maze_solve(1, 7, _))).
	:- endif.
	benchmark(s33, maze::solve(1, 7, _)).

	benchmark(s41, graph_path(0, 4, _)).
	:- if(current_logtalk_flag(modules, supported)).
	benchmark(s42, ':'(module, mod_graph_path(0, 4, _))).
	:- endif.
	benchmark(s43, graph::path(0, 4, _)).

	% some benchmark tests for category predicate calls:
	benchmark(c1, leaf::obj_local).
	benchmark(c2, leaf::ctg_super).
	benchmark(c3, leaf::ctg_self).

	% some benchmark tests for dynamic code:
	benchmark(d1, (create_object(xpto, [], [], []), abolish_object(xpto))).
	benchmark(d2, plain_dyndb(_)).
	benchmark(d3, database::this_dyndb(_)).
	benchmark(d4, database::self_dyndb(_)).
	benchmark(d5, database::other_dyndb(_)).

	% repeat a goal N times without using call/1 and using a failure-driven loop to
	% try to avoid the interference of Prolog compiler memory management mechanism
	% (such as garbage collection) on the results
	do_benchmark(empty_loop, N) :-
		{between(1, N, _)},
		fail.
	do_benchmark(empty_loop, _).

	do_benchmark(s11, N) :-
		{generate_list(30, List)},
		{between(1, N, _)},
			{my_length(List, _)},
		fail.
	do_benchmark(s11, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s12, N) :-
		{generate_list(30, List)},
		{between(1, N, _)},
			':'(module, mod_length(List, _)),
		fail.
	do_benchmark(s12, _).
	:- endif.

	do_benchmark(s13, N) :-
		{generate_list(30, List)},
		{between(1, N, _)},
			object::length(List, _),
		fail.
	do_benchmark(s13, _).

	do_benchmark(s21, N) :-
		{generate_list(30, List)},
		{between(1, N, _)},
			{my_nrev(List, _)},
		fail.
	do_benchmark(s21, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s22, N) :-
		{generate_list(30, List)},
		{between(1, N, _)},
			':'(module, mod_nrev(List, _)),
		fail.
	do_benchmark(s22, _).
	:- endif.

	do_benchmark(s23, N) :-
		{generate_list(30, List)},
		{between(1, N, _)},
			object::nrev(List, _),
		fail.
	do_benchmark(s23, _).

	do_benchmark(s31, N) :-
		{between(1, N, _)},
			({maze_solve(1, 7, _)} -> true; fail),
		fail.
	do_benchmark(s31, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s32, N) :-
		{between(1, N, _)},
			(':'(module, mod_maze_solve(1, 7, _)) -> true; fail),
		fail.
	do_benchmark(s32, _).
	:- endif.

	do_benchmark(s33, N) :-
		{between(1, N, _)},
			(maze::solve(1, 7, _) -> true; fail),
		fail.
	do_benchmark(s33, _).

	do_benchmark(s41, N) :-
		{between(1, N, _)},
			{graph_path(0, 4, _)},
		fail.
	do_benchmark(s41, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s42, N) :-
		{between(1, N, _)},
			':'(module, mod_graph_path(0, 4, _)),
		fail.
	do_benchmark(s42, _).
	:- endif.

	do_benchmark(s43, N) :-
		{between(1, N, _)},
			graph::path(0, 4, _),
		fail.
	do_benchmark(s43, _).

	do_benchmark(c1, N) :-
		{between(1, N, _)},
			leaf::obj_local,
		fail.
	do_benchmark(c1, _).

	do_benchmark(c2, N) :-
		{between(1, N, _)},
			leaf::ctg_super,
		fail.
	do_benchmark(c2, _).

	do_benchmark(c3, N) :-
		{between(1, N, _)},
			leaf::ctg_self,
		fail.
	do_benchmark(c3, _).

	do_benchmark(d1, N) :-
		{between(1, N, _)},
			create_object(xpto, [], [], []),
			abolish_object(xpto),
		fail.
	do_benchmark(d1, _).

	do_benchmark(d2, N) :-
		{between(1, N, _)},
			{plain_dyndb(N)},
		fail.
	do_benchmark(d2, _).

	do_benchmark(d3, N) :-
		{between(1, N, _)},
			database::this_dyndb(N),
		fail.
	do_benchmark(d3, _).

	do_benchmark(d4, N) :-
		{between(1, N, _)},
			database::self_dyndb(N),
		fail.
	do_benchmark(d4, _).

	do_benchmark(d5, N) :-
		{between(1, N, _)},
			database::other_dyndb(N),
		fail.
	do_benchmark(d5, _).

:- end_object.

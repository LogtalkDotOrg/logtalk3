%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(benchmarks).

	:- info([
		version is 5.2,
		author is 'Paulo Moura',
		date is 2011/09/24,
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
	:- mode(move(?atom, -callable), zero_or_more).
	:- info(move/2, [
		comment is 'Table of benchmark identifiers and benchmark goals.',
		argnames is ['Id', 'Goal']
	]).

	% run all benchmarks the default number of times:
	run :-
		run(100000).

	% run all benchmark tests N times:
	run(N) :-
		benchmark(Id, Goal),
		run(Id, N, LoopTime, GoalTime, Average, Speed),
		report(Id, Goal, N, LoopTime, GoalTime, Average, Speed),
		fail.
	run(_).

	% run a specific benchmark test:
	run(Id, N) :-
		benchmark(Id, Goal),
		run(Id, N, LoopTime, GoalTime, Average, Speed),
		report(Id, Goal, N, LoopTime, GoalTime, Average, Speed).

	run(Id, N, LoopTime, GoalTime, Average, Speed) :-
		{'$lgt_cpu_time'(Seconds1)},		% defined in the adapter files
		do_benchmark(empty_loop, N),
		{'$lgt_cpu_time'(Seconds2)},
		LoopTime is Seconds2 - Seconds1,
		{'$lgt_cpu_time'(Seconds3)},
		do_benchmark(Id, N),
		{'$lgt_cpu_time'(Seconds4)},
		GoalTime is Seconds4 - Seconds3,
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
	benchmark(c2, leaf::ctg_direct).
	benchmark(c3, leaf::ctg_self).

	% some benchmark tests for dynamic code:
	benchmark(d1, (create_object(xpto, [], [], []), abolish_object(xpto))).
	benchmark(d2, plain_dyndb(_)).
	benchmark(d3, database::this_dyndb(_)).
	benchmark(d4, database::self_dyndb(_)).
	benchmark(d5, database::obj_dyndb(_)).

	% repeat a goal N times without using call/1 and using a failure-driven loop to 
	% try to avoid the interference of Prolog compiler memory management mechanism
	% (such as garbage collection) on the results 
	do_benchmark(empty_loop, N) :-
		{my_repeat(N)},
		fail.
	do_benchmark(empty_loop, _).

	do_benchmark(s11, N) :-
		{generate_list(30, List)},
		{my_repeat(N)},
			{my_length(List, _)},
		fail.
	do_benchmark(s11, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s12, N) :-
		{generate_list(30, List)},
		{my_repeat(N)},
			':'(module, mod_length(List, _)),
		fail.
	do_benchmark(s12, _).
	:- endif.

	do_benchmark(s13, N) :-
		{generate_list(30, List)},
		{my_repeat(N)},
			object::length(List, _),
		fail.
	do_benchmark(s13, _).

	do_benchmark(s21, N) :-
		{generate_list(30, List)},
		{my_repeat(N)},
			{my_nrev(List, _)},
		fail.
	do_benchmark(s21, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s22, N) :-
		{generate_list(30, List)},
		{my_repeat(N)},
			':'(module, mod_nrev(List, _)),
		fail.
	do_benchmark(s22, _).
	:- endif.

	do_benchmark(s23, N) :-
		{generate_list(30, List)},
		{my_repeat(N)},
			object::nrev(List, _),
		fail.
	do_benchmark(s23, _).

	do_benchmark(s31, N) :-
		{my_repeat(N)},
			({maze_solve(1, 7, _)} -> true),
		fail.
	do_benchmark(s31, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s32, N) :-
		{my_repeat(N)},
			(':'(module, mod_maze_solve(1, 7, _)) -> true),
		fail.
	do_benchmark(s32, _).
	:- endif.

	do_benchmark(s33, N) :-
		{my_repeat(N)},
			(maze::solve(1, 7, _) -> true),
		fail.
	do_benchmark(s33, _).

	do_benchmark(s41, N) :-
		{my_repeat(N)},
			{graph_path(0, 4, _)},
		fail.
	do_benchmark(s41, _).

	:- if(current_logtalk_flag(modules, supported)).
	do_benchmark(s42, N) :-
		{my_repeat(N)},
			':'(module, mod_graph_path(0, 4, _)),
		fail.
	do_benchmark(s42, _).
	:- endif.

	do_benchmark(s43, N) :-
		{my_repeat(N)},
			graph::path(0, 4, _),
		fail.
	do_benchmark(s43, _).

	do_benchmark(c1, N) :-
		{my_repeat(N)},
			leaf::obj_local,
		fail.
	do_benchmark(c1, _).

	do_benchmark(c2, N) :-
		{my_repeat(N)},
			leaf::ctg_direct,
		fail.
	do_benchmark(c2, _).

	do_benchmark(c3, N) :-
		{my_repeat(N)},
			leaf::ctg_self,
		fail.
	do_benchmark(c3, _).

	do_benchmark(d1, N) :-
		{my_repeat(N)},
			create_object(xpto, [], [], []),
			abolish_object(xpto),
		fail.
	do_benchmark(d1, _).

	do_benchmark(d2, N) :-
		{my_repeat(N)},
			{plain_dyndb(N)},
		fail.
	do_benchmark(d2, _).

	do_benchmark(d3, N) :-
		{my_repeat(N)},
			database::this_dyndb(N),
		fail.
	do_benchmark(d3, _).

	do_benchmark(d4, N) :-
		{my_repeat(N)},
			database::self_dyndb(N),
		fail.
	do_benchmark(d4, _).

	do_benchmark(d5, N) :-
		{my_repeat(N)},
			database::obj_dyndb(N),
		fail.
	do_benchmark(d5, _).

:- end_object.

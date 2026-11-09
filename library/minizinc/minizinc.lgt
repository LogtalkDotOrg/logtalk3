%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(minizinc,
	implements(minizinc_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Process-backed MiniZinc CLI solving, source generation, and collected result inspection.'
	]).

	:- uses(list, [
		append/2, append/3, member/2
	]).

	:- uses(os, [
		absolute_file_name/2, file_exists/1, resolve_command_path/2
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	solve(Source, Result) :-
		solve(Source, Result, []).

	solve(Source, Result, UserOptions) :-
		context(Context),
		check(ground, Source, Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(executable(Command), Options, executable(minizinc)),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		solve_source(Source, ProcessSource, SourceMap),
		data_path(Options, DataPath),
		solver_arguments(Options, Arguments),
		minizinc_process::run(Executable, Arguments, ProcessSource, DataPath, Codes, Diagnostics, RawStatus, NormalizedStatus),
		catch(
			minizinc_output::decode(Codes, RawStatus, NormalizedStatus, Diagnostics, Result),
			error(Formal, _),
			throw(error(minizinc_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
		).

	solve_source(object(Object), codes(Codes), SourceMap) :-
		!,
		object_source(Object, Codes, SourceMap).
	solve_source(file(Path), file(Absolute), []) :-
		!,
		context(Context),
		check(atom, Path, Context),
		absolute_file_name(Path, Absolute).
	solve_source(Source, codes(Codes), []) :-
		program(Source, Codes).

	data_path(Options, DataPath) :-
		^^option(data(Path), Options),
		(	Path == none ->
			DataPath = none
		;	absolute_file_name(Path, DataPath)
		).

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(solver(Solver)) :-
		atom(Solver),
		Solver \== ''.
	valid_option(time_limit(Milliseconds)) :-
		integer(Milliseconds),
		Milliseconds >= 0.
	valid_option(all_solutions(Bool)) :-
		once((Bool == true; Bool == false)).
	valid_option(num_solutions(Count)) :-
		integer(Count),
		Count >= 0.
	valid_option(random_seed(Seed)) :-
		(	Seed == auto ->
			true
		;	integer(Seed),
			Seed >= 0
		).
	valid_option(free_search(Bool)) :-
		once((Bool == true; Bool == false)).
	valid_option(parallel(Count)) :-
		(	Count == auto ->
			true
		;	integer(Count),
			Count > 0
		).
	valid_option(statistics(Bool)) :-
		once((Bool == true; Bool == false)).
	valid_option(data(Path)) :-
		(	Path == none ->
			true
		;	atom(Path),
			Path \== ''
		).
	valid_option(json_data(Pairs)) :-
		ground(Pairs),
		valid(list(pair), Pairs),
		valid_json_data(Pairs).

	default_option(executable(minizinc)).
	default_option(solver(gecode)).
	default_option(time_limit(0)).
	default_option(all_solutions(false)).
	default_option(num_solutions(0)).
	default_option(random_seed(auto)).
	default_option(free_search(false)).
	default_option(parallel(auto)).
	default_option(statistics(false)).
	default_option(data(none)).
	default_option(json_data([])).

	valid_json_data([]).
	valid_json_data([_- Term| Pairs]) :-
		catch(value_to_json(Term, _), _, fail),
		valid_json_data(Pairs).

	solver_arguments(Options, Arguments) :-
		^^option(solver(Solver), Options),
		^^option(time_limit(TimeLimit), Options),
		^^option(all_solutions(AllSolutions), Options),
		^^option(num_solutions(NumSolutions), Options),
		^^option(random_seed(RandomSeed), Options),
		^^option(free_search(FreeSearch), Options),
		^^option(parallel(Parallel), Options),
		^^option(statistics(Statistics), Options),
		^^option(json_data(JSONData), Options),
		time_limit_arguments(TimeLimit, TimeArguments),
		all_solutions_arguments(AllSolutions, AllSolutionsArguments),
		num_solutions_arguments(NumSolutions, NumSolutionsArguments),
		random_seed_arguments(RandomSeed, RandomSeedArguments),
		free_search_arguments(FreeSearch, FreeSearchArguments),
		parallel_arguments(Parallel, ParallelArguments),
		statistics_arguments(Statistics, StatisticsArguments),
		json_data_arguments(JSONData, JSONDataArguments),
		append([
			['--json-stream', '--output-mode', json, '--output-time', '--output-objective', '--solver', Solver],
			TimeArguments, AllSolutionsArguments, NumSolutionsArguments, RandomSeedArguments,
			FreeSearchArguments, ParallelArguments, StatisticsArguments, JSONDataArguments
		], Arguments).

	time_limit_arguments(0, []) :-
		!.
	time_limit_arguments(Milliseconds, ['--time-limit', Atom]) :-
		integer_atom(Milliseconds, Atom).

	all_solutions_arguments(false, []) :-
		!.
	all_solutions_arguments(true, ['-a']).

	num_solutions_arguments(0, []) :-
		!.
	num_solutions_arguments(Count, ['-n', Atom]) :-
		integer_atom(Count, Atom).

	random_seed_arguments(auto, []) :-
		!.
	random_seed_arguments(Seed, ['-r', Atom]) :-
		integer_atom(Seed, Atom).

	free_search_arguments(false, []) :-
		!.
	free_search_arguments(true, ['-f']).

	parallel_arguments(auto, []) :-
		!.
	parallel_arguments(Count, ['-p', Atom]) :-
		integer_atom(Count, Atom).

	statistics_arguments(false, []) :-
		!.
	statistics_arguments(true, ['--solver-statistics']).

	json_data_arguments([], []) :-
		!.
	json_data_arguments(Pairs, ['--cmdline-json-data', Atom]) :-
		pairs_to_json_object(Pairs, JSONPairs),
		json(list, dash, atom)::generate(atom(Atom), json(JSONPairs)).

	pairs_to_json_object([], []).
	pairs_to_json_object([Name- Term| Pairs], [Name-JSONValue| JSONPairs]) :-
		value_to_json(Term, JSONValue),
		pairs_to_json_object(Pairs, JSONPairs).

	value_to_json(int(Integer), Integer) :-
		integer(Integer),
		!.
	value_to_json(float(Float), Float) :-
		float(Float),
		!.
	value_to_json(bool(true), '@'(true)) :-
		!.
	value_to_json(bool(false), '@'(false)) :-
		!.
	value_to_json(string(Codes), Atom) :-
		valid(codes, Codes),
		!,
		atom_codes(Atom, Codes).
	value_to_json(array(Elements), JSONList) :-
		!,
		values_to_json(Elements, JSONList).
	value_to_json(array2d(Rows), JSONRows) :-
		!,
		rows_to_json(Rows, JSONRows).
	value_to_json(set(Elements), json([set-JSONList])) :-
		!,
		values_to_json(Elements, JSONList).
	value_to_json(Term, _) :-
		domain_error(minizinc_json_data, Term).

	values_to_json([], []).
	values_to_json([Term| Terms], [JSONValue| JSONValues]) :-
		value_to_json(Term, JSONValue),
		values_to_json(Terms, JSONValues).

	rows_to_json([], []).
	rows_to_json([Row| Rows], [JSONRow| JSONRows]) :-
		values_to_json(Row, JSONRow),
		rows_to_json(Rows, JSONRows).

	integer_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		atom_codes(Atom, Codes).

	program(Source) :-
		context(Context),
		check(ground, Source, Context),
		program_source(Source, Context, Codes),
		print_program(Codes).

	program(Source, Codes) :-
		context(Context),
		check(ground, Source, Context),
		program_source(Source, Context, Codes).

	print_program(Codes) :-
		context(Context),
		check(codes, Codes, Context),
		forall(member(Code, Codes), put_code(Code)).

	program_source(terms(Statements), _, Codes) :-
		!,
		minizinc_program::serialize(Statements, Codes).
	program_source(codes(SourceCodes), Context, Codes) :-
		!,
		check(codes, SourceCodes, Context),
		Codes = SourceCodes.
	program_source(file(Path), Context, Codes) :-
		!,
		check(atom, Path, Context),
		file_to_codes(Path, Codes).
	program_source(object(Object), _, Codes) :-
		!,
		object_source(Object, Codes, _).
	program_source(Source, _, _) :-
		domain_error(minizinc_source, Source).

	object_source(Object, Codes, SourceMap) :-
		(	conforms_to_protocol(Object, minizinc_problem_protocol) ->
			findall(Statement-Origin, Object::minizinc_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			minizinc_program::serialize(Statements, Origins, Codes, SourceMap)
		;	domain_error(minizinc_source, object(Object))
		).

	statements_origins([], [], []).
	statements_origins([Statement-Origin| Pairs], [Statement| Statements], [Origin| Origins]) :-
		statements_origins(Pairs, Statements, Origins).

	model(Result, Solution) :-
		check_result(Result),
		Result = minizinc_result(_, Solutions, _),
		member(Solution, Solutions).

	result_property(Result, Property) :-
		check_result(Result),
		Result = minizinc_result(Status, _, Properties),
		(	Property = status(Status)
		;	member(Property, Properties)
		).

	check_result(Result) :-
		(	ground(Result) ->
			(	Result = minizinc_result(Status, Solutions, Properties),
				member(Status, [satisfiable, unsatisfiable, unknown, error]),
				valid(list(compound), Solutions),
				valid(list(compound), Properties) ->
				true
			;	domain_error(minizinc_result, Result)
			)
		;	instantiation_error
		).

	print_result(Result) :-
		check_result(Result),
		Result = minizinc_result(Status, Solutions, Properties),
		print_solutions(Solutions),
		print_status_line(Status, Properties),
		print_time_line(Properties).

	print_solutions([]).
	print_solutions([solution(Values, _, _)| Solutions]) :-
		print_values(Values),
		write('----------'), nl,
		print_solutions(Solutions).

	print_values([]).
	print_values([Name-Value| Values]) :-
		write(Name), write(' = '), write(Value), write(';'), nl,
		print_values(Values).

	print_status_line(Status, Properties) :-
		(	member(solver_status(SolverStatus), Properties) ->
			true
		;	SolverStatus = none
		),
		(	Status == error ->
			write('=====ERROR====='), nl
		;	SolverStatus == unsat_or_unbounded ->
			write('=====UNSATorUNBOUNDED====='), nl
		;	SolverStatus == unbounded ->
			write('=====UNBOUNDED====='), nl
		;	Status == unsatisfiable ->
			write('=====UNSATISFIABLE====='), nl
		;	Status == unknown ->
			write('=====UNKNOWN====='), nl
		;	write('=========='), nl
		).

	print_time_line(Properties) :-
		(	member(time(Time), Properties),
			Time \== none ->
			write('% time elapsed: '), write(Time), write(' ms'), nl
		;	true
		).

:- end_object.

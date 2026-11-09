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


:- object(asp,
	implements(asp_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Process-backed Clingo and Zetesis CLI solving, ASP source generation, and collected result inspection.'
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
		^^option(solver(Solver), Options),
		check_solver_options(Solver, Options),
		default_executable(Solver, DefaultExecutable),
		^^option(executable(Command), Options, executable(DefaultExecutable)),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		solve_source(Source, ProcessSource, SourceMap),
		solver_arguments(Solver, Options, Arguments),
		asp_process::run(Executable, Arguments, ProcessSource, Codes, Diagnostics, RawStatus, NormalizedStatus),
		catch(
			asp_output::decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result, Options),
			error(Formal, _),
			throw(error(asp_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
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

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(solver(Solver)) :-
		once((Solver == clingo; Solver == zetesis)).
	valid_option(backend(Backend)) :-
		once((Backend == cpu; Backend == gpu; Backend == metal; Backend == vulkan)).
	valid_option(grounder(Grounder)) :-
		once((Grounder == auto; Grounder == eager; Grounder == lazy)).
	valid_option(threads(Threads)) :-
		(	Threads == auto ->
			true
		;	integer(Threads),
			Threads > 0
		).
	valid_option(memory(Bytes)) :-
		(	Bytes == auto ->
			true
		;	integer(Bytes),
			Bytes > 0
		).
	valid_option(models(Count)) :-
		integer(Count),
		Count >= 0.
	valid_option(time_limit(Seconds)) :-
		integer(Seconds),
		Seconds >= 0.
	valid_option(opt_mode(Mode)) :-
		once((Mode == opt; Mode == optN; Mode == ignore)).
	valid_option(statistics(Level)) :-
		integer(Level),
		Level >= 0,
		Level =< 2.
	valid_option(constants(Pairs)) :-
		ground(Pairs),
		valid(list(pair), Pairs),
		valid_constants(Pairs).

	default_option(solver(clingo)).
	default_option(backend(cpu)).
	default_option(grounder(auto)).
	default_option(threads(auto)).
	default_option(memory(auto)).
	default_option(models(1)).
	default_option(time_limit(0)).
	default_option(opt_mode(opt)).
	default_option(statistics(0)).
	default_option(constants([])).

	valid_constants([]).
	valid_constants([Name-Value| Pairs]) :-
		catch(asp_program::serialize([constant(Name, Value)], _), _, fail),
		valid_constants(Pairs).

	default_executable(clingo, clingo).
	default_executable(zetesis, zetesis).

	check_solver_options(clingo, Options) :-
		!,
		check_default_solver_option(clingo, backend(cpu), Options),
		check_default_solver_option(clingo, grounder(auto), Options),
		check_default_solver_option(clingo, threads(auto), Options),
		check_default_solver_option(clingo, memory(auto), Options).
	check_solver_options(zetesis, Options) :-
		check_default_solver_option(zetesis, opt_mode(opt), Options),
		check_default_solver_option(zetesis, constants([]), Options).

	check_default_solver_option(Solver, Default, Options) :-
		functor(Default, Name, Arity),
		functor(Option, Name, Arity),
		^^option(Option, Options),
		(	Option == Default ->
			true
		;	domain_error(asp_solver_option, Solver-Option)
		).

	solver_arguments(clingo, Options, Arguments) :-
		^^option(models(Models), Options),
		^^option(time_limit(Seconds), Options),
		^^option(opt_mode(Mode), Options),
		^^option(statistics(Level), Options),
		^^option(constants(Constants), Options),
		integer_atom(Models, ModelArgument),
		integer_atom(Seconds, SecondsAtom),
		integer_atom(Level, LevelAtom),
		atom_concat('--time-limit=', SecondsAtom, TimeArgument),
		atom_concat('--opt-mode=', Mode, OptArgument),
		atom_concat('--stats=', LevelAtom, StatisticsArgument),
		constant_arguments(Constants, ConstantArguments),
		Arguments = ['--outf=2', '--quiet=0', ModelArgument, TimeArgument, OptArgument, StatisticsArgument| ConstantArguments].
	solver_arguments(zetesis, Options, Arguments) :-
		^^option(models(Models), Options),
		^^option(time_limit(Seconds), Options),
		^^option(statistics(Level), Options),
		^^option(backend(Backend), Options),
		^^option(grounder(Grounder), Options),
		^^option(threads(Threads), Options),
		^^option(memory(Memory), Options),
		zetesis_model_arguments(Models, ModelArguments),
		zetesis_limit_arguments('--time-limit', Seconds, TimeArguments),
		zetesis_setting_arguments('--threads', Threads, ThreadArguments),
		zetesis_setting_arguments('--memory', Memory, MemoryArguments),
		(	Level == 0 ->
			StatisticsArguments = []
		;	StatisticsArguments = ['--stats']
		),
		append([ModelArguments, TimeArguments, ThreadArguments, MemoryArguments, StatisticsArguments], Extra),
		Arguments = [solve, '--json', '--backend', Backend, '--grounder', Grounder| Extra].

	zetesis_model_arguments(0, ['--all']) :-
		!.
	zetesis_model_arguments(Models, ['--answers', Atom]) :-
		integer_atom(Models, Atom).

	zetesis_limit_arguments(_, 0, []) :-
		!.
	zetesis_limit_arguments(Flag, Limit, [Flag, Atom]) :-
		integer_atom(Limit, Atom).

	zetesis_setting_arguments(_, auto, []) :-
		!.
	zetesis_setting_arguments(Flag, Setting, [Flag, Atom]) :-
		integer_atom(Setting, Atom).

	integer_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		atom_codes(Atom, Codes).

	constant_arguments([], []).
	constant_arguments([Name-Value| Pairs], [Argument| Arguments]) :-
		asp_program::serialize([constant(Name, Value)], Codes),
		Codes = [35,99,111,110,115,116,32| Definition],
		append(ConstantCodes, [46,10], Definition),
		atom_codes(Constant, ConstantCodes),
		atom_concat('--const=', Constant, Argument),
		constant_arguments(Pairs, Arguments).

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
		asp_program::serialize(Statements, Codes).
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
		domain_error(asp_source, Source).

	object_source(Object, Codes, SourceMap) :-
		(	conforms_to_protocol(Object, asp_problem_protocol) ->
			findall(Statement-Origin, Object::asp_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			asp_program::serialize(Statements, Origins, Codes, SourceMap)
		;	domain_error(asp_source, object(Object))
		).

	statements_origins([], [], []).
	statements_origins([Statement-Origin| Pairs], [Statement| Statements], [Origin| Origins]) :-
		statements_origins(Pairs, Statements, Origins).

	model(Result, Model) :-
		check_result(Result),
		Result = asp_result(_, Models, _),
		member(Model, Models).

	result_property(Result, Property) :-
		check_result(Result),
		Result = asp_result(Status, _, Properties),
		(	Property = status(Status)
		;	member(Property, Properties)
		).

	print_result(Result) :-
		check_result(Result),
		Result = asp_result(Status, Models, Properties),
		(	member(solver_version(Banner), Properties),
			atom(Banner),
			Banner \== unknown ->
			write(Banner), nl
		;	member(solver(Solver), Properties),
			atom(Solver),
			Solver \== unknown ->
			write(Solver), nl
		;	true
		),
		print_models(Models, 1),
		(	Status == satisfiable ->
			(	member(optimality(proven), Properties) ->
				write('OPTIMUM FOUND')
			;	write('SATISFIABLE')
			)
		;	Status == unsatisfiable ->
			write('UNSATISFIABLE')
		; 	write('UNKNOWN')
		),
		nl,
		nl,
		(	member(termination(time_limit), Properties) ->
			write('TIME LIMIT   : 1'), nl
		;	member(termination(interrupted), Properties) ->
			write('INTERRUPTED  : 1'), nl
		;	true
		),
		write('Models       : '),
		(	member(model_count(Count), Properties),
			integer(Count),
			Count >= 0 ->
			write(Count)
		;	write(unknown)
		),
		(	member(enumeration_complete(false), Properties) ->
			write('+')
		;	member(enumeration_complete(true), Properties) ->
			true
		;	write('?')
		),
		nl,
		(	member(optimality(proven), Properties) ->
			write('  Optimum    : yes'), nl
		;	member(optimality(unproven), Properties) ->
			write('  Optimum    : unknown'), nl
		;	true
		),
		(	member(costs(Costs), Properties),
			valid(list(integer), Costs) ->
			write('Optimization : '),
			print_values(Costs),
			nl
		;	true
		),
		(	member(time(json(Time)), Properties),
			valid(list(pair), Time) ->
			print_time('Total', 'Time         : ', Time),
			print_time('CPU', 'CPU Time     : ', Time)
		;	true
		).

	print_models([], _) :-
		!.
	print_models([model(_, Costs, Values)| Models], Number) :-
		write('Answer: '),
		write(Number),
		nl,
		print_values(Values),
		nl,
		(	Costs == none ->
			true
		;	write('Optimization: '),
			print_values(Costs),
			nl
		),
		Next is Number + 1,
		print_models(Models, Next).

	print_values([]) :-
		!.
	print_values([Value| Values]) :-
		write(Value),
		forall(
			member(Next, Values),
			(	write(' '),
				write(Next)
			)
		).

	print_time(Key, Label, Time) :-
		(	member(Key-Seconds, Time),
			number(Seconds) ->
			write(Label),
			write(Seconds),
			write(s), nl
		;	true
		).

	check_result(Result) :-
		(	ground(Result) ->
			(	Result = asp_result(Status, Models, Properties),
				member(Status, [satisfiable, unsatisfiable, unknown]),
				valid(list(compound), Models),
				valid(list(compound), Properties),
				valid_models(Models) ->
				true
			;	domain_error(asp_result, Result)
			)
		;	instantiation_error
		).

	valid_models([]).
	valid_models([model(Symbols, Costs, RawValues)| Models]) :-
		valid(list, Symbols),
		valid(list(atom), RawValues),
		(	Costs == none ->
			true
		;	valid(list(integer), Costs)
		),
		valid_models(Models).

:- end_object.

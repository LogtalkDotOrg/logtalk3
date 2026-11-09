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


:- object(smt,
	implements(smt_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-08,
		comment is 'Process-backed Z3, cvc5, Bitwuzla, and Yices 2 CLI solving, SMT-LIB 2 source generation, and collected result inspection.'
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	:- uses(os, [
		absolute_file_name/2, file_exists/1, resolve_command_path/2
	]).

	solve(Source, Results) :-
		solve(Source, Results, []).

	solve(Source, Results, UserOptions) :-
		context(Context),
		check(ground, Source, Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(solver(Solver), Options),
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
		smt_process::run(Executable, Arguments, ProcessSource, Codes, Diagnostics, RawStatus, NormalizedStatus),
		catch(
			smt_output::decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Results),
			error(Formal, _),
			throw(error(smt_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
		).

	solve_source(object(Object), codes(Codes), SourceMap) :-
		!,
		object_source(Object, Codes, SourceMap).
	solve_source(file(Path), file(Absolute), []) :-
		!,
		context(Context),
		check(atom, Path, Context),
		absolute_file_name(Path, Absolute).
	solve_source(terms(Statements), codes(Codes), []) :-
		!,
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		smt_program::serialize(Statements, Codes).
	solve_source(codes(SourceCodes), codes(Codes), []) :-
		!,
		context(Context),
		check(codes, SourceCodes, Context),
		Codes = SourceCodes.
	solve_source(Source, _, _) :-
		domain_error(smt_source, Source).

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(solver(Solver)) :-
		once((Solver == z3; Solver == cvc5; Solver == bitwuzla; Solver == yices2)).
	valid_option(time_limit(Seconds)) :-
		integer(Seconds),
		Seconds >= 0.

	default_option(solver(z3)).
	default_option(time_limit(0)).

	% The executable default depends on the selected solver, so it is resolved with
	% ^^option/3 at the point of use instead of through a fixed default_option/1 fact.
	default_executable(z3, z3).
	default_executable(cvc5, cvc5).
	default_executable(bitwuzla, bitwuzla).
	default_executable(yices2, 'yices-smt2').

	% 0 is the library-wide unlimited sentinel, consistently applied across solvers.
	solver_arguments(z3, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		(	Seconds =:= 0 ->
			Arguments = []
		;	joined_argument('-T:', Seconds, Argument),
			Arguments = [Argument]
		).
	% cvc5 rejects push/pop unless --incremental is given, so it is always passed;
	% its timeout is in milliseconds, unlike the seconds used elsewhere (verified
	% against cvc5 1.3.0: a fine-grained --tlimit can abort the process with
	% SIGABRT on some hard problems instead of reporting "unknown"; decode/7 turns
	% an abnormal exit with no parsed results into a smt_process_status error).
	solver_arguments(cvc5, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		(	Seconds =:= 0 ->
			TimeArguments = []
		;	MilliSeconds is Seconds * 1000,
			joined_argument('--tlimit=', MilliSeconds, TimeArgument),
			TimeArguments = [TimeArgument]
		),
		Arguments = ['--incremental'| TimeArguments].
	% Bitwuzla's --time-limit is in milliseconds (verified against bitwuzla 0.9.1
	% "--help"); push/pop work without any extra flag.
	solver_arguments(bitwuzla, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		(	Seconds =:= 0 ->
			Arguments = []
		;	MilliSeconds is Seconds * 1000,
			joined_argument('--time-limit=', MilliSeconds, Argument),
			Arguments = [Argument]
		).
	% yices-smt2 rejects push/pop unless --incremental is given, so it is always
	% passed; --timeout is in seconds (verified against Yices 2.7.0 "--help").
	solver_arguments(yices2, Options, Arguments) :-
		^^option(time_limit(Seconds), Options),
		(	Seconds =:= 0 ->
			TimeArguments = []
		;	joined_argument('--timeout=', Seconds, TimeArgument),
			TimeArguments = [TimeArgument]
		),
		Arguments = ['--incremental'| TimeArguments].

	joined_argument(Prefix, Value, Argument) :-
		integer_atom(Value, ValueAtom),
		atom_concat(Prefix, ValueAtom, Argument).

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
		smt_program::serialize(Statements, Codes).
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
		domain_error(smt_source, Source).

	object_source(Object, Codes, SourceMap) :-
		(	conforms_to_protocol(Object, smt_problem_protocol) ->
			findall(Statement-Origin, Object::smt_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			smt_program::serialize(Statements, Origins, Codes, SourceMap)
		;	domain_error(smt_source, object(Object))
		).

	statements_origins([], [], []).
	statements_origins([Statement-Origin| Pairs], [Statement| Statements], [Origin| Origins]) :-
		statements_origins(Pairs, Statements, Origins).

	status(Result, Status) :-
		check_result(Result),
		Result = smt_result(Status, _, _).

	model_binding(Result, Name-Value) :-
		check_result(Result),
		Result = smt_result(_, Model, _),
		Model = bindings(Bindings),
		member(binding(Name, Value), Bindings).

	model_value(Result, Name, Value) :-
		context(Context),
		check(atom, Name, Context),
		check_result(Result),
		Result = smt_result(_, Model, _),
		Model = bindings(Bindings),
		memberchk(binding(Name, Value), Bindings).

	unsat_core_name(Result, Name) :-
		check_result(Result),
		Result = smt_result(_, _, Properties),
		member(core(Core), Properties),
		Core \== none,
		member(Name, Core).

	result_property(Result, Property) :-
		check_result(Result),
		Result = smt_result(Status, _, Properties),
		(	Property = status(Status)
		;	member(Property, Properties)
		).

	print_result(Result) :-
		check_result(Result),
		Result = smt_result(Status, Model, Properties),
		write('status: '), write(Status), nl,
		print_model(Model),
		print_core(Properties),
		print_values(Properties).

	print_results(Results) :-
		context(Context),
		check(list, Results, Context),
		print_results_(Results).

	print_results_([]).
	print_results_([Result]) :-
		!,
		print_result(Result).
	print_results_([Result| Results]) :-
		print_result(Result),
		nl,
		print_results_(Results).

	print_model(none) :-
		!.
	print_model(bindings(Bindings)) :-
		forall(
			member(binding(Name, Value), Bindings),
			(	write('  '), write(Name), write(' = '), write(Value), nl
			)
		).

	print_core(Properties) :-
		(	member(core(Core), Properties),
			Core \== none ->
			write('unsat core: '), write(Core), nl
		;	true
		).

	print_values(Properties) :-
		(	member(values(Values), Properties),
			Values \== [] ->
			forall(
				member(value(Term, Value), Values),
				(	write('  '), write(Term), write(' = '), write(Value), nl
				)
			)
		;	true
		).

	check_result(Result) :-
		(	ground(Result) ->
			(	Result = smt_result(Status, Model, Properties),
				member(Status, [satisfiable, unsatisfiable, unknown]),
				valid(list(compound), Properties),
				valid_model(Model) ->
				true
			;	domain_error(smt_result, Result)
			)
		;	instantiation_error
		).

	valid_model(none) :-
		!.
	valid_model(bindings(List)) :-
		valid(list(compound), List).

	% Advisory capability table (non-blocking): a solver call may still succeed,
	% fail, or report "unknown" for a feature outside this table. Verified against
	% Z3 5.3.3, cvc5 1.3.0, Bitwuzla 0.9.1, and Yices 2.7.0: Bitwuzla rejects plain
	% linear-arithmetic logics (e.g. QF_LIA) outright, lacking "linear_arithmetic".
	capability(z3, quantifiers).
	capability(z3, strings).
	capability(z3, linear_arithmetic).
	capability(z3, non_linear_arithmetic).
	capability(z3, unsat_cores).
	capability(z3, incremental).
	capability(z3, bit_vectors).
	capability(z3, arrays).
	capability(z3, floating_point).
	capability(z3, algebraic_datatypes).
	capability(cvc5, quantifiers).
	capability(cvc5, strings).
	capability(cvc5, linear_arithmetic).
	capability(cvc5, non_linear_arithmetic).
	capability(cvc5, unsat_cores).
	capability(cvc5, incremental).
	capability(cvc5, bit_vectors).
	capability(cvc5, arrays).
	capability(cvc5, floating_point).
	capability(cvc5, algebraic_datatypes).
	capability(bitwuzla, unsat_cores).
	capability(bitwuzla, incremental).
	capability(bitwuzla, bit_vectors).
	capability(bitwuzla, arrays).
	capability(bitwuzla, floating_point).
	capability(yices2, linear_arithmetic).
	capability(yices2, non_linear_arithmetic).
	capability(yices2, unsat_cores).
	capability(yices2, incremental).
	capability(yices2, bit_vectors).
	capability(yices2, arrays).

:- end_object.

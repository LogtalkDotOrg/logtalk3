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

:- object(omt,
	implements(omt_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-14,
		comment is 'Process-backed single-objective optimization with Z3 and OptiMathSAT.'
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(os, [
		absolute_file_name/2, file_exists/1, resolve_command_path/2
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	solve(Source, Results) :-
		solve(Source, Results, []).

	solve(Source, Results, Options) :-
		validate_source(Source),
		context(Context),
		^^check_options(Options),
		^^merge_options(Options, Merged),
		^^option(solver(Solver), Merged),
		^^option(time_limit(Seconds), Merged),
		solver_arguments(Solver, Seconds, Arguments),
		default_executable(Solver, Default),
		^^option(executable(Command), Merged, executable(Default)),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		solve_source(Source, ProcessSource, SourceMap),
		smt_process::run(Executable, Arguments, ProcessSource, Codes, Diagnostics, RawStatus, NormalizedStatus),
		catch(
			smt_output::decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Results),
			error(Formal, _),
			throw(error(omt_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
		),
		(	NormalizedStatus == exit(0) ->
			true
		;	throw(error(omt_solver_error(smt_process_status(NormalizedStatus), NormalizedStatus, Diagnostics, SourceMap), Context))
		).

	valid_option(solver(Solver)) :-
		once((Solver == z3; Solver == optimathsat)).
	valid_option(executable(Path)) :-
		atom(Path), Path \== ''.
	valid_option(time_limit(Seconds)) :-
		integer(Seconds), Seconds >= 0.

	default_option(solver(z3)).
	default_option(time_limit(0)).

	default_executable(z3, z3).
	default_executable(optimathsat, optimathsat).

	solver_arguments(z3, 0, []) :-
		!.
	solver_arguments(z3, Seconds, [Argument]) :-
		!,
		atomic_concat('-T:', Seconds, Argument).
	solver_arguments(optimathsat, 0, ['-input=smt2', '-optimization=true', '-model_generation=true']) :-
		!.
	solver_arguments(optimathsat, Seconds, _) :-
		domain_error(optimathsat_time_limit, Seconds).

	solve_source(object(Object), codes(Codes), SourceMap) :-
		!,
		findall(Statement-Origin, Object::smt_statement(Statement, Origin), Pairs),
		statements_origins(Pairs, Statements, Origins),
		smt_program::serialize(Statements, Origins, Codes, SourceMap).
	solve_source(terms(Statements), codes(Codes), []) :-
		!,
		smt_program::serialize(Statements, Codes).
	solve_source(codes(Codes), codes(Codes), []) :-
		!,
		context(Context),
		check(codes, Codes, Context).
	solve_source(file(Path), file(Absolute), []) :-
		!,
		context(Context),
		check(atom, Path, Context),
		absolute_file_name(Path, Absolute).
	solve_source(Source, _, _) :-
		domain_error(omt_source, Source).

	statements_origins([], [], []).
	statements_origins([Statement-Origin| Pairs], [Statement| Statements], [Origin| Origins]) :-
		statements_origins(Pairs, Statements, Origins).

	program(Source) :-
		validate_source(Source),
		smt::program(Source).

	program(Source, Codes) :-
		validate_source(Source),
		smt::program(Source, Codes).

	print_program(Codes) :-
		smt::print_program(Codes).

	status(Result, Status) :-
		smt::status(Result, Status).

	model_binding(Result, Binding) :-
		smt::model_binding(Result, Binding).

	model_value(Result, Name, Value) :-
		smt::model_value(Result, Name, Value).

	unsat_core_name(Result, Name) :-
		smt::unsat_core_name(Result, Name).

	result_property(Result, Property) :-
		smt::result_property(Result, Property).

	objective(Result, Expression, Bound) :-
		smt::result_property(Result, objectives(Objectives)),
		member(objective(Expression, Bound), Objectives).

	print_result(Result) :-
		smt::print_result(Result),
		forall(
			objective(Result, Expression, Bound),
			(	write('objective: '), write(Expression), write(' = '), write(Bound), nl	)
		).

	print_results(Results) :-
		context(Context),
		check(list, Results, Context),
		print_results_(Results).

	print_results_([]).
	print_results_([Result]) :-
		!, print_result(Result).
	print_results_([Result| Results]) :-
		print_result(Result), nl, print_results_(Results).

	capability(z3, linear_arithmetic).
	capability(z3, incremental).
	capability(optimathsat, linear_arithmetic).
	capability(optimathsat, incremental).

	validate_source(Source) :-
		context(Context),
		check(ground, Source, Context),
		(	Source = terms(Statements) ->
			smt_program::serialize(Statements, _),
			validate_scopes(Statements, none, [])
		;	Source = object(Object) ->
			(	conforms_to_protocol(Object, omt_problem_protocol) ->
				findall(Statement, Object::smt_statement(Statement, _), Statements),
				smt_program::serialize(Statements, _),
				validate_scopes(Statements, none, [])
			;	domain_error(omt_source, Source)
			)
		;	true
		).

	validate_scopes([], _, _).
	validate_scopes([Statement| Statements], Active, Stack) :-
		(	Statement = minimize(_) ->
			add_objective(Statement, Active, Next), NextStack = Stack
		;	Statement = maximize(_) ->
			add_objective(Statement, Active, Next), NextStack = Stack
		;	Statement = push(Count) ->
			push_scopes(Count, Active, Stack, NextStack), Next = Active
		;	Statement = pop(Count) ->
			pop_scopes(Count, Active, Stack, Next, NextStack)
		;	Next = Active, NextStack = Stack
		),
		validate_scopes(Statements, Next, NextStack).

	add_objective(_, none, active) :-
		!.
	add_objective(Statement, _, _) :-
		domain_error(omt_objective_scope, Statement).

	push_scopes(0, _, Stack, Stack) :-
		!.
	push_scopes(Count, Active, Stack, Result) :-
		Next is Count - 1,
		push_scopes(Next, Active, [Active| Stack], Result).

	pop_scopes(0, Active, Stack, Active, Stack) :-
		!.
	pop_scopes(Count, _, [Active| Stack], Result, ResultStack) :-
		!,
		Next is Count - 1,
		pop_scopes(Next, Active, Stack, Result, ResultStack).
	pop_scopes(Count, _, [], _, _) :-
		domain_error(omt_pop_scope, Count).

:- end_object.

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


:- object(problog,
	implements(problog_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-12,
		comment is 'ProbLog CLI exact inference, source generation, and collected result inspection.'
	]).

	:- uses(list, [
		append/2, member/2
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
		^^option(executable(Command), Options),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		solve_source(Source, ProcessSource, SourceMap),
		solver_arguments(Options, Arguments),
		problog_process::run(Executable, Arguments, ProcessSource, Codes, Diagnostics, RawStatus, Status),
		problog_output::decode(Codes, Diagnostics, RawStatus, Status, SourceMap, Result).

	solve_source(object(Object), codes(Codes), SourceMap) :-
		!,
		object_source(Object, Codes, SourceMap).
	solve_source(file(Path), file(Absolute), []) :-
		!,
		context(Context),
		check(atom, Path, Context),
		absolute_file_name(Path, Absolute),
		( file_exists(Absolute) -> true ; existence_error(source_sink, Path) ).
	solve_source(Source, codes(Codes), []) :-
		program(Source, Codes).

	valid_option(executable(Path)) :-
		atom(Path), Path \== ''.
	valid_option(knowledge(Backend)) :-
		nonvar(Backend),
		member(Backend, [auto, sdd, sddx, bdd, nnf, ddnnf, fsdd, fbdd]).
	valid_option(timeout(Seconds)) :-
		nonvar(Seconds),
		valid_timeout(Seconds).
	valid_option(compile_timeout(Seconds)) :-
		nonvar(Seconds),
		valid_timeout(Seconds).
	valid_option(logspace(Bool)) :-
		once((Bool == true; Bool == false)).
	valid_option(propagate_evidence(Bool)) :-
		once((Bool == true; Bool == false)).
	valid_option(libraries(Paths)) :-
		ground(Paths),
		valid(list(atom), Paths),
		nonempty_paths(Paths).
	valid_option(arguments(Arguments)) :-
		ground(Arguments),
		valid(list(atom), Arguments).

	valid_timeout(none).
	valid_timeout(Seconds) :-
		integer(Seconds), Seconds > 0.

	nonempty_paths([]).
	nonempty_paths([Path| Paths]) :-
		Path \== '',
		nonempty_paths(Paths).

	default_option(executable(problog)).
	default_option(knowledge(auto)).
	default_option(timeout(none)).
	default_option(compile_timeout(none)).
	default_option(logspace(true)).
	default_option(propagate_evidence(true)).
	default_option(libraries([])).
	default_option(arguments([])).

	solver_arguments(Options, Arguments) :-
		^^option(knowledge(Backend), Options),
		^^option(timeout(Timeout), Options),
		^^option(compile_timeout(CompileTimeout), Options),
		^^option(logspace(Logspace), Options),
		^^option(propagate_evidence(Propagate), Options),
		^^option(libraries(Libraries), Options),
		^^option(arguments(ModelArguments), Options),
		value_argument(auto, Backend, '--knowledge', KnowledgeArguments),
		time_argument(Timeout, '--timeout', TimeoutArguments),
		time_argument(CompileTimeout, '--compile-timeout', CompileArguments),
		logspace_argument(Logspace, LogspaceArgument),
		propagate_argument(Propagate, PropagateArgument),
		absolute_paths(Libraries, AbsoluteLibraries),
		repeated_arguments(AbsoluteLibraries, '-L', LibraryArguments),
		repeated_arguments(ModelArguments, '-a', ModelArgumentList),
		append([
			['--format', prolog, LogspaceArgument, PropagateArgument],
			KnowledgeArguments, TimeoutArguments, CompileArguments, LibraryArguments, ModelArgumentList
		], Arguments).

	value_argument(Default, Value, _, []) :-
		Value == Default,
		!.
	value_argument(_, Value, Flag, [Flag, Value]).

	time_argument(none, _, []) :-
		!.
	time_argument(Seconds, Flag, [Flag, Atom]) :-
		number_codes(Seconds, Codes),
		atom_codes(Atom, Codes).

	logspace_argument(true, '--logspace').
	logspace_argument(false, '--nologspace').

	propagate_argument(true, '--propagate-evidence').
	propagate_argument(false, '--dont-propagate-evidence').

	absolute_paths([], []).
	absolute_paths([Path| Paths], [Absolute| Absolutes]) :-
		absolute_file_name(Path, Absolute),
		absolute_paths(Paths, Absolutes).

	repeated_arguments([], _, []).
	repeated_arguments([Value| Values], Flag, [Argument| Arguments]) :-
		% Equals form prevents a model argument beginning with "-" becoming a CLI flag.
		atom_concat(Flag, '=', Prefix),
		atom_concat(Prefix, Value, Argument),
		repeated_arguments(Values, Flag, Arguments).

	program(Source) :-
		program(Source, Codes),
		print_program(Codes).

	program(Source, Codes) :-
		context(Context),
		check(ground, Source, Context),
		program_source(Source, Context, Codes).

	program_source(terms(Statements), _, Codes) :-
		!,
		problog_program::serialize(Statements, Codes).
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
		domain_error(problog_source, Source).

	object_source(Object, Codes, SourceMap) :-
		(	conforms_to_protocol(Object, problog_problem_protocol) ->
			findall(Statement-Origin, Object::problog_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			problog_program::serialize(Statements, Origins, Codes, SourceMap)
		;	domain_error(problog_source, object(Object))
		).

	statements_origins([], [], []).
	statements_origins([Statement-Origin| Pairs], [Statement| Statements], [Origin| Origins]) :-
		statements_origins(Pairs, Statements, Origins).

	print_program(Codes) :-
		context(Context),
		check(codes, Codes, Context),
		forall(member(Code, Codes), put_code(Code)).

	probability(Result, Query, Probability) :-
		check_result(Result),
		Result = problog_result(Probabilities, _),
		member(probability(Query, Probability), Probabilities).

	result_property(Result, Property) :-
		check_result(Result),
		Result = problog_result(_, Properties),
		member(Property, Properties).

	check_result(Result) :-
		(	\+ ground(Result) ->
			instantiation_error
		;	problog_output::valid_result(Result) ->
			true
		;	domain_error(problog_result, Result)
		).

	print_result(Result) :-
		check_result(Result),
		Result = problog_result(Probabilities, _),
		print_probabilities(Probabilities).

	print_probabilities([]).
	print_probabilities([probability(Query, Probability)| Probabilities]) :-
		write_term(Query, [quoted(true)]),
		write(': '), write(Probability), nl,
		print_probabilities(Probabilities).

:- end_object.

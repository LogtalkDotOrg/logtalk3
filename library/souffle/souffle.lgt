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


:- object(souffle,
	implements(souffle_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Process-backed Souffle CLI solving, Datalog source generation, and collected result inspection.'
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
		^^option(executable(Command), Options, executable(souffle)),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		solve_source(Source, ProcessSource, SourceMap),
		facts_spec(Options, FactsSpec),
		^^option(outputs(Outputs), Options),
		^^option(printsizes(Printsizes), Options),
		output_relation_names(Outputs, OutputRelations),
		solver_arguments(Options, Arguments),
		souffle_process::run(Executable, Arguments, ProcessSource, FactsSpec, OutputRelations, Codes, Diagnostics, RelationCodes, RawStatus, NormalizedStatus),
		catch(
			souffle_output::decode(Codes, Diagnostics, RelationCodes, RawStatus, NormalizedStatus, Outputs, Printsizes, Result),
			error(Formal, _),
			throw(error(souffle_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
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

	facts_spec(Options, FactsSpec) :-
		^^option(facts(FactsPath), Options),
		^^option(facts_data(FactsData), Options),
		(	FactsPath \== none,
			FactsData \== [] ->
			domain_error(souffle_facts_option, facts(FactsPath)-facts_data(FactsData))
		;	FactsPath \== none ->
			absolute_file_name(FactsPath, Absolute),
			FactsSpec = external(Absolute)
		;	FactsData \== [] ->
			FactsSpec = materialize(FactsData)
		;	FactsSpec = none
		).

	output_relation_names([], []).
	output_relation_names([Name-_| Pairs], [Name| Names]) :-
		output_relation_names(Pairs, Names).

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(jobs(Jobs)) :-
		(	Jobs == auto ->
			true
		;	integer(Jobs),
			Jobs >= 1
		).
	valid_option(warnings(Bool)) :-
		once((Bool == true; Bool == false)).
	valid_option(compile(Bool)) :-
		once((Bool == true; Bool == false)).
	valid_option(facts(Path)) :-
		(	Path == none ->
			true
		;	atom(Path),
			Path \== ''
		).
	valid_option(facts_data(Pairs)) :-
		ground(Pairs),
		valid(list(pair), Pairs),
		valid_facts_data(Pairs).
	valid_option(outputs(Pairs)) :-
		ground(Pairs),
		valid(list(pair), Pairs),
		valid_outputs(Pairs).
	valid_option(printsizes(Names)) :-
		ground(Names),
		valid(list(atom), Names),
		valid_relation_names(Names).

	default_option(executable(souffle)).
	default_option(jobs(1)).
	default_option(warnings(true)).
	default_option(compile(false)).
	default_option(facts(none)).
	default_option(facts_data([])).
	default_option(outputs([])).
	default_option(printsizes([])).

	valid_facts_data([]).
	valid_facts_data([Name-Rows| Pairs]) :-
		atom(Name),
		Name \== '',
		valid(list(list), Rows),
		valid_facts_data(Pairs).

	valid_outputs([]).
	valid_outputs([Name-Types| Pairs]) :-
		atom(Name),
		Name \== '',
		valid(list, Types),
		valid_column_types(Types),
		valid_outputs(Pairs).

	valid_column_types([]).
	valid_column_types([Type| Types]) :-
		souffle_program::valid_column_type(Type),
		valid_column_types(Types).

	valid_relation_names([]).
	valid_relation_names([Name| Names]) :-
		Name \== '',
		valid_relation_names(Names).

	solver_arguments(Options, Arguments) :-
		^^option(jobs(Jobs), Options),
		^^option(warnings(Warnings), Options),
		^^option(compile(Compile), Options),
		jobs_arguments(Jobs, JobsArguments),
		warnings_arguments(Warnings, WarningsArguments),
		compile_arguments(Compile, CompileArguments),
		append([JobsArguments, WarningsArguments, CompileArguments], Arguments).

	jobs_arguments(auto, ['-jauto']) :-
		!.
	jobs_arguments(Jobs, [Argument]) :-
		integer_atom(Jobs, Atom),
		atom_concat('-j', Atom, Argument).

	warnings_arguments(true, []) :-
		!.
	warnings_arguments(false, ['-w']).

	compile_arguments(false, []) :-
		!.
	compile_arguments(true, ['-c']).

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
		souffle_program::serialize(Statements, Codes).
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
		domain_error(souffle_source, Source).

	object_source(Object, Codes, SourceMap) :-
		(	conforms_to_protocol(Object, souffle_problem_protocol) ->
			findall(Statement-Origin, Object::souffle_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			souffle_program::serialize(Statements, Origins, Codes, SourceMap)
		;	domain_error(souffle_source, object(Object))
		).

	statements_origins([], [], []).
	statements_origins([Statement-Origin| Pairs], [Statement| Statements], [Origin| Origins]) :-
		statements_origins(Pairs, Statements, Origins).

	relation(Result, Name, Columns, Rows) :-
		check_result(Result),
		Result = souffle_result(Relations, _, _),
		member(relation(Name, Columns, Rows), Relations).

	size(Result, Name, Count) :-
		check_result(Result),
		Result = souffle_result(_, Sizes, _),
		member(size(Name, Count), Sizes).

	result_property(Result, Property) :-
		check_result(Result),
		Result = souffle_result(_, _, Properties),
		member(Property, Properties).

	check_result(Result) :-
		(	ground(Result) ->
			(	Result = souffle_result(Relations, Sizes, Properties),
				valid(list(compound), Relations),
				valid(list(compound), Sizes),
				valid(list(compound), Properties) ->
				true
			;	domain_error(souffle_result, Result)
			)
		;	instantiation_error
		).

	print_result(Result) :-
		check_result(Result),
		Result = souffle_result(Relations, Sizes, _),
		print_relations(Relations),
		print_sizes(Sizes).

	print_relations([]).
	print_relations([relation(Name, _, Rows)| Relations]) :-
		write(Name), write(':'), nl,
		print_rows(Rows),
		print_relations(Relations).

	print_rows([]).
	print_rows([Row| Rows]) :-
		write(Row), nl,
		print_rows(Rows).

	print_sizes([]).
	print_sizes([size(Name, Count)| Sizes]) :-
		write(Name), write(' = '), write(Count), nl,
		print_sizes(Sizes).

:- end_object.

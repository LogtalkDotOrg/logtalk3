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


:- object(wmc,
	implements(wmc_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-25,
		comment is 'Weighted model counting using external Ganak and SharpSAT-TD CLI counters or a reference counter written in Logtalk, CNF source generation, and collected result inspection.'
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	:- uses(term_io, [
		read_term_from_codes/3, write_term_to_codes/3
	]).

	:- uses(os, [
		absolute_file_name/2, file_exists/1, resolve_command_path/2
	]).

	solve(Source, Result) :-
		solve(Source, Result, []).

	solve(Source, Result, UserOptions) :-
		context(Context),
		check(ground, Source, Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(solver(Solver), Options),
		solve_source(Source, Codes, SourceMap, Variables),
		(	Solver == reference ->
			reference_result(Codes, SourceMap, Variables, Result)
		;	external_result(Solver, Options, Codes, SourceMap, Variables, Context, Result)
		).

	reference_result(Codes, SourceMap, Variables, Result) :-
		wmc_reference::count(Codes, Value),
		(	Value =:= 0 ->
			Status = unsatisfiable
		;	Status = counted
		),
		(	integer(Value) ->
			NumberType = integer
		;	NumberType = float
		),
		(	Variables == none ->
			VariableCount = unknown
		;	length(Variables, VariableCount)
		),
		Result = wmc_result(Status, Value, [
			number_type(NumberType), solver(reference), raw_status(none), process_status(none),
			termination(completed), variables(Variables), variable_count(VariableCount),
			diagnostics([]), source_map(SourceMap), raw_output([])
		]).

	external_result(Solver, Options, Codes, SourceMap, Variables, Context, Result) :-
		default_executable(Solver, DefaultExecutable),
		^^option(executable(Command), Options, executable(DefaultExecutable)),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		(	weighted_source(Codes) ->
			Weighted = true
		;	Weighted = false
		),
		solver_command(Solver, Executable, Weighted, Options, SolverExecutable, SolverArguments),
		^^option(time_limit(Seconds), Options),
		time_limit_command(Seconds, SolverExecutable, SolverArguments, RunExecutable, RunArguments),
		wmc_process::run(RunExecutable, RunArguments, codes(Codes), Output, Diagnostics, RawStatus, NormalizedStatus),
		catch(
			wmc_output::decode(Solver, Output, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Variables, Result),
			error(Formal, _),
			throw(error(wmc_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
		).

	% Ganak: --mode 0 counts, --mode 1 computes exact rational weighted counts
	solver_command(ganak, Executable, Weighted, _, Executable, ['--mode', Mode]) :-
		(	Weighted == true -> Mode = '1' ; Mode = '0' ).
	% SharpSAT-TD must run from the directory of its binaries (it looks for flow_cutter_pace17 there)
	% and requires a temporary directory and a time to compute tree decompositions; all handled by
	% a launcher script, which receives the solver executable followed by the solver arguments
	solver_command(sharpsat_td, Executable, Weighted, Options, Launcher, Arguments) :-
		launcher_script(Launcher),
		^^option(decomposition_time(Time), Options),
		number_atom(Time, TimeAtom),
		(	Weighted == true ->
			Arguments = [Executable, '-decot', TimeAtom, '-decow', '100', '-WE', '-prec', '20']
		;	Arguments = [Executable, '-decot', TimeAtom, '-decow', '100']
		).

	launcher_script(Path) :-
		object_property(wmc, file(_, Directory)),
		atom_concat(Directory, 'scripts/wmc_sharpsat_td.sh', Path),
		(	file_exists(Path) ->
			true
		;	existence_error(command, Path)
		).

	% neither counter has a time limit option; the timeout command (GNU coreutils) is used
	% instead and exits with 124 when the time limit is reached (137 if it had to kill the counter)
	time_limit_command(0, Executable, Arguments, Executable, Arguments) :-
		!.
	time_limit_command(Seconds, Executable, Arguments, TimeoutExecutable, ['-k', '2', SecondsAtom, Executable| Arguments]) :-
		(	resolve_command_path(timeout, TimeoutExecutable) ->
			true
		;	existence_error(command, timeout)
		),
		number_atom(Seconds, SecondsAtom).

	number_atom(Number, Atom) :-
		number_codes(Number, Codes),
		atom_codes(Atom, Codes).

	% a source is weighted if it has "c p weight" lines
	weighted_source(Codes) :-
		split_lines(Codes, Lines),
		member(Line, Lines),
		phrase(weight_prefix, Line, _),
		!.

	weight_prefix -->
		blanks, [0'c], blank, blanks, [0'p], blank, blanks,
		[0'w, 0'e, 0'i, 0'g, 0'h, 0't], blank.

	blanks -->
		blank, !, blanks.
	blanks -->
		[].

	blank -->
		[Code], {member(Code, [9, 13, 32])}.

	solve_source(object(Object), Codes, SourceMap, Variables) :-
		!,
		object_source(Object, false, Codes, SourceMap, Variables).
	solve_source(file(Path), Codes, [], Variables) :-
		!,
		context(Context),
		check(atom, Path, Context),
		file_to_codes(Path, Codes),
		recovered_variables(Codes, Variables).
	solve_source(terms(Statements), Codes, [], Variables) :-
		!,
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		wmc_program::serialize(Statements, Codes),
		wmc_program::variables(Statements, Variables).
	solve_source(codes(SourceCodes), Codes, [], Variables) :-
		!,
		context(Context),
		check(codes, SourceCodes, Context),
		Codes = SourceCodes,
		recovered_variables(Codes, Variables).
	solve_source(Source, _, _, _) :-
		domain_error(wmc_source, Source).

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(solver(Solver)) :-
		once((Solver == reference; Solver == ganak; Solver == sharpsat_td)).
	valid_option(time_limit(Seconds)) :-
		integer(Seconds),
		Seconds >= 0.
	valid_option(decomposition_time(Time)) :-
		number(Time),
		Time > 0.
	valid_option(embed_variables(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	default_option(solver(reference)).
	default_option(time_limit(0)).
	default_option(decomposition_time(1)).
	default_option(embed_variables(true)).

	default_executable(ganak, ganak).
	default_executable(sharpsat_td, 'sharpSAT').

	program(Source) :-
		program(Source, Codes, []),
		print_program(Codes).

	program(Source, Codes) :-
		program(Source, Codes, []).

	program(Source, Codes, Options) :-
		context(Context),
		check(ground, Source, Context),
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		program_source(Source, Context, MergedOptions, Codes).

	print_program(Codes) :-
		context(Context),
		check(codes, Codes, Context),
		forall(member(Code, Codes), put_code(Code)).

	program_source(terms(Statements), _, Options, Codes) :-
		!,
		^^option(embed_variables(Embed), Options),
		embed_terms(Embed, Statements, AllStatements),
		wmc_program::serialize(AllStatements, Codes).
	program_source(codes(SourceCodes), Context, _, Codes) :-
		!,
		check(codes, SourceCodes, Context),
		Codes = SourceCodes.
	program_source(file(Path), Context, _, Codes) :-
		!,
		check(atom, Path, Context),
		file_to_codes(Path, Codes).
	program_source(object(Object), _, Options, Codes) :-
		!,
		^^option(embed_variables(Embed), Options),
		object_source(Object, Embed, Codes, _, _).
	program_source(Source, _, _, _) :-
		domain_error(wmc_source, Source).

	object_source(Object, Embed, Codes, SourceMap, Variables) :-
		(	conforms_to_protocol(Object, wmc_problem_protocol) ->
			findall(Statement-Origin, Object::wmc_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			wmc_program::variables(Statements, Variables),
			embed_object(Embed, Variables, Statements, Origins, AllStatements, AllOrigins),
			wmc_program::serialize(AllStatements, AllOrigins, Codes, SourceMap)
		;	domain_error(wmc_source, object(Object))
		).

	embed_terms(false, Statements, Statements) :-
		!.
	embed_terms(true, Statements, AllStatements) :-
		wmc_program::variables(Statements, Variables),
		variable_comments(Variables, Comments),
		append(Comments, Statements, AllStatements).

	embed_object(false, _, Statements, Origins, Statements, Origins) :-
		!.
	embed_object(true, Variables, Statements, Origins, AllStatements, AllOrigins) :-
		variable_comments(Variables, Comments),
		length(Comments, Count),
		comment_origins(Count, CommentOrigins),
		append(Comments, Statements, AllStatements),
		append(CommentOrigins, Origins, AllOrigins).

	comment_origins(0, []) :-
		!.
	comment_origins(Count, [unknown| Origins]) :-
		Count > 0,
		Next is Count - 1,
		comment_origins(Next, Origins).

	% embeds a "c v Number Term" marker comment per declared variable, letting
	% solve/2-3 recover a symbolic variable table from codes/1 or file/1 sources
	variable_comments([], []).
	variable_comments([Term-Number| Variables], [comment(MarkerCodes)| Comments]) :-
		marker_codes(Number, Term, MarkerCodes),
		variable_comments(Variables, Comments).

	marker_codes(Number, Term, [0'v, 0' | Rest]) :-
		number_codes(Number, NumberCodes),
		write_term_to_codes(Term, TermCodes, [quoted(true)]),
		append(NumberCodes, [0' | TermCodes], Rest).

	recovered_variables(Codes, Variables) :-
		split_lines(Codes, Lines),
		findall(Pair, (member(Line, Lines), marker_pair(Line, Pair)), Pairs),
		(	Pairs == [] ->
			Variables = none
		;	Variables = Pairs
		).

	split_lines(Codes, Lines) :-
		split_lines(Codes, [], Lines).

	split_lines([], Line, [RevLine]) :-
		!,
		reverse(Line, RevLine).
	split_lines([10| Codes], Line, [RevLine| Lines]) :-
		!,
		reverse(Line, RevLine),
		split_lines(Codes, [], Lines).
	split_lines([Code| Codes], Line, Lines) :-
		split_lines(Codes, [Code| Line], Lines).

	marker_pair([0'c, 0' , 0'v, 0' | Rest], Term-Number) :-
		append(NumberCodes, [0' | TermCodes], Rest),
		!,
		catch(
			(	number_codes(Number, NumberCodes),
				read_term_from_codes(TermCodes, Term, [])
			),
			_,
			fail
		).

	statements_origins([], [], []).
	statements_origins([Statement-Origin| Pairs], [Statement| Statements], [Origin| Origins]) :-
		statements_origins(Pairs, Statements, Origins).

	value(Result, Value) :-
		check_result(Result),
		Result = wmc_result(_, Value, _),
		Value \== none.

	result_property(Result, Property) :-
		check_result(Result),
		Result = wmc_result(Status, _, Properties),
		(	Property = status(Status)
		;	member(Property, Properties)
		).

	print_result(Result) :-
		check_result(Result),
		Result = wmc_result(Status, Value, _),
		status_label(Status, Label),
		write('s '), write(Label), nl,
		(	integer(Value) ->
			write('c s exact arb int '), write(Value), nl
		;	number(Value) ->
			write('c s exact double float '), write(Value), nl
		;	true
		).

	status_label(counted, 'SATISFIABLE').
	status_label(unsatisfiable, 'UNSATISFIABLE').
	status_label(unknown, 'UNKNOWN').

	check_result(Result) :-
		(	ground(Result) ->
			(	Result = wmc_result(Status, Value, Properties),
				member(Status, [counted, unsatisfiable, unknown]),
				(	Value == none ; number(Value) ),
				valid(list(compound), Properties) ->
				true
			;	domain_error(wmc_result, Result)
			)
		;	instantiation_error
		).

:- end_object.

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


:- object(max_sat,
	implements(max_sat_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-23,
		comment is 'Process-backed RC2, Open-WBO, and Z3 CLI weighted partial MaxSAT solving, WCNF source generation, and collected result inspection.'
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(os, [
		absolute_file_name/2, file_exists/1, resolve_command_path/2
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	:- uses(term_io, [
		read_term_from_codes/3, write_term_to_codes/3
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	solve(Source, Result) :-
		context(Context),
		solve(Source, Result, Context, []).

	solve(Source, Result, UserOptions) :-
		context(Context),
		solve(Source, Result, Context, UserOptions).

	solve(Source, Result, Context, UserOptions) :-
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
		^^option(wcnf_format(Format), Options),
		^^option(anytime(Anytime), Options),
		solve_source(Source, Format, ProcessSource, SourceMap, Variables),
		solver_arguments(Solver, Options, Arguments),
		max_sat_process::run(Executable, Arguments, ProcessSource, Codes, Diagnostics, RawStatus, NormalizedStatus),
		catch(
			max_sat_output::decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Variables, Anytime, Result),
			error(Formal, _),
			throw(error(max_sat_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
		).

	solve_source(object(Object), Format, codes(Codes), SourceMap, Variables) :-
		!,
		object_source(Object, false, Format, Codes, SourceMap, Variables).
	solve_source(file(Path), _, codes(Codes), [], Variables) :-
		!,
		context(Context),
		check(atom, Path, Context),
		% the solver selects the input format from the file name extension, thus the
		% file contents are always passed using a file named with the .wcnf extension
		file_to_codes(Path, Codes),
		recovered_variables(Codes, Variables).
	solve_source(terms(Statements), Format, codes(Codes), [], Variables) :-
		!,
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		max_sat_program::serialize(Statements, Format, Codes),
		max_sat_program::variables(Statements, Variables).
	solve_source(codes(SourceCodes), _, codes(Codes), [], Variables) :-
		!,
		context(Context),
		check(codes, SourceCodes, Context),
		Codes = SourceCodes,
		recovered_variables(Codes, Variables).
	solve_source(Source, _, _, _, _) :-
		domain_error(max_sat_source, Source).

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(solver(Solver)) :-
		once((Solver == rc2; Solver == open_wbo; Solver == z3)).
	valid_option(time_limit(Seconds)) :-
		integer(Seconds),
		Seconds >= 0.
	valid_option(anytime(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(wcnf_format(Format)) :-
		once((Format == old; Format == new)).
	valid_option(embed_variables(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	default_option(solver(open_wbo)).
	default_option(time_limit(0)).
	default_option(anytime(false)).
	default_option(wcnf_format(old)).
	default_option(embed_variables(true)).

	% the executable default depends on the selected solver
	default_executable(rc2, 'rc2.py').
	default_executable(open_wbo, 'open-wbo').
	default_executable(z3, z3).

	% A time limit of zero (the default) means unlimited and omits the solver option.
	% RC2 needs -vv to print the model (without it, only the status and cost are printed);
	% its -T option stops the search without reporting a status. Open-WBO -cpu-lim stops the
	% search reporting its best solution so far (an anytime solver). Z3 -T: is a hard timeout
	% that reports just "timeout"; the -model option prints the model in SMT-LIB format.
	solver_arguments(rc2, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		(	Seconds =:= 0 ->
			Arguments = ['-vv']
		;	integer_atom(Seconds, SecondsAtom),
			Arguments = ['-vv', '-T', SecondsAtom]
		).
	solver_arguments(open_wbo, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		joined_limit_argument('-cpu-lim=', Seconds, Arguments).
	solver_arguments(z3, Options, Arguments) :-
		^^option(time_limit(Seconds), Options),
		joined_limit_argument('-T:', Seconds, TimeArguments),
		Arguments = ['-model'| TimeArguments].

	joined_limit_argument(_, 0, []) :-
		!.
	joined_limit_argument(Prefix, Limit, [Argument]) :-
		integer_atom(Limit, LimitAtom),
		atom_concat(Prefix, LimitAtom, Argument).

	integer_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		atom_codes(Atom, Codes).

	program(Source) :-
		context(Context),
		program(Source, Codes, Context, []),
		print_program(Codes).

	program(Source, Codes) :-
		context(Context),
		program(Source, Codes, Context, []).

	program(Source, Codes, UserOptions) :-
		context(Context),
		program(Source, Codes, Context, UserOptions).

	program(Source, Codes, Context, UserOptions) :-
		check(ground, Source, Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		program_source(Source, Context, Options, Codes).

	print_program(Codes) :-
		context(Context),
		check(codes, Codes, Context),
		forall(member(Code, Codes), put_code(Code)).

	program_source(terms(Statements), _, Options, Codes) :-
		!,
		^^option(embed_variables(Embed), Options),
		^^option(wcnf_format(Format), Options),
		embed_terms(Embed, Statements, AllStatements),
		max_sat_program::serialize(AllStatements, Format, Codes).
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
		^^option(wcnf_format(Format), Options),
		object_source(Object, Embed, Format, Codes, _, _).
	program_source(Source, _, _, _) :-
		domain_error(max_sat_source, Source).

	object_source(Object, Embed, Format, Codes, SourceMap, Variables) :-
		(	conforms_to_protocol(Object, max_sat_problem_protocol) ->
			findall(Statement-Origin, Object::wcnf_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			max_sat_program::variables(Statements, Variables),
			embed_object(Embed, Variables, Statements, Origins, AllStatements, AllOrigins),
			max_sat_program::serialize(AllStatements, AllOrigins, Format, Codes, SourceMap)
		;	domain_error(max_sat_source, object(Object))
		).

	embed_terms(false, Statements, Statements) :-
		!.
	embed_terms(true, Statements, AllStatements) :-
		max_sat_program::variables(Statements, Variables),
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

	% best-effort: only ever upgrades variables(none); never raises an error
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

	literal(Result, Literal) :-
		check_result(Result),
		Result = max_sat_result(_, Model, _),
		Model = assignment(Literals),
		member(Literal, Literals).

	cost(Result, Cost) :-
		check_result(Result),
		Result = max_sat_result(_, _, Properties),
		memberchk(cost(Cost), Properties).

	result_property(Result, Property) :-
		check_result(Result),
		Result = max_sat_result(Status, _, Properties),
		(	Property = status(Status)
		;	member(Property, Properties)
		).

	print_result(Result) :-
		check_result(Result),
		Result = max_sat_result(Status, Model, Properties),
		(	member(cost(Cost), Properties) ->
			write('o '), write(Cost), nl
		;	true
		),
		status_label(Status, Label),
		write('s '), write(Label), nl,
		(	Model = assignment(Literals) ->
			print_witness(Literals, Properties)
		;	true
		).

	status_label(optimal, 'OPTIMUM FOUND').
	status_label(satisfiable, 'SATISFIABLE').
	status_label(unsatisfiable, 'UNSATISFIABLE').
	status_label(unknown, 'UNKNOWN').

	print_witness(Literals, Properties) :-
		(	member(variables(Variables), Properties),
			Variables \== none ->
			true
		;	Variables = none
		),
		literal_numbers(Literals, Variables, Numbers),
		write('v '),
		print_numbers(Numbers),
		nl.

	literal_numbers([], _, []).
	literal_numbers([Literal| Literals], Variables, [Number| Numbers]) :-
		literal_number(Literal, Variables, Number),
		literal_numbers(Literals, Variables, Numbers).

	literal_number(+Term, none, Term) :-
		!.
	literal_number(-Term, none, Number) :-
		!,
		Number is -Term.
	literal_number(+Term, Variables, Number) :-
		!,
		memberchk(Term-Number, Variables).
	literal_number(-Term, Variables, Number) :-
		memberchk(Term-VariableNumber, Variables),
		Number is -VariableNumber.

	print_numbers([]).
	print_numbers([Number| Numbers]) :-
		write(Number),
		forall(
			member(Next, Numbers),
			(	write(' '),
				write(Next)
			)
		).

	check_result(Result) :-
		(	ground(Result) ->
			(	Result = max_sat_result(Status, Model, Properties),
				member(Status, [optimal, satisfiable, unsatisfiable, unknown]),
				valid(list(compound), Properties),
				valid_model(Model) ->
				true
			;	domain_error(max_sat_result, Result)
			)
		;	instantiation_error
		).

	valid_model(none) :-
		!.
	valid_model(assignment(Literals)) :-
		valid(list(compound), Literals),
		valid_literals(Literals).

	valid_literals([]).
	valid_literals([+_| Literals]) :-
		!,
		valid_literals(Literals).
	valid_literals([-_| Literals]) :-
		valid_literals(Literals).

:- end_object.

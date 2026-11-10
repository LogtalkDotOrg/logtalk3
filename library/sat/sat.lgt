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


:- object(sat,
	implements(sat_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Process-backed Kissat, CaDiCaL, Glucose, and Z3 CLI solving, DIMACS CNF source generation, and collected result inspection.'
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
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
		default_executable(Solver, DefaultExecutable),
		^^option(executable(Command), Options, executable(DefaultExecutable)),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		solve_source(Source, ProcessSource, SourceMap, Variables),
		solver_arguments(Solver, Options, Arguments),
		sat_process::run(Executable, Arguments, ProcessSource, Codes, Diagnostics, RawStatus, NormalizedStatus),
		catch(
			sat_output::decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Variables, Result),
			error(Formal, _),
			throw(error(sat_solver_error(Formal, NormalizedStatus, Diagnostics, SourceMap), Context))
		).

	solve_source(object(Object), codes(Codes), SourceMap, Variables) :-
		!,
		object_source(Object, false, Codes, SourceMap, Variables).
	solve_source(file(Path), file(Absolute), [], Variables) :-
		!,
		context(Context),
		check(atom, Path, Context),
		absolute_file_name(Path, Absolute),
		recovered_file_variables(Absolute, Variables).
	solve_source(terms(Statements), codes(Codes), [], Variables) :-
		!,
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		sat_program::serialize(Statements, Codes),
		sat_program::variables(Statements, Variables).
	solve_source(codes(SourceCodes), codes(Codes), [], Variables) :-
		!,
		context(Context),
		check(codes, SourceCodes, Context),
		Codes = SourceCodes,
		recovered_variables(Codes, Variables).
	solve_source(Source, _, _, _) :-
		domain_error(sat_source, Source).

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(solver(Solver)) :-
		once((Solver == kissat; Solver == cadical; Solver == glucose; Solver == z3)).
	valid_option(time_limit(Seconds)) :-
		integer(Seconds),
		Seconds >= 0.
	valid_option(conflicts(Limit)) :-
		integer(Limit),
		Limit >= 0.
	valid_option(decisions(Limit)) :-
		integer(Limit),
		Limit >= 0.
	valid_option(seed(Seed)) :-
		integer(Seed),
		Seed >= 0.
	valid_option(witness(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(embed_variables(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	default_option(solver(kissat)).
	default_option(time_limit(0)).
	default_option(conflicts(0)).
	default_option(decisions(0)).
	default_option(seed(0)).
	default_option(witness(true)).
	default_option(embed_variables(true)).

	% The executable default depends on the selected solver, so it is resolved with
	% ^^option/3 at the point of use instead of through a fixed default_option/1 fact.
	default_executable(kissat, kissat).
	default_executable(cadical, cadical).
	default_executable(glucose, 'glucose-syrup').
	default_executable(z3, z3).

	% Kissat rejects an explicit "--time=0"; 0 is instead the library-wide unlimited
	% sentinel, consistently applied to conflicts and decisions, and across solvers.
	solver_arguments(kissat, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		^^option(conflicts(Conflicts), Options),
		^^option(decisions(Decisions), Options),
		^^option(seed(Seed), Options),
		^^option(witness(Witness), Options),
		joined_argument('--seed=', Seed, SeedArgument),
		joined_limit_argument('--time=', Seconds, TimeArguments),
		joined_limit_argument('--conflicts=', Conflicts, ConflictArguments),
		joined_limit_argument('--decisions=', Decisions, DecisionArguments),
		no_witness_arguments(Witness, WitnessArguments),
		append(TimeArguments, ConflictArguments, Partial0),
		append(Partial0, DecisionArguments, Partial1),
		append(Partial1, WitnessArguments, ExtraArguments),
		Arguments = ['--quiet=true', SeedArgument| ExtraArguments].
	% CaDiCaL accepts an explicit zero time/conflict/decision limit (immediate stop);
	% the same omit-when-zero sentinel is used for consistency with the other solvers.
	solver_arguments(cadical, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		^^option(conflicts(Conflicts), Options),
		^^option(decisions(Decisions), Options),
		^^option(seed(Seed), Options),
		^^option(witness(Witness), Options),
		joined_argument('--seed=', Seed, SeedArgument),
		spaced_limit_argument('-t', Seconds, TimeArguments),
		spaced_limit_argument('-c', Conflicts, ConflictArguments),
		spaced_limit_argument('-d', Decisions, DecisionArguments),
		no_witness_arguments(Witness, WitnessArguments),
		append(TimeArguments, ConflictArguments, Partial0),
		append(Partial0, DecisionArguments, Partial1),
		append(Partial1, WitnessArguments, ExtraArguments),
		Arguments = ['--quiet=true', SeedArgument| ExtraArguments].
	% Glucose exposes only a CPU time budget (no conflict or decision counters), so
	% conflicts/1 and decisions/1 are validated but have no effect for this solver.
	% The witness is off by default, the opposite polarity of Kissat and CaDiCaL.
	% Glucose also rejects an explicit zero random seed, so seed(0) (the shared
	% default) omits "-rnd-seed" instead, matching the time/conflicts/decisions
	% omit-at-zero convention used across all three solvers.
	solver_arguments(glucose, Options, Arguments) :-
		!,
		^^option(time_limit(Seconds), Options),
		^^option(seed(Seed), Options),
		^^option(witness(Witness), Options),
		joined_limit_argument('-rnd-seed=', Seed, SeedArguments),
		joined_limit_argument('-cpu-lim=', Seconds, TimeArguments),
		model_arguments(Witness, WitnessArguments),
		append(SeedArguments, TimeArguments, Partial0),
		append(Partial0, WitnessArguments, ExtraArguments),
		Arguments = ['-verb=0'| ExtraArguments].
	% Z3 exits 0 regardless of the reported status (see sat_output's Solver-aware
	% validate_status/4), so its result is read entirely from the "s" status line.
	% Z3 exposes a conflict budget ("sat.max_conflicts=") but no decision counter,
	% so decisions/1 is validated but has no effect for this solver. Z3 also always
	% prints the witness line for a satisfiable result in DIMACS mode, so witness/1
	% is validated but has no effect for this solver either.
	solver_arguments(z3, Options, Arguments) :-
		^^option(time_limit(Seconds), Options),
		^^option(conflicts(Conflicts), Options),
		^^option(seed(Seed), Options),
		joined_argument('sat.random_seed=', Seed, SeedArgument),
		joined_limit_argument('-T:', Seconds, TimeArguments),
		joined_limit_argument('sat.max_conflicts=', Conflicts, ConflictArguments),
		append(TimeArguments, ConflictArguments, ExtraArguments),
		Arguments = ['-dimacs', '-model', '-nw', SeedArgument| ExtraArguments].

	joined_limit_argument(_, 0, []) :-
		!.
	joined_limit_argument(Prefix, Limit, [Argument]) :-
		joined_argument(Prefix, Limit, Argument).

	joined_argument(Prefix, Value, Argument) :-
		integer_atom(Value, ValueAtom),
		atom_concat(Prefix, ValueAtom, Argument).

	spaced_limit_argument(_, 0, []) :-
		!.
	spaced_limit_argument(Flag, Limit, [Flag, LimitAtom]) :-
		integer_atom(Limit, LimitAtom).

	no_witness_arguments(true, []).
	no_witness_arguments(false, ['-n']).

	model_arguments(true, ['-model']).
	model_arguments(false, []).

	integer_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		atom_codes(Atom, Codes).

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
		sat_program::serialize(AllStatements, Codes).
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
		domain_error(sat_source, Source).

	object_source(Object, Embed, Codes, SourceMap, Variables) :-
		(	conforms_to_protocol(Object, sat_problem_protocol) ->
			findall(Statement-Origin, Object::cnf_statement(Statement, Origin), Pairs),
			statements_origins(Pairs, Statements, Origins),
			sat_program::variables(Statements, Variables),
			embed_object(Embed, Variables, Statements, Origins, AllStatements, AllOrigins),
			sat_program::serialize(AllStatements, AllOrigins, Codes, SourceMap)
		;	domain_error(sat_source, object(Object))
		).

	embed_terms(false, Statements, Statements) :-
		!.
	embed_terms(true, Statements, AllStatements) :-
		sat_program::variables(Statements, Variables),
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

	recovered_file_variables(Absolute, Variables) :-
		(	catch(file_to_codes(Absolute, FileCodes), _, fail) ->
			recovered_variables(FileCodes, Variables)
		;	Variables = none
		).

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
		Result = sat_result(_, Model, _),
		Model = assignment(Literals),
		member(Literal, Literals).

	result_property(Result, Property) :-
		check_result(Result),
		Result = sat_result(Status, _, Properties),
		(	Property = status(Status)
		;	member(Property, Properties)
		).

	print_result(Result) :-
		check_result(Result),
		Result = sat_result(Status, Model, Properties),
		status_label(Status, Label),
		write('s '), write(Label), nl,
		(	Model = assignment(Literals) ->
			print_witness(Literals, Properties)
		;	true
		).

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
		write(' 0'), nl.

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

	print_numbers([]) :-
		!.
	print_numbers([Number| Numbers]) :-
		write(Number),
		forall(member(Next, Numbers), (
			write(' '),
			write(Next)
		)).

	check_result(Result) :-
		(	ground(Result) ->
			(	Result = sat_result(Status, Model, Properties),
				member(Status, [satisfiable, unsatisfiable, unknown]),
				valid(list(compound), Properties),
				valid_model(Model) ->
				true
			;	domain_error(sat_result, Result)
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

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


:- category(linear_programming_cli,
	extends(linear_programming_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-27,
		comment is 'Support for linear programming backends that run an external solver command-line tool: the problem is written using the CPLEX LP format, the solver is run in an isolated directory, and its output is decoded into a result term. Importing objects must define the ``solver_name/1``, ``default_executable/1``, ``solver_arguments/2``, and ``decode_output/6`` protected predicates.'
	]).

	:- protected(solver_name/1).
	:- mode(solver_name(-atom), one).
	:- info(solver_name/1, [
		comment is 'Name of the solver.',
		argnames is ['Name']
	]).

	:- protected(default_executable/1).
	:- mode(default_executable(-atom), one).
	:- info(default_executable/1, [
		comment is 'Default name of the solver executable, resolved using the system path.',
		argnames is ['Executable']
	]).

	:- protected(solver_arguments/2).
	:- mode(solver_arguments(+list(compound), -list), one_or_error).
	:- info(solver_arguments/2, [
		comment is 'Command-line arguments given the merged options. An argument is an atom or a list of atoms to be concatenated where ``$input`` is the path of the LP file and ``$solution`` the path of a file that the solver can write its solution to.',
		argnames is ['Options', 'Arguments'],
		exceptions is [
			'An option is unsupported by the selected solver' - domain_error(option, 'Option')
		]
	]).

	:- protected(decode_output/6).
	:- mode(decode_output(+list(code), +list(code), -atom, -term, -term, -atom), one_or_error).
	:- info(decode_output/6, [
		comment is 'Decodes the solver standard output and solution file contents into a status (``optimal``, ``feasible``, ``infeasible``, ``unbounded``, ``infeasible_or_unbounded``, ``time_limit``, ``iteration_limit``, ``node_limit``, or ``numerical_error``), an objective value (or ``none``), solution data represented as ``solution(Pairs)`` when a solution section was parsed or ``none`` otherwise, and the status text reported by the solver. The pairs use the one-based variable index; absent variables in a parsed solution have value zero.',
		argnames is ['Output', 'Solution', 'Status', 'Objective', 'SolutionData', 'Text'],
		exceptions is [
			'The solver output is missing required data or cannot be decoded' - domain_error(linear_programming_output, 'Reason')
		]
	]).

	:- protected(assemble_result/9).
	:- mode(assemble_result(+atom, +compound, +term, +compound, +atom, +term, +term, +atom, -compound), one_or_error).
	:- info(assemble_result/9, [
		comment is 'Assembles a validated result from already decoded solver data.',
		argnames is [
			'Solver', 'Problem', 'RawStatus', 'NormalizedStatus', 'Status', 'ReportedObjective',
			'SolutionData', 'Text', 'Result'
		],
		exceptions is [
			'An ``optimal`` or ``feasible`` result has no complete decoded solution' - domain_error(linear_programming_output, missing_solution),
			'The decoded solution is inconsistent with the problem or objective' - domain_error(linear_programming_output, inconsistent_solution)
		]
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(os, [
		absolute_file_name/2, file_exists/1, resolve_command_path/2
	]).

	solve(Problem, Result) :-
		::solve(Problem, Result, []).

	solve(Problem, Result, UserOptions) :-
		context(Context),
		^^check_problem(Problem),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Problem = linear_program(Variables, _, Objective),
		check_solvable_problem(Variables, Objective),
		::default_executable(DefaultExecutable),
		^^option(executable(Command), Options, executable(DefaultExecutable)),
		(	file_exists(Command) ->
			absolute_file_name(Command, Executable)
		;	resolve_command_path(Command, Executable) ->
			true
		;	existence_error(command, Command)
		),
		::solver_arguments(Options, Arguments),
		lp_format::generate(Problem, Codes),
		linear_programming_process::run(Executable, Arguments, codes(Codes), Output, Solution, Diagnostics, RawStatus, NormalizedStatus),
		::solver_name(Solver),
		catch(
			decode_result(Solver, Problem, Output, Solution, RawStatus, NormalizedStatus, Result),
			error(Formal, _),
			throw(error(linear_programming_solver_error(Formal, NormalizedStatus, Diagnostics), Context))
		).

	check_solvable_problem([], _) :-
		domain_error(linear_programming_problem, empty).
	check_solvable_problem(_, none) :-
		domain_error(linear_programming_problem, missing_objective).
	check_solvable_problem(_, objective(_, _)).

	valid_option(executable(Path)) :-
		atom(Path),
		Path \== ''.
	valid_option(time_limit(Seconds)) :-
		number(Seconds),
		Seconds >= 0.
	valid_option(mip_gap(Gap)) :-
		(	Gap == none ->
			true
		;	number(Gap),
			Gap >= 0
		).

	default_option(time_limit(0)).
	default_option(mip_gap(none)).

	decode_result(Solver, Problem, Output, Solution, RawStatus, NormalizedStatus, Result) :-
		(	NormalizedStatus == exit(0) ->
			true
		;	domain_error(linear_programming_process_status, NormalizedStatus)
		),
		::decode_output(Output, Solution, Status, ReportedObjective, SolutionData, Text),
		assemble_result(Solver, Problem, RawStatus, NormalizedStatus, Status, ReportedObjective, SolutionData, Text, Result).

	assemble_result(Solver, Problem, RawStatus, NormalizedStatus, Status, ReportedObjective, SolutionData, Text, Result) :-
		(	( Status == optimal ; Status == feasible ) ->
			(	Problem = linear_program(Variables, _, objective(_, _)),
				length(Variables, VariableCount),
				valid_solution(SolutionData, ReportedObjective, VariableCount, Pairs) ->
				true
			;	domain_error(linear_programming_output, missing_solution)
			),
			variable_values(Variables, 1, Pairs, Values),
			(	valid_reported_solution(Problem, Values, ReportedObjective) ->
				Objective = ReportedObjective
			;	domain_error(linear_programming_output, inconsistent_solution)
			)
		;	Objective = none,
			Values = []
		),
		Result = linear_programming_result(Status, Objective, Values, [solver(Solver), solver_status(Text), process_status(NormalizedStatus), raw_status(RawStatus)]).

	valid_solution(solution(Pairs), Objective, VariableCount, Pairs) :-
		number(Objective),
		valid_solution_pairs(Pairs, VariableCount, []).

	valid_solution_pairs([], _VariableCount, _Seen).
	valid_solution_pairs([Index-Value| Pairs], VariableCount, Seen) :-
		integer(Index),
		Index >= 1,
		Index =< VariableCount,
		number(Value),
		\+ memberchk(Index, Seen),
		valid_solution_pairs(Pairs, VariableCount, [Index| Seen]).

	valid_reported_solution(linear_program(Variables, Constraints, objective(Expression, _Sense)), Values, ReportedObjective) :-
		Tolerance = 1.0e-6,
		reported_values_within_bounds(Variables, Values, Tolerance),
		reported_constraints_satisfied(Constraints, Values, Tolerance),
		evaluate_expression(Expression, Values, EvaluatedObjective),
		Scale is max(1, max(abs(ReportedObjective), abs(EvaluatedObjective))),
		abs(ReportedObjective - EvaluatedObjective) =< 1.0e-5 * Scale.

	reported_values_within_bounds([], _Values, _Tolerance).
	reported_values_within_bounds([variable(Name, Type, Lower, Upper)| Variables], Values, Tolerance) :-
		memberchk(Name-Value, Values),
		(	Type == continuous ->
			true
		;	integer(Value)
		),
		reported_lower_bound_satisfied(Lower, Value, Tolerance),
		reported_upper_bound_satisfied(Value, Upper, Tolerance),
		reported_values_within_bounds(Variables, Values, Tolerance).

	reported_lower_bound_satisfied(-inf, _Value, _Tolerance) :-
		!.
	reported_lower_bound_satisfied(Lower, Value, Tolerance) :-
		Value >= Lower - Tolerance.

	reported_upper_bound_satisfied(_Value, inf, _Tolerance) :-
		!.
	reported_upper_bound_satisfied(Value, Upper, Tolerance) :-
		Value =< Upper + Tolerance.

	reported_constraints_satisfied([], _Values, _Tolerance).
	reported_constraints_satisfied([constraint(Expression, Sense, RightHandSide)| Constraints], Values, Tolerance) :-
		evaluate_expression(Expression, Values, Value),
		reported_constraint_satisfied(Sense, Value, RightHandSide, Tolerance),
		reported_constraints_satisfied(Constraints, Values, Tolerance).

	reported_constraint_satisfied((=<), Value, RightHandSide, Tolerance) :-
		Value =< RightHandSide + Tolerance.
	reported_constraint_satisfied((>=), Value, RightHandSide, Tolerance) :-
		Value >= RightHandSide - Tolerance.
	reported_constraint_satisfied((=), Value, RightHandSide, Tolerance) :-
		abs(Value - RightHandSide) =< Tolerance.

	% variables not reported by the solver are zero; values of integer variables are snapped to
	% the closest integer when within a small tolerance
	variable_values([], _, _, []).
	variable_values([variable(Name, Type, _, _)| Variables], Index, Pairs, [Name-Value| Values]) :-
		(	memberchk(Index-Value0, Pairs) ->
			true
		;	Value0 = 0
		),
		snap(Type, Value0, Value),
		Next is Index + 1,
		variable_values(Variables, Next, Pairs, Values).

	snap(continuous, Value, Value) :-
		!.
	snap(_, Value0, Value) :-
		(	integer(Value0) ->
			Rounded is Value0
		;	Rounded is round(Value0)
		),
		(	abs(Value0 - Rounded) =< 1.0e-6 ->
			Value = Rounded
		;	Value = Value0
		).

	evaluate_expression(Expression, Values, Value) :-
		evaluate_expression(Expression, Values, 0, Value).

	evaluate_expression([], _, Value, Value).
	evaluate_expression([Coefficient*Name| Expression], Values, Value0, Value) :-
		memberchk(Name-Variable, Values),
		Value1 is Value0 + Coefficient * Variable,
		evaluate_expression(Expression, Values, Value1, Value).

	% auxiliary predicates for the decoders of the importing objects

	:- protected(split_lines/2).
	:- mode(split_lines(+list(code), -list(list(code))), one).
	:- info(split_lines/2, [
		comment is 'Splits character codes into lines (removing carriage returns).',
		argnames is ['Codes', 'Lines']
	]).

	:- protected(words/2).
	:- mode(words(+list(code), -list(list(code))), one).
	:- info(words/2, [
		comment is 'Splits a line into blank-separated words.',
		argnames is ['Line', 'Words']
	]).

	:- protected(parse_number/2).
	:- mode(parse_number(+list(code), -number), zero_or_one).
	:- info(parse_number/2, [
		comment is 'Parses a decimal number (optional sign, fraction, and exponent). Fails if the codes are not a number.',
		argnames is ['Codes', 'Number']
	]).

	:- protected(has_text/2).
	:- mode(has_text(+list(list(code)), +atom), zero_or_one).
	:- info(has_text/2, [
		comment is 'True if some line contains the given text.',
		argnames is ['Lines', 'Text']
	]).

	:- protected(line_after/3).
	:- mode(line_after(+list(list(code)), +atom, -list(code)), zero_or_one).
	:- info(line_after/3, [
		comment is 'Finds the first line starting with the given prefix and returns the rest of the line without leading blanks.',
		argnames is ['Lines', 'Prefix', 'Rest']
	]).

	:- protected(normalized_text/2).
	:- mode(normalized_text(+list(code), -atom), one).
	:- info(normalized_text/2, [
		comment is 'Converts character codes to an atom with the blanks normalized to single spaces and without leading and trailing blanks.',
		argnames is ['Codes', 'Atom']
	]).

	:- protected(number_atom/2).
	:- mode(number_atom(+number, -atom), one).
	:- info(number_atom/2, [
		comment is 'Converts a number to an atom.',
		argnames is ['Number', 'Atom']
	]).

	:- protected(first_number/2).
	:- mode(first_number(+list(list(code)), -number), zero_or_one).
	:- info(first_number/2, [
		comment is 'Returns the first word of a list of words that is a number.',
		argnames is ['Words', 'Number']
	]).

	normalized_text(Codes, Atom) :-
		words(Codes, Words),
		join_words(Words, Joined),
		atom_codes(Atom, Joined).

	join_words([], []).
	join_words([Word], Word) :-
		!.
	join_words([Word| Words], Joined) :-
		join_words(Words, Rest),
		append(Word, [32| Rest], Joined).

	number_atom(Number, Atom) :-
		number_codes(Number, Codes),
		atom_codes(Atom, Codes).

	first_number([Word| Words], Number) :-
		(	parse_number(Word, Number0) ->
			Number = Number0
		;	first_number(Words, Number)
		).

	split_lines(Codes, Lines) :-
		split_lines(Codes, [], Lines).

	split_lines([], Buffer, [Line]) :-
		!,
		reverse(Buffer, Buffered),
		strip_cr(Buffered, Line).
	split_lines([10| Codes], Buffer, [Line| Lines]) :-
		!,
		reverse(Buffer, Buffered),
		strip_cr(Buffered, Line),
		split_lines(Codes, [], Lines).
	split_lines([Code| Codes], Buffer, Lines) :-
		split_lines(Codes, [Code| Buffer], Lines).

	strip_cr(Line0, Line) :-
		(	append(Line, [13], Line0) ->
			true
		;	Line = Line0
		).

	words(Codes, Words) :-
		drop_blanks(Codes, Trimmed),
		(	Trimmed == [] ->
			Words = []
		;	take_word(Trimmed, Word, Rest),
			Words = [Word| Words1],
			words(Rest, Words1)
		).

	take_word([], [], []).
	take_word([Code| Codes], Word, Rest) :-
		(	blank_code(Code) ->
			Word = [],
			Rest = Codes
		;	Word = [Code| Word1],
			take_word(Codes, Word1, Rest)
		).

	drop_blanks([Code| Codes0], Codes) :-
		blank_code(Code),
		!,
		drop_blanks(Codes0, Codes).
	drop_blanks(Codes, Codes).

	blank_code(9).
	blank_code(13).
	blank_code(32).

	has_text(Lines, Text) :-
		atom_codes(Text, Codes),
		member(Line, Lines),
		append(_, Rest, Line),
		append(Codes, _, Rest),
		!.

	line_after(Lines, Prefix, Rest) :-
		atom_codes(Prefix, Codes),
		member(Line, Lines),
		append(Codes, Rest0, Line),
		!,
		drop_blanks(Rest0, Rest).

	parse_number(Codes0, Number) :-
		words(Codes0, [Codes]),
		(	Codes = [0'-| Rest] ->
			Sign = -1,
			Unsigned = Rest
		;	Codes = [0'+| Rest] ->
			Sign = 1,
			Unsigned = Rest
		;	Sign = 1,
			Unsigned = Codes
		),
		split_exponent(Unsigned, Mantissa, Exponent),
		split_fraction(Mantissa, IntegerPart, FractionPart),
		IntegerPart \== [],
		all_digits(IntegerPart),
		all_digits(FractionPart),
		(	Exponent == none ->
			ExponentCodes = []
		;	Exponent = exponent(ExponentSign, ExponentDigits),
			ExponentDigits \== [],
			all_digits(ExponentDigits),
			append([0'e| ExponentSign], ExponentDigits, ExponentCodes)
		),
		(	FractionPart == [],
			ExponentCodes == [] ->
			NumberCodes = IntegerPart
		;	(	FractionPart == [] ->
				Fraction = [0'0]
			;	Fraction = FractionPart
			),
			append(IntegerPart, [0'.| Fraction], Decimal),
			append(Decimal, ExponentCodes, NumberCodes)
		),
		number_codes(Magnitude, NumberCodes),
		Number is Sign * Magnitude.

	split_exponent(Codes, Mantissa, Exponent) :-
		(	append(Mantissa0, [E| Rest], Codes),
			(	E =:= 0'e
			;	E =:= 0'E
			) ->
			Mantissa = Mantissa0,
			(	Rest = [0'-| Digits] -> Exponent = exponent([0'-], Digits)
			;	Rest = [0'+| Digits] -> Exponent = exponent([], Digits)
			;	Exponent = exponent([], Rest)
			)
		;	Mantissa = Codes,
			Exponent = none
		).

	split_fraction(Codes, IntegerPart, FractionPart) :-
		(	append(IntegerPart0, [0'.| FractionPart0], Codes) ->
			IntegerPart = IntegerPart0,
			FractionPart = FractionPart0
		;	IntegerPart = Codes,
			FractionPart = []
		).

	all_digits([]).
	all_digits([Code| Codes]) :-
		Code >= 0'0, Code =< 0'9,
		all_digits(Codes).

:- end_category.

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


:- object(sat_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Whole-document Kissat, CaDiCaL, Glucose, and Z3 plain-text output decoding.'
	]).

	:- public(decode/8).
	:- mode(decode(+atom, +list(code), +term, +compound, +list(code), +list(compound), +term, -compound), one_or_error).
	:- info(decode/8, [
		comment is 'Decodes captured stdout and process metadata into a collected ``sat_result(Status, Model, Properties)`` term, mapping reported literals back through an optional ``Variables`` table.',
		argnames is ['Solver', 'Codes', 'RawStatus', 'NormalizedStatus', 'Diagnostics', 'SourceMap', 'Variables', 'Result'],
		exceptions is [
			'``Solver`` is not ground' - instantiation_error,
			'``Solver`` is neither a variable nor an atom' - type_error(atom, 'Solver'),
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is neither a partial list nor a list of character codes' - type_error(codes, 'Codes'),
			'``Codes`` has no status line or an unsupported status label' - domain_error(sat_output, 'Field'-'Value'),
			'The witness line is missing its zero terminator' - domain_error(sat_output, witness_terminator_missing),
			'A witness literal number has no entry in the variable table' - domain_error(sat_output, variable('Number')),
			'``RawStatus`` is not ground' - instantiation_error,
			'``NormalizedStatus`` is not ground' - instantiation_error,
			'``NormalizedStatus`` is fatal or inconsistent with the decoded solver result' - domain_error(sat_process_status, 'NormalizedStatus'),
			'``Diagnostics`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Diagnostics`` is neither a partial list nor a list of character codes' - type_error(codes, 'Diagnostics'),
			'``SourceMap`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``SourceMap`` is neither a variable nor a list' - type_error(list, 'SourceMap'),
			'``Variables`` is not ground' - instantiation_error,
			'``Variables`` is ground but neither ``none`` nor a list of pairs' - type_error(list(pair), 'Variables')
		]
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		member/2, append/3, length/2, reverse/2
	]).

	decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Variables, Result) :-
		context(Context),
		check(ground, Solver-Codes-RawStatus-NormalizedStatus-Diagnostics-SourceMap-Variables, Context),
		check(atom, Solver, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		check(list, SourceMap, Context),
		(	Variables == none ->
			true
		;	valid(list(pair), Variables) ->
			true
		;	type_error(list(pair), Variables)
		),
		split_lines(Codes, Lines),
		(	status_line(Solver, Lines, Label) ->
			true
		;	domain_error(sat_output, 'Result'-missing)
		),
		(	result_status(Label, Status) ->
			true
		;	domain_error(sat_output, 'Result'-Label)
		),
		(	Status == satisfiable,
			witness_literals(Lines, Numbers) ->
			parse_literals(Numbers, Variables, Literals),
			Model = assignment(Literals)
		;	Model = none
		),
		validate_status(Solver, NormalizedStatus, Status, Termination),
		variable_count(Variables, Model, VariableCount),
		Result = sat_result(Status, Model, [
			solver(Solver), raw_status(RawStatus), process_status(NormalizedStatus),
			termination(Termination), variables(Variables), variable_count(VariableCount),
			diagnostics(Diagnostics), source_map(SourceMap), raw_output(Codes)
		]).

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

	status_line(_, Lines, Label) :-
		member([0's, 0' | Rest], Lines),
		atom_codes(Label, Rest),
		!.
	% CaDiCaL reports an unproven result as a bare "c UNKNOWN" comment line
	% instead of the "s UNKNOWN" status line used by Kissat and Glucose.
	status_line(cadical, Lines, 'UNKNOWN') :-
		member(Line, Lines),
		atom_codes('c UNKNOWN', Line),
		!.

	result_status('SATISFIABLE', satisfiable).
	result_status('UNSATISFIABLE', unsatisfiable).
	result_status('UNKNOWN', unknown).

	witness_literals(Lines, Numbers) :-
		witness_tokens(Lines, Tokens),
		Tokens \== [],
		take_until_zero(Tokens, Numbers).

	witness_tokens([], []).
	witness_tokens([[0'v, 0' | Rest]| Lines], Tokens) :-
		!,
		tokenize(Rest, LineTokens),
		witness_tokens(Lines, RestTokens),
		append(LineTokens, RestTokens, Tokens).
	witness_tokens([_| Lines], Tokens) :-
		witness_tokens(Lines, Tokens).

	tokenize(Codes, Numbers) :-
		phrase(integer_tokens(Numbers), Codes).

	integer_tokens([Number| Numbers]) -->
		blanks, integer_token(Number),
		!,
		integer_tokens(Numbers).
	integer_tokens([]) -->
		blanks.

	blanks -->
		[Code],
		{ blank_code(Code) },
		!,
		blanks.
	blanks -->
		[].

	integer_token(Number) -->
		sign(Sign), digits(Digits),
		{ Digits \== [], number_codes(Magnitude, Digits), Number is Sign * Magnitude }.

	sign(-1) -->
		[0'-],
		!.
	sign(1) -->
		[].

	digits([Digit| Digits]) -->
		[Digit],
		{ Digit >= 0'0, Digit =< 0'9 },
		!,
		digits(Digits).
	digits([]) -->
		[].

	blank_code(9).
	blank_code(32).

	take_until_zero([], _) :-
		domain_error(sat_output, witness_terminator_missing).
	take_until_zero([0| _], []) :-
		!.
	take_until_zero([Number| Tokens], [Number| Numbers]) :-
		take_until_zero(Tokens, Numbers).

	parse_literals([], _, []).
	parse_literals([Number| Numbers], Variables, [Literal| Literals]) :-
		(	Number > 0 ->
			literal_term(Number, Variables, Term),
			Literal = pos(Term)
		;	Magnitude is -Number,
			literal_term(Magnitude, Variables, Term),
			Literal = neg(Term)
		),
		parse_literals(Numbers, Variables, Literals).

	literal_term(Number, none, Number) :-
		!.
	literal_term(Number, Variables, Term) :-
		(	member(Term-Number, Variables) ->
			true
		;	domain_error(sat_output, variable(Number))
		).

	variable_count(Variables, Model, Count) :-
		(	Variables \== none ->
			length(Variables, Count)
		;	Model = assignment(Literals) ->
			length(Literals, Count)
		;	Count = unknown
		).

	% Z3 exits 0 regardless of the reported status, unlike Kissat, CaDiCaL, and
	% Glucose, which use the SAT competition exit code convention (10 for
	% satisfiable, 20 for unsatisfiable, 0 for unknown); the resource-limit
	% versus completed distinction for Z3 is instead read from the status label.
	validate_status(z3, exit(Code), Status, Termination) :-
		integer(Code),
		!,
		(	Code =:= 0 ->
			(	Status == unknown ->
				Termination = resource_limit
			;	Termination = completed
			)
		;	domain_error(sat_process_status, exit(Code))
		).
	validate_status(_, exit(Code), Status, Termination) :-
		integer(Code),
		!,
		(	Code =:= 10, Status == satisfiable ->
			Termination = completed
		;	Code =:= 20, Status == unsatisfiable ->
			Termination = completed
		;	Code =:= 0, Status == unknown ->
			Termination = resource_limit
		;	domain_error(sat_process_status, exit(Code))
		).
	validate_status(_, unknown(_), _, interrupted) :-
		!.
	validate_status(_, killed(_), _, interrupted) :-
		!.
	validate_status(_, NormalizedStatus, _, _) :-
		domain_error(sat_process_status, NormalizedStatus).

:- end_object.

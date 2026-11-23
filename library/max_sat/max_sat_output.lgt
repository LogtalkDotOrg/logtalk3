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


:- object(max_sat_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-23,
		comment is 'Whole-document RC2, Open-WBO, and Z3 plain-text MaxSAT output decoding.'
	]).

	:- public(decode/9).
	:- mode(decode(+atom, +list(code), +term, +compound, +list(code), +list(compound), +term, +atom, -compound), one_or_error).
	:- info(decode/9, [
		comment is 'Decodes captured stdout and process metadata into a collected ``max_sat_result(Status, Model, Properties)`` term, mapping reported literals back through an optional ``Variables`` table. ``Status`` is one of ``optimal``, ``satisfiable`` (a best-known solution of an interrupted anytime run that was accepted because ``Anytime`` is ``true``), ``unsatisfiable``, and ``unknown``.',
		argnames is [
			'Solver', 'Codes', 'RawStatus', 'NormalizedStatus', 'Diagnostics', 'SourceMap', 'Variables',
			'Anytime', 'Result'
		],
		exceptions is [
			'``Solver`` is not a ground term' - instantiation_error,
			'``Solver`` is neither a variable nor an atom' - type_error(atom, 'Solver'),
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is neither a partial list nor a list of character codes' - type_error(codes, 'Codes'),
			'``Codes`` has no status line or an unsupported status label' - domain_error(max_sat_output, 'Field'-'Value'),
			'A witness literal number has no entry in the variable table' - domain_error(max_sat_output, variable('Number')),
			'``NormalizedStatus`` is fatal or inconsistent with the decoded solver result' - domain_error(max_sat_process_status, 'NormalizedStatus'),
			'``Variables`` is ground but neither ``none`` nor a list of pairs' - type_error(list(pair), 'Variables')
		]
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		member/2, append/3, length/2, reverse/2, memberchk/2
	]).

	decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Variables, Anytime, Result) :-
		context(Context),
		check(ground, Solver-Codes-RawStatus-NormalizedStatus-Diagnostics-SourceMap-Variables-Anytime, Context),
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
		(	Solver == z3 ->
			z3_result(Lines, Variables, Status0, Model0, Cost0)
		;	mse_result(Solver, Lines, Variables, Status0, Model0, Cost0)
		),
		accept_best_known(Status0, Model0, Cost0, Anytime, Status, Model, Cost),
		validate_status(Solver, NormalizedStatus, Status, Termination),
		variable_count(Variables, Model, VariableCount),
		cost_property(Cost, CostProperties),
		append(CostProperties, [
			solver(Solver), raw_status(RawStatus), process_status(NormalizedStatus),
			termination(Termination), anytime(Anytime), variables(Variables), variable_count(VariableCount),
			diagnostics(Diagnostics), source_map(SourceMap), raw_output(Codes)
		], Properties),
		Result = max_sat_result(Status, Model, Properties).

	cost_property(none, []) :-
		!.
	cost_property(Cost, [cost(Cost)]).

	% a best-known (non-optimal) solution is only accepted for anytime(true) runs
	accept_best_known(satisfiable, Model, Cost, Anytime, Status, Model1, Cost1) :-
		!,
		(	Anytime == true ->
			Status = satisfiable, Model1 = Model, Cost1 = Cost
		;	Status = unknown, Model1 = none, Cost1 = none
		).
	accept_best_known(Status, Model, Cost, _, Status, Model, Cost).

	% RC2 and Open-WBO: MaxSAT Evaluation style "o" (cost), "s" (status), and "v" (witness) lines

	mse_result(Solver, Lines, Variables, Status, Model, Cost) :-
		(	member([0's, 0' | Rest], Lines) ->
			atom_codes(Label, Rest),
			(	mse_status(Label, Status) ->
				true
			;	domain_error(max_sat_output, 'Result'-Label)
			)
		;	Solver == rc2,
			member(Line, Lines),
			unfinished_line(Line) ->
			% RC2 prints no status line when its time limit is hit before an optimum is found
			Status = unknown
		;	domain_error(max_sat_output, 'Result'-missing)
		),
		last_cost(Lines, Cost),
		(	( 	Status == optimal
			;	Status == satisfiable
			),
			witness_literals(Lines, Numbers) ->
			parse_literals(Numbers, Variables, Literals),
			Model = assignment(Literals)
		;	Model = none
		).

	% when interrupted, RC2 prints either "c interrupted; processing the last found core" or
	% nothing special; in both cases its last line is "c oracle time: ..." and there is no status line
	unfinished_line(Line) :-
		(	atom_codes('c interrupted', Prefix)
		;	atom_codes('c oracle time', Prefix)
		),
		append(Prefix, _, Line),
		!.

	mse_status('OPTIMUM FOUND', optimal).
	mse_status('SATISFIABLE', satisfiable).
	mse_status('UNSATISFIABLE', unsatisfiable).
	mse_status('UNKNOWN', unknown).

	last_cost(Lines, Cost) :-
		findall(Value, (member([0'o, 0' | Rest], Lines), integer_line(Rest, Value)), Values),
		(	reverse(Values, [Cost| _]) ->
			true
		;	Cost = none
		).

	integer_line(Codes, Value) :-
		trim(Codes, Trimmed),
		Trimmed \== [],
		all_digits(Trimmed),
		number_codes(Value, Trimmed).

	all_digits([]).
	all_digits([Code| Codes]) :-
		Code >= 0'0, Code =< 0'9,
		all_digits(Codes).

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

	% the zero terminator is optional (Open-WBO does not print it)
	take_until_zero([], []).
	take_until_zero([0| _], []) :-
		!.
	take_until_zero([Number| Tokens], [Number| Numbers]) :-
		take_until_zero(Tokens, Numbers).

	parse_literals([], _, []).
	parse_literals([Number| Numbers], Variables, [Literal| Literals]) :-
		(	Number > 0 ->
			literal_term(Number, Variables, Term),
			Literal = +Term
		;	Magnitude is -Number,
			literal_term(Magnitude, Variables, Term),
			Literal = -Term
		),
		parse_literals(Numbers, Variables, Literals).

	literal_term(Number, none, Number) :-
		!.
	literal_term(Number, Variables, Term) :-
		(	member(Term-Number, Variables) ->
			true
		;	domain_error(max_sat_output, variable(Number))
		).

	% Z3: "sat"/"unsat"/"unknown"/"timeout" status line, "(define-fun k!N () Bool" models
	% (value on the next line), and the objective value as a lone integer line

	z3_result(Lines, Variables, Status, Model, Cost) :-
		(	member(Line, Lines),
			trim(Line, Trimmed),
			z3_status(Trimmed, Status0) ->
			Status = Status0
		;	domain_error(max_sat_output, 'Result'-missing)
		),
		(	Status == optimal ->
			z3_assignments(Lines, Pairs),
			z3_complete(Variables, Pairs, Literals),
			Model = assignment(Literals),
			z3_cost(Lines, Cost)
		;	Model = none,
			Cost = none
		).

	z3_status(Codes, Status) :-
		atom_codes(Label, Codes),
		z3_label(Label, Status).

	z3_label(sat, optimal).
	z3_label(unsat, unsatisfiable).
	z3_label(unknown, unknown).
	z3_label(timeout, unknown).

	z3_assignments([], []).
	z3_assignments([Line| Lines], Pairs) :-
		trim(Line, Trimmed),
		(	atom_codes('(define-fun k!', Prefix),
			append(Prefix, Name, Trimmed),
			z3_name(Name, Number),
			Lines = [Next| Rest],
			trim(Next, NextTrimmed),
			z3_value(NextTrimmed, Value) ->
			Pairs = [Number-Value| Pairs1],
			z3_assignments(Rest, Pairs1)
		;	z3_assignments(Lines, Pairs)
		).

	% the name is followed by " () Bool"
	z3_name(Codes, Number) :-
		append(DigitCodes, [0' | _], Codes),
		DigitCodes \== [],
		all_digits(DigitCodes),
		!,
		number_codes(Number, DigitCodes).

	z3_value(Codes, Value) :-
		atom_codes(Atom, Codes),
		(	Atom == 'true)' ->
			Value = true
		;	Atom == 'false)',
			Value = false
		).

	% variables missing from the model (e.g., eliminated by simplification) are reported as false
	z3_complete(none, Pairs, Literals) :-
		!,
		pairs_literals(Pairs, Literals).
	z3_complete([], _, []).
	z3_complete([Term-Number| Variables], Pairs, [Literal| Literals]) :-
		(	memberchk(Number-true, Pairs) ->
			Literal = +Term
		;	Literal = -Term
		),
		z3_complete(Variables, Pairs, Literals).

	pairs_literals([], []).
	pairs_literals([Number-true| Pairs], [+Number| Literals]) :-
		!,
		pairs_literals(Pairs, Literals).
	pairs_literals([Number-false| Pairs], [-Number| Literals]) :-
		pairs_literals(Pairs, Literals).

	z3_cost(Lines, Cost) :-
		findall(Value, (member(Line, Lines), integer_line_only(Line, Value)), Values),
		(	reverse(Values, [Cost| _]) ->
			true
		;	Cost = none
		).

	integer_line_only(Line, Value) :-
		trim(Line, Trimmed),
		Trimmed \== [],
		all_digits(Trimmed),
		number_codes(Value, Trimmed).

	trim(Codes0, Codes) :-
		drop_blanks(Codes0, Codes1),
		reverse(Codes1, Reversed0),
		drop_blanks(Reversed0, Reversed),
		reverse(Reversed, Codes).

	drop_blanks([Code| Codes0], Codes) :-
		blank_code(Code),
		!,
		drop_blanks(Codes0, Codes).
	drop_blanks(Codes, Codes).

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

	variable_count(Variables, Model, Count) :-
		(	Variables \== none ->
			length(Variables, Count)
		;	Model = assignment(Literals) ->
			length(Literals, Count)
		;	Count = unknown
		).

	% Open-WBO uses the SAT competition exit codes extended with 30 (optimum found) and
	% 40 (best-known solution of an interrupted run); RC2 always exits with 0 and Z3 exits with 0
	% except after reporting an unsatisfiable problem with the -model option (110)
	% some backends (e.g., GNU Prolog) cannot normalize nonzero exit codes, reporting them as
	% unknown(Code); such a code is accepted as an exit code when valid for the solver and
	% consistent with the decoded status, and is otherwise interpreted as an interrupted process
	validate_status(Solver, unknown(Code), Status, Termination) :-
		integer(Code),
		exit_code_status(Solver, Code, Status),
		!,
		termination(Status, Termination).
	validate_status(_, unknown(_), _, interrupted) :-
		!.
	validate_status(_, killed(_), _, interrupted) :-
		!.
	validate_status(Solver, exit(Code), Status, Termination) :-
		integer(Code),
		!,
		(	exit_code_status(Solver, Code, Status) ->
			termination(Status, Termination)
		;	domain_error(max_sat_process_status, exit(Code))
		).
	validate_status(_, NormalizedStatus, _, _) :-
		domain_error(max_sat_process_status, NormalizedStatus).

	exit_code_status(open_wbo, 30, optimal).
	exit_code_status(open_wbo, 20, unsatisfiable).
	exit_code_status(open_wbo, 40, unknown).
	exit_code_status(open_wbo, 40, satisfiable).
	exit_code_status(open_wbo, 0, unknown).
	exit_code_status(rc2, 0, _).
	exit_code_status(z3, 0, _).
	% Z3 exits with 110 after reporting "unsat" when asked to print a model (-model)
	exit_code_status(z3, 110, unsatisfiable).

	termination(optimal, completed).
	termination(unsatisfiable, completed).
	termination(satisfiable, resource_limit).
	termination(unknown, resource_limit).

:- end_object.

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


:- object(smt_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Whole-document S-expression tokenizing, parsing, and content-based response classification for Z3, cvc5, Bitwuzla, and Yices 2 plain-text output decoding.'
	]).

	:- public(decode/7).
	:- mode(decode(+atom, +list(code), +term, +compound, +list(code), +list(compound), -list(compound)), one_or_error).
	:- info(decode/7, [
		comment is 'Decodes captured stdout and process metadata into a list of collected ``smt_result(Status, Model, Properties)`` terms, one per ``check-sat``/``check-sat-assuming`` response, in order. Each response is classified by its shape (status atom, model, unsat core, or get-value pairs), so the decoder does not need the originating statement list and supports every source kind.',
		argnames is ['Solver', 'Codes', 'RawStatus', 'NormalizedStatus', 'Diagnostics', 'SourceMap', 'Results'],
		exceptions is [
			'``Solver`` is not ground' - instantiation_error,
			'``Solver`` is neither a variable nor an atom' - type_error(atom, 'Solver'),
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is neither a partial list nor a list of character codes' - type_error(codes, 'Codes'),
			'``Codes`` contains an unrecognized character or an unbalanced parenthesis' - domain_error(smt_output, 'Field'-'Value'),
			'A model, unsat core, or get-value response appears before any status response' - domain_error(smt_output, 'Response'-unexpected),
			'The solver reported an ``(error ...)`` response' - smt_solver_reported_error('Message'),
			'``RawStatus`` is not ground' - instantiation_error,
			'``NormalizedStatus`` is not ground' - instantiation_error,
			'``NormalizedStatus`` is neither a variable nor one of ``exit(Code)``, ``unknown(Details)``, nor ``killed(Details)``' - domain_error(smt_process_status, 'NormalizedStatus'),
			'No response was parsed and the process did not exit cleanly' - domain_error(smt_process_status, 'NormalizedStatus'),
			'``Diagnostics`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Diagnostics`` is neither a partial list nor a list of character codes' - type_error(codes, 'Diagnostics'),
			'``SourceMap`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``SourceMap`` is neither a variable nor a list' - type_error(list, 'SourceMap')
		]
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(list, [
		append/3, length/2, valid/1 as proper_list/1
	]).

	decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Results) :-
		context(Context),
		check(ground, Solver-Codes-RawStatus-NormalizedStatus-Diagnostics-SourceMap, Context),
		check(atom, Solver, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		check(list, SourceMap, Context),
		validate_status(NormalizedStatus, Termination),
		tokenize(Codes, Tokens),
		parse_sexprs(Tokens, Sexprs),
		walk(Sexprs, none, Episodes),
		build_results(Episodes, Solver, RawStatus, NormalizedStatus, Termination, Diagnostics, SourceMap, Codes, Results0),
		% A nonzero exit or an interrupted process with no parsed results means the
		% solver aborted before answering (verified against cvc5 1.3.0, which can
		% SIGABRT on a tight --tlimit instead of reporting "unknown"), not that the
		% script legitimately had no check-sat command.
		(	Results0 == [], NormalizedStatus \== exit(0) ->
			domain_error(smt_process_status, NormalizedStatus)
		;	Results = Results0
		).

	% Process status validation: the authoritative signal is the parsed stdout
	% content (per check-sat/get-* response), not the exit code, since exit-code
	% conventions in SMT-LIB 2 batch mode vary and are not fully verified across
	% all four backends; only a killed or indeterminate raw status is fatal.
	validate_status(exit(Code), completed) :-
		integer(Code),
		!.
	validate_status(unknown(_), interrupted) :-
		!.
	validate_status(killed(_), interrupted) :-
		!.
	validate_status(NormalizedStatus, _) :-
		domain_error(smt_process_status, NormalizedStatus).

	% Content-based response classification and episode accumulation. Each
	% top-level response is classified by its own shape (a status atom, an
	% "(error ...)" pair, a model, an unsat core, or get-value pairs), so no
	% positional correlation against the originating statements is required.

	walk([], CurrentEpisode, Episodes) :-
		!,
		flush_episode(CurrentEpisode, Episodes, []).
	walk([Sexpr| Sexprs], CurrentEpisode, Episodes) :-
		classify(Sexpr, Class),
		apply_class(Class, CurrentEpisode, UpdatedEpisode, Episodes, Episodes1),
		walk(Sexprs, UpdatedEpisode, Episodes1).

	apply_class(status(Status), CurrentEpisode, NewEpisode, Episodes, Episodes1) :-
		!,
		flush_episode(CurrentEpisode, Episodes, Episodes1),
		NewEpisode = episode(Status, [], none, []).
	apply_class(error(Message), _, _, _, _) :-
		!,
		context(Context),
		throw(error(smt_solver_reported_error(Message), Context)).
	apply_class(model(Bindings), CurrentEpisode, UpdatedEpisode, Episodes, Episodes) :-
		!,
		require_episode(CurrentEpisode),
		CurrentEpisode = episode(Status, _, Core, Values),
		UpdatedEpisode = episode(Status, Bindings, Core, Values).
	apply_class(core(Names), CurrentEpisode, UpdatedEpisode, Episodes, Episodes) :-
		!,
		require_episode(CurrentEpisode),
		CurrentEpisode = episode(Status, Bindings, _, Values),
		UpdatedEpisode = episode(Status, Bindings, Names, Values).
	apply_class(values(NewValues), CurrentEpisode, UpdatedEpisode, Episodes, Episodes) :-
		!,
		require_episode(CurrentEpisode),
		CurrentEpisode = episode(Status, Bindings, Core, OldValues),
		append(OldValues, NewValues, AllValues),
		UpdatedEpisode = episode(Status, Bindings, Core, AllValues).
	apply_class(unknown, CurrentEpisode, CurrentEpisode, Episodes, Episodes).

	require_episode(none) :-
		domain_error(smt_output, 'Response'-unexpected).
	require_episode(_).

	flush_episode(none, Episodes, Episodes) :-
		!.
	flush_episode(Episode, [Episode| Episodes], Episodes).

	classify(sat, status(satisfiable)) :-
		!.
	classify(unsat, status(unsatisfiable)) :-
		!.
	classify(unknown, status(unknown)) :-
		!.
	% Z3 5.3.3 reports a resource-limit result as the bare word "timeout" rather
	% than the standard "unknown" status token in plain SMT-LIB 2 mode.
	classify(timeout, status(unknown)) :-
		!.
	classify([error, string(Message)], error(Message)) :-
		!.
	classify([error, Message], error(Message)) :-
		atom(Message),
		!.
	classify([model| Defs], model(Bindings)) :-
		!,
		definitions_bindings(Defs, Bindings).
	classify(Sexpr, model(Bindings)) :-
		proper_list(Sexpr), Sexpr \== [],
		all_definitions(Sexpr),
		!,
		definitions_bindings(Sexpr, Bindings).
	classify(Sexpr, values(Values)) :-
		proper_list(Sexpr), Sexpr \== [],
		all_pairs(Sexpr),
		!,
		pairs_values(Sexpr, Values).
	classify([], core([])) :-
		!.
	classify(Sexpr, core(Sexpr)) :-
		proper_list(Sexpr), Sexpr \== [],
		all_atoms(Sexpr),
		!.
	classify(_, unknown).

	all_definitions([]).
	all_definitions([[Kind, Name| _]| Defs]) :-
		atom(Name),
		(	Kind == 'define-fun'
		;	Kind == 'define-fun-rec'
		),
		!,
		all_definitions(Defs).

	all_pairs([]).
	all_pairs([[_, _]| Pairs]) :-
		!,
		all_pairs(Pairs).

	all_atoms([]).
	all_atoms([Atom| Atoms]) :-
		atom(Atom),
		!,
		all_atoms(Atoms).

	build_results([], _, _, _, _, _, _, _, []).
	build_results([episode(Status, Bindings, Core, Values)| Episodes], Solver, RawStatus, NormalizedStatus, Termination, Diagnostics, SourceMap, Codes, [Result| Results]) :-
		(	Bindings == [] ->
			Model = none
		;	Model = bindings(Bindings)
		),
		Properties = [
			solver(Solver), raw_status(RawStatus), process_status(NormalizedStatus),
			termination(Termination), diagnostics(Diagnostics), source_map(SourceMap),
			raw_output(Codes), core(Core), values(Values)
		],
		Result = smt_result(Status, Model, Properties),
		build_results(Episodes, Solver, RawStatus, NormalizedStatus, Termination, Diagnostics, SourceMap, Codes, Results).

	% Response interpretation: model and get-value responses.

	definitions_bindings([], []).
	definitions_bindings([[Kind, Name, _Params, _Sort, ValueSexpr]| Defs], [binding(Name, Value)| Bindings]) :-
		(	Kind == 'define-fun'
		;	Kind == 'define-fun-rec'
		),
		!,
		sexpr_to_term(ValueSexpr, Value),
		definitions_bindings(Defs, Bindings).
	definitions_bindings([_| Defs], Bindings) :-
		definitions_bindings(Defs, Bindings).

	pairs_values([], []).
	pairs_values([[TermSexpr, ValueSexpr]| Pairs], [value(Term, Value)| Values]) :-
		!,
		sexpr_to_term(TermSexpr, Term),
		sexpr_to_term(ValueSexpr, Value),
		pairs_values(Pairs, Values).
	pairs_values([_| Pairs], Values) :-
		pairs_values(Pairs, Values).

	% Generic parsed S-expression to Logtalk term conversion. Deeply nested
	% qualified or indexed identifiers that are not bit-vector literals fall
	% back to an ``apply(Head, Args)`` wrapper rather than failing.
	sexpr_to_term([], smt_nil) :-
		!.
	sexpr_to_term(['_', Op, N], bv(Value, Width)) :-
		atom(Op),
		integer(N),
		atom_concat(bv, NumAtom, Op),
		catch((atom_codes(NumAtom, NumCodes), number_codes(Value, NumCodes)), _, fail),
		!,
		Width = N.
	sexpr_to_term(['-', N], Negative) :-
		number(N),
		!,
		Negative is -N.
	sexpr_to_term([Head| Args], Term) :-
		!,
		sexpr_to_terms(Args, ConvertedArgs),
		(	atom(Head) ->
			(	ConvertedArgs == [] ->
				Term = Head
			;	Term =.. [Head| ConvertedArgs]
			)
		;	sexpr_to_term(Head, HeadTerm),
			Term = apply(HeadTerm, ConvertedArgs)
		).
	sexpr_to_term(Leaf, Leaf).

	sexpr_to_terms([], []).
	sexpr_to_terms([Sexpr| Sexprs], [Term| Terms]) :-
		sexpr_to_term(Sexpr, Term),
		sexpr_to_terms(Sexprs, Terms).

	% S-expression parsing: flat token list to nested-list S-expression trees.

	parse_sexprs([], []) :-
		!.
	parse_sexprs(Tokens, [Sexpr| Sexprs]) :-
		parse_one(Tokens, Sexpr, Rest),
		parse_sexprs(Rest, Sexprs).

	parse_one([open| Tokens], List, Rest) :-
		!,
		parse_list(Tokens, List, Rest).
	parse_one([close| _], _, _) :-
		domain_error(smt_output, unexpected_close_paren).
	parse_one([Leaf| Tokens], Leaf, Tokens).

	parse_list([close| Tokens], [], Tokens) :-
		!.
	parse_list([], _, _) :-
		domain_error(smt_output, unterminated_list).
	parse_list(Tokens, [Sexpr| Sexprs], Rest) :-
		parse_one(Tokens, Sexpr, Tokens1),
		parse_list(Tokens1, Sexprs, Rest).

	% Tokenizing: character codes to a flat token list.

	tokenize(Codes, Tokens) :-
		skip_ignorable(Codes, Rest),
		(	Rest == [] ->
			Tokens = []
		;	next_token(Rest, Token, Rest1),
			Tokens = [Token| Tokens1],
			tokenize(Rest1, Tokens1)
		).

	skip_ignorable([Code| Codes], Rest) :-
		blank_code(Code),
		!,
		skip_ignorable(Codes, Rest).
	skip_ignorable([0';| Codes], Rest) :-
		!,
		skip_line_comment(Codes, Rest1),
		skip_ignorable(Rest1, Rest).
	skip_ignorable(Codes, Codes).

	blank_code(32).
	blank_code(9).
	blank_code(10).
	blank_code(13).

	skip_line_comment([], []) :-
		!.
	skip_line_comment([10| Codes], Codes) :-
		!.
	skip_line_comment([_| Codes], Rest) :-
		skip_line_comment(Codes, Rest).

	next_token([0'(| Codes], open, Codes) :-
		!.
	next_token([0')| Codes], close, Codes) :-
		!.
	next_token([0'"| Codes], string(Atom), Rest) :-
		!,
		scan_string(Codes, Chars, Rest),
		atom_codes(Atom, Chars).
	next_token([0'|| Codes], Atom, Rest) :-
		!,
		scan_quoted_symbol(Codes, Chars, Rest),
		atom_codes(Atom, Chars).
	next_token([0'#, 0'x| Codes], bv(Value, Width), Rest) :-
		!,
		scan_hex(Codes, HexCodes, Rest),
		length(HexCodes, NumDigits),
		Width is NumDigits * 4,
		hex_value(HexCodes, Value).
	next_token([0'#, 0'b| Codes], bv(Value, Width), Rest) :-
		!,
		scan_bin(Codes, BinCodes, Rest),
		length(BinCodes, Width),
		bin_value(BinCodes, Value).
	next_token(Codes, Number, Rest) :-
		scan_number(Codes, NumberCodes, Rest),
		NumberCodes \== [],
		!,
		number_codes(Number, NumberCodes).
	next_token(Codes, Atom, Rest) :-
		scan_symbol(Codes, SymbolCodes, Rest),
		SymbolCodes \== [],
		!,
		atom_codes(Atom, SymbolCodes).
	next_token([Code| _], _, _) :-
		domain_error(smt_output, character(Code)).

	scan_string([0'", 0'"| Codes], [0'"| Chars], Rest) :-
		!,
		scan_string(Codes, Chars, Rest).
	scan_string([0'"| Codes], [], Codes) :-
		!.
	scan_string([], [], []) :-
		!.
	scan_string([Code| Codes], [Code| Chars], Rest) :-
		scan_string(Codes, Chars, Rest).

	scan_quoted_symbol([0'|| Codes], [], Codes) :-
		!.
	scan_quoted_symbol([], [], []) :-
		!.
	scan_quoted_symbol([Code| Codes], [Code| Chars], Rest) :-
		scan_quoted_symbol(Codes, Chars, Rest).

	scan_hex([Code| Codes], [Code| Hex], Rest) :-
		is_hex_digit(Code),
		!,
		scan_hex(Codes, Hex, Rest).
	scan_hex(Codes, [], Codes).

	is_hex_digit(Code) :-
		(	Code >= 0'0, Code =< 0'9
		;	Code >= 0'a, Code =< 0'f
		;	Code >= 0'A, Code =< 0'F
		),
		!.

	hex_value(Codes, Value) :-
		hex_value(Codes, 0, Value).

	hex_value([], Value, Value).
	hex_value([Code| Codes], Accumulator, Value) :-
		hex_digit_value(Code, Digit),
		Accumulator1 is Accumulator * 16 + Digit,
		hex_value(Codes, Accumulator1, Value).

	hex_digit_value(Code, Digit) :-
		Code >= 0'0, Code =< 0'9,
		!,
		Digit is Code - 0'0.
	hex_digit_value(Code, Digit) :-
		Code >= 0'a, Code =< 0'f,
		!,
		Digit is Code - 0'a + 10.
	hex_digit_value(Code, Digit) :-
		Digit is Code - 0'A + 10.

	scan_bin([Code| Codes], [Code| Bin], Rest) :-
		is_bin_digit(Code),
		!,
		scan_bin(Codes, Bin, Rest).
	scan_bin(Codes, [], Codes).

	is_bin_digit(0'0) :-
		!.
	is_bin_digit(0'1).

	bin_value(Codes, Value) :-
		bin_value(Codes, 0, Value).

	bin_value([], Value, Value).
	bin_value([Code| Codes], Accumulator, Value) :-
		(	Code =:= 0'0 ->
			Digit = 0
		;	Digit = 1
		),
		Accumulator1 is Accumulator * 2 + Digit,
		bin_value(Codes, Accumulator1, Value).

	scan_number(Codes, NumberCodes, Rest) :-
		scan_digits(Codes, IntDigits, Rest1),
		IntDigits \== [],
		(	Rest1 = [0'.| Rest2],
			scan_digits(Rest2, FracDigits, Rest3),
			FracDigits \== [] ->
			append(IntDigits, [0'.| FracDigits], NumberCodes),
			Rest = Rest3
		;	NumberCodes = IntDigits,
			Rest = Rest1
		).

	scan_digits([Code| Codes], [Code| Digits], Rest) :-
		Code >= 0'0, Code =< 0'9,
		!,
		scan_digits(Codes, Digits, Rest).
	scan_digits(Codes, [], Codes).

	scan_symbol([Code| Codes], [Code| Chars], Rest) :-
		is_symbol_char(Code),
		!,
		scan_symbol(Codes, Chars, Rest).
	scan_symbol(Codes, [], Codes).

	is_symbol_char(Code) :-
		\+ blank_code(Code),
		Code \== 0'(,
		Code \== 0'),
		Code \== 0'",
		Code \== 0'|,
		Code \== 0';.

:- end_object.

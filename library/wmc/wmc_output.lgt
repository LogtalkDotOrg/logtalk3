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


:- object(wmc_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-24,
		comment is 'Whole-document Ganak and SharpSAT-TD plain-text model counting output decoding using the Model Counting Competition output format.'
	]).

	:- public(decode/8).
	:- mode(decode(+atom, +list(code), ++ground, ++ground, +list(code), +list(compound), +term, -compound), one_or_error).
	:- info(decode/8, [
		comment is 'Decodes captured stdout and process metadata into a collected ``wmc_result(Status, Value, Properties)`` term. ``Status`` is one of ``counted``, ``unsatisfiable`` (the count is zero), and ``unknown`` (no count, e.g., after a time limit or an interrupted process). ``Value`` is a number (an integer for unweighted counts, otherwise a float) or ``none``.',
		argnames is ['Solver', 'Codes', 'RawStatus', 'NormalizedStatus', 'Diagnostics', 'SourceMap', 'Variables', 'Result'],
		exceptions is [
			'``Solver`` is a variable' - instantiation_error,
			'``Solver`` is neither a variable nor an atom' - type_error(atom, 'Solver'),
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is neither a partial list nor a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Element`` of the ``Codes`` list is neither a variable nor an integer' - type_error(integer, 'Element'),
			'An element ``Element`` of the ``Codes`` list is an integer but not a valid character code' - domain_error(character_code, 'Element'),
			'``Codes`` has no status line, no ``c s exact`` result line, or an invalid value' - domain_error(wmc_output, 'Field'-'Value'),
			'``RawStatus`` is not a ground term' - instantiation_error,
			'``NormalizedStatus`` is not a ground term' - instantiation_error,
			'``NormalizedStatus`` is a ground term bit not a valid status' - domain_error(wmc_process_status, 'NormalizedStatus'),
			'``Diagnostics`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Diagnostics`` is neither a partial list nor a list of character codes' - type_error(codes, 'Diagnostics'),
			'An element ``Element`` of the ``Diagnostics`` list is neither a variable nor an integer' - type_error(integer, 'Element'),
			'An element ``Element`` of the ``Diagnostics`` list is an integer but not a valid character code' - domain_error(character_code, 'Element'),
			'``SourceMap`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``SourceMap`` is neither a partial list nor a list of ground compound terms' - type_error(list(ground(compound)), 'SourceMap'),
			'An element ``Element`` of the ``SourceMap`` list is neither a variable nor a compound' - type_error(compound, 'Element'),
			'``Variables`` is a not a ground term' - instantiation_error,
			'``Variables`` is ground but neither ``none`` nor a list of ground pairs' - type_error(list(pair(ground,ground)), 'Variables')
		]
	]).

	:- public(parse_number/2).
	:- mode(parse_number(+list(code), -number), zero_or_one).
	:- info(parse_number/2, [
		comment is 'Parses a decimal number (optionally signed, with optional fraction and exponent, e.g., ``0.41``, ``4``, ``4.1e-01``, ``1e-5``) from character codes. Fails if the codes are not a number.',
		argnames is ['Codes', 'Number']
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		member/2, append/3, length/2, reverse/2
	]).

	decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Variables, Result) :-
		context(Context),
		check(atom, Solver, Context),
		check(codes, Codes, Context),
		check(ground, RawStatus, Context),
		check(ground, NormalizedStatus, Context),
		check(codes, Diagnostics, Context),
		check(list(ground(compound)), SourceMap, Context),
		(	Variables == none ->
			true
		;	check(list(pair(ground,ground)), Variables, Context)
		),
		(	time_limit_status(NormalizedStatus) ->
			% the time limit wrapper (the timeout command) stops the counter without any result
			Status = unknown, Value = none, Termination = resource_limit, Extra = []
		;	validate_status(NormalizedStatus, Termination0),
			split_lines(Codes, Lines),
			counter_result(Lines, Status, Value, Extra),
			Termination = Termination0
		),
		variable_count(Variables, VariableCount),
		value_properties(Value, ValueProperties),
		append(ValueProperties, Extra, Properties0),
		append(Properties0, [
			solver(Solver), raw_status(RawStatus), process_status(NormalizedStatus),
			termination(Termination), variables(Variables), variable_count(VariableCount),
			diagnostics(Diagnostics), source_map(SourceMap), raw_output(Codes)
		], Properties),
		Result = wmc_result(Status, Value, Properties).

	value_properties(none, []) :-
		!.
	value_properties(Value, [number_type(Type)]) :-
		(	integer(Value) ->
			Type = integer
		;	Type = float
		).

	% exit codes of the timeout command: 124 (time limit reached) and 137 (killed after the grace period)
	time_limit_status(exit(124)).
	time_limit_status(exit(137)).
	time_limit_status(unknown(124)).
	time_limit_status(unknown(137)).

	validate_status(exit(0), completed) :-
		!.
	validate_status(unknown(_), interrupted) :-
		!.
	validate_status(killed(_), interrupted) :-
		!.
	validate_status(NormalizedStatus, _) :-
		domain_error(wmc_process_status, NormalizedStatus).

	% "s SATISFIABLE" / "s UNSATISFIABLE", "c s type mc|wmc", "c s log10-estimate X",
	% and "c s exact PRECISION KIND VALUE" (KIND is int, float, or frac) lines

	counter_result(Lines, Status, Value, Extra) :-
		(	member([0's, 0' | Rest], Lines) ->
			atom_codes(Label, Rest),
			(	status_label(Label, Status0) ->
				true
			;	domain_error(wmc_output, 'Result'-Label)
			)
		;	domain_error(wmc_output, 'Result'-missing)
		),
		(	Status0 == unknown ->
			Status = unknown, Value = none, Extra = []
		;	exact_line(Lines, Precision, Kind, Text) ->
			parse_value(Kind, Text, Value, ValueExtra),
			Status = Status0,
			log10_estimate(Lines, LogExtra),
			append([representation(Precision-Kind), value_text(Text)| ValueExtra], LogExtra, Extra)
		;	domain_error(wmc_output, 'Result'-no_count)
		).

	status_label('SATISFIABLE', counted).
	status_label('UNSATISFIABLE', unsatisfiable).
	status_label('UNKNOWN', unknown).

	exact_line(Lines, Precision, Kind, Text) :-
		atom_codes('c s exact ', Prefix),
		member(Line, Lines),
		append(Prefix, Rest, Line),
		!,
		split_words(Rest, [PrecisionCodes, KindCodes, TextCodes]),
		atom_codes(Precision, PrecisionCodes),
		atom_codes(Kind, KindCodes),
		atom_codes(Text, TextCodes).

	log10_estimate(Lines, Extra) :-
		atom_codes('c s log10-estimate ', Prefix),
		(	member(Line, Lines),
			append(Prefix, Rest, Line),
			parse_number(Rest, Estimate) ->
			Extra = [log10_estimate(Estimate)]
		;	Extra = []
		).

	parse_value(int, Text, Value, []) :-
		!,
		atom_codes(Text, Codes),
		(	parse_integer(Codes, Value0) ->
			Value = Value0
		;	parse_big_integer(Codes, Value0) ->
			% integers beyond the range of backends with bounded integers are approximated
			Value = Value0
		;	domain_error(wmc_output, value-Text)
		).
	parse_value(float, Text, Value, []) :-
		!,
		atom_codes(Text, Codes),
		(	parse_number(Codes, Number) ->
			Value = Number
		;	domain_error(wmc_output, value-Text)
		).
	parse_value(frac, Text, Value, Extra) :-
		!,
		atom_codes(Text, Codes),
		(	append(NumeratorCodes, [0'/| DenominatorCodes], Codes) ->
			(	parse_integer(NumeratorCodes, Numerator),
				parse_integer(DenominatorCodes, Denominator),
				Denominator > 0 ->
				% fractions are always reported as floats; the exact fraction is kept as a property
				Value is Numerator * 1.0 / Denominator,
				Extra = [exact_fraction(Numerator/Denominator)]
			;	digits_float(NumeratorCodes, NumeratorFloat),
				digits_float(DenominatorCodes, DenominatorFloat),
				DenominatorFloat > 0 ->
				% terms too large for backends with bounded integers
				Value is NumeratorFloat / DenominatorFloat,
				Extra = []
			;	domain_error(wmc_output, value-Text)
			)
		;	parse_integer(Codes, Numerator) ->
			Value is Numerator * 1.0,
			Extra = []
		;	digits_float(Codes, Value) ->
			Extra = []
		;	domain_error(wmc_output, value-Text)
		).
	parse_value(Kind, _, _, _) :-
		domain_error(wmc_output, kind-Kind).

	digits_float(Digits, Value) :-
		Digits \== [],
		all_digits(Digits),
		append(Digits, [0'., 0'0], FloatCodes),
		number_codes(Value, FloatCodes).

	parse_big_integer(Digits, Value) :-
		Digits \== [],
		all_digits(Digits),
		append(Digits, [0'., 0'0], FloatCodes),
		number_codes(Value, FloatCodes).

	parse_integer([0'-| Digits], Value) :-
		!,
		Digits \== [],
		all_digits(Digits),
		catch(number_codes(Magnitude, Digits), _, fail),
		Value is -Magnitude.
	parse_integer(Digits, Value) :-
		Digits \== [],
		all_digits(Digits),
		catch(number_codes(Value, Digits), _, fail).

	all_digits([]).
	all_digits([Code| Codes]) :-
		0'0 =< Code, Code =< 0'9,
		all_digits(Codes).

	% decimal numbers with optional fraction and exponent (e.g., 0.41, 4, 4.1e-01, 1e-5)
	parse_number(Codes0, Number) :-
		trim(Codes0, Codes),
		(	Codes = [0'-| Rest] -> Sign = -1, Unsigned = Rest
		;	Codes = [0'+| Rest] -> Sign = 1, Unsigned = Rest
		;	Sign = 1, Unsigned = Codes
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
		(	FractionPart == [], ExponentCodes == [] ->
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
			( E =:= 0'e ; E =:= 0'E ) ->
			Mantissa = Mantissa0,
			(	Rest = [0'-| Digits] ->
				Exponent = exponent([0'-], Digits)
			;	Rest = [0'+| Digits] ->
				Exponent = exponent([], Digits)
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

	split_words(Codes, Words) :-
		drop_blanks(Codes, Trimmed),
		(	Trimmed == [] ->
			Words = []
		;	take_word(Trimmed, Word, Rest),
			Words = [Word| Words1],
			split_words(Rest, Words1)
		).

	take_word([], [], []).
	take_word([Code| Codes], Word, Rest) :-
		(	blank_code(Code) ->
			Word = [],
			Rest = Codes
		;	Word = [Code| Word1],
			take_word(Codes, Word1, Rest)
		).

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

	blank_code(9).
	blank_code(13).
	blank_code(32).

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

	variable_count(Variables, Count) :-
		(	Variables \== none ->
			length(Variables, Count)
		;	Count = unknown
		).

:- end_object.

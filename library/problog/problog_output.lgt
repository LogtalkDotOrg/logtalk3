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


:- object(problog_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-12,
		comment is 'Strict data-only decoding of ProbLog --format prolog query probabilities.'
	]).

	:- public(decode/6).
	:- mode(decode(+list(integer), +list(integer), +term, +compound, +list(compound), -compound), one_or_error).
	:- info(decode/6, [
		comment is 'Decodes a complete output document and process metadata. Nonzero status and malformed output raise problog_solver_error/4 retaining both output streams.',
		argnames is ['Codes', 'Diagnostics', 'RawStatus', 'NormalizedStatus', 'SourceMap', 'Result']
	]).

	:- public(valid_result/1).
	:- mode(valid_result(@term), zero_or_one).
	:- info(valid_result/1, [
		comment is 'True for a ground, structurally valid collected inference result.',
		argnames is ['Result']
	]).

	:- uses(list, [
		append/2, append/3
	]).

	:- uses(term_io, [
		read_term_from_codes/4
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	decode(Codes, Diagnostics, RawStatus, Status, SourceMap, Result) :-
		context(Context),
		check(ground, Codes-Diagnostics-RawStatus-Status-SourceMap, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		check(list, SourceMap, Context),
		(	Status == exit(0) ->
			true
		;	throw(error(problog_solver_error(process_status(Status, stdout(Codes)), Status, Diagnostics, SourceMap), Context))
		),
		catch(
			(	normalize_exponents(Codes, Normalized),
				decode_records(Normalized, Probabilities)
			),
			error(Formal, _),
			throw(error(problog_solver_error(output(Formal, stdout(Codes)), Status, Diagnostics, SourceMap), Context))
		),
		Result = problog_result(Probabilities, [
			raw_status(RawStatus), process_status(Status), diagnostics(Diagnostics)
		]).

	decode_records(Codes, Probabilities) :-
		skip_space(Codes, Rest),
		(	Rest == [] ->
			Probabilities = []
		;	read_term_from_codes(Rest, Record, Tail, []),
			(	Record = problog_result(Query, Probability),
				ground(Record),
				callable(Query),
				valid_probability(Probability) ->
				Probabilities = [probability(Query, Probability)| More],
				decode_records(Tail, More)
			;	domain_error(problog_output, Record)
			)
		).

	skip_space([Code| Codes], Rest) :-
		(Code == 32; Code == 9; Code == 10; Code == 13),
		!,
		skip_space(Codes, Rest).
	skip_space(Codes, Codes).

	% Python prints integer mantissas as 1e-09, which some Prolog readers reject.
	normalize_exponents([], []).
	normalize_exponents([Quote| Codes], [Quote| Normalized]) :-
		(Quote == 39; Quote == 34),
		!,
		quoted_codes(Codes, Quote, Normalized, Rest, Tail),
		normalize_exponents(Rest, Tail).
	normalize_exponents([Code| Codes], Normalized) :-
		digit(Code),
		!,
		digits(Codes, More, Rest),
		(	Rest = [46, Fraction| FractionCodes],
			digit(Fraction) ->
			digits(FractionCodes, FractionDigits, NumberRest),
			append([Code| More], [46,Fraction| FractionDigits], Mantissa),
			Padding = []
		;	Mantissa = [Code| More],
			NumberRest = Rest,
			Padding = [46,48]
		),
		(	NumberRest = [Exponent| ExponentCodes],
			(Exponent == 101; Exponent == 69),
			exponent(ExponentCodes, ExponentDigits, Tail) ->
			normalize_exponents(Tail, Next),
			append([Mantissa, Padding, [Exponent| ExponentDigits], Next], Normalized)
		;	append(Mantissa, Next, Normalized),
			normalize_exponents(NumberRest, Next)
		).
	normalize_exponents([Code| Codes], [Code| Normalized]) :-
		identifier_code(Code),
		!,
		identifier_codes(Codes, Normalized, Rest, Tail),
		normalize_exponents(Rest, Tail).
	normalize_exponents([Code| Codes], [Code| Normalized]) :-
		normalize_exponents(Codes, Normalized).

	quoted_codes([Quote| Codes], Quote, [Quote| Tail], Codes, Tail) :-
		!.
	quoted_codes([92, Code| Codes], Quote, [92, Code| Normalized], Rest, Tail) :-
		!,
		quoted_codes(Codes, Quote, Normalized, Rest, Tail).
	quoted_codes([Code| Codes], Quote, [Code| Normalized], Rest, Tail) :-
		quoted_codes(Codes, Quote, Normalized, Rest, Tail).
	quoted_codes([], _, [], [], []).

	digit(Code) :-
		48 =< Code, Code =< 57.

	digits([Code| Codes], [Code| Digits], Rest) :-
		digit(Code),
		!,
		digits(Codes, Digits, Rest).
	digits(Codes, [], Codes).

	exponent([Sign, Code| Codes], [Sign, Code| Digits], Rest) :-
		(Sign == 43; Sign == 45),
		digit(Code),
		!,
		digits(Codes, Digits, Rest).
	exponent([Code| Codes], [Code| Digits], Rest) :-
		digit(Code),
		digits(Codes, Digits, Rest).

	identifier_code(Code) :-
		(	65 =< Code, Code =< 90 ->
			true
		;	97 =< Code, Code =< 122 ->
			true
		;	Code == 95 ->
			true
		;	digit(Code)
		).

	identifier_codes([Code| Codes], [Code| Normalized], Rest, Tail) :-
		identifier_code(Code),
		!,
		identifier_codes(Codes, Normalized, Rest, Tail).
	identifier_codes(Codes, Tail, Codes, Tail).

	valid_probability(Probability) :-
		number(Probability),
		Probability >= 0,
		Probability =< 1.

	valid_result(Result) :-
		ground(Result),
		Result = problog_result(Probabilities, [
			raw_status(_), process_status(exit(0)), diagnostics(Diagnostics)
		]),
		valid(codes, Diagnostics),
		valid(list, Probabilities),
		valid_probabilities(Probabilities).

	valid_probabilities([]).
	valid_probabilities([probability(Query, Probability)| Probabilities]) :-
		callable(Query),
		valid_probability(Probability),
		valid_probabilities(Probabilities).

:- end_object.

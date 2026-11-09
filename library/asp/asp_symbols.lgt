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


:- object(asp_symbols).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Parser for shown Clingo ground symbols.'
	]).

	:- public(parse/2).
	:- mode(parse(+list(integer), -term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses one complete ground symbol into a canonical tagged term.',
		argnames is ['Codes', 'Symbol'],
		exceptions is [
			'``Codes`` is insufficiently instantiated' - instantiation_error,
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'``Codes`` does not represent a supported ground symbol' - domain_error(asp_symbol, 'Codes'),
			'A numeric symbol cannot be represented as an integer' - representation_error(asp_integer)
		]
	]).

	:- uses(type, [
		check/3
	]).

	parse(Codes, Symbol) :-
		context(Context),
		check(ground, Codes, Context),
		check(codes, Codes, Context),
		(	phrase(complete_symbol(Parsed), Codes) ->
			Symbol = Parsed
		;	domain_error(asp_symbol, Codes)
		).

	complete_symbol(Symbol) -->
		white_space, symbol(Symbol), white_space.

	symbol(Symbol) -->
		[Code], symbol_start(Code, Symbol).

	symbol_start(0'-, Symbol) -->
		!, negative_symbol(Symbol).
	symbol_start(0'", string(Codes)) -->
		!, string_content(Codes).
	symbol_start(0'#, Symbol) -->
		!, special_symbol(Symbol).
	symbol_start(0'(, tuple(Terms)) -->
		!, white_space, tuple_content(Terms).
	symbol_start(Code, number(Integer)) -->
		{ digit(Code) }, !,
		digits(Rest), { integer_value([Code| Rest], Integer) }.
	symbol_start(Code, function(Name, Arguments, positive)) -->
		{ identifier_start(Code) },
		identifier_tail(Rest),
		{ atom_codes(Name, [Code| Rest]), valid_identifier(Name) },
		white_space, arguments(Arguments).

	negative_symbol(number(Integer)) -->
		[Code], { digit(Code) }, !,
		digits(Rest), { integer_value([0'-, Code| Rest], Integer) }.
	negative_symbol(function(Name, Arguments, negative)) -->
		[Code], { identifier_start(Code) }, identifier_tail(Rest),
		{ atom_codes(Name, [Code| Rest]), valid_identifier(Name) },
		white_space, arguments(Arguments).

	special_symbol(infimum) -->
		[0'i, 0'n, 0'f].
	special_symbol(supremum) -->
		[0's, 0'u, 0'p].

	arguments(Arguments) -->
		[0'(], !, white_space, function_arguments(Arguments).
	arguments([]) -->
		[].

	function_arguments([]) -->
		[0')], !.
	function_arguments([Term| Terms]) -->
		symbol(Term), white_space, argument_tail(Terms).

	argument_tail([]) -->
		[0')], !.
	argument_tail([Term| Terms]) -->
		[0',], white_space, symbol(Term), white_space, argument_tail(Terms).

	tuple_content([]) -->
		[0')], !.
	tuple_content([Term| Terms]) -->
		symbol(Term), white_space, [0',], white_space, tuple_tail(Terms).

	tuple_tail([]) -->
		[0')], !.
	tuple_tail([Term| Terms]) -->
		symbol(Term), white_space, tuple_end(Terms).

	tuple_end([]) -->
		[0')], !.
	tuple_end(Terms) -->
		[0',], white_space, tuple_tail(Terms).

	string_content([]) -->
		[0'"], !.
	string_content([Code| Codes]) -->
		[0'\\], !, string_escape(Code), string_content(Codes).
	string_content([Code| Codes]) -->
		[Code], { Code >= 32, Code =\= 127 }, string_content(Codes).

	string_escape(10) -->
		[0'n].
	string_escape(0'") -->
		[0'"].
	string_escape(0'\\) -->
		[0'\\].

	white_space -->
		[Code], { whitespace_code(Code) }, !, white_space.
	white_space -->
		[].

	digits([Code| Codes]) -->
		[Code], { digit(Code) }, !, digits(Codes).
	digits([]) -->
		[].

	identifier_tail([Code| Codes]) -->
		[Code], { identifier_character(Code) }, !, identifier_tail(Codes).
	identifier_tail([]) -->
		[].

	integer_value(Codes, Integer) :-
		catch(number_codes(Number, Codes), _, representation_error(asp_integer)),
		(	integer(Number) ->
			Integer = Number
		;	representation_error(asp_integer)
		).

	valid_identifier(Name) :-
		asp_program::valid_identifier(Name).

	digit(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	identifier_start(Code) :-
		(	Code =:= 0'_ ->
			true
		;	Code >= 0'a,
			Code =< 0'z
		).

	identifier_character(Code) :-
		(	identifier_start(Code) ->
			true
		;	digit(Code) ->
			true
		;	Code >= 0'A,
			Code =< 0'Z
		).

	whitespace_code(9).
	whitespace_code(10).
	whitespace_code(13).
	whitespace_code(32).

:- end_object.

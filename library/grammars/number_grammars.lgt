%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(number_grammars(_Format_)).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2022-02-15,
		comment is 'Number grammars.',
		parnames is ['Format']
	]).

	:- public(bit//1).
	:- mode(bit(?integer), zero_or_one).
	:- info(bit//1, [
		comment is 'Parses a single bit.'
	]).

	:- public(bits//1).
	:- mode(bits(?list(integer)), zero_or_one).
	:- info(bits//1, [
		comment is 'Parses a sequence of one of more bits.'
	]).

	:- public(digit//1).
	:- mode(digit(?atomic), zero_or_one).
	:- info(digit//1, [
		comment is 'Parses a single decimal digit.'
	]).

	:- public(digits//1).
	:- mode(digits(?list(atomic)), one).
	:- info(digits//1, [
		comment is 'Parses a sequence of zero of more digits.'
	]).

	:- public(hex_digit//1).
	:- mode(hex_digit(?atomic), zero_or_one).
	:- info(hex_digit//1, [
		comment is 'Parses a single hexa-decimal digit.'
	]).

	:- public(hex_digits//1).
	:- mode(hex_digits(?list(atomic)), one).
	:- info(hex_digits//1, [
		comment is 'Parses a sequence of zero or more hexa-decimal digits.'
	]).

	:- public(natural//1).
	:- mode(natural(?non_negative_integer), zero_or_one).
	:- info(natural//1, [
		comment is 'Parses a natural number (a non signed integer).'
	]).

	:- public(integer//1).
	:- mode(integer(?integer), zero_or_one).
	:- info(integer//1, [
		comment is 'Parses an integer.'
	]).

	:- public(float//1).
	:- mode(float(?float), zero_or_one).
	:- info(float//1, [
		comment is 'Parses a float.'
	]).

	:- public(number//1).
	:- mode(number(?number), zero_or_one).
	:- info(number//1, [
		comment is 'Parses a number (an integer or a float).'
	]).

	:- public(sign//1).
	:- mode(sign(?atomic), zero_or_one).
	:- info(sign//1, [
		comment is 'Parses a number sign.'
	]).

	:- public(dot//1).
	:- mode(dot(?atomic), zero_or_one).
	:- info(dot//1, [
		comment is 'Parses a decimal dot.'
	]).

	:- uses(list, [
		append/3
	]).

	bit(Bit) -->
		bit(_Format_, Bit).

	bit(chars, 0) -->
		['0'], !.
	bit(chars, 1) -->
		['1'].

	bit(codes, 0) -->
		[0'0], !.
	bit(codes, 1) -->
		[0'1].

	bits(Bits) --> bits(_Format_, Bits).

	bits(_Format_, [Bit| Bits]) -->
		bit(_Format_, Bit), !, bits(_Format_, Bits).
	bits(_, []) -->
		[].

	digit(Digit) -->
		digit(_Format_, Digit).

	digit(chars, Digit) -->
		[Digit],
		{'0' @=< Digit, Digit @=< '9'}.
	digit(codes, Digit) -->
		[Digit],
		{0'0 @=< Digit, Digit @=< 0'9}.

	digits(Digits) -->
		digits(_Format_, Digits).

	digits(_Format_, [Digit| Digits]) -->
		digit(_Format_, Digit), !, digits(_Format_, Digits).
	digits(_, []) -->
		[].

	hex_digit(Digit) -->
		hex_digit(_Format_, Digit).

	hex_digit(chars, Digit) -->
		[Digit],
		{'0' @=< Digit, Digit @=< '9'; 'a' @=< Digit, Digit @=< 'f'; 'A' @=< Digit, Digit @=< 'F'}, !.
	hex_digit(codes, Digit) -->
		[Digit],
		{0'0 @=< Digit, Digit @=< 0'9; 0'a @=< Digit, Digit @=< 0'f; 0'A @=< Digit, Digit @=< 0'F}, !.

	hex_digits(Digits) --> hex_digits(_Format_, Digits).

	hex_digits(_Format_, [Digit| Digits]) -->
		hex_digit(_Format_, Digit), !, hex_digits(_Format_, Digits).
	hex_digits(_, []) -->
		[].

	sign(Sign) -->
		sign(_Format_, Sign).

	sign(chars, '+') -->
		['+'], !.
	sign(chars, '-') -->
		['-'].

	sign(codes, 0'+) -->
		[0'+], !.
	sign(codes, 0'-) -->
		[0'-].

	natural(Natural) -->
		natural(_Format_, Natural).

	natural(chars, Natural) -->
		digit(chars, Digit), digits(chars, Digits),
		{number_chars(Natural, [Digit| Digits])}.
	natural(codes, Natural) -->
		digit(codes, Digit), digits(codes, Digits),
		{number_codes(Natural, [Digit| Digits])}.

	integer(Integer) -->
		integer(_Format_, Integer).

	integer(chars, Integer) -->
		integer_elements(chars, Elements),
		{number_chars(Integer, Elements)}.
	integer(codes, Integer) -->
		integer_elements(codes, Elements),
		{number_codes(Integer, Elements)}.

	float(Float) -->
		float(_Format_, Float).

	float(chars, Number) -->
		float_elements(chars, Elements), {number_chars(Number,Elements)}.
	float(codes, Number) -->
		float_elements(codes, Elements), {number_codes(Number,Elements)}.

	float_elements(_Format_, FloatElements) -->
		integer_elements(_Format_, Elements),
		(	dot(_Format_, Dot), digit(_Format_, Digit1), digits(_Format_, Digits1) ->
			{append(Elements, [Dot, Digit1| Digits1], MantissaElements)}
		;	{MantissaElements = Elements}
		),
		radix(_Format_, RadixElements),
		(	{MantissaElements == Elements} ->
			{RadixElements \== []},
			(	{_Format_ == chars} ->
				{append(MantissaElements, ['.', '0'| RadixElements], FloatElements)}
			;	{append(MantissaElements, [0'., 0'0| RadixElements], FloatElements)}
			)
		;	{append(MantissaElements, RadixElements, FloatElements)}
		).

	number(Number) -->
		number(_Format_, Number).

	number(chars, Number) -->
		number_elements(chars, Elements), {number_chars(Number,Elements)}.
	number(codes, Number) -->
		number_elements(codes, Elements), {number_codes(Number,Elements)}.

	number_elements(_Format_, NumberElements) -->
		integer_elements(_Format_, Elements),
		(	dot(_Format_, Dot), digit(_Format_, Digit1), digits(_Format_, Digits1) ->
			{append(Elements, [Dot, Digit1| Digits1], MantissaElements)}
		;	{MantissaElements = Elements}
		),
		radix(_Format_, RadixElements),
		(	{RadixElements \== [], MantissaElements == Elements} ->
			(	{_Format_ == chars} ->
				{append(MantissaElements, ['.', '0'| RadixElements], NumberElements)}
			;	{append(MantissaElements, [0'., 0'0| RadixElements], NumberElements)}
			)
		;	{append(MantissaElements, RadixElements, NumberElements)}
		).

	mantissa(_Format_, Mantissa) -->
		integer_elements(_Format_, Digits0), dot(_Format_, Dot), digit(_Format_, Digit1), digits(_Format_, Digits1),
		{append(Digits0, [Dot, Digit1| Digits1], Mantissa)}.

	radix(_Format_, [Exp| Elements]) -->
		exp(_Format_, Exp), !, integer_elements(_Format_, Elements).
	radix(_, []) -->
		[].

	integer_elements(chars, [Digit| Digits]) -->
		['+'], !, digit(chars, Digit), digits(chars, Digits).
	integer_elements(chars, ['-', Digit| Digits]) -->
		['-'], !, digit(chars, Digit), digits(chars, Digits).
	integer_elements(codes, [Digit| Digits]) -->
		[0'+], !, digit(codes, Digit), digits(codes, Digits).
	integer_elements(codes, [0'-, Digit| Digits]) -->
		[0'-], !, digit(codes, Digit), digits(codes, Digits).
	integer_elements(_Format_, [Digit| Digits]) -->
		digit(_Format_, Digit), digits(_Format_, Digits).

	dot(Dot) -->
		dot(_Format_, Dot).

	dot(chars, '.') -->
		['.'].
	dot(codes, 0'.) -->
		[0'.].

	exp(Exp) -->
		exp(_Format_, Exp).

	exp(chars, 'e') -->
		['e'], !.
	exp(chars, 'e') -->
		['E'].

	exp(codes, 0'e) -->
		[0'e], !.
	exp(codes, 0'e) -->
		[0'E].

:- end_object.

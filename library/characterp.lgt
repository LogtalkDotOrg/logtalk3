%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- protocol(characterp).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2011/02/19,
		comment is 'Character protocol.'
	]).

	:- public(is_ascii/1).
	:- mode(is_ascii(+char), zero_or_one).
	:- info(is_ascii/1, [
		comment is 'True if the argument is an ASCII character.',
		argnames is ['Char']
	]).

	:- public(is_alphanumeric/1).
	:- mode(is_alphanumeric(+char), zero_or_one).
	:- info(is_alphanumeric/1, [
		comment is 'True if the argument is an alphanumeric character.',
		argnames is ['Char']
	]).

	:- public(is_alpha/1).
	:- mode(is_alpha(+char), zero_or_one).
	:- info(is_alpha/1, [
		comment is 'True if the argument is a letter or an underscore.',
		argnames is ['Char']
	]).

	:- public(is_letter/1).
	:- mode(is_letter(+char), zero_or_one).
	:- info(is_letter/1, [
		comment is 'True if the argument is a letter.',
		argnames is ['Char']
	]).

	:- public(is_bin_digit/1).
	:- mode(is_bin_digit(+char), zero_or_one).
	:- info(is_bin_digit/1, [
		comment is 'True if the argument is a binary digit.',
		argnames is ['Char']
	]).

	:- public(is_octal_digit/1).
	:- mode(is_octal_digit(+char), zero_or_one).
	:- info(is_octal_digit/1, [
		comment is 'True if the argument is an octal digit.',
		argnames is ['Char']
	]).

	:- public(is_dec_digit/1).
	:- mode(is_dec_digit(+char), zero_or_one).
	:- info(is_dec_digit/1, [
		comment is 'True if the argument is a decimal digit.',
		argnames is ['Char']
	]).

	:- public(is_hex_digit/1).
	:- mode(is_hex_digit(+char), zero_or_one).
	:- info(is_hex_digit/1, [
		comment is 'True if the argument is an hexadecimal digit.',
		argnames is ['Char']
	]).

	:- public(is_lower_case/1).
	:- mode(is_lower_case(+char), zero_or_one).
	:- info(is_lower_case/1, [
		comment is 'True if the argument is a lower case letter.',
		argnames is ['Char']
	]).

	:- public(is_upper_case/1).
	:- mode(is_upper_case(+char), zero_or_one).
	:- info(is_upper_case/1, [
		comment is 'True if the argument is a upper case letter.',
		argnames is ['Char']
	]).

	:- public(is_vowel/1).
	:- mode(is_vowel(+char), zero_or_one).
	:- info(is_vowel/1, [
		comment is 'True if the argument is a vowel.',
		argnames is ['Char']
	]).

	:- public(is_white_space/1).
	:- mode(is_white_space(+char), zero_or_one).
	:- info(is_white_space/1, [
		comment is 'True if the argument is a white space character (a space or a tab) inside a line of characters.',
		argnames is ['Char']
	]).

	:- public(is_layout/1).
	:- mode(is_layout(+char), zero_or_one).
	:- info(is_layout/1, [
		comment is 'True if the argument is a layout character.',
		argnames is ['Char']
	]).

	:- public(is_quote/1).
	:- mode(is_quote(+char), zero_or_one).
	:- info(is_quote/1, [
		comment is 'True if the argument is a quote character.',
		argnames is ['Char']
	]).

	:- public(is_punctation/1).
	:- mode(is_punctation(+char), zero_or_one).
	:- info(is_punctation/1, [
		comment is 'True if the argument is a sentence punctation character.',
		argnames is ['Char']
	]).

	:- public(is_period/1).
	:- mode(is_period(+char), zero_or_one).
	:- info(is_period/1, [
		comment is 'True if the argument is a character that ends a sentence.',
		argnames is ['Char']
	]).

	:- public(is_control/1).
	:- mode(is_control(+char), zero_or_one).
	:- info(is_control/1, [
		comment is 'True if the argument is an ASCII control character.',
		argnames is ['Char']
	]).

	:- public(is_newline/1).
	:- mode(is_newline(+char), zero_or_one).
	:- info(is_newline/1, [
		comment is 'True if the argument is the ASCII newline character.',
		argnames is ['Char']
	]).

	:- public(is_end_of_line/1).
	:- mode(is_end_of_line(+char), zero_or_one).
	:- info(is_end_of_line/1, [
		comment is 'True if the argument is the ASCII end-of-line character (either a carriage return or a line feed).',
		argnames is ['Char']
	]).

	:- public(parenthesis/2).
	:- mode(parenthesis(?char, ?char), zero_or_more).
	:- mode(parenthesis(+char, ?char), zero_or_one).
	:- mode(parenthesis(?char, +char), zero_or_one).
	:- info(parenthesis/2, [
		comment is 'Recognises and converts between open and close parenthesis.',
		argnames is ['Char1', 'Char2']
	]).

	:- public(lower_upper/2).
	:- mode(lower_upper(?char, ?char), zero_or_more).
	:- mode(lower_upper(+char, ?char), zero_or_one).
	:- mode(lower_upper(?char, +char), zero_or_one).
	:- info(lower_upper/2, [
		comment is 'Recognises and converts between lower and upper case letters.',
		argnames is ['Char1', 'Char2']
	]).

:- end_protocol.

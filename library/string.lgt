%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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



:- object(string).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2009/10/08,
		comment is 'Predicates over strings, represented as lists of character codes.']).

	:- uses(list, [reverse/2]).

	:- public(expand_tabs/3).
	:- mode(expand_tabs(+string, +string, -string), one).
	:- info(expand_tabs/3, [
		comment is 'Description',
		argnames is ['String', 'TabSize', 'ConvertedString']]).

	:- public(camel_case/1).
	:- mode(camel_case(+string), zero_or_one).
	:- info(camel_case/1, [
		comment is 'Checks if a string represents a word where the first character is in upper case and the remaining characters are in lower case.',
		argnames is ['String']]).

	:- public(lower_case/1).
	:- mode(lower_case(+string), zero_or_one).
	:- info(lower_case/1, [
		comment is 'Checks if a string represents a word in lower case.',
		argnames is ['String']]).

	:- public(lower_case/2).
	:- mode(lower_case(+string, -string), one).
	:- info(lower_case/2, [
		comment is 'Description',

	:- public(title_case/1).
	:- mode(title_case(+string), zero_or_one).
	:- info(title_case/1, [
		comment is 'Checks if a string represents a word where the first character is in upper case and the remaining characters are in lower case.',
		argnames is ['String']]).

	:- public(upper_case/1).
	:- mode(upper_case(+string), zero_or_one).
	:- info(upper_case/1, [
		comment is 'Checks if a string represents a word in upper case.',
		argnames is ['String']]).

	:- public(upper_case/2).
	:- mode(upper_case(+string, -string), one).
	:- info(upper_case/2, [
		comment is 'Description',
		argnames is ['String', 'ConvertedString']]).

	:- public(capitalize_words/2).
	:- mode(capitalize_words(+string, -string), one).
	:- info(capitalize_words/2, [
		comment is 'Description',
		argnames is ['String', 'ConvertedString']]).

	:- public(replace_word/3).
	:- mode(replace_word(+string, +string, -string), one).
	:- info(replace_word/3, [
		comment is 'Description',
		argnames is ['String', 'Word', 'ConvertedString']]).

	:- public(split/3).
	:- mode(split(+string, +string, -list(string)), one).
	:- info(split/3,
		[comment is 'Splits a string using another string as a delimiter.',
		 argnames is ['String', 'Delimiter', 'Strings']]).

	:- public(trim/2).
	:- mode(trim(+string, -string), one).
	:- info(trim/2, [
		comment is 'Description',
		argnames is ['String', 'TrimedString']]).

	:- public(valid/1).
	:- mode(valid(@nonvar), zero_or_one).
	:- info(valid/1, [
		comment is 'Term is a valid string.',
		argnames is ['Term']]).

	length(Codes, Length) :-
		length(Codes, 0, Length).

	length([], Length, Length).
	length([_| Codes], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Codes, Acc2, Length).

	lower_case([], []).
	lower_case([Code1| Codes1], [Code2| Codes2]) :-
		(	lower_upper(Code2, Code1) ->
			true
		;	Code2 = Code1
		),
		lower_case(Codes1, Codes2).

	upper_case([], []).
	upper_case([Code1| Codes1], [Code2| Codes2]) :-
		(	lower_upper(Code1, Code2) ->
			true
		;	Code2 = Code1
		),
		upper_case(Codes1, Codes2).

	lower_upper(0'a, 0'A).
	lower_upper(0'b, 0'B).
	lower_upper(0'c, 0'C).
	lower_upper(0'd, 0'D).
	lower_upper(0'e, 0'E).
	lower_upper(0'f, 0'F).
	lower_upper(0'g, 0'G).
	lower_upper(0'h, 0'H).
	lower_upper(0'i, 0'I).
	lower_upper(0'j, 0'J).
	lower_upper(0'k, 0'K).
	lower_upper(0'l, 0'L).
	lower_upper(0'm, 0'M).
	lower_upper(0'n, 0'N).
	lower_upper(0'o, 0'O).
	lower_upper(0'p, 0'P).
	lower_upper(0'q, 0'Q).
	lower_upper(0'r, 0'R).
	lower_upper(0's, 0'S).
	lower_upper(0't, 0'T).
	lower_upper(0'u, 0'U).
	lower_upper(0'v, 0'V).
	lower_upper(0'w, 0'W).
	lower_upper(0'x, 0'X).
	lower_upper(0'y, 0'Y).
	lower_upper(0'z, 0'Z).

	trim(String, TrimmedString) :-
		trim_starting_whitespace(String, TrimmedString0),
		reverse(TrimmedString0, TrimmedString1),
		trim_starting_whitespace(TrimmedString1, TrimmedString2),
		reverse(TrimmedString2, TrimmedString).

	trim_starting_whitespace([], []).
	trim_starting_whitespace([Code| Codes], TrimmedString) :-
		(	is_white_space(Code) ->
			trim_starting_whitespace(Codes, TrimmedString)
		;	TrimmedString = [Code| Codes]
		).

	is_white_space( 9).		% ASCII tab
	is_white_space(32).		% ASCII space

	valid((-)) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Code| Codes]) :-
		integer(Code),
		valid(Codes).

:- end_object.

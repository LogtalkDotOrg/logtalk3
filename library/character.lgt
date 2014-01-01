%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
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



:- object(character,
	implements(characterp),
	extends(atom)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2011/11/10,
		comment is 'Character predicates (most of them assume an ASCII representation).'
	]).

	is_ascii(Char) :-
		char_code(Char, Code),
		Code >= 0,
		Code =< 127.

	is_alpha('_').
	is_alpha(Char) :-
		is_letter(Char).

	is_letter(Char) :-
		is_lower_case(Char).
	is_letter(Char) :-
		is_upper_case(Char).

	is_alphanumeric(Char) :-
		is_alpha(Char).
	is_alphanumeric(Char) :-
		is_dec_digit(Char).

	is_bin_digit('0').
	is_bin_digit('1').

	is_octal_digit(Digit) :-
		Digit @>= '0',
		Digit @=< '7'.

	is_dec_digit(Digit) :-
		Digit @>= '0',
		Digit @=< '9'.

	is_hex_digit(Digit) :-
		Digit @>= '0',
		Digit @=< '9'.
	is_hex_digit(Digit) :-
		Digit @>= 'A',
		Digit @=< 'F'.
	is_hex_digit(Digit) :-
		Digit @>= a,
		Digit @=< f.

	is_lower_case(Char) :-
		Char @>= a,
		Char @=< z.

	is_upper_case(Char) :-
		Char @>= 'A',
		Char @=< 'Z'.

	is_vowel(a).
	is_vowel(e).
	is_vowel(i).
	is_vowel(o).
	is_vowel(u).
	is_vowel('A').
	is_vowel('E').
	is_vowel('I').
	is_vowel('O').
	is_vowel('U').

	is_white_space(' ').
	is_white_space('\t').

	is_layout(' ').
	is_layout('\t').
	is_layout('\f').
	is_layout('\r').
	is_layout('\n').
	is_layout('\v').

	is_quote('''').
	is_quote('"').
	is_quote('`').

	is_punctation(',').
	is_punctation(';').
	is_punctation(':').
	is_punctation('.').
	is_punctation('?').
	is_punctation('!').

	is_period('.').
	is_period('?').
	is_period('!').

	is_control(Char) :-
		char_code(Char, Code),
		Code >= 0,
		Code =< 31.

	is_newline(Char) :-
		char_code(Char, 10).

	is_end_of_line(Char) :-
		(	char_code(Char, 10) ->
			true
		;	char_code(Char, 13)
		).

	parenthesis('(', ')').
	parenthesis('[', ']').
	parenthesis('{', '}').

	lower_upper(a, 'A').
	lower_upper(b, 'B').
	lower_upper(c, 'C').
	lower_upper(d, 'D').
	lower_upper(e, 'E').
	lower_upper(f, 'F').
	lower_upper(g, 'G').
	lower_upper(h, 'H').
	lower_upper(i, 'I').
	lower_upper(j, 'J').
	lower_upper(k, 'K').
	lower_upper(l, 'L').
	lower_upper(m, 'M').
	lower_upper(n, 'N').
	lower_upper(o, 'O').
	lower_upper(p, 'P').
	lower_upper(q, 'Q').
	lower_upper(r, 'R').
	lower_upper(s, 'S').
	lower_upper(t, 'T').
	lower_upper(u, 'U').
	lower_upper(v, 'V').
	lower_upper(w, 'W').
	lower_upper(x, 'X').
	lower_upper(y, 'Y').
	lower_upper(z, 'Z').
	lower_upper(Char, Char) :-
		\+ (Char @>= a, Char @=< z),
		\+ (Char @>= 'A', Char @=< 'Z').

	valid(Character) :-
		atom(Character),
		atom_length(Character, 1).

	check(Term) :-
		this(This),
		sender(Sender),
		(	atom(Term), atom_length(Term, 1) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.

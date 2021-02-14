%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(term_io_protocol).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-02-14,
		comment is 'Predicates for term input/output from/to atom, chars, and codes. The predicates are declared as synchronized when the library is compiled using a backend supporting threads.'
	]).

	:- public(read_term_from_atom/3).
	:- mode(read_term_from_atom(@term, -atom, +list(read_option)), one_or_error).
	:- info(read_term_from_atom/3, [
		comment is 'Reads a term from an atom using the given read options. A period at the end of the atom is optional.',
		argnames is ['Atom', 'Term', 'Options']
	]).

	:- public(read_from_atom/2).
	:- mode(read_from_atom(@term, -atom), one_or_error).
	:- info(read_from_atom/2, [
		comment is 'Reads a term from an atom using default read options. Shorthand for ``read_term_from_atom(Atom,Term,[])``. A period at the end of the atom is optional.',
		argnames is ['Atom', 'Term']
	]).

	:- public(read_term_from_chars/3).
	:- mode(read_term_from_chars(@term, -list(character), +list(read_option)), one_or_error).
	:- info(read_term_from_chars/3, [
		comment is 'Reads a term from a list of characters using the given read options. A period at the end of the list is optional.',
		argnames is ['Chars', 'Term', 'Options']
	]).

	:- public(read_from_chars/2).
	:- mode(read_from_chars(@term, -list(character)), one_or_error).
	:- info(read_from_chars/2, [
		comment is 'Reads a term from a list of characters using default read options. Shorthand for ``read_term_from_chars(Chars,Term,[])``. A period at the end of the list is optional.',
		argnames is ['Chars', 'Term']
	]).

	:- public(read_term_from_codes/3).
	:- mode(read_term_from_codes(@term, -list(character_code), +list(read_option)), one_or_error).
	:- info(read_term_from_codes/3, [
		comment is 'Reads a term from a list of character codes using the given read options. A period at the end of the list is optional.',
		argnames is ['Codes', 'Term', 'Options']
	]).

	:- public(read_from_codes/2).
	:- mode(read_from_codes(@term, -list(character_code)), one_or_error).
	:- info(read_from_codes/2, [
		comment is 'Reads a term from a list of character codes using default read options. Shorthand for ``read_term_from_codes(Codes,Term,[])``. A period at the end of the list is optional.',
		argnames is ['Codes', 'Term']
	]).

	:- public(write_term_to_atom/3).
	:- mode(write_term_to_atom(@term, -atom, +list(write_option)), one).
	:- info(write_term_to_atom/3, [
		comment is 'Writes a term to an atom using the given write options.',
		argnames is ['Term', 'Atom', 'Options']
	]).

	:- public(write_to_atom/2).
	:- mode(write_to_atom(@term, -atom), one).
	:- info(write_to_atom/2, [
		comment is 'Writes a term to an atom using default write options. Shorthand for ``write_term_to_atom(Term,Atom,[])``.',
		argnames is ['Term', 'Atom']
	]).

	:- public(write_term_to_chars/3).
	:- mode(write_term_to_chars(@term, -list(character), +list(write_option)), one).
	:- info(write_term_to_chars/3, [
		comment is 'Writes a term to a list of characters using the given write options. Shorthand for ``write_term_to_chars(Term,Chars,[],Options)``.',
		argnames is ['Term', 'Chars', 'Options']
	]).

	:- public(write_term_to_chars/4).
	:- mode(write_term_to_chars(@term, -list(character), @term, +list(write_option)), one).
	:- info(write_term_to_chars/4, [
		comment is 'Writes a term to a list of characters with the given tail using the given write options.',
		argnames is ['Term', 'Chars', 'Tail', 'Options']
	]).

	:- public(write_to_chars/2).
	:- mode(write_to_chars(@term, -list(character)), one).
	:- info(write_to_chars/2, [
		comment is 'Writes a term to a list of characters using default write options. Shorthand for ``write_term_to_chars(Term,Chars,[],[])``.',
		argnames is ['Term', 'Chars']
	]).

	:- public(write_term_to_codes/3).
	:- mode(write_term_to_codes(@term, -list(character_code), +list(write_option)), one).
	:- info(write_term_to_codes/3, [
		comment is 'Writes a term to a list of character codes using the given write options. Shorthand for ``write_term_to_codes(Term,Codes,[],Options)``.',
		argnames is ['Term', 'Codes', 'Options']
	]).

	:- public(write_term_to_codes/4).
	:- mode(write_term_to_codes(@term, -list(character_code), @term, +list(write_option)), one).
	:- info(write_term_to_codes/4, [
		comment is 'Writes a term to a list of character codes with the given tail using the given write options.',
		argnames is ['Term', 'Codes', 'Tail', 'Options']
	]).

	:- public(write_to_codes/2).
	:- mode(write_to_codes(@term, -list(character_code)), one).
	:- info(write_to_codes/2, [
		comment is 'Writes a term to a list of character codes using default write options. Shorthand for ``write_term_to_chars(Term,Codes,[],[])``.',
		argnames is ['Term', 'Codes']
	]).

	:- public(format_to_atom/3).
	:- mode(format_to_atom(@atom, +list(term), -atom), one).
	:- info(format_to_atom/3, [
		comment is 'Writes a list of arguments to an atom using the given format.',
		argnames is ['Format', 'Arguments', 'Atom']
	]).

	:- public(format_to_chars/3).
	:- mode(format_to_chars(@term, +list(term), -list(character)), one).
	:- info(format_to_chars/3, [
		comment is 'Writes a list of arguments to a list of characters using the given format. Shorthand for ``format_to_chars(Format,Arguments,Chars,[])``.',
		argnames is ['Format', 'Arguments', 'Chars']
	]).

	:- public(format_to_chars/4).
	:- mode(format_to_chars(@term, +list(term), -list(character), @term), one).
	:- info(format_to_chars/4, [
		comment is 'Writes a term to a list of characters with the given tail using the given format.',
		argnames is ['Format', 'Arguments', 'Chars', 'Tail']
	]).

	:- public(format_to_codes/3).
	:- mode(format_to_codes(@term, +list(term), -list(character_code)), one).
	:- info(format_to_codes/3, [
		comment is 'Writes a list of arguments to a list of character codes using the given format. Shorthand for ``format_to_codes(Format,Arguments,Codes,[])``.',
		argnames is ['Format', 'Arguments', 'Codes']
	]).

	:- public(format_to_codes/4).
	:- mode(format_to_codes(@term, +list(term), -list(character_code), @term), one).
	:- info(format_to_codes/4, [
		comment is 'Writes a list of arguments to a list of character codes with the given tail using the given format.',
		argnames is ['Format', 'Arguments', 'Codes', 'Tail']
	]).

	:- public(with_output_to/2).
	:- meta_predicate(with_output_to(*, 0)).
	:- mode(with_output_to(+callable, -atom), zero_or_one).
	:- info(with_output_to/2, [
		comment is 'Calls a goal deterministically with output to the given format: {``atom(Atom)``, ``chars(Chars)``, ``chars(Chars,Tail)``, ``codes(Codes)``, ``codes(Codes,Tail)``}.',
		argnames is ['Output', 'Goal']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			read_term_from_atom/3, read_term_from_chars/3, read_term_from_codes/3,
			write_term_to_atom/3, write_term_to_chars/4, write_term_to_codes/4,
			format_to_atom/3, format_to_chars/4, format_to_codes/4,
			with_output_to/2
		]).
	:- endif.

:- end_protocol.

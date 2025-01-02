%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- category(ulid_types).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-05-19,
		comment is 'ULID type definition.',
		remarks is [
			'Provided types' - 'This category adds a ``ulid(Representation)`` type for type-checking when using the ``ulid`` library object. Valid representation values are ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [ulid(_), ulid]
	]).

	:- multifile(type::type/1).
	% clauses for the type::type/1 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::type(ulid(_)).

	:- multifile(type::check/2).
	% clauses for the type::check/2 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::check(ulid(Representation), Term) :-
		check(Representation, Term).

	check(atom, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	\+ atom_length(Term, 26) ->
			throw(domain_error(ulid, Term))
		;	atom_chars(Term, Chars),
			list::member(Char, Chars),
			\+ valid_base_32_char(Char) ->
			throw(domain_error(ulid, Term))
		;	Term @> '7ZZZZZZZZZZZZZZZZZZZZZZZZZ' ->
			throw(domain_error(ulid, Term))
		;	true
		).
	check(chars, Term) :-
		type::check(chars, Term),
		(	\+ list::length(Term, 26) ->
			throw(domain_error(ulid, Term))
		;	list::member(Char, Term),
			\+ valid_base_32_char(Char) ->
			throw(domain_error(ulid, Term))
		;	Term @> ['7','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z'] ->
			throw(domain_error(ulid, Term))
		;	true
		).
	check(codes, Term) :-
		type::check(codes, Term),
		(	\+ list::length(Term, 26) ->
			throw(domain_error(ulid, Term))
		;	list::member(Code, Term),
			char_code(Char, Code),
			\+ valid_base_32_char(Char) ->
			throw(domain_error(ulid, Term))
		;	Term @> [0'7,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z] ->
			throw(domain_error(ulid, Term))
		;	true
		).

	% Crockford's Base32 encoding
	valid_base_32_char('0').
	valid_base_32_char('1').
	valid_base_32_char('2').
	valid_base_32_char('3').
	valid_base_32_char('4').
	valid_base_32_char('5').
	valid_base_32_char('6').
	valid_base_32_char('7').
	valid_base_32_char('8').
	valid_base_32_char('9').
	valid_base_32_char('A').
	valid_base_32_char('B').
	valid_base_32_char('C').
	valid_base_32_char('D').
	valid_base_32_char('E').
	valid_base_32_char('F').
	valid_base_32_char('G').
	valid_base_32_char('H').
	valid_base_32_char('J').
	valid_base_32_char('K').
	valid_base_32_char('M').
	valid_base_32_char('N').
	valid_base_32_char('P').
	valid_base_32_char('Q').
	valid_base_32_char('R').
	valid_base_32_char('S').
	valid_base_32_char('T').
	valid_base_32_char('V').
	valid_base_32_char('W').
	valid_base_32_char('X').
	valid_base_32_char('Y').
	valid_base_32_char('Z').

:- end_category.

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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2022-05-24,
		comment is 'Unit tests for the ISO Prolog standard char_code/2 built-in predicate.'
	]).

	:- uses(integer, [
		between/3
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.6.4

	test(iso_char_code_2_01, true(Code == 0'a)) :-
		{char_code(a, Code)}.

	test(iso_char_code_2_02, true(Char == Atom)) :-
		{char_code(Char, 99), atom_codes(Atom, [99])}.

	test(iso_char_code_2_03, true(Char == c)) :-
		{char_code(Char, 0'c)}.

	test(iso_char_code_2_04, true) :-
		% the ISO standard also allows a representation_error(character_code)
		catch({char_code(_Char, 163)}, Error, true),
		(	var(Error) ->
			true
		;	subsumes_term(error(representation_error(character_code),_), Error)
		).

	test(iso_char_code_2_05, true) :-
		{char_code(b, 0'b)}.

	test(iso_char_code_2_06, error(type_error(character,'ab'))) :-
		{char_code('ab', _Code)}.

	test(iso_char_code_2_07, error(instantiation_error)) :-
		{char_code(_Char, _Code)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_char_code_2_08, error(type_error(integer,x))) :-
		{char_code(a, x)}.

	test(eddbali_char_code_2_09, error(representation_error(character_code))) :-
		{char_code(_Char, -2)}.

	% tests from the Logtalk portability work

	test(lgt_char_code_2_10, true) :-
		catch({char_code(_, 0)}, error(Error, _), Error == representation_error(character_code)).

	test(lgt_char_code_2_11, error(type_error(character,42))) :-
		{char_code(42, _Code)}.

	test(lgt_char_code_2_12, all((char_code(Char,Code0), char_code(Char,Code), Code0 == Code))) :-
		between(1, 127, Code0).

:- end_object.

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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Unit tests for the "nanoid" library.'
	]).

	cover(nanoid).
	cover(nanoid(_, _, _)).

	test(nanoid_generate_1_default_atom_type, true(atom(NanoID))) :-
		nanoid::generate(NanoID).

	test(nanoid_generate_1_default_atom_size, true(atom_length(NanoID, 21))) :-
		nanoid::generate(NanoID).

	test(nanoid_generate_1_chars_representation, true(list::length(NanoID, 10))) :-
		nanoid(chars, 10, 'abcDEF012')::generate(NanoID).

	test(nanoid_generate_1_codes_representation, true(list::length(NanoID, 32))) :-
		nanoid(codes, 32, [0'a,0'b,0'c,0'd,0'e,0'f,0'0,0'1,0'2,0'3])::generate(NanoID).

	test(nanoid_generate_1_custom_alphabet_membership, true)
		:-
		nanoid(chars, 24, ['a','b','c'])::generate(NanoID),
		forall(list::member(Symbol, NanoID), list::member(Symbol, ['a','b','c'])).

	test(nanoid_generate_1_error_representation_var, error(instantiation_error)) :-
		nanoid(_, 10, abc)::generate(_).

	test(nanoid_generate_1_error_representation_domain, error(domain_error(nanoid_representation, text))) :-
		nanoid(text, 10, abc)::generate(_).

	test(nanoid_generate_1_error_size_var, error(instantiation_error)) :-
		nanoid(atom, _, abc)::generate(_).

	test(nanoid_generate_1_error_size_type, error(type_error(integer, ten))) :-
		nanoid(atom, ten, abc)::generate(_).

	test(nanoid_generate_1_error_size_domain, error(domain_error(not_less_than_one, 0))) :-
		nanoid(atom, 0, abc)::generate(_).

	test(nanoid_generate_1_error_alphabet_var, error(instantiation_error)) :-
		nanoid(atom, 10, _)::generate(_).

	test(nanoid_generate_1_error_alphabet_type, error(type_error(text, 42))) :-
		nanoid(atom, 10, 42)::generate(_).

	test(nanoid_generate_1_error_alphabet_domain_empty, error(domain_error(nanoid_alphabet, ''))) :-
		nanoid(atom, 10, '')::generate(_).

	test(nanoid_generate_1_error_alphabet_domain_singleton, error(domain_error(nanoid_alphabet, 'a'))) :-
		nanoid(atom, 10, 'a')::generate(_).

	test(nanoid_generate_1_error_alphabet_domain_duplicates, error(domain_error(nanoid_alphabet, 'aabc'))) :-
		nanoid(atom, 10, 'aabc')::generate(_).

	quick_check(nanoid_default_valid, nanoid(-atom)).
	quick_check(nanoid_atom_valid, nanoid({atom}, +between(integer,1,64), {'_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'}, -atom)).
	quick_check(nanoid_chars_valid, nanoid({chars}, +between(integer,1,64), {[a,b,c,d,e,f,'0','1','2','3']}, -chars)).
	quick_check(nanoid_codes_valid, nanoid({codes}, +between(integer,1,64), {[97,98,99,100,101,102,48,49,50,51]}, -codes)).

	nanoid(NanoID) :-
		nanoid::generate(NanoID).

	nanoid(Representation, Size, Alphabet, NanoID) :-
		nanoid(Representation, Size, Alphabet)::generate(NanoID).

:- end_object.

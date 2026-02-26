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
		comment is 'Unit tests for the "ksuid" library.'
	]).

	cover(ksuid).
	cover(ksuid(_, _)).

	test(ksuid_generate_1_default_atom_type, true(atom(KSUID))) :-
		ksuid::generate(KSUID).

	test(ksuid_generate_1_default_atom_size, true(atom_length(KSUID, 27))) :-
		ksuid::generate(KSUID).

	test(ksuid_generate_1_chars_representation, true(list::length(KSUID, 27))) :-
		ksuid(chars, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')::generate(KSUID).

	test(ksuid_generate_1_codes_representation, true(list::length(KSUID, 27))) :-
		ksuid(codes, [0'0,0'1,0'2,0'3,0'4,0'5,0'6,0'7,0'8,0'9,0'A,0'B,0'C,0'D,0'E,0'F,0'G,0'H,0'I,0'J,0'K,0'L,0'M,0'N,0'O,0'P,0'Q,0'R,0'S,0'T,0'U,0'V,0'W,0'X,0'Y,0'Z,0'a,0'b,0'c,0'd,0'e,0'f,0'g,0'h,0'i,0'j,0'k,0'l,0'm,0'n,0'o,0'p,0'q,0'r,0's,0't,0'u,0'v,0'w,0'x,0'y,0'z])::generate(KSUID).

	test(ksuid_generate_1_custom_alphabet_membership, true)
		:-
		ksuid(chars, ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'])::generate(KSUID),
		forall(list::member(Symbol, KSUID), list::member(Symbol, ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'])).

	test(ksuid_generate_1_error_representation_var, error(instantiation_error)) :-
		ksuid(_, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')::generate(_).

	test(ksuid_generate_1_error_representation_domain, error(domain_error(ksuid_representation, text))) :-
		ksuid(text, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')::generate(_).

	test(ksuid_generate_1_error_alphabet_var, error(instantiation_error)) :-
		ksuid(atom, _)::generate(_).

	test(ksuid_generate_1_error_alphabet_type, error(type_error(text, 42))) :-
		ksuid(atom, 42)::generate(_).

	test(ksuid_generate_1_error_alphabet_size, error(domain_error(ksuid_alphabet, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxy'))) :-
		ksuid(atom, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxy')::generate(_).

	test(ksuid_generate_1_error_alphabet_duplicates, error(domain_error(ksuid_alphabet, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyy'))) :-
		ksuid(atom, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyy')::generate(_).

	quick_check(ksuid_default_valid, ksuid(-atom)).
	quick_check(ksuid_atom_valid, ksuid({atom}, {'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'}, -atom)).
	quick_check(ksuid_chars_valid, ksuid({chars}, {'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'}, -chars)).
	quick_check(ksuid_codes_valid, ksuid({codes}, {'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'}, -codes)).

	ksuid(KSUID) :-
		ksuid::generate(KSUID).

	ksuid(Representation, Alphabet, KSUID) :-
		ksuid(Representation, Alphabet)::generate(KSUID).

:- end_object.

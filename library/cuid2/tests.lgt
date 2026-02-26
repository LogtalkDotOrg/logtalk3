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
		comment is 'Unit tests for the "cuid2" library.'
	]).

	cover(cuid2).
	cover(cuid2(_, _, _)).

	test(cuid2_generate_1_default_atom_type, true(atom(Cuid2))) :-
		cuid2::generate(Cuid2).

	test(cuid2_generate_1_default_atom_size, true(atom_length(Cuid2, 24))) :-
		cuid2::generate(Cuid2).

	test(cuid2_generate_1_default_atom_first_letter, true)
		:-
		cuid2::generate(Cuid2),
		atom_chars(Cuid2, [Head| _]),
		list::member(Head, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

	test(cuid2_generate_1_chars_representation, true(list::length(Cuid2, 10))) :-
		cuid2(chars, 10, 'abcdef012')::generate(Cuid2).

	test(cuid2_generate_1_codes_representation, true(list::length(Cuid2, 32))) :-
		cuid2(codes, 32, [0'a,0'b,0'c,0'd,0'e,0'f,0'0,0'1,0'2,0'3])::generate(Cuid2).

	test(cuid2_generate_1_custom_alphabet_membership, true)
		:-
		cuid2(chars, 24, ['a','b','c'])::generate(Cuid2),
		forall(list::member(Symbol, Cuid2), list::member(Symbol, ['a','b','c'])).

	test(cuid2_generate_1_error_representation_var, error(instantiation_error)) :-
		cuid2(_, 10, abc)::generate(_).

	test(cuid2_generate_1_error_representation_domain, error(domain_error(cuid2_representation, text))) :-
		cuid2(text, 10, abc)::generate(_).

	test(cuid2_generate_1_error_size_var, error(instantiation_error)) :-
		cuid2(atom, _, abc)::generate(_).

	test(cuid2_generate_1_error_size_type, error(type_error(integer, ten))) :-
		cuid2(atom, ten, abc)::generate(_).

	test(cuid2_generate_1_error_size_domain, error(domain_error(not_less_than_one, 0))) :-
		cuid2(atom, 0, abc)::generate(_).

	test(cuid2_generate_1_error_alphabet_var, error(instantiation_error)) :-
		cuid2(atom, 10, _)::generate(_).

	test(cuid2_generate_1_error_alphabet_type, error(type_error(text, 42))) :-
		cuid2(atom, 10, 42)::generate(_).

	test(cuid2_generate_1_error_alphabet_domain_empty, error(domain_error(cuid2_alphabet, ''))) :-
		cuid2(atom, 10, '')::generate(_).

	test(cuid2_generate_1_error_alphabet_domain_singleton, error(domain_error(cuid2_alphabet, 'a'))) :-
		cuid2(atom, 10, 'a')::generate(_).

	test(cuid2_generate_1_error_alphabet_domain_duplicates, error(domain_error(cuid2_alphabet, 'aabc'))) :-
		cuid2(atom, 10, 'aabc')::generate(_).

	test(cuid2_generate_1_error_alphabet_domain_no_letter, error(domain_error(cuid2_alphabet, '0123456789'))) :-
		cuid2(atom, 10, '0123456789')::generate(_).

	quick_check(cuid2_default_valid, cuid2(-atom)).
	quick_check(cuid2_atom_valid, cuid2({atom}, +between(integer,1,64), {'abcdefghijklmnopqrstuvwxyz0123456789'}, -atom)).
	quick_check(cuid2_chars_valid, cuid2({chars}, +between(integer,1,64), {'abcdef012345'}, -chars)).
	quick_check(cuid2_codes_valid, cuid2({codes}, +between(integer,1,64), {'abcdef012345'}, -codes)).

	cuid2(Cuid2) :-
		cuid2::generate(Cuid2).

	cuid2(Representation, Size, Alphabet, Cuid2) :-
		cuid2(Representation, Size, Alphabet)::generate(Cuid2).

:- end_object.

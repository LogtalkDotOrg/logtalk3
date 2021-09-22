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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2021-09-22,
		comment is 'Unit tests for the ISO Prolog standard sub_atom/5 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.3.4

	test(iso_sub_atom_5_01, true(S2 == 'abrac')) :-
		{sub_atom(abracadabra, 0, 5, _, S2)}.

	test(iso_sub_atom_5_02, true(S2 == 'dabra')) :-
		{sub_atom(abracadabra, _, 5, 0, S2)}.

	test(iso_sub_atom_5_03, true(Length-S2 == 5-'acada')) :-
		{sub_atom(abracadabra, 3, Length, 3, S2)}.

	test(iso_sub_atom_5_04, true(L == [0-9, 7-2])) :-
		findall(Before-After, {sub_atom(abracadabra,Before,2,After,ab)}, L).

	test(iso_sub_atom_5_05, true(S2 == 'an')) :-
		{sub_atom('Banana', 3, 2, _, S2)}.

	test(iso_sub_atom_5_06, true(L == ['cha', 'har', 'ari', 'rit', 'ity'])) :-
		findall(S2, {sub_atom('charity',_,3,_,S2)}, L).

	test(iso_sub_atom_5_07, true(L == [0-0-'', 0-1-'a', 0-2-'ab', 1-0-'', 1-1-'b', 2-0-''])) :-
		findall(Start-Lenght-SubAtom, {sub_atom('ab',Start,Lenght,_,SubAtom)}, L).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_sub_atom_5_08, error(instantiation_error)) :-
		{sub_atom(_Banana, 3, 2, _, _S)}.

	test(eddbali_sub_atom_5_09, error(type_error(atom,f(a)))) :-
		{sub_atom(f(a), 2, 2, _, _S2)}.

	test(eddbali_sub_atom_5_10, error(type_error(atom,2))) :-
		{sub_atom('Banana', 4, 2, _, 2)}.

	test(eddbali_sub_atom_5_11, error(type_error(integer,a))) :-
		{sub_atom('Banana', a, 2, _, _)}.

	test(eddbali_sub_atom_5_12, error(type_error(integer,n))) :-
		{sub_atom('Banana', 4, n, _, _)}.

	test(eddbali_sub_atom_5_13, error(type_error(integer,m))) :-
		{sub_atom('Banana', 4, _, m, _)}.

	test(sics_sub_atom_5_14, error(domain_error(not_less_than_zero,-2))) :-
		{sub_atom('Banana', -2, 3, 4, _)}.

	test(sics_sub_atom_5_15, error(domain_error(not_less_than_zero,-3))) :-
		{sub_atom('Banana', 2, -3, 4, _)}.

	test(sics_sub_atom_5_16, error(domain_error(not_less_than_zero,-4))) :-
		{sub_atom('Banana', 2, 3, -4, _)}.

	test(sics_sub_atom_5_17, true(A == 1)) :-
		{sub_atom('Banana', 2, 3, A, 'nan')}.

	test(sics_sub_atom_5_18, true(B == 2)) :-
		{sub_atom('Banana', B, 3, 1, 'nan')}.

	test(sics_sub_atom_5_19, true(L == 3)) :-
		{sub_atom('Banana', 2, L, 1, 'nan')}.

	test(sics_sub_atom_5_20, true(A-L == 1-3)) :-
		{sub_atom('Banana', 2, L, A, 'nan')}.

	test(sics_sub_atom_5_21, true(B-L == 2-3)) :-
		{sub_atom('Banana', B, L, 1, 'nan')}.

	test(sics_sub_atom_5_22, false) :-
		{sub_atom('Banana', 2, 3, 1, 'ana')}.

	test(sics_sub_atom_5_23, false) :-
		{sub_atom('Banana', 2, 3, 2, 'nan')}.

	test(sics_sub_atom_5_24, false) :-
		{sub_atom('Banana', 2, 3, 2, _)}.

	test(sics_sub_atom_5_25, false) :-
		{sub_atom('Banana', 2, 3, 1, 'anan')}.

	test(sics_sub_atom_5_26, false) :-
		{sub_atom('Banana', 0, 7, 0, _)}.

	test(sics_sub_atom_5_27, false) :-
		{sub_atom('Banana', 7, 0, 0, _)}.

	test(sics_sub_atom_5_28, false) :-
		{sub_atom('Banana', 0, 0, 7, _)}.

	% the following four tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- test(sics_sub_atom_5_29, true(A-S == 5-'ók')) :-
		{sub_atom('Bartók Béla', 4, 2, A, S)}.

	- test(sics_sub_atom_5_30, true(L-S == 2-'ók')) :-
		{sub_atom('Bartók Béla', 4, L, 5, S)}.

	- test(sics_sub_atom_5_31, true(B-S == 4-'ók')) :-
		{sub_atom('Bartók Béla', B, 2, 5, S)}.

	- test(sics_sub_atom_5_32, true(L == [0-2-'Pé', 1-1-'éc', 2-0-'cs'])) :-
		findall(B-A-S, {sub_atom('Pécs',B,2,A,S)}, L).

	test(sics_sub_atom_5_33, true(L == [0-4-7, 7-4-0])) :-
		findall(B-L-A, {sub_atom(abracadabra,B,L,A,abra)}, L).

	% tests from the Logtalk portability work

	test(lgt_sub_atom_5_34, true) :-
		forall({sub_atom('123', _, _, _, SA)}, {atom(SA)}).

	test(lgt_sub_atom_5_35, error(type_error(atom,2))) :-
		{sub_atom(2, _, _, _, _)}.

	test(lgt_sub_atom_5_36, error(type_error(atom,2.2))) :-
		{sub_atom(2.2, _, _, _, _)}.

	test(lgt_sub_atom_5_37, true) :-
		{sub_atom(abc, _, _, 0, c)}.

	test(lgt_sub_atom_5_38, false) :-
		{sub_atom(abc, _, _, 0, d)}.

	test(lgt_sub_atom_5_39, true(S-L == ''-0)) :-
		{sub_atom('', 0, L, 0, S)}.

	test(lgt_sub_atom_5_40, false) :-
		{sub_atom('', _, 1, _, _)}.

	test(lgt_sub_atom_5_41, true) :-
		{sub_atom('/abc/def/ghi/', 12, 1, 0, '/')}.

	test(lgt_sub_atom_5_42, true(Before == 12)) :-
		{sub_atom('/abc/def/ghi/', Before, 1, 0, '/')}.

	test(lgt_sub_atom_5_43, true(Length == 1)) :-
		{sub_atom('/abc/def/ghi/', 12, Length, 0, '/')}.

	test(lgt_sub_atom_5_44, true(Before-Length == 12-1)) :-
		{sub_atom('/abc/def/ghi/', Before, Length, 0, '/')}.

	test(lgt_sub_atom_5_45, deterministic) :-
		{sub_atom('/abc/def/ghi/', _, _, 0, '/')}.

	test(lgt_sub_atom_5_46, deterministic) :-
		{sub_atom('/abc/def/ghi/', 0, _, _, '/')}.

	test(lgt_sub_atom_5_47, deterministic) :-
		{sub_atom('/abc/def/ghi/', 1, 3, _, _)}.

	test(lgt_sub_atom_5_48, deterministic) :-
		{sub_atom('/abc/def/ghi/', 1, _, 3, _)}.

	test(lgt_sub_atom_5_49, deterministic) :-
		{sub_atom('/abc/def/ghi/', _, 1, 3, _)}.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/09,
		comment is 'Unit tests for the ISO Prolog standard integer/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.3.4

	succeeds(iso_sub_atom_5_01) :-
		{sub_atom(abracadabra, 0, 5, _, S2)},
		S2 == 'abrac'.

	succeeds(iso_sub_atom_5_02) :-
		{sub_atom(abracadabra, _, 5, 0, S2)},
		S2 == 'dabra'.

	succeeds(iso_sub_atom_5_03) :-
		{sub_atom(abracadabra, 3, Length, 3, S2)},
		Length == 5, S2 == 'acada'.

	succeeds(iso_sub_atom_5_04) :-
		findall(Before-After, {sub_atom(abracadabra,Before,2,After,ab)}, L),
		L == [0-9, 7-2].

	succeeds(iso_sub_atom_5_05) :-
		{sub_atom('Banana', 3, 2, _, S2)},
		S2 == 'an'.

	succeeds(iso_sub_atom_5_06) :-
		findall(S2, {sub_atom('charity',_,3,_,S2)}, L),
		L == ['cha', 'har', 'ari', 'rit', 'ity'].

	succeeds(iso_sub_atom_5_07) :-
		findall(Start-Lenght-SubAtom, {sub_atom('ab',Start,Lenght,_,SubAtom)}, L),
		L == [0-0-'', 0-1-'a', 0-2-'ab', 1-0-'', 1-1-'b', 2-0-''].

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_sub_atom_5_08, error(instantiation_error,_)) :-
		{sub_atom(_Banana, 3, 2, _, _S)}.

	throws(eddbali_sub_atom_5_09, error(type_error(atom,f(a)),_)) :-
		{sub_atom(f(a), 2, 2, _, _S2)}.

	throws(eddbali_sub_atom_5_10, error(type_error(atom,2),_)) :-
		{sub_atom('Banana', 4, 2, _, 2)}.

	throws(eddbali_sub_atom_5_11, error(type_error(integer,a),_)) :-
		{sub_atom('Banana', a, 2, _, _)}.

	throws(eddbali_sub_atom_5_12, error(type_error(integer,n),_)) :-
		{sub_atom('Banana', 4, n, _, _)}.

	throws(eddbali_sub_atom_5_13, error(type_error(integer,m),_)) :-
		{sub_atom('Banana', 4, _, m, _)}.

	throws(sics_sub_atom_5_14, error(domain_error(not_less_than_zero,-2),_)) :-
		{sub_atom('Banana', -2, 3, 4,_)}.

	throws(sics_sub_atom_5_15, error(domain_error(not_less_than_zero,-3),_)) :-
		{sub_atom('Banana', 2, -3, 4,_)}.

	throws(sics_sub_atom_5_16, error(domain_error(not_less_than_zero,-4),_)) :-
		{sub_atom('Banana', 2, 3, -4,_)}.

	succeeds(sics_sub_atom_5_17) :-
		{sub_atom('Banana', 2, 3, A, 'nan')},
		A == 1.

	succeeds(sics_sub_atom_5_18) :-
		{sub_atom('Banana', B, 3, 1, 'nan')},
		B == 2.

	succeeds(sics_sub_atom_5_19) :-
		{sub_atom('Banana', 2, L, 1, 'nan')},
		L == 3.

	succeeds(sics_sub_atom_5_20) :-
		{sub_atom('Banana', 2, L, A, 'nan')},
		A == 1, L == 3.

	succeeds(sics_sub_atom_5_21) :-
		{sub_atom('Banana', B, L, 1, 'nan')},
		B == 2, L == 3.

	fails(sics_sub_atom_5_22) :-
		{sub_atom('Banana', 2, 3, 1, 'ana')}.

	fails(sics_sub_atom_5_23) :-
		{sub_atom('Banana', 2, 3, 2, 'nan')}.

	fails(sics_sub_atom_5_24) :-
		{sub_atom('Banana', 2, 3, 2, _)}.

	fails(sics_sub_atom_5_25) :-
		{sub_atom('Banana', 2, 3, 1, 'anan')}.

	fails(sics_sub_atom_5_26) :-
		{sub_atom('Banana', 0, 7, 0, _)}.

	fails(sics_sub_atom_5_27) :-
		{sub_atom('Banana', 7, 0, 0, _)}.

	fails(sics_sub_atom_5_28) :-
		{sub_atom('Banana', 0, 0, 7, _)}.

	% the following four tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- succeeds(sics_sub_atom_5_31) :-
		{sub_atom('Bartók Béla', 4, 2, A, S)},
		A == 5, S == 'ók'.

	- succeeds(sics_sub_atom_5_32) :-
		{sub_atom('Bartók Béla', 4, L, 5, S)},
		L == 2, S == 'ók'.

	- succeeds(sics_sub_atom_5_33) :-
		{sub_atom('Bartók Béla', B, 2, 5, S)},
		B == 4, S == 'ók'.

	- succeeds(sics_sub_atom_5_34) :-
		findall(B-A-S, {sub_atom('Pécs',B,2,A,S)}, L),
		L == [0-2-'Pé', 1-1-'éc', 2-0-'cs'].

	succeeds(sics_sub_atom_5_35) :-
		findall(B-L-A, {sub_atom(abracadabra,B,L,A,abra)}, L),
		L == [0-4-7, 7-4-0].

	% tests from the Logtalk portability work

	succeeds(lgt_sub_atom_5_36) :-
		forall({sub_atom('123', _, _, _, SA)}, {atom(SA)}).

:- end_object.

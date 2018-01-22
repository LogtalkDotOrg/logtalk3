%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/18,
		comment is 'Unit tests for the ISO Prolog standard atom_chars/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.4.4

	succeeds(iso_atom_chars_2_01) :-
		{atom_chars('', L)},
		L == [].

	succeeds(iso_atom_chars_2_02) :-
		{atom_chars([], L)},
		L == ['[',']'].

	succeeds(iso_atom_chars_2_03) :-
		{atom_chars('''', L)},
		L == [''''].

	succeeds(iso_atom_chars_2_04) :-
		{atom_chars('ant', L)},
		L == ['a','n','t'].

	succeeds(iso_atom_chars_2_05) :-
		{atom_chars(Str, ['s','o','p'])},
		Str == 'sop'.

	succeeds(iso_atom_chars_2_06) :-
		{atom_chars('North', ['N'| X])},
		X == ['o','r','t','h'].

	fails(iso_atom_chars_2_07) :-
		{atom_chars('soap', ['s','o','p'])}.

	throws(iso_atom_chars_2_08, error(instantiation_error,_)) :-
		{atom_chars(_X, _Y)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_atom_chars_2_09, error(instantiation_error,_)) :-
		{atom_chars(_A, [a,_E,c])}.

	throws(eddbali_atom_chars_2_10, error(instantiation_error,_)) :-
		{atom_chars(_A, [a,b|_L])}.

	throws(eddbali_atom_chars_2_11, error(type_error(atom,f(a)),_)) :-
		{atom_chars(f(a), _L)}.

	throws(eddbali_atom_chars_2_12, error(type_error(list,iso),_)) :-
		{atom_chars(_A, iso)}.

	throws(eddbali_atom_chars_2_13, error(type_error(character,f(b)),_)) :-
		{atom_chars(_A, [a,f(b)])}.

	% the following two tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- succeeds(sics_atom_chars_2_14) :-
		{atom_chars('Pécs', L)},
		L == ['P','é','c','s'].

	- succeeds(sics_atom_chars_2_15) :-
		{atom_chars(A, ['P','é','c','s'])},
		A == 'Pécs'.

	% tests from the Logtalk portability work

	throws(lgt_atom_chars_2_16, error(type_error(character,1),_)) :-
		{atom_chars(abc, [1,2,3])}.

	succeeds(lgt_atom_codes_2_17) :-
		{atom_chars('', Chars)},
		Chars == [].

	succeeds(lgt_atom_codes_2_18) :-
		{atom_chars(Atom, [])},
		Atom == ''.

	succeeds(lgt_atom_codes_2_19) :-
		{atom_chars('', [])}.

:- end_object.

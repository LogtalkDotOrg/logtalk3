%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2015-04-18,
		comment is 'Unit tests for the ISO Prolog standard atom_codes/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.5.4

	succeeds(iso_atom_codes_2_01) :-
		{atom_codes('', L)},
		L == [].

	succeeds(iso_atom_codes_2_02) :-
		{atom_codes([], L)},
		L == [0'[, 0']].

	succeeds(iso_atom_codes_2_03) :-
		{atom_codes('''', L)},
		L == [39].

	succeeds(iso_atom_codes_2_04) :-
		{atom_codes('ant', L)},
		L == [0'a, 0'n, 0't].

	succeeds(iso_atom_codes_2_05) :-
		{atom_codes(Str, [0's,0'o,0'p])},
		Str == 'sop'.

	succeeds(iso_atom_codes_2_06) :-
		{atom_codes('North', [0'N| X])},
		X == [0'o,0'r,0't,0'h].

	fails(iso_atom_codes_2_07) :-
		{atom_codes('soap', [0's, 0'o, 0'p])}.

	throws(iso_atom_codes_2_08, error(instantiation_error,_)) :-
		{atom_codes(_X, _Y)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_atom_codes_2_09, error(type_error(atom,f(a)),_)) :-
		{atom_codes(f(a), _L)}.

	throws(eddbali_atom_codes_2_10, error(type_error(list,0'x),_)) :-
		{atom_codes(_, 0'x)}.

	throws(eddbali_atom_codes_2_11, error(representation_error(character_code),_)) :-
		{atom_codes(_A, [0'i,0's,-1])}.

	% tests from the Logtalk portability work

	throws(lgt_atom_codes_2_12, error(type_error(integer,a),_)) :-
		{atom_codes(abc, [a,b,c])}.

	succeeds(lgt_atom_codes_2_13) :-
		{atom_codes('', Codes)},
		Codes == [].

	succeeds(lgt_atom_codes_2_14) :-
		{atom_codes(Atom, [])},
		Atom == ''.

	succeeds(lgt_atom_codes_2_15) :-
		{atom_codes('', [])}.

	% the following two tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers
	% 
	% they also result in a syntax error with several Prolog compilers

%	succeeds(sics_atom_codes_2_12) :-
%		{atom_codes('Pécs', C)},
%		C == [0'P,0'é,0'c,0's].
%
%	succeeds(sics_atom_codes_2_13) :-
%		{atom_codes(A, [0'P,0'é,0'c,0's])},
%		A == 'Pécs'.

:- end_object.

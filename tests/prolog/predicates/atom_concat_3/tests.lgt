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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014-10-14,
		comment is 'Unit tests for the ISO Prolog standard atom_concat/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.2.4

	succeeds(iso_atom_concat_3_01) :-
		{atom_concat('hello', ' world', S3)},
		S3 == 'hello world'.

	succeeds(iso_atom_concat_3_02) :-
		{atom_concat('hello', ' world', S3)},
		S3 == 'hello world'.

	fails(iso_atom_concat_3_03) :-
		{atom_concat('hello',' world', 'small world')}.

	succeeds(iso_atom_concat_3_04) :-
		findall(T1-T2, {atom_concat(T1, T2, 'hello')}, L),
		L == [''-'hello', 'h'-'ello', 'he'-'llo', 'hel'-'lo', 'hell'-'o', 'hello'-''].

	throws(iso_atom_concat_3_05, error(instantiation_error,_)) :-
		{atom_concat(small, _V2, _V4)}.

	throws(eddbali_atom_concat_3_06, error(instantiation_error,_)) :-
		{atom_concat(_A, 'iso', _C)}.

	throws(eddbali_atom_concat_3_07, error(instantiation_error,_)) :-
		{atom_concat('iso', _B, _C)}.

	throws(eddbali_atom_concat_3_08, error(type_error(atom,f(a)),_)) :-
		{atom_concat(f(a), 'iso', _C)}.

	throws(eddbali_atom_concat_3_09, error(type_error(atom,f(a)),_)) :-
		{atom_concat('iso', f(a), _C)}.

	throws(eddbali_atom_concat_3_10, error(type_error(atom,f(a)),_)) :-
		{atom_concat(_A, _B, f(a))}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	% the following four tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- succeeds(sics_atom_concat_3_11) :-
		{atom_concat('Bartók ', 'Béla', N)},
		N == 'Bartók Béla'.

	- succeeds(sics_atom_concat_3_12) :-
		{atom_concat(N, 'Béla', 'Bartók Béla')},
		N == 'Bartók '.

	- succeeds(sics_atom_concat_3_13) :-
		{atom_concat('Bartók ', N, 'Bartók Béla')},
		N == 'Béla'.

	- succeeds(sics_atom_concat_3_14) :-
		findall(T1-T2, {atom_concat(T1, T2, 'Pécs')}, L),
		L == [''-'Pécs', 'P'-'écs', 'Pé'-'cs', 'Péc'-'s', 'Pécs'-''].

:- end_object.

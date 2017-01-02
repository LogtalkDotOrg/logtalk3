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
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard atom_length/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.1.4

	succeeds(iso_atom_length_2_01) :-
		{atom_length('enchanted evening', N)},
		N == 17.

	succeeds(iso_atom_length_2_02) :-
		{atom_length('enchanted\
 evening', N)},
		N == 17.

	succeeds(iso_atom_length_2_03) :-
		{atom_length('', N)},
		N == 0.

	fails(iso_atom_length_2_04) :-
		{atom_length('scarlet', 5)}.

	throws(iso_atom_length_2_05, error(instantiation_error,_)) :-
		{atom_length(_Atom, 4)}.

	throws(iso_atom_length_2_06, error(type_error(atom,1.23),_)) :-
		{atom_length(1.23, 4)}.

	throws(iso_atom_length_2_07, error(type_error(integer,'4'),_)) :-
		{atom_length(atom, '4')}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_atom_length_2_08, error(domain_error(not_less_than_zero,-4),_)) :-
		{atom_length(atom, -4)}.

	% the following test is disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8
	% for all Logtalk supported backend Prolog compilers

	- succeeds(sics_atom_length_2_09) :-
		{atom_length('Bartók Béla', L)},
		L == 11.

:- end_object.

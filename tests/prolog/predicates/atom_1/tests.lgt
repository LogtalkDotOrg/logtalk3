%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard atom/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.2.4

	succeeds(iso_atom_1_01) :-
		{atom(atom)}.

	succeeds(iso_atom_1_02) :-
		{atom('string')}.

	fails(iso_atom_1_03) :-
		{atom(a(b))}.

	fails(iso_atom_1_04) :-
		{atom(_Var)}.

	succeeds(iso_atom_1_05) :-
		{atom([])}.

	fails(iso_atom_1_06) :-
		{atom(6)}.

	fails(iso_atom_1_07) :-
		{atom(3.3)}.

	% tests from the Logtalk portability work

	succeeds(lgt_atom_1_08) :-
		{atom(!)}.

	succeeds(lgt_atom_1_09) :-
		{atom({})}.

:- end_object.

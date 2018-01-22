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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard atomic/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.5.4

	succeeds(iso_atomic_1_01) :-
		{atomic(atom)}.

	fails(iso_atomic_1_02) :-
		{atomic(a(b))}.

	fails(iso_atomic_1_03) :-
		{atomic(_Var)}.

	succeeds(iso_atomic_1_04) :-
		{atomic(6)}.

	succeeds(iso_atomic_1_05) :-
		{atomic(3.3)}.

	% tests from the Logtalk portability work

	succeeds(lgt_atomic_1_06) :-
		{atomic([])}.

	succeeds(lgt_atomic_1_07) :-
		{atom(!)}.

	succeeds(lgt_atomic_1_08) :-
		{atom({})}.

:- end_object.

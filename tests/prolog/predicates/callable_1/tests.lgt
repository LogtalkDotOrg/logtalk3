%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the ISO Prolog standard callable/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.3.9.4

	succeeds(iso_callable_1_01) :-
		{callable(a)}.

	fails(iso_callable_1_02) :-
		{callable(3)}.

	fails(iso_callable_1_03) :-
		{callable(_X)}.

	succeeds(iso_callable_1_04) :-
		{callable((1,2))}.

	% tests from the Logtalk portability work

	succeeds(lgt_callable_1_05) :-
		{callable([])}.

	succeeds(lgt_callable_1_06) :-
		{callable([a])}.

	succeeds(lgt_callable_1_07) :-
		{callable({})}.

	succeeds(lgt_callable_1_08) :-
		{callable({a})}.

	% tests from the ECLiPSe test suite

	succeeds(eclipse_callable_1_09) :-
		{callable(f(b))}.

	fails(eclipse_callable_1_10) :-
		{callable(3.1)}.

:- end_object.

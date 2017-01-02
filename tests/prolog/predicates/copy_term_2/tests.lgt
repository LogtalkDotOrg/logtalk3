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
		date is 2014/11/21,
		comment is 'Unit tests for the ISO Prolog standard copy_term/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.4.4

	succeeds(iso_copy_term_2_01) :-
		{copy_term(_X, _Y)}.

	succeeds(iso_copy_term_2_02) :-
		{copy_term(_X, 3)}.

	succeeds(iso_copy_term_2_03) :-
		{copy_term(_, a)}.

	succeeds(iso_copy_term_2_04) :-
		{copy_term(a+X, X+b)},
		X == a.

	succeeds(iso_copy_term_2_05) :-
		{copy_term(_, _)}.

	succeeds(iso_copy_term_2_06) :-
		{copy_term(X+X+_Y, A+B+B)},
		A == B.

	fails(iso_copy_term_2_07) :-
		{copy_term(a, b)}.

	fails(iso_copy_term_2_08) :-
		{copy_term(a+X,X+b), copy_term(a+X,X+b)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_copy_term_2_09) :-
			{copy_term(demoen(X,X), demoen(Y,f(Y)))}.
	:- else.
		- succeeds(iso_copy_term_2_09) :-
			% STO; Undefined
			{copy_term(demoen(X,X), demoen(Y,f(Y)))}.
	:- endif.

:- end_object.

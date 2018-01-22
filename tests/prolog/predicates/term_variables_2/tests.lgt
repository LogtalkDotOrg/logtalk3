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
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard term_variables/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.5.5.4

	succeeds(iso_term_variables_2_01) :-
		{term_variables(t, Vars)},
		Vars == [].

	succeeds(iso_term_variables_2_02) :-
		{term_variables(A+B*C/B-D, Vars)},
		Vars == [A, B, C, D].

	throws(iso_term_variables_2_03, error(type_error(list,[_,_|a]),_)) :-
		{term_variables(t, [_, _|a])}.

	succeeds(iso_term_variables_2_04) :-
		{S=B+T, T=A*B, term_variables(S, Vars)},
		Vars == [B, A], T == A*B, S == B+A*B.

	succeeds(iso_term_variables_2_05) :-
		{T=A*B, S=B+T, term_variables(S, Vars)},
		Vars == [B, A], T == A*B, S == B+A*B.

	succeeds(iso_term_variables_2_06) :-
		{term_variables(A+B+B, [B|Vars])},
		A == B, Vars == [B].

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_term_variables_2_07) :-
			{term_variables(_X+Vars, Vars), Vars = [_, _]}.
	:- else.
		- succeeds(iso_term_variables_2_07) :-
			% STO; Undefined.
			{term_variables(_X+Vars, Vars), Vars = [_, _]}.
	:- endif.

	% tests from the ECLiPSe test suite

	throws(eclipse_term_variables_2_08, error(type_error(list,3),_)) :-
		{term_variables(foo, 3)}.

	throws(eclipse_term_variables_2_09, error(type_error(list,[a|b]),_)) :-
		{term_variables(foo, [a|b])}.

	succeeds(eclipse_term_variables_2_10) :-
		{term_variables(foo(X,Y,X,Z), Vs)},
		Vs == [X,Y,Z].

:- end_object.

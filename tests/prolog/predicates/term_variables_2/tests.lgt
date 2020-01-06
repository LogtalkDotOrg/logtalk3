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
		version is 1.3,
		author is 'Paulo Moura',
		date is 2020/01/06,
		comment is 'Unit tests for the ISO Prolog standard term_variables/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.5.5.4

	test(iso_term_variables_2_01, true(Vars == [])) :-
		{term_variables(t, Vars)}.

	test(iso_term_variables_2_02, true(Vars == [A, B, C, D])) :-
		{term_variables(A+B*C/B-D, Vars)}.

	test(iso_term_variables_2_03, error(type_error(list,[_,_|a]))) :-
		{term_variables(t, [_, _|a])}.

	test(iso_term_variables_2_04, true) :-
		{S=B+T, T=A*B, term_variables(S, Vars)},
		^^assertion(Vars == [B, A]),
		^^assertion(T == A*B),
		^^assertion(S == B+A*B).

	test(iso_term_variables_2_05, true) :-
		{T=A*B, S=B+T, term_variables(S, Vars)},
		^^assertion(Vars == [B, A]),
		^^assertion(T == A*B),
		^^assertion(S == B+A*B).

	test(iso_term_variables_2_06, true) :-
		{term_variables(A+B+B, [B|Vars])},
		^^assertion(A == B),
		^^assertion(Vars == [B]).

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		test(iso_term_variables_2_07, true(Vars = [_, _])) :-
			{term_variables(_X+Vars, Vars)}.
	:- else.
		- test(iso_term_variables_2_07, true(Vars = [_, _])) :-
			% STO; Undefined.
			{term_variables(_X+Vars, Vars)}.
	:- endif.

	% tests from the ECLiPSe test suite

	test(eclipse_term_variables_2_08, error(type_error(list,3))) :-
		{term_variables(foo, 3)}.

	test(eclipse_term_variables_2_09, error(type_error(list,[a|b]))) :-
		{term_variables(foo, [a|b])}.

	test(eclipse_term_variables_2_10, true(Vs == [X,Y,Z])) :-
		{term_variables(foo(X,Y,X,Z), Vs)}.

	% tests from the Logtalk portability work

	test(lgt_term_variables_2_11, true(Vs == [Z])) :-
		{term_variables([Z,Z], Vs)}.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2022-04-06,
		comment is 'Unit tests for the ISO Prolog standard subsumes_term/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.2.4.4

	succeeds(iso_subsumes_term_2_01) :-
		{subsumes_term(a, a)}.

	succeeds(iso_subsumes_term_2_02) :-
		{subsumes_term(f(_X,_Y), f(Z,Z))}.

	fails(iso_subsumes_term_2_03) :-
		{subsumes_term(f(Z,Z), f(_X,_Y))}.

	fails(iso_subsumes_term_2_04) :-
		{subsumes_term(g(X), g(f(X)))}.

	fails(iso_subsumes_term_2_05) :-
		{subsumes_term(X, f(X))}.

	succeeds(iso_subsumes_term_2_06) :-
		{subsumes_term(X, Y), subsumes_term(Y, f(X))}.

	% tests from the Logtalk portability work

	succeeds(lgt_subsumes_term_2_07) :-
		{subsumes_term(A, A)}.

	succeeds(lgt_subsumes_term_2_08) :-
		{subsumes_term(t(A,B), t(A,B))}.

	succeeds(lgt_subsumes_term_2_09) :-
		{subsumes_term(A-B, A-B)}.

	succeeds(lgt_subsumes_term_2_10) :-
		{subsumes_term(c(A, [e(B)]), c(A, [e(B)]))}.

	succeeds(lgt_subsumes_term_2_11) :-
		{subsumes_term(_A, _B)}.

	succeeds(lgt_subsumes_term_2_12) :-
		{subsumes_term(t(_A,_B), t(_C,_D))}.

	succeeds(lgt_subsumes_term_2_13) :-
		{subsumes_term(_A-_B, _C-_D)}.

	succeeds(lgt_subsumes_term_2_14) :-
		{subsumes_term(c(_A, [e(_B)]), c(_C, [e(_D)]))}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		succeeds(lgt_subsumes_term_2_15) :-
			X = f(X),
			{subsumes_term(f(_), f(X))}.

		fails(lgt_subsumes_term_2_16) :-
			X = f(X),
			{subsumes_term(f(X), f(_))}.

		succeeds(lgt_subsumes_term_2_17) :-
			X = f(X),
			{subsumes_term(X, X)}.

	:- else.

		- succeeds(lgt_subsumes_term_2_15) :-
			% STO; Undefined.
			X = f(X),
			{subsumes_term(f(_), f(X))}.

		- fails(lgt_subsumes_term_2_16) :-
			% STO; Undefined.
			X = f(X),
			{subsumes_term(f(X), f(_))}.

		- succeeds(lgt_subsumes_term_2_17) :-
			% STO; Undefined.
			X = f(X),
			{subsumes_term(X, X)}.

	:- endif.

:- end_object.

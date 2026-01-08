%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2023-04-10,
		comment is 'Unit tests for the ISO Prolog standard (\\=)/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.2.3.4

	test(iso_not_unifiable_2_01, false) :-
		{'\\='(1, 1)}.

	test(iso_not_unifiable_2_02, false) :-
		{\=(_X, 1)}.

	test(iso_not_unifiable_2_03, false) :-
		{'\\='(_X, _Y)}.

	test(iso_not_unifiable_2_04, false) :-
		{\=(_, _)}.

	test(iso_not_unifiable_2_05, false) :-
		{\=(f(_X,def), f(def,_Y))}.

	test(iso_not_unifiable_2_06, true) :-
		{'\\='(1, 2)}.

	test(iso_not_unifiable_2_07, true) :-
		{\=(1, 1.0)}.

	test(iso_not_unifiable_2_08, true) :-
		{'\\='(g(X), f(f(X)))}.

	test(iso_not_unifiable_2_09, true) :-
		{\=(f(X,1), f(a(X)))}.

	test(iso_not_unifiable_2_10, true) :-
		{'\\='(f(X,Y,X), f(a(X),a(Y),Y,2))}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_not_unifiable_2_11, false) :-
			{\=(X, a(X))}.

		test(iso_not_unifiable_2_12, true) :-
			{'\\='(f(X,1), f(a(X),2))}.

		test(iso_not_unifiable_2_13, true) :-
			{'\\='(f(1,X,1), f(2,a(X),2))}.

		test(iso_not_unifiable_2_14, true) :-
			{\=(f(1,X), f(2,a(X)))}.

		test(iso_not_unifiable_2_15, true) :-
			{'\\='(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.

	:- else.

		- test(iso_not_unifiable_2_11, false, [note('STO')]) :-
			% STO; Undefined
			{\=(X, a(X))}.

		- test(iso_not_unifiable_2_12, true, [note('STO')]) :-
			% STO; Undefined
			{'\\='(f(X,1), f(a(X),2))}.

		- test(iso_not_unifiable_2_13, true, [note('STO')]) :-
			% STO; Undefined
			{'\\='(f(1,X,1), f(2,a(X),2))}.

		- test(iso_not_unifiable_2_14, true, [note('STO')]) :-
			% STO; Undefined
			{\=(f(1,X), f(2,a(X)))}.

		- test(iso_not_unifiable_2_15, true, [note('STO')]) :-
			% STO; Undefined
			{'\\='(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.

	:- endif.

:- end_object.

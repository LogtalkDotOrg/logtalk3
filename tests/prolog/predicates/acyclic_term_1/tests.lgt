%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2023-04-10,
		comment is 'Unit tests for the ISO Prolog standard acyclic_term/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.3.11.4

	succeeds(iso_acyclic_term_1_01) :-
		{acyclic_term(a(1, _))}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_acyclic_term_1_02, false) :-
			{X = f(X), acyclic_term(X)}.

		test(lgt_acyclic_term_1_03, false) :-
			{X = [_| X], acyclic_term(X)}.

	:- else.

		- test(iso_acyclic_term_1_02, false, [note('STO')]) :-
			% STO; Undefined
			{X = f(X), acyclic_term(X)}.

		- test(lgt_acyclic_term_1_03, false, [note('STO')]) :-
			% STO; Undefined
			{X = [_| X], acyclic_term(X)}.

	:- endif.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(dif).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2023-10-02,
		comment is 'Provides dif/2 and derived predicates.',
		remarks is [
			'Supported backend Prolog systems' - 'B-Prolog, ECLiPSe, SICStus Prolog, SWI-Prolog, Trealla Prolog, and YAP.'
		]
	]).

	:- public(dif/2).
	:- mode(dif(+term, +term), zero_or_one).
	:- info(dif/2, [
		comment is 'Sets a constraint that is true iff the two terms are different.',
		argnames is ['Term1', 'Term2']
	]).

	:- public(dif/1).
	:- mode(dif(+list(term)), zero_or_one).
	:- info(dif/1, [
		comment is 'Sets a set of constraints that are true iff all terms in a list are different.',
		argnames is ['Terms']
	]).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		dif(Term1, Term2) :-
			{dif(Term1, Term2)}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		dif(Term1, Term2) :-
			sicstus:dif(Term1, Term2).

	:- elif(current_logtalk_flag(prolog_dialect, arriba)).

		dif(Term1, Term2) :-
			user::dif(Term1, Term2).

	:- elif((current_logtalk_flag(prolog_dialect, lvm);current_logtalk_flag(prolog_dialect, arriba))).

		dif(Term1, Term2) :-
			user::dif(Term1, Term2).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		dif(Term1, Term2) :-
			user:dif(Term1, Term2).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		dif(Term1, Term2) :-
			dif:dif(Term1, Term2).

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

		dif(Term1, Term2) :-
			dif:dif(Term1, Term2).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		dif(Term1, Term2) :-
			constraintLib:when(?=(Term1, Term2), Term1 \== Term2).

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		dif(Term1, Term2) :-
			user:dif(Term1, Term2).

	:- endif.

	dif([]).
	dif([Term | Terms]) :-
		dif_(Terms, Term),
		dif(Terms).

	dif_([], _).
	dif_([Next| Terms], Term) :-
		dif(Term, Next),
		dif_(Terms, Term).

:- end_object.

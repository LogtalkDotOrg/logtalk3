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


:- object(coroutining).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2019/11/28,
		comment is 'Coroutining predicates.',
		remarks is [
			'Supported backend Prolog systems' - 'ECLiPSe, SICStus Prolog, SWI-Prolog, and YAP.'
		]
	]).

	:- public(dif/2).
	:- mode(dif(+term, +term), zero_or_one).
	:- info(dif/2, [
		comment is 'Sets a constraint that is true iff the two terms are different.',
		argnames is ['Term1', 'Term2']
	]).

	:- public(freeze/2).
	:- meta_predicate(freeze(*,0)).
	:- mode(freeze(+term, +callable), zero_or_more).
	:- info(freeze/2, [
		comment is 'Delays the execution of a goal until a variable is bound.',
		argnames is ['Variable', 'Goal']
	]).

	:- public(frozen/2).
	:- mode(frozen(@var, --callable), one).
	:- info(frozen/2, [
		comment is 'Unifies ``Goal`` with the goal delayed by ``Variable``. When no goals are frozen on ``Variable``, ``Goal`` is unified with true.',
		argnames is ['Variable', 'Goal']
	]).

	:- public(when/2).
	:- meta_predicate(when(*,0)).
	:- mode(when(+callable, +callable), zero_or_more).
	:- info(when/2, [
		comment is 'Calls ``Goal`` when ``Condition`` becomes true. The portable conditions are: ``nonvar/1``, ``ground/1``, ``(,)/2``, and ``(;)/2``.',
		argnames is ['Condition', 'Goal']
	]).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

		:- meta_predicate(sicstus:freeze(*, 0)).
		:- meta_predicate(sicstus:when(*, 0)).

		dif(Term1, Term2) :-
			sicstus:dif(Term1, Term2).

		freeze(Variable, Goal) :-
			sicstus:freeze(Variable, Goal).

		frozen(Variable, Goal) :-
			sicstus:frozen(Variable, Goal).

		when(Condition, Goal) :-
			sicstus:when(Condition, Goal).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		dif(Term1, Term2) :-
			user:dif(Term1, Term2).

		freeze(Variable, Goal) :-
			user:freeze(Variable, Goal).

		frozen(Variable, Goal) :-
			user:frozen(Variable, Goal).

		when(Condition, Goal) :-
			user:when(Condition, Goal).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		dif(Term1, Term2) :-
			dif:dif(Term1, Term2).

		freeze(Variable, Goal) :-
			user:freeze(Variable, Goal).

		frozen(Variable, Goal) :-
			user:frozen(Variable, Goal).

		when(Condition, Goal) :-
			when:when(Condition, Goal).

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		dif(Term1, Term2) :-
			user:dif(Term1, Term2).

		freeze(Variable, Goal) :-
			user:freeze(Variable, Goal).

		frozen(Variable, Goal) :-
			user:frozen(Variable, Goal).

		when(Condition, Goal) :-
			user:when(Condition, Goal).

	:- endif.

:- end_object.

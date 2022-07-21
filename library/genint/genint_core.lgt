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


:- category(genint_core).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2022-07-21,
		comment is 'Predicates for generating increasing non-negative integers. The predicates are declared as synchronized when the library is compiled using a backend supporting threads.'
	]).

	:- public(reset_genint/0).
	:- mode(reset_genint, one).
	:- info(reset_genint/0, [
		comment is 'Resets all counters.'
	]).

	:- public(reset_genint/1).
	:- mode(reset_genint(+atom), one).
	:- info(reset_genint/1, [
		comment is 'Resets the given counter.',
		argnames is ['Counter']
	]).

	:- public(genint/2).
	:- mode(genint(+atom, -non_negative_integer), one).
	:- info(genint/2, [
		comment is 'Returns the next integer for a given counter.',
		argnames is ['Counter', 'Integer']
	]).

	:- private(counter_/2).
	:- dynamic(counter_/2).
	:- mode(counter_(?atom, ?non_negative_integer), zero_or_more).
	:- info(counter_/2, [
		comment is 'Table of current state of counters.',
		argnames is ['Counter', 'Latest']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			reset_genint/0,
			reset_genint/1,
			genint/2
		]).
	:- endif.

	reset_genint :-
		retract(counter_(Counter, _)),
		asserta(counter_(Counter, -1)),
		fail.
	reset_genint.

	reset_genint(Counter) :-
		retractall(counter_(Counter, _)),
		asserta(counter_(Counter, -1)).

	genint(Counter, Next) :-
		(	retract(counter_(Counter, Latest)) ->
			Next is Latest + 1
		;	Next is 0
		),
		asserta(counter_(Counter, Next)).

:- end_category.

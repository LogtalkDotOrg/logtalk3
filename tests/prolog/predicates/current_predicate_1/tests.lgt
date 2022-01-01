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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.8.2.4

:- dynamic(cat/0).
cat.

:- dynamic(dog/0).
dog :- true.

elk(X) :- moose(X).

:- dynamic(insect/1).
insect(ant).
insect(bee).

% declared predicates with no clauses must also be reported

:- dynamic(unicorn/0).

:- multifile(fenix/1).

:- discontiguous(scattered/2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2020-11-18,
		comment is 'Unit tests for the ISO Prolog standard current_predicate/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.8.2.4

	succeeds(iso_current_predicate_1_01) :-
		{current_predicate(dog/0)}.

	fails(iso_current_predicate_1_02) :-
		{current_predicate(current_predicate/1)}.

	succeeds(iso_current_predicate_1_03) :-
		{current_predicate(elk/Arity)},
		Arity == 1.

	fails(iso_current_predicate_1_04) :-
		{current_predicate(foo/_A)}.

	succeeds(iso_current_predicate_1_05) :-
		findall(Name, {current_predicate(Name/1)}, Names),
		memberchk(elk, Names), memberchk(insect, Names).

	throws(iso_current_predicate_1_06, error(type_error(predicate_indicator,4),_)) :-
		{current_predicate(4)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_current_predicate_1_07, error(type_error(predicate_indicator,dog),_)) :-
		{current_predicate(dog)}.

	throws(eddbali_current_predicate_1_08, [error(type_error(predicate_indicator,0/dog),_), error(type_error(atom,0),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(0/dog)}.

	% tests from the ECLiPSe test suite

	throws(eclipse_current_predicate_1_09, [error(type_error(predicate_indicator,3/3),_), error(type_error(atom,3),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(3/3)}.

	throws(eclipse_current_predicate_1_10, [error(type_error(predicate_indicator,f/f),_), error(type_error(integer,f),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(f/f)}.

	throws(eclipse_current_predicate_1_11, [error(type_error(predicate_indicator,f/ -1),_), error(domain_error(not_less_than_zero,-1),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(f/ -1)}.

	throws(eclipse_current_predicate_1_12, error(type_error(predicate_indicator,a+1),_)) :-
		{current_predicate(a+1)}.

	% tests from the Logtalk portability work

	succeeds(lgt_current_predicate_1_13) :-
		{current_predicate(unicorn/0)}.

	succeeds(lgt_current_predicate_1_14) :-
		{current_predicate(fenix/1)}.

	succeeds(lgt_current_predicate_1_15) :-
		{current_predicate(scattered/2)}.

	succeeds(iso_current_predicate_1_16) :-
		findall(Name, {current_predicate(Name/0)}, Names),
		memberchk(unicorn, Names).

	succeeds(iso_current_predicate_1_17) :-
		findall(Name, {current_predicate(Name/1)}, Names),
		memberchk(fenix, Names).

	succeeds(iso_current_predicate_1_18) :-
		findall(Name, {current_predicate(Name/2)}, Names),
		memberchk(scattered, Names).

	% avoid library dependencies
	memberchk(Element, [Head| _]) :-
		Element == Head,
		!.
	memberchk(Element, [_| Tail]) :-
		nonvar(Tail),
		memberchk(Element, Tail).

:- end_object.

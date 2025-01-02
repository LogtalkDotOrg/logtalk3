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


:- object(mutations_store,
	implements(expanding)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-11-23,
		comment is 'Stores mutation definitions for selected types. User extensible by defining objects or categories defining clauses for the ``mutation/3`` predicate and using this object as a hook object for their compilation.',
		see_also is [type]
	]).

	:- uses(fast_random, [
		between/3
	]).

	:- public(mutation/3).
	:- mode(mutation(@callable, @term, -term), one).
	:- info(mutation/3, [
		comment is 'Returns a random mutation of a term into another term of the same type. The input ``Term`` is assumed to be valid for the given ``Type``.',
		argnames is ['Type', 'Term', 'Mutation']
	]).

	:- public(counter/2).
	:- mode(counter(?callable, ?positive_integer), zero_or_more).
	:- info(counter/2, [
		comment is 'Table of the number of mutations available per type.',
		argnames is ['Type', 'N']
	]).

	:- private(mutation/4).
	:- multifile(mutation/4).
	:- mode(mutation(?callable, ?positive_integer, @term, -term), zero_or_more).
	:- info(mutation/4, [
		comment is 'Returns a random mutation of a term into another term of the same type using mutator ``N``. The input ``Term`` is assume to be valid for the given ``Type``.',
		argnames is ['Type', 'N', 'Term', 'Mutation']
	]).

	:- private(counter_/2).
	:- dynamic(counter_/2).
	:- mode(counter_(?callable, ?positive_integer), zero_or_more).
	:- info(counter_/2, [
		comment is 'Internal counter for the number of mutations available for a given type.',
		argnames is ['Type', 'N']
	]).

	mutation(Type, Term, Mutation) :-
		counter_(Type, N),
		repeat,
			between(1, N, Random),
			mutation(Type, Random, Term, Mutation),
		!.

	counter(Type, N) :-
		counter_(Type, N).

	term_expansion((:- Directive), [(:- Directive), (:- multifile(mutations_store::mutation/4))]) :-
		callable(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		).
	term_expansion((mutation(Type, Atom, Mutation) :- Body), (mutations_store::mutation(Type, N, Atom, Mutation) :- Body)) :-
		(	retract(counter_(Type, M)) ->
			N is M + 1
		;	N is 1
		),
		assertz(counter_(Type, N)).
	term_expansion(mutation(Type, Atom, Mutation), mutations_store::mutation(Type, N, Atom, Mutation)) :-
		(	retract(counter_(Type, M)) ->
			N is M + 1
		;	N is 1
		),
		assertz(counter_(Type, N)).

:- end_object.

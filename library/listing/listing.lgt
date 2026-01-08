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


:- category(listing).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-01-26,
		comment is 'Listing predicates.'
	]).

	:- public(listing/0).
	:- mode(listing, one).
	:- info(listing/0, [
		comment is 'Lists all clauses of all visible dynamic predicates to the current output stream.'
	]).

	:- public(listing/1).
	:- mode(listing(+predicate_indicator), one_or_error).
	:- mode(listing(+non_terminal_indicator), one_or_error).
	:- mode(listing(+callable), one_or_error).
	:- info(listing/1, [
		comment is 'Lists all clauses of a visible dynamic predicate or non-terminal to the current output stream. When the argument is a clause head, lists all matching clauses.',
		argnames is ['Spec'],
		exceptions is [
			'``Spec`` is not ground' - instantiation_error,
			'``Spec`` is ground but not a valid predicate indicator' - type_error(predicate_indicator, 'Spec'),
			'``Spec`` is ground but not a valid non-terminal indicator' - type_error(non_terminal_indicator, 'Spec'),
			'``Spec`` is a predicate indicator but not a visible predicate' - existence_error(predicate, 'Spec'),
			'``Spec`` is a non-terminal indicator but not a visible non-terminal' - existence_error(non_terminal, 'Spec'),
			'``Spec`` is a callable term with a ``Functor/Arity`` indicator but not a visible predicate' - existence_error(predicate, 'Functor/Arity'),
			'``Spec`` is a predicate indicator of a visible predicate but not a dynamic predicate' - permission_error(access, predicate, 'Spec'),
			'``Spec`` is a non-terminal indicator of a visible non-terminal but not a dynamic non-terminal' - permission_error(access, non_terminal, 'Spec'),
			'``Spec`` is a callable term for a visible predicate with a ``Functor/Arity`` indicator but not a dynamic predicate' - permission_error(access, predicate, 'Functor/Arity')
		]
	]).

	:- public(portray_clause/1).
	:- mode(portray_clause(+clause), one).
	:- info(portray_clause/1, [
		comment is 'Pretty prints a clause to the current output stream.',
		argnames is ['Clause']
	]).

	listing :-
		::current_predicate(Functor/Arity),
		functor(Head, Functor, Arity),
		::predicate_property(Head, (dynamic)),
		listing_clauses(Head),
		fail.
	listing.

	listing(Predicate) :-
		var(Predicate),
		instantiation_error.
	listing(Functor/Arity) :-
		\+ ground(Functor/Arity),
		instantiation_error.
	listing(Functor//Arity) :-
		\+ ground(Functor//Arity),
		instantiation_error.
	listing(Predicate) :-
		\+ callable(Predicate),
		type_error(predicate_indicator, Predicate).
	listing(Functor/Arity) :-
		\+ (atom(Functor), integer(Arity), Arity >= 0),
		type_error(predicate_indicator, Functor/Arity).
	listing(Functor//Arity) :-
		\+ (atom(Functor), integer(Arity), Arity >= 0),
		type_error(non_terminal_indicator, Functor/Arity).
	listing(Functor/Arity) :-
		\+ ::current_predicate(Functor/Arity),
		existence_error(predicate, Functor/Arity).
	listing(Functor//Arity) :-
		ExtArity is Arity + 2,
		\+ ::current_predicate(Functor/ExtArity),
		existence_error(non_terminal, Functor//Arity).
	listing(Functor//Arity) :-
		ExtArity is Arity + 2,
		::current_predicate(Functor/ExtArity),
		functor(Head, Functor, ExtArity),
		\+ ::predicate_property(Head, non_terminal(Functor//Arity)),
		existence_error(non_terminal, Functor//Arity).
	listing(Head) :-
		Head \= _/_,
		Head \= _//_,
		functor(Head, Functor, Arity),
		\+ ::current_predicate(Functor/Arity),
		existence_error(predicate, Functor/Arity).
	listing(Functor/Arity) :-
		!,
		functor(Head, Functor, Arity),
		(	::predicate_property(Head, (dynamic)) ->
			listing_clauses(Head)
		;	permission_error(access, predicate, Functor/Arity)
		).
	listing(Functor//Arity) :-
		!,
		ExtArity is Arity + 2,
		functor(Head, Functor, ExtArity),
		(	::predicate_property(Head, (dynamic)) ->
			listing_clauses(Head)
		;	permission_error(access, non_terminal, Functor//Arity)
		).
	listing(Head) :-
		(	::predicate_property(Head, (dynamic)) ->
			listing_clauses(Head)
		;	functor(Head, Functor, Arity),
			permission_error(access, predicate, Functor/Arity)
		).

	listing_clauses(Head) :-
		::clause(Head, Body),
		::portray_clause((Head :- Body)),
		fail.
	listing_clauses(Head) :-
		(	\+ \+ ::clause(Head, _) ->
			nl
		;	true
		).

	portray_clause(Clause) :-
		var(Clause),
		instantiation_error.
	portray_clause(Clause) :-
		\+ callable(Clause),
		type_error(clause, Clause).
	portray_clause((Head :- Body)) :-
		!,
		(	Body == true ->
			numbervars(Head, 0, _),
			write_term(Head, [numbervars(true), quoted(true)]), write('.'), nl
		;	numbervars((Head:-Body), 0, _),
			write_term(Head, [numbervars(true), quoted(true)]), write(' :-'), nl,
			write('    '), write_term(Body, [numbervars(true), quoted(true)]),
			write('.'), nl
		).
	portray_clause(Fact) :-
		numbervars(Fact, 0, _),
		write_term(Fact, [numbervars(true), quoted(true)]), write('.'), nl.

:- end_category.

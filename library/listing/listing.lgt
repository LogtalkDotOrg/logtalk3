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


:- category(listing).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-01-25,
		comment is 'Listing predicates.'
	]).

	:- public(listing/0).
	:- mode(listing, one).
	:- info(listing/0, [
		comment is 'Lists all clauses of all visible dynamic predicates to the current output stream.'
	]).

	:- public(listing/1).
	:- mode(listing(+predicate_indicator), one).
	:- info(listing/1, [
		comment is 'Lists all clauses of a visible dynamic predicate to the current output stream.',
		argnames is ['Predicate']
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
		listing_clauses(Head, Functor, Arity),
		fail.
	listing.

	listing(Predicate) :-
		\+ ground(Predicate),
		instantiation_error.
	listing(Predicate) :-
		Predicate \= _/_,
		type_error(predicate_indicator, Predicate).
	listing(Functor/Arity) :-
		\+ (atom(Functor), integer(Arity), Arity >= 0),
		type_error(predicate_indicator, Functor/Arity).
	listing(Functor/Arity) :-
		::current_predicate(Functor/Arity),
		functor(Head, Functor, Arity),
		once(::predicate_property(Head, (dynamic))),
		listing_clauses(Head, Functor, Arity).

	listing_clauses(Head, _, _) :-
		::clause(Head, Body),
		::portray_clause((Head :- Body)),
		fail.
	listing_clauses(_, _, _) :-
		nl.

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

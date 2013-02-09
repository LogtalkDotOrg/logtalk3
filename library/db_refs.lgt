%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- category(db_refs).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2009/03/06,
		comment is 'Logtalk portable version of database predicates working with clause references. Objects importing this category must call init_db_refs/0 in their initialization goal.'
	]).

	:- uses(gensym, [gensym/2]).

	:- public(asserta/2).
	:- mode(asserta(+clause, -reference), one).
	:- info(asserta/2, [
		comment is 'Asserts a new clause returning the clause reference.',
		argnames is ['Clause', 'Reference']
	]).

	:- public(assertz/2).
	:- mode(assertz(+clause, -reference), one).
	:- info(assertz/2, [
		comment is 'Asserts a new clause returning the clause reference.',
		argnames is ['Clause', 'Reference']
	]).

	:- public(clause/3).
	:- mode(clause(+callable, ?callable, ?reference), zero_or_more).
	:- info(clause/3, [
		comment is 'Returns clauses and their references.',
		argnames is ['Head', 'Body', 'Reference']
	]).

	:- public(retract/2).
	:- mode(retract(+callable, ?reference), zero_or_more).
	:- info(retract/2, [
		comment is 'Retracts a matching clause with the given reference.',
		argnames is ['Clause', 'Reference']
	]).

	:- public(retractall/2).
	:- mode(retractall(+callable, ?reference), one).
	:- info(retractall/2, [
		comment is 'Retracts all matching clauses with the given reference.',
		argnames is ['Head', 'Reference']
	]).

	:- protected(init_db_refs/0).
	:- mode(init_db_refs, one).
	:- info(init_db_refs/0, [
		comment is 'Initializes the clause database references.'
	]).

	:- private(db_ref/1).
	:- mode(db_ref(@reference), one).
	:- info(db_ref/1, [
		comment is 'This predicate is called when using clauses with database references. Is argument is ignored and it is always true.',
		argnames is ['Reference']
	]).

	asserta((Head :- Body), Ref) :-
		!,
		gensym('db_ref', Ref),
		asserta((Head :- (db_ref(Ref), Body))).
	asserta(Fact, Ref) :-
		!,
		gensym('db_ref', Ref),
		asserta((Fact :- db_ref(Ref))).

	assertz((Head :- Body), Ref) :-
		!,
		gensym('db_ref', Ref),
		assertz((Head :- (db_ref(Ref), Body))).
	assertz(Fact, Ref) :-
		!,
		gensym('db_ref', Ref),
		assertz((Fact :- db_ref(Ref))).

	clause(Head, Body, Ref) :-
		clause(Head, (db_ref(Ref), Body)).

	retract((Head :- Body), Ref) :-
		!,
		retract((Head :- (db_ref(Ref), Body))).
	retract(Fact, Ref) :-
		retract((Fact :- db_ref(Ref))).

	retractall(Fact, Ref) :-
		retract((Fact :- db_ref(Ref))),
		fail.
	retractall(Head, Ref) :-
		retract((Head :- (db_ref(Ref), _))),
		fail.
	retractall(_, _).

	init_db_refs :-
		retractall(db_ref(_)),
		assertz(db_ref(_)).

:- end_category.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(named_database).

	:- public(db_create/1).
	:- mode(db_create(+atom), one).
	:- info(db_create/1, [
		comment is 'Creates a new named database.',
		argnames is ['Database']
	]).

	:- public(db_dynamic/2).
	:- mode(db_dynamic(+atom, +predicate_indicator), one).
	:- info(db_dynamic/2, [
		comment is 'Declares a new dynamic predicate.',
		argnames is ['Database', 'Predicate']
	]).

	:- public(db_abolish/2).
	:- mode(db_abolish(+atom, +predicate_indicator), one).
	:- info(db_abolish/2, [
		comment is 'Abolishes a dynamic predicate.',
		argnames is ['Database', 'Predicate']
	]).

	:- public(db_asserta/2).
	:- mode(db_asserta(+atom, +clause), one).
	:- info(db_asserta/2, [
		comment is 'Asserts a clause for a dynamic predicate.',
		argnames is ['Database', 'Clause']
	]).

	:- public(db_assertz/2).
	:- mode(db_assertz(+atom, +clause), one).
	:- info(db_assertz/2, [
		comment is 'Asserts a clause for a dynamic predicate.',
		argnames is ['Database', 'Clause']
	]).

	:- public(db_retract/2).
	:- mode(db_retract(+atom, +clause), one).
	:- info(db_retract/2, [
		comment is 'Retracts a matching clause for a dynamic predicate.',
		argnames is ['Database', 'Clause']
	]).

	:- public(db_retractall/2).
	:- mode(db_retractall(+atom, @callable), one).
	:- info(db_retractall/2, [
		comment is 'Retracts all clauses for a dynamic predicate with a matching head.',
		argnames is ['Database', 'Head']
	]).

	:- public(db_clause/3).
	:- mode(db_clause(+atom, +callable, ?callable), one).
	:- info(db_clause/3, [
		comment is 'Retrieves clauses for dynamic predicates in the named database.',
		argnames is ['Database', 'Head', 'Body']
	]).

	:- public(db_call/2).
	:- mode(db_call(+atom, +callable), one).
	:- info(db_call/2, [
		comment is 'Proves a goal using the predicate clauses in the named database.',
		argnames is ['Database', 'Goal']
	]).

	:- public(db_once/2).
	:- mode(db_once(+atom, +callable), one).
	:- info(db_once/2, [
		comment is 'Proves a goal once using the predicate clauses in the named database.',
		argnames is ['Database', 'Goal']
	]).

	:- public(db_listing/1).
	:- mode(db_listing(+integer), one).
	:- info(db_listing/1, [
		comment is 'Lists all dynamic predicates in the named database.',
		argnames is ['Database']
	]).

	:- public(db_load/2).
	:- mode(db_load(+integer, +integer), one).
	:- info(db_load/2, [
		comment is 'Loads a Prolog file into a named database.',
		argnames is ['Database', 'File']
	]).

	:- public(db_save/2).
	:- mode(db_save(+integer, +integer), one).
	:- info(db_save/2, [
		comment is 'Saves all dynamic predicates to a file.',
		argnames is ['Database', 'File']
	]).

	:- public(db_clear/1).
	:- mode(db_clear(+integer), one).
	:- info(db_clear/1, [
		comment is 'Abolishes all dynamic predicates.',
		argnames is ['Database']
	]).

:- if(current_logtalk_flag(prolog_dialect, eclipse)).

	db_create(Database) :-
		(	{current_module(Database)} ->
			db_clear(Database)
		;	{create_module(Database, [], eclipse_language)}
		).

	db_dynamic(Database, Predicate) :-
		{'@'(dynamic(Predicate), Database)}.

	db_abolish(Database, Predicate) :-
		{'@'(abolish(Predicate), Database)}.

	db_asserta(Database, Clause) :-
		{'@'(asserta(Clause), Database)}.

	db_assertz(Database, Clause) :-
		{'@'(assertz(Clause), Database)}.

	db_retract(Database, Clause) :-
		{'@'(retract(Clause), Database)}.

	db_retractall(Database, Head) :-
		{'@'(retractall(Head), Database)}.

	db_clause(Database, Head, Body) :-
		{'@'(clause(Head, Body), Database)}.

	db_call(Database, Goal) :-
		{'@'(Goal, Database)}.

	db_once(Database, Goal) :-
		{once('@'(Goal, Database))}.

	db_listing(Database) :-
		{(	'@'(current_predicate(Predicate), Database),
			'@'(is_dynamic(Predicate), Database),
			writeq((:- dynamic(Predicate))), write('.'), nl,
			'@'(listing(Predicate), Database),
			fail
		;	true
		)}.

	db_load(Database, File) :-
		{'@'(compile(File), Database)}.

	db_save(Database, File) :-
		{open(File, write, Stream),
		current_output(Current),
		set_output(Stream)},
		db_listing(Database),
		{close(Stream),
		set_output(Current)}.

	db_clear(Database) :-
		{'@'(current_predicate(Predicate), Database),
		'@'(is_dynamic(Predicate), Database),
		'@'(abolish(Predicate), Database),
		fail}.
	db_clear(_).

:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	db_create(Database) :-
		(	{current_module(Database)} ->
			db_clear(Database)
		;	true
		).

	db_dynamic(Database, Functor/Arity) :-
		{functor(Clause, Functor, Arity),
		assertz(Database:Clause),
		retract(Database:Clause)}.

	db_abolish(Database, Predicate) :-
		{abolish(Database:Predicate)}.

	db_asserta(Database, Clause) :-
		{asserta(Database:Clause)}.

	db_assertz(Database, Clause) :-
		{assertz(Database:Clause)}.

	db_retract(Database, Clause) :-
		{retract(Database:Clause)}.

	db_retractall(Database, Head) :-
		{retractall(Database:Head)}.

	db_clause(Database, Head, Body) :-
		{clause(Database:Head, Body)}.

	db_call(Database, Goal) :-
		{Database:Goal}.

	db_once(Database, Goal) :-
		{once(Database:Goal)}.

	db_listing(Database) :-
		{(	current_predicate(Database:Functor/Arity),
			functor(Template, Functor, Arity),
			predicate_property(Database:Template, (dynamic)),
			writeq((:- dynamic(Functor/Arity))), write('.'), nl,
			clause(Database:Template, Body),
			(	Body == true ->
				writeq(Template), write('.'), nl
			;	writeq((Template :- Body)), write('.'), nl
			),
			fail
		;	true
		)}.

	db_load(Database, File) :-
		{Database:reconsult(File)}.

	db_save(Database, File) :-
		{open(File, write, Stream),
		current_output(Current),
		set_output(Stream),
		db_listing(Database)},
		{close(Stream),
		set_output(Current)}.

	db_clear(Database) :-
		{current_predicate(Database:Functor/Arity),
		functor(Template, Functor, Arity),
		predicate_property(Database:Template, (dynamic)),
		abolish(Database:Functor/Arity),
		fail}.
	db_clear(_).

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	db_create(Database) :-
		{(	current_module(Database) ->
			db_clear(Database)
		;	true
		)}.

	db_dynamic(Database, Predicate) :-
		{dynamic(Database:Predicate)}.

	db_abolish(Database, Predicate) :-
		{abolish(Database:Predicate)}.

	db_asserta(Database, Clause) :-
		{asserta(Database:Clause)}.

	db_assertz(Database, Clause) :-
		{assertz(Database:Clause)}.

	db_retract(Database, Clause) :-
		{retract(Database:Clause)}.

	db_retractall(Database, Head) :-
		{retractall(Database:Head)}.

	db_clause(Database, Head, Body) :-
		{clause(Database:Head, Body)}.

	db_call(Database, Goal) :-
		{Database:Goal}.

	db_once(Database, Goal) :-
		{once(Database:Goal)}.

	db_listing(Database) :-
		{listing(Database:_)}.

	db_load(Database, File) :-
		{Database:reconsult(File)}.

	db_save(Database, File) :-
		{open(File, write, Stream),
		current_output(Current),
		set_output(Stream),
		listing(Database:_),
		close(Stream),
		set_output(Current)}.

	db_clear(Database) :-
		{current_predicate(Database:Predicate),
		abolish(Database:Predicate),
		fail}.
	db_clear(_).

:- else.

	:- initialization((write('WARNING: named databases not supported on this backend Prolog compiler!'), nl)).

:- endif.

:- end_object.

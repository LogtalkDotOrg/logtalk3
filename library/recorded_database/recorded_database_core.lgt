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


:- category(recorded_database_core,
	extends(gensym_core)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-12-16,
		comment is 'Legacy recorded database predicates. Can be imported into an object to provide a local database.',
		remarks is [
			'References' - 'Opaque ground terms.'
		]
	]).

	:- public(recorda/3).
	:- mode(recorda(+key, +term, --reference), one_or_error).
	:- info(recorda/3, [
		comment is 'Adds a term as the first term for the given key, returning its reference.',
		argnames is ['Key', 'Term', 'Reference'],
		exceptions is [
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atomic term or compound term' - type_error(recorded_database_key, 'Key'),
			'``Reference`` is a not a variable' - uninstantiation_error('Reference')
		]
	]).

	:- public(recorda/2).
	:- mode(recorda(+key, +term), one_or_error).
	:- info(recorda/2, [
		comment is 'Adds a term as the first term for the given key.',
		argnames is ['Key', 'Term'],
		exceptions is [
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atomic term or compound term' - type_error(recorded_database_key, 'Key')
		]
	]).

	:- public(recordz/3).
	:- mode(recordz(+key, +term, --reference), one_or_error).
	:- info(recordz/3, [
		comment is 'Adds a term as the last term for the given key, returning its reference.',
		argnames is ['Key', 'Term', 'Reference'],
		exceptions is [
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atomic term or compound term' - type_error(recorded_database_key, 'Key'),
			'``Reference`` is a not a variable' - uninstantiation_error('Reference')
		]
	]).

	:- public(recordz/2).
	:- mode(recordz(+key, +term), one_or_error).
	:- info(recordz/2, [
		comment is 'Adds a term as the last term for the given key.',
		argnames is ['Key', 'Term'],
		exceptions is [
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atomic term or compound term' - type_error(recorded_database_key, 'Key')
		]
	]).

	:- public(recorded/3).
	:- mode(recorded(?key, ?value, -reference), zero_or_more).
	:- mode(recorded(?key, ?value, +reference), zero_or_one).
	:- info(recorded/3, [
		comment is 'Enumerates, by backtracking, all record key-value pairs and their references.',
		argnames is ['Key', 'Term', 'Reference']
	]).

	:- public(recorded/2).
	:- mode(recorded(?key, ?value), zero_or_more).
	:- info(recorded/2, [
		comment is 'Enumerates, by backtracking, all record key-value pairs.',
		argnames is ['Key', 'Term']
	]).

	:- public(erase/1).
	:- mode(erase(+reference), zero_or_one_or_error).
	:- info(erase/1, [
		comment is 'Erases the record indexed by the given reference. Fails if there is no record with the given reference.',
		argnames is ['Reference'],
		exceptions is [
			'``Reference`` is a variable' - instantiation_error
		]
	]).

	:- public(instance/2).
	:- mode(instance(+reference, ?term), zero_or_one_or_error).
	:- info(instance/2, [
		comment is '.',
		argnames is ['Reference', 'Term'],
		exceptions is [
			'``Reference`` is a variable' - instantiation_error
		]
	]).

	:- private(record_/3).
	:- dynamic(record_/3).
	:- mode(record_(?key, ?value, ?reference), zero_or_more).
	:- info(record_/3, [
		comment is 'Records table.',
		argnames is ['Key', 'Term', 'Reference']
	]).

	recorda(Key, Term, Reference) :-
		(	nonvar(Reference) ->
			uninstantiation_error(Reference)
		;	^^gensym(ref, Reference)
		),
		(	atom(Key) ->
			asserta(record_(Key, Term, Reference))
		;	integer(Key) ->
			asserta(record_(Key, Term, Reference))
		;	compound(Key) ->
			functor(Key, Functor, Arity),
			functor(Template, Functor, Arity),
			asserta(record_(Template, Term, Reference))
		;	var(Key) ->
			instantiation_error
		;	type_error(recorded_database_key, Key)
		).

	recorda(Key, Term) :-
		recorda(Key, Term, _).

	recordz(Key, Term, Reference) :-
		(	nonvar(Reference) ->
			uninstantiation_error(Reference)
		;	^^gensym(ref, Reference)
		),
		(	atom(Key) ->
			assertz(record_(Key, Term, Reference))
		;	integer(Key) ->
			assertz(record_(Key, Term, Reference))
		;	compound(Key) ->
			functor(Key, Functor, Arity),
			functor(Template, Functor, Arity),
			assertz(record_(Template, Term, Reference))
		;	var(Key) ->
			instantiation_error
		;	type_error(recorded_database_key, Key)
		).

	recordz(Key, Term) :-
		recordz(Key, Term, _).

	recorded(Key, Term, Reference) :-
		(	var(Reference) ->
			record_(Key, Term, Reference)
		;	record_(Key, Term, Reference),
			!
		).

	recorded(Key, Term) :-
		record_(Key, Term, _).

	erase(Reference) :-
		(	var(Reference) ->
			instantiation_error
		;	retract(record_(_, _, Reference))
		),
		!.

	instance(Reference, Term) :-
		(	var(Reference) ->
			instantiation_error
		;	record_(_, Term, Reference)
		),
		!.

:- end_category.

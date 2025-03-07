%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2025 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
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


:- category(code_metrics_utilities).

	:- info([
		version is 0:7:0,
		author is 'Ebrahim Azarisooreh',
		date is 2024-03-28,
		comment is 'Internal predicates for analyzing source code.',
		remarks is [
			'Usage' - 'This is meant to be imported by any metric added to the system.',
			'Predicate Scope' - 'This is meant for internal use by metrics only. As such, all provided predicates are protected.'
		]
	]).

	:- protected(ancestor/4).
	:- mode(ancestor(?entity, ?entity_identifier, ?entity, ?entity_identifier), zero_or_more).
	:- info(ancestor/4, [
		comment is 'True if ``Entity`` descends from ``Ancestor``, and ``EntityKind`` and ``AncestorKind`` unify with their respective entity types.',
		argnames is ['EntityKind', 'Entity', 'AncestorKind', 'Ancestor']
	]).

	:- protected(current_entity/1).
	:- mode(current_entity(?entity_identifier), zero_or_more).
	:- info(current_entity/1, [
		comment is 'True if ``Entity`` is a currently loaded entity.',
		argnames is ['Entity']
	]).

	:- protected(declares_predicate/2).
	:- mode(declares_predicate(?entity_identifier, ?predicate_indicator), zero_or_more).
	:- info(declares_predicate/2, [
		comment is 'True if ``Entity`` declares ``Predicate`` internally.',
		argnames is ['Entity', 'Predicate']
	]).

	:- protected(defines_predicate/2).
	:- mode(defines_predicate(?entity_identifier, ?predicate_indicator), zero_or_more).
	:- info(defines_predicate/2, [
		comment is 'True if ``Entity`` defines an implementation of ``Predicate`` internally. Auxiliary predicates are excluded from results.',
		argnames is ['Entity', 'Predicate']
	]).

	:- protected(defines_predicate/3).
	:- mode(defines_predicate(?entity_identifier, ?predicate_indicator, ?term), zero_or_more).
	:- info(defines_predicate/3, [
		comment is 'Same as ``defines_predicate/2``, except ``Property`` is unified with a property of the predicate.',
		argnames is ['Entity', 'Predicate', 'Property']
	]).

	:- protected(entity_calls/3).
	:- mode(entity_calls(?entity_identifier, ?predicate_indicator, ?predicate_indicator), zero_or_one).
	:- info(entity_calls/3, [
		comment is 'True if a predicate ``Caller`` within ``Entity`` makes a ``Call``.',
		argnames is ['Entity', 'Caller', 'Call']
	]).

	:- protected(entity_kind/2).
	:- mode(entity_kind(+entity_identifier, -entity), zero_or_one).
	:- info(entity_kind/2, [
		comment is 'True if ``Kind`` defines ``Entity`` and is one of category, protocol, or object.',
		argnames is ['Entity', 'Kind']
	]).

	:- protected(entity_property/2).
	:- mode(entity_property(+entity_identifier, -term), zero_or_more).
	:- info(entity_property/2, [
		comment is 'True if ``Property`` is a valid property of ``Entity``. Entity can be either a category, a protocol, or an object.',
		argnames is ['Entity', 'Property']

	]).

	:- protected(entity_updates/3).
	:- mode(entity_updates(+entity_identifier, ?predicate_indicator, ?predicate_indicator), zero_or_one).
	:- info(entity_updates/3, [
		comment is 'True if a predicate ``Updater`` within ``Entity`` makes a dynamic update to ``Updated`` (by using e.g. the ``asserta/1`` or ``retract/1`` predicates).',
		argnames is ['Entity', 'Updater', 'Updated']
	]).

	:- protected(not_excluded_file/3).
	:- mode(not_excluded_file(+list(atom), +atom, +atom), zero_or_one).
	:- info(not_excluded_file/3, [
		comment is 'True if the file is not being excluded.',
		argnames is ['ExcludedFiles', 'Path', 'Basename']
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(logtalk, [
		file_type_extension/2
	]).

	current_entity(Entity) :-
		current_object(Entity).
	current_entity(Entity) :-
		current_category(Entity).
	current_entity(Entity) :-
		(	nonvar(Entity) ->
			atom(Entity),
			current_protocol(Entity)
		;	current_protocol(Entity)
		).

	entity_kind(Entity, Kind) :-
		(	current_object(Entity) ->
			Kind = object
		;	current_category(Entity) ->
			Kind = category
		;	atom(Entity),
			current_protocol(Entity),
			Kind = protocol
		).

	entity_property(Entity, Property) :-
		(	current_object(Entity) ->
			object_property(Entity, Property)
		;	current_category(Entity) ->
			category_property(Entity, Property)
		;	atom(Entity),
			current_protocol(Entity),
			protocol_property(Entity, Property)
		).

	entity_calls(Entity, Caller, Call) :-
		entity_kind(Entity, Kind),
		entity_calls(Kind, Entity, Caller, Call),
		ground(Call).

	entity_calls(object, Entity, Caller, Call) :-
		object_property(Entity, calls(Call, Properties)),
		memberchk(caller(Caller), Properties).

	entity_calls(category, Entity, Caller, Call) :-
		category_property(Entity, calls(Call, Properties)),
		memberchk(caller(Caller), Properties).

	entity_updates(Entity, Updater, Update) :-
		entity_kind(Entity, Kind),
		entity_updates(Kind, Entity, Updater, Update),
		ground(Update).

	entity_updates(object, Entity, Updater, Update) :-
		object_property(Entity, updates(Update, Properties)),
		memberchk(updater(Updater), Properties).

	entity_updates(category, Entity, Updater, Update) :-
		category_property(Entity, updates(Update, Properties)),
		memberchk(updater(Updater), Properties).

	defines_predicate(Entity, Predicate) :-
		current_entity(Entity),
		entity_kind(Entity, Kind),
		defines_predicate(Kind, Entity, Predicate, _).

	defines_predicate(Entity, Predicate, Properties) :-
		current_entity(Entity),
		entity_kind(Entity, Kind),
		defines_predicate(Kind, Entity, Predicate, Properties).

	defines_predicate(object, Entity, Predicate, Properties) :-
		object_property(Entity, defines(Predicate, Properties)),
		\+ member(auxiliary, Properties).
	defines_predicate(category, Entity, Predicate, Properties) :-
		category_property(Entity, defines(Predicate, Properties)),
		\+ member(auxiliary, Properties).

	declares_predicate(Entity, Predicate) :-
		current_entity(Entity),
		entity_kind(Entity, Kind),
		declares_predicate(Kind, Entity, Predicate).

	declares_predicate(protocol, Entity, Functor/N) :-
		protocol_property(Entity, declares(Functor/N, _)).

	declares_predicate(category, Entity, Functor/N) :-
		category_property(Entity, declares(Functor/N, _)).

	declares_predicate(object, Entity, Functor/N) :-
		object_property(Entity, declares(Functor/N, _)).

	% protocol ancestors
	ancestor(protocol, Entity, protocol, Ancestor) :-
		extends_protocol(Entity, Ancestor).
	% category ancestors
	ancestor(category, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(category, Entity, category, Ancestor) :-
		extends_category(Entity, Ancestor).
	% object ancestors
	ancestor(object, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(object, Entity, category, Ancestor) :-
		imports_category(Entity, Ancestor).
	ancestor(object, Entity, object, Ancestor) :-
		extends_object(Entity, Ancestor).
	ancestor(object, Entity, object, Ancestor) :-
		instantiates_class(Entity, Ancestor),
		Entity \== Ancestor,
		\+ instantiates_class(Ancestor, Entity),
		\+ specializes_class(Ancestor, Entity).
	ancestor(object, Entity, object, Ancestor) :-
		specializes_class(Entity, Ancestor),
		\+ instantiates_class(Ancestor, Entity),
		\+ specializes_class(Ancestor, Entity).

	not_excluded_file([], _, _).
	not_excluded_file([ExcludedFile| ExcludedFiles], Path, Basename) :-
		% files in the exclusion list may be given by full path or by basename
		\+ member(Path, [ExcludedFile| ExcludedFiles]),
		\+ member(Basename, [ExcludedFile| ExcludedFiles]),
		% files in the exclusion list may be given with or without extension
		\+ (	file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Path),
				member(Source, [ExcludedFile| ExcludedFiles])
		),
		\+ (	file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Basename),
				member(Source, [ExcludedFile| ExcludedFiles])
		).

:- end_category.

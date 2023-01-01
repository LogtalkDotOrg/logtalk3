%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2018-05-08,
		comment is 'Unit tests for the "logtalk" built-in object.'
	]).

	% basic properties

	succeeds(logtalk_01) :-
		current_object(logtalk).

	succeeds(logtalk_02) :-
		object_property(logtalk, built_in).

	succeeds(logtalk_03) :-
		object_property(logtalk, static).

	succeeds(logtalk_04) :-
		(	current_logtalk_flag(threads, supported) ->
			object_property(logtalk, threaded)
		;	true
		).

	succeeds(logtalk_05) :-
		object_property(logtalk, context_switching_calls).

	succeeds(logtalk_06) :-
		\+ object_property(logtalk, dynamic_declarations).

	succeeds(logtalk_07) :-
		\+ object_property(logtalk, complements).

	succeeds(logtalk_08) :-
		\+ object_property(logtalk, events).

	% entity_prefix/2 tests

	succeeds(logtalk_09) :-
		logtalk::entity_prefix(foo, Prefix),
		logtalk::entity_prefix(Entity, Prefix),
		Entity == foo.

	succeeds(logtalk_10) :-
		logtalk::entity_prefix(foo(_), Prefix),
		logtalk::entity_prefix(Entity, Prefix),
		^^variant(Entity, foo(_)).

	succeeds(logtalk_11) :-
		logtalk::compile_predicate_heads(bar(_), logtalk, Compiled, _),
		logtalk::decompile_predicate_heads(Compiled, Entity, Type, Decompiled),
		Entity == logtalk,
		Type == object,
		^^variant(Decompiled, bar(_)).

	succeeds(logtalk_12) :-
		logtalk::compile_predicate_indicators(bar/1, logtalk, Compiled),
		logtalk::decompile_predicate_indicators(Compiled, Entity, Type, Decompiled),
		Entity == logtalk,
		Type == object,
		Decompiled == bar/1.

	% loaded_file/1 tests

	succeeds(logtalk_13) :-
		forall(
			logtalk::loaded_file(File),
			atom(File)
		).

	fails(logtalk_14) :-
		logtalk::loaded_file(non_loaded_file).

	succeeds(logtalk_15) :-
		forall(
			(	logtalk::loaded_file(File),
				os::decompose_file_name(File, Directory, Basename)
			),
			(	logtalk::loaded_file_property(File, directory(Directory0)), Directory0 == Directory,
				logtalk::loaded_file_property(File, basename(Basename0)), Basename0 == Basename
			)
		).

	% loaded_file_property/2 tests

	succeeds(logtalk_16) :-
		logtalk::loaded_file_property(SourceFile, basename('tests.lgt')), !,
		logtalk::loaded_file_property(SourceFile, directory(Directory)), atom(Directory),
		logtalk::loaded_file_property(SourceFile, mode(Mode)), atom(Mode), mode(Mode),
		logtalk::loaded_file_property(SourceFile, flags(Flags)), ground(Flags), flags(Flags),
		logtalk::loaded_file_property(SourceFile, text_properties(Properties)), ground(Properties), text_properties(Properties),
		logtalk::loaded_file_property(SourceFile, target(ObjectFile)), atom(ObjectFile),
		logtalk::loaded_file_property(SourceFile, modified(TimeStamp)),	ground(TimeStamp),
		logtalk::loaded_file_property(SourceFile, parent(ParentFile)), atom_concat(_, 'tester.lgt', ParentFile),
		(	logtalk::loaded_file_property(SourceFile, library(Library)) ->
			Library == startup
		;	true
		),
		logtalk::loaded_file_property(SourceFile, object(Object)), Object == tests.

	% expand_library_path/2 tests

	deterministic(logtalk_17) :-
		logtalk::expand_library_path(core, Path),
		atom(Path).

	deterministic(logtalk_18) :-
		logtalk::expand_library_path(core(logtalk), Path),
		atom(Path).

	fails(logtalk_19) :-
		logtalk::expand_library_path(non_existing_library_alias, _).

	fails(logtalk_20) :-
		logtalk::expand_library_path(non_existing_library_alias(some_file), _).

	% file_type_extension/2 tests

	succeeds(logtalk_21) :-
		forall(
			logtalk::file_type_extension(Type, Extension),
			(atom(Type), atom(Extension))
		).

	succeeds(logtalk_22) :-
		setof(
			Type,
			Extension^(logtalk::file_type_extension(Type, Extension)),
			Extensions
		),
		(	Extensions == [logtalk, object, prolog, source, tmp] ->
			true
		;	% not all backend Prolog compilers generate temporary
			% files when compiling source files
			Extensions == [logtalk, object, prolog, source]
		).

	succeeds(logtalk_23) :-
		findall(
			LogtalkExtension,
			logtalk::file_type_extension(logtalk, LogtalkExtension),
			LogtalkExtensions
		),
		findall(
			PrologExtension,
			logtalk::file_type_extension(prolog, PrologExtension),
			LogtalkPrologExtensions0,
			LogtalkExtensions
		),
		sort(LogtalkPrologExtensions0, LogtalkPrologExtensions),
		setof(
			SourceExtension,
			logtalk::file_type_extension(source, SourceExtension),
			SourceExtensions
		),
		LogtalkPrologExtensions == SourceExtensions.

	% auxiliary predicates

	mode(debug).
	mode(normal).
	mode(optimal).

	flags([]).
	flags([Flag| Flags]) :-
		flag(Flag),
		flags(Flags).

	flag(Flag) :-
		functor(Flag, Functor, 1),
		atom(Functor).

	text_properties([]).
	text_properties([Property| Properties]) :-
		text_property(Property),
		text_properties(Properties).

	text_property(bom(BOM)) :-
		(BOM == true -> true; BOM == false).
	text_property(encoding(Encoding)) :-
		atom(Encoding).

:- end_object.

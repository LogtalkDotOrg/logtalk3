%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2016/11/22,
		comment is 'Unit tests for the "logtalk" built-in object.'
	]).

	succeeds(logtalk_01) :-
		current_object(logtalk),
		object_property(logtalk, built_in).

	% entity_prefix/2 tests

	succeeds(logtalk_02) :-
		logtalk::entity_prefix(foo, Prefix),
		logtalk::entity_prefix(Entity, Prefix),
		Entity == foo.

	succeeds(logtalk_03) :-
		logtalk::entity_prefix(foo(_), Prefix),
		logtalk::entity_prefix(Entity, Prefix),
		::variant(Entity, foo(_)).

	succeeds(logtalk_04) :-
		logtalk::compile_predicate_heads(bar(_), logtalk, Compiled, _),
		logtalk::decompile_predicate_heads(Compiled, Entity, Type, Decompiled),
		Entity == logtalk,
		Type == object,
		::variant(Decompiled, bar(_)).

	succeeds(logtalk_05) :-
		logtalk::compile_predicate_indicators(bar/1, logtalk, Compiled),
		logtalk::decompile_predicate_indicators(Compiled, Entity, Type, Decompiled),
		Entity == logtalk,
		Type == object,
		Decompiled == bar/1.

	% loaded_file/1 tests

	succeeds(logtalk_06) :-
		forall(
			logtalk::loaded_file(File),
			atom(File)
		).

	fails(logtalk_07) :-
		logtalk::loaded_file(non_loaded_file).

	succeeds(logtalk_08) :-
		forall(
			(	logtalk::loaded_file(File),
				os::decompose_file_name(File, Directory, Name, Extension),
				atom_concat(Name, Extension, Basename)
			),
			(	logtalk::loaded_file_property(File, directory(Directory0)), Directory0 == Directory,
				logtalk::loaded_file_property(File, basename(Basename0)), Basename0 == Basename
			)
		).

	% loaded_file_property/2 tests

	succeeds(logtalk_09) :-
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

	deterministic(logtalk_10) :-
		logtalk::expand_library_path(core, Path),
		atom(Path).

	deterministic(logtalk_11) :-
		logtalk::expand_library_path(core(logtalk), Path),
		atom(Path).

	fails(logtalk_12) :-
		logtalk::expand_library_path(non_existing_library_alias, _).

	fails(logtalk_13) :-
		logtalk::expand_library_path(non_existing_library_alias(some_file), _).

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

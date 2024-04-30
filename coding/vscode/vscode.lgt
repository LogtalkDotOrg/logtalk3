%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2021-2024 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 1998-2021 Jacob Friedman <jfriedman@graphstax.com>
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


:- object(vscode).

	:- info([
		version is 0:32:0,
		author is 'Paulo Moura and Jacob Friedman',
		date is 2024-04-30,
		comment is 'Support for Visual Studio Code programatic features.'
	]).

	:- set_logtalk_flag(unknown_entities, silent).

	:- public(load/2).
	:- mode(load(+atom, +atom), one).
	:- info(load/2, [
		comment is 'Loads a file from a directory.',
		argnames is ['Directory', 'File']
	]).

	:- public(documentation/2).
	:- mode(documentation(+atom, +atom), one).
	:- info(documentation/2, [
		comment is 'Generates documentation given a loader file and a marker directory.',
		argnames is ['Directory', 'File']
	]).

	:- public(diagrams/3).
	:- mode(diagrams(+atom, +atom, +atom), one).
	:- info(diagrams/3, [
		comment is 'Generates diagrams given a loader file and a marker directory.',
		argnames is ['Project', 'Directory', 'File']
	]).

	:- public(find_declaration/4).
	:- mode(find_declaration(+atom, @callable, +atom, +integer), one).
	:- info(find_declaration/4, [
		comment is 'Find the called predicate declaration file and line.',
		argnames is ['Directory', 'Call', 'CallFile', 'CallLine']
	]).

	:- public(find_definition/4).
	:- mode(find_definition(+atom, @callable, +atom, +integer), one).
	:- info(find_definition/4, [
		comment is 'Find the called predicate definition file and line.',
		argnames is ['Directory', 'Call', 'CallFile', 'CallLine']
	]).

	:- public(find_type_definition/4).
	:- mode(find_type_definition(+atom, @entity_identifier, +atom, +integer), one).
	:- info(find_type_definition/4, [
		comment is 'Find the referenced entity file and line.',
		argnames is ['Directory', 'Entity', 'CallFile', 'CallLine']
	]).

	:- public(find_references/4).
	:- mode(find_references(+atom, @callable, +atom, +integer), one).
	:- info(find_references/4, [
		comment is 'Find the called predicate references.',
		argnames is ['Directory', 'Call', 'CallFile', 'CallLine']
	]).

	:- public(find_implementations/4).
	:- mode(find_implementations(+atom, @predicate_indicator, +atom, +integer), one).
	:- info(find_implementations/4, [
		comment is 'Find the called predicate implementations.',
		argnames is ['Directory', 'Resource', 'CallFile', 'CallLine']
	]).

	:- public(find_callers/4).
	:- mode(find_callers(+atom, @predicate_indicator, +atom, +integer), one).
	:- info(find_callers/4, [
		comment is 'Find the predicate callers.',
		argnames is ['Directory', 'Resource', 'CallFile', 'CallLine']
	]).

	:- public(find_callees/4).
	:- mode(find_callees(+atom, @predicate_indicator, +atom, +integer), one).
	:- info(find_callees/4, [
		comment is 'Find the predicate callees.',
		argnames is ['Directory', 'Resource', 'CallFile', 'CallLine']
	]).

	:- public(find_parent_file/2).
	:- mode(find_parent_file(+atom, +atom), one).
	:- info(find_parent_file/2, [
		comment is 'Find the loader file.',
		argnames is ['Directory', 'File']
	]).

	% loading

	load(Directory, File) :-
		atom_concat(Directory, '/.vscode_loading_done', Marker),
		ignore(logtalk_load(File)),
		open(Marker, append, Stream),
		close(Stream).

	% documentation

	documentation(Directory, File) :-
		atom_concat(Directory, '/.vscode_xml_files_done', Marker),
		atom_concat(Directory, '/xml_docs', XMLDocs),
		ignore((
			logtalk_load(lgtdoc(loader)),
			logtalk_load(File),
			lgtdoc::directory(Directory, [xml_docs_directory(XMLDocs)])
		)),
		open(Marker, append, Stream),
		close(Stream).

	% diagrams

	diagrams(Project, Directory, File) :-
		atom_concat(Directory, '/.vscode_dot_files_done', Marker),
		atom_concat(Directory, '/dot_dias', DotDias),
		ignore((
			logtalk_load(diagrams(loader)),
			logtalk_load(File),
			diagrams::directory(Project, Directory, [output_directory(DotDias)])
		)),
		open(Marker, append, Stream),
		close(Stream).

	% declarations

	find_declaration(Directory, Call, CallFile0, CallLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(CallFile0, CallFile)},
		atom_concat(Directory, '/.vscode_declaration', Data),
		atom_concat(Directory, '/.vscode_declaration_done', Marker),
		open(Data, write, DataStream),
		(	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) ->
			{format(DataStream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_declaration_(Call, CallerEntity, CallLine, DeclarationFile, DeclarationLine).

	find_declaration_(Alias::Name/Arity, Entity, _, File, Line) :-
		callable(Alias),
		ground(Name/Arity),
		(	entity_property(Entity, _, alias(Alias, Properties)),
			member(for(Object), Properties) ->
			true
		;	Object = Alias
		),
		functor(Template, Name, Arity),
		Object::predicate_property(Template, declared_in(DeclarationEntity, Line)),
		entity_property(DeclarationEntity, _, file(File)).

	% multifile predicate
	find_declaration_(Other::Name/Arity, Entity, _, File, Line) :-
		callable(Other),
		ground(Name/Arity),
		entity_property(Entity, _, provides(Name/Arity, Other, _)),
		entity_property(Other, Kind, declares(Name/Arity, Line, Properties)),
		entity_property(Other, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	% multifile non-terminal
	find_declaration_(Other::Name/Arity, Entity, _, File, Line) :-
		callable(Other),
		ground(Name/Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, provides(Name/ExtArity, Other, _)),
		entity_property(Other, Kind, declares(Name/ExtArity, Properties)),
		memberchk(non_terminal(Name//Arity), Properties),
		entity_property(Other, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	find_declaration_(::Name/Arity, Entity, CallerLine, File, Line) :-
		(	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) ->
			true
		;	ExtArity is Arity + 2,
			entity_property(Entity, _, calls(::Name/ExtArity, Properties)),
			memberchk(line_count(CallerLine), Properties),
			find_declaration_(::Name/ExtArity, Entity, CallerLine, File, Line)
		).

	find_declaration_(^^Name/Arity, Entity, CallerLine, File, Line) :-
		(	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) ->
			true
		;	ExtArity is Arity + 2,
			entity_property(Entity, _, calls(^^Name/ExtArity, Properties)),
			memberchk(line_count(CallerLine), Properties),
			find_declaration_(^^Name/ExtArity, Entity, CallerLine, File, Line)
		).

	% locally declared
	find_declaration_(Name/Arity, Entity, _, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, Kind, declares(Name/Arity, Properties)),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	% non-local declaration
	find_declaration_(Name/Arity, Entity, _, File, Line) :-
		ground(Name/Arity),
		functor(Template, Name, Arity),
		(	current_object(Entity) ->
			(	Entity<<predicate_property(Template, declared_in(DeclarationEntity, Line)) ->
				true
			;	once((
					instantiates_class(Entity, _)
				;	specializes_class(Entity, _)
				)),
				create_object(Obj, [instantiates(Entity)], [], []),
				Obj<<predicate_property(Template, declared_in(DeclarationEntity, Line))
			)
		;	% current_category(Entity),
			create_object(Obj, [imports(Entity)], [], []),
			Obj<<predicate_property(Template, declared_in(DeclarationEntity, Line))
		),
		(	var(Obj) ->
			true
		;	abolish_object(Obj)
		),
		entity_property(DeclarationEntity, _, file(File)).

	% non-terminal
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		ExtArity is Arity + 2,
		entity_property(Entity, _, defines(Name/ExtArity, Properties)),
		memberchk(non_terminal(Name//Arity), Properties),
		find_declaration_(Name/ExtArity, Entity, CallerLine, File, Line).

	% predicate listed in a uses/2 directive
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, calls(Object::Name/Arity, Properties)),
		callable(Object),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_declaration_(Object::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in a uses/2 directive
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
		memberchk(alias(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_declaration_(Object::OriginalName/Arity, Entity, CallerLine, File, Line).

	% predicate listed in an alias/2 directive
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, alias(_/Arity, Properties)),
		memberchk(for(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(from(Entity), Properties),
		find_declaration_(Entity::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in an alias/2 directive
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, alias(Name/Arity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(for(OriginalName/Arity), Properties),
		memberchk(from(Entity), Properties),
		find_declaration_(Entity::OriginalName/Arity, Entity, CallerLine, File, Line).

	% definitions

	find_definition(Directory, Call, CallFile0, CallLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(CallFile0, CallFile)},
		atom_concat(Directory, '/.vscode_definition', Data),
		atom_concat(Directory, '/.vscode_definition_done', Marker),
		open(Data, write, DataStream),
		(	find_definition(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) ->
			{format(DataStream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	find_definition(Call, CallFile, CallLine, DefinitionFile, DefinitionLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_definition_(Call, CallerEntity, CallLine, DefinitionFile, DefinitionLine).

	find_definition_(Alias::Name/Arity, Entity, _, File, Line) :-
		callable(Alias),
		ground(Name/Arity),
		(	entity_property(Entity, _, alias(Alias, AliasProperties)),
			member(for(Object), AliasProperties) ->
			true
		;	Object = Alias
		),
		functor(Template, Name, Arity),
		current_object(Object),
		Object::predicate_property(Template, defined_in(Primary)),
		(	% local definitions
			entity_property(Primary, _, defines(Name/Arity, Properties)),
			DefinitionEntity = Primary
		;	% multifile definitions
			entity_property(Primary, _, includes(Name/Arity, DefinitionEntity, Properties))
		;	% predicate listed in a uses/2 directive
			entity_property(Primary, _, calls(_, Properties)),
			memberchk(alias(Name/Arity), Properties),
			memberchk(caller(Name/Arity), Properties),
			DefinitionEntity = Primary
		),
		entity_property(DefinitionEntity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_definition_(Object::Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
		callable(Object),
		ground(Name/Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(Object::Name/ExtArity, Properties)),
		memberchk(line_count(CallLine), Properties),
		find_definition_(Object::Name/ExtArity, Entity, CallLine, File, Line).

	find_definition_(::Name/Arity, This, _, File, Line) :-
		ground(Name/Arity),
		functor(Template, Name, Arity),
		(	% definition
			(	current_object(This) ->
				(	\+ instantiates_class(This, _),
					\+ specializes_class(This, _) ->
					This<<predicate_property(Template, declared_in(DeclarationEntity)),
					This<<predicate_property(Template, defined_in(Primary))
				;	create_object(Obj, [instantiates(This)], [], []),
					Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
					Obj<<predicate_property(Template, defined_in(Primary)),
					abolish_object(Obj)
				)
			;	%current_category(This) ->
				create_object(Obj, [imports(This)], [], []),
				Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
				Obj<<predicate_property(Template, defined_in(Primary)),
				abolish_object(Obj)
			),
			entity_property(Primary, _, defines(Name/Arity, Properties)),
			Entity = Primary
		;	% multifile definitions
			entity_property(Primary, _, includes(Name/Arity, Entity, Properties))
		;	% local definition
			Entity = This,
			entity_property(This, _, defines(Name/Arity, Properties))
		),
		entity_property(Entity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_definition_(::Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
		ground(Name/Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(::Name/ExtArity, Properties)),
		memberchk(line_count(CallLine), Properties),
		find_definition_(::Name/ExtArity, Entity, CallLine, File, Line).

	find_definition_(^^Name/Arity, This, _, File, Line) :-
		ground(Name/Arity),
		functor(Template, Name, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This<<predicate_property(Template, declared_in(DeclarationEntity)),
				(	This<<predicate_property(Template, redefined_from(Entity, Line)) ->
					true
				;	This<<predicate_property(Template, defined_in(Entity, Line))
				)
			;	create_object(Obj, [instantiates(This)], [], []),
				Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
				(	Obj<<predicate_property(Template, redefined_from(Entity, Line)) ->
					true
				;	Obj<<predicate_property(Template, defined_in(Entity, Line))
				),
				abolish_object(Obj)
			)
		;	%current_category(This) ->
			create_object(Obj, [imports(This)], [], []),
			Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
			(	Obj<<predicate_property(Template, redefined_from(Entity, Line)) ->
				true
			;	Obj<<predicate_property(Template, defined_in(Entity, Line))
			),
			abolish_object(Obj)
		),
		entity_property(Entity, _, file(File)).

	find_definition_(^^Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
		ground(Name/Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(^^Name/ExtArity, Properties)),
		memberchk(line_count(CallLine), Properties),
		find_definition_(^^Name/ExtArity, Entity, CallLine, File, Line).

	find_definition_(Name/Arity, This, _, File, Line) :-
		ground(Name/Arity),
		functor(Template, Name, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This<<predicate_property(Template, defined_in(Entity, Line))
			;	create_object(Obj, [instantiates(This)], [], []),
				Obj<<predicate_property(Template, defined_in(Entity, Line)),
				abolish_object(Obj)
			)
		;	%current_category(This) ->
			create_object(Obj, [imports(This)], [], []),
			Obj<<predicate_property(Template, defined_in(Entity, Line)),
			abolish_object(Obj)
		),
		entity_property(Entity, _, file(File)).

	% local predicate
	find_definition_(Name/Arity, Entity, _CallerLine, File, Line) :-
		ground(Name/Arity),
		(	% definition
			entity_property(Entity, _, defines(Name/Arity, Properties)) ->
			entity_property(Entity, _, file(File)),
			memberchk(line_count(Line), Properties)
		;	% multifile definitions
			entity_property(Entity, _, includes(Name/Arity, DefinitionEntity, Properties)) ->
			entity_property(DefinitionEntity, _, file(File)),
			memberchk(line_count(Line), Properties)
		;	% non-terminal
			ExtArity is Arity + 2,
			entity_property(Entity, _, defines(Name/ExtArity, Properties)),
			entity_property(Entity, _, file(File)),
			memberchk(line_count(Line), Properties)
		).

	% predicate listed in a uses/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, calls(Object::Name/Arity, Properties)),
		callable(Object),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_definition_(Object::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in a uses/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
		memberchk(alias(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_definition_(Object::OriginalName/Arity, Entity, CallerLine, File, Line).

	% predicate listed in an alias/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, alias(_/Arity, Properties)),
		memberchk(for(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(from(Entity), Properties),
		find_definition_(Entity::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in an alias/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		ground(Name/Arity),
		entity_property(Entity, _, alias(Name/Arity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(for(OriginalName/Arity), Properties),
		memberchk(from(Entity), Properties),
		find_definition_(Entity::OriginalName/Arity, Entity, CallerLine, File, Line).

	% type definitions (entities)

	find_type_definition(Directory, Name/Arity, ReferenceFile, ReferenceLine) :-
		atom_concat(Directory, '/.vscode_type_definition', Data),
		atom_concat(Directory, '/.vscode_type_definition_done', Marker),
		entity(ReferenceFile, ReferenceLine, ReferenceEntity),
		open(Data, write, DataStream),
		(	find_type_definition_(Name/Arity, ReferenceEntity, File, Line) ->
			{format(DataStream, 'File:~w;Line:~d~n', [File, Line])}
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	find_type_definition_(Name/Arity, _, DefinitionFile, DefinitionLine) :-
		functor(Entity, Name, Arity),
		entity_property(Entity, Type, file(DefinitionFile)),
		entity_property(Entity, Type, lines(DefinitionLine, _)).
	% object alias
	find_type_definition_(Name/Arity, ReferenceEntity, DefinitionFile, DefinitionLine) :-
		functor(Entity, Name, Arity),
		entity_property(ReferenceEntity, _, alias(Entity, Properties)),
		memberchk(for(Other), Properties),
		entity_property(Other, Type, file(DefinitionFile)),
		entity_property(Other, Type, lines(DefinitionLine, _)).

	% references

	find_references(Directory, Resource, ResourceFile0, ResourceLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(ResourceFile0, ResourceFile)},
		atom_concat(Directory, '/.vscode_references', Data),
		atom_concat(Directory, '/.vscode_references_done', Marker),
		open(Data, write, DataStream),
		(	find_references_(Resource, ResourceFile, ResourceLine, References) ->
			forall(
				member(File-Line, References),
				{format(DataStream, 'File:~w;Line:~d~n', [File, Line])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	find_references_(Resource, File, Line, References) :-
		entity(File, Line, Entity),
		(	entity_property(Entity, _, file(File)),
			entity_property(Entity, _, lines(Line, _)) ->
			find_entity_references(Resource, References)
		;	find_predicate_references(Resource, Entity, File, Line, References)
		).

	find_predicate_references(Name//Arity, _, File, Line, References) :-
		% predicate scope directive
		ground(Name/Arity),
		ExtArity is Arity + 2,
		find_references_(Name/ExtArity, File, Line, References).

	find_predicate_references(Name/Arity, Entity, File, Line, References) :-
		% predicate scope directive
		ground(Name/Arity),
		entity_property(Entity, _, declares(Name/Arity, Properties)),
		memberchk(line_count(Line), Properties),
		!,
		findall(
			CallerFile-CallerLine,
			(	entity_property(Caller, _, calls(Object::Name/Arity, CallsProperties)),
				callable(Object),
				memberchk(line_count(CallerLine), CallsProperties),
				find_declaration_(Object::Name/Arity, Caller, CallerLine, File, Line),
				entity_property(Caller, _, file(CallerFile))
			),
			References0
		),
		findall(
			CallerFile-CallerLine,
			(	entity_property(Caller, _, calls(::Name/Arity, CallsProperties)),
				memberchk(line_count(CallerLine), CallsProperties),
				find_declaration_(::Name/Arity, Caller, CallerLine, File, Line),
				entity_property(Caller, _, file(CallerFile))
			),
			References1,
			References0
		),
		findall(
			CallerFile-CallerLine,
			(	entity_property(Caller, _, calls(^^Name/Arity, CallsProperties)),
				memberchk(line_count(CallerLine), CallsProperties),
				find_declaration_(^^Name/Arity, Caller, CallerLine, File, Line),
				entity_property(Caller, _, file(CallerFile))
			),
			References2,
			References1
		),
		findall(
			File-CallerLine,
			(	entity_property(Entity, _, calls(Name/Arity, CallsProperties)),
				memberchk(line_count(CallerLine), CallsProperties)
			),
			References,
			References2
		).

	% predicate listed in a uses/2 directive
	find_predicate_references(Name/Arity, Entity, _, Line, [DeclarationFile-DeclarationLine| References]) :-
		ground(Name/Arity),
		entity_property(Entity, _, calls(Object::Name/Arity, Properties)),
		callable(Object),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		find_declaration_(Object::Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).
	% predicate alias listed in a uses/2 directive
	find_predicate_references(Name/Arity, Entity, _, Line, [DeclarationFile-DeclarationLine| References]) :-
		ground(Name/Arity),
		entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
		memberchk(alias(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		find_declaration_(Object::OriginalName/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(OriginalName/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).

	% predicate listed in an alias/2 directive
	find_predicate_references(Name/Arity, Entity, _, Line, [DeclarationFile-DeclarationLine| References]) :-
		ground(Name/Arity),
		entity_property(Entity, _, alias(_/Arity, Properties)),
		memberchk(for(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		memberchk(from(Object), Properties),
		find_declaration_(Object::Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).
	% predicate alias listed in an alias/2 directive
	find_predicate_references(Name/Arity, Entity, _, Line, [DeclarationFile-DeclarationLine| References]) :-
		ground(Name/Arity),
		entity_property(Entity, _, alias(Name/Arity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		memberchk(for(OriginalName/Arity), Properties),
		memberchk(from(Object), Properties),
		find_declaration_(Object::OriginalName/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(OriginalName/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).

	% local predicate call; no declaration
	find_predicate_references(Name/Arity, Entity, _, _, References) :-
		findall(
			Reference,
			find_predicate_local_reference(Name/Arity, Entity, Reference),
			References
		),
		% require at least one reference other than the selected one
		References = [_, _| _].

	find_predicate_references(Alias::Name/Arity, Entity, _, Line, [DeclarationFile-DeclarationLine| References]) :-
		!,
		callable(Alias),
		ground(Name/Arity),
		(	entity_property(Entity, _, alias(Alias, Properties)),
			member(for(Object), Properties) ->
			true
		;	Object = Alias
		),
		find_declaration_(Object::Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).

	find_predicate_references(::Name/Arity, Entity, _, Line, [DeclarationFile-DeclarationLine| References]) :-
		!,
		ground(Name/Arity),
		(	find_declaration_(::Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
			entity(DeclarationFile, DeclarationLine, DeclarationEntity),
			find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References) ->
			true
		;	ExtArity is Arity + 2,
			entity_property(Entity, _, calls(::Name/ExtArity, Properties)),
			memberchk(line_count(Line), Properties),
			find_declaration_(::Name/ExtArity, Entity, Line, DeclarationFile, DeclarationLine),
			entity(DeclarationFile, DeclarationLine, DeclarationEntity),
			find_predicate_references(Name/ExtArity, DeclarationEntity, DeclarationFile, DeclarationLine, References)
		).

	find_predicate_references(^^Name/Arity, Entity, _, Line, [DeclarationFile-DeclarationLine| References]) :-
		!,
		ground(Name/Arity),
		(	find_declaration_(^^Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
			entity(DeclarationFile, DeclarationLine, DeclarationEntity),
			find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References) ->
			true
		;	ExtArity is Arity + 2,
			entity_property(Entity, _, calls(^^Name/ExtArity, Properties)),
			memberchk(line_count(Line), Properties),
			find_declaration_(^^Name/ExtArity, Entity, Line, DeclarationFile, DeclarationLine),
			entity(DeclarationFile, DeclarationLine, DeclarationEntity),
			find_predicate_references(Name/ExtArity, DeclarationEntity, DeclarationFile, DeclarationLine, References)
		).

	find_predicate_local_reference(Name/Arity, Entity, File-Line) :-
		% local predicate
		ground(Name/Arity),
		entity_property(Entity, Kind, file(File)),
		(	entity_property(Entity, Kind, calls(Name/Arity, _)) ->
			ExtArity = Arity
		;	ExtArity is Arity + 2,
			entity_property(Entity, Kind, defines(Name/ExtArity, DefinesProperties)),
			memberchk(non_terminal(Name//Arity), DefinesProperties)
		),
		entity_property(Entity, Kind, calls(Name/ExtArity, CallsProperties)),
		memberchk(line_count(Line), CallsProperties).

	find_entity_references(Entity, References) :-
		ground(Entity),
		findall(
			Reference,
			find_entity_reference(Entity, Reference),
			References
		).

	% entity opening directives
	find_entity_reference(Name/Arity, File-Line) :-
		functor(Template, Name, Arity),
		(	atom(Template),
			current_protocol(Template) ->
			(	extends_protocol(Entity, Template)
			;	implements_protocol(Entity, Template)
			)
		;	current_object(Template) ->
			(	extends_object(Entity, Template)
			;	instantiates_class(Entity, Template)
			;	specializes_class(Entity, Template)
			)
		;	current_category(Template),
			(	extends_category(Entity, Template)
			;	imports_category(Entity, Template)
			)
		),
		entity_property(Entity, Kind, file(File)),
		entity_property(Entity, Kind, lines(Line, _)).
	% uses/2 directives
	find_entity_reference(Name/Arity, File-Line) :-
		functor(Template, Name, Arity),
		current_object(Template),
		entity_property(Entity, Kind, calls(Object::Predicate, Properties)),
		callable(Object),
		Object = Template,
		memberchk(caller(Predicate), Properties),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties).
	% uses/1 and alias/2 directives
	find_entity_reference(Name/Arity, File-Line) :-
		functor(Template, Name, Arity),
		current_object(Template),
		entity_property(Entity, Kind, alias(_, Properties)),
		(	member(from(Template), Properties) ->
			% predicate alias
			true
		;	% object alias
			memberchk(for(Template), Properties)
		),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	% implementations

	find_implementations(Directory, Predicate, ReferenceFile0, ReferenceLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(ReferenceFile0, ReferenceFile)},
		atom_concat(Directory, '/.vscode_implementations', Data),
		atom_concat(Directory, '/.vscode_implementations_done', Marker),
		open(Data, write, DataStream),
		(	find_implementations_(Predicate, ReferenceFile, ReferenceLine, Implementations) ->
			forall(
				member(ImplementationFile-ImplementationLine, Implementations),
				{format(DataStream, 'File:~w;Line:~d~n', [ImplementationFile, ImplementationLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	find_implementations_(Predicate, File, Line, Implementations) :-
		ground(Predicate),
		entity(File, Line, Entity),
		findall(
			Implementation,
			find_implementation(Predicate, Entity, Implementation),
			Implementations
		).

	% non-terminal
	find_implementation(Name//Arity, Entity, File-Line) :-
		ExtArity is Arity + 2,
		find_implementation(Name/ExtArity, Entity, File-Line).
	% locally defined predicate
	find_implementation(Name/Arity, Entity, File-Line) :-
		entity_property(Entity, Kind, defines(Name/Arity, Properties)),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties).
	% multifile predicate
	find_implementation(Name/Arity, Entity, File-Line) :-
		entity_property(Entity, _, includes(Name/Arity, Other, Properties)),
		entity_property(Other, _, file(File)),
		memberchk(line_count(Line), Properties).
	% descendant definitions
	find_implementation(Name/Arity, Entity, File-Line) :-
		functor(Template, Name, Arity),
		entity_property(ImplementationEntity, Kind, defines(Name/Arity, Properties)),
		ImplementationEntity \= Entity,
		(	current_object(ImplementationEntity) ->
			(	\+ instantiates_class(ImplementationEntity, _),
				\+ specializes_class(ImplementationEntity, _) ->
				ImplementationEntity<<predicate_property(Template, declared_in(DeclarationEntity))
			;	(	create_object(Obj, [instantiates(ImplementationEntity)], [], []),
					Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
					abolish_object(Obj)
				;	ImplementationEntity<<predicate_property(Template, declared_in(DeclarationEntity))
				)
			)
		;	%current_category(ImplementationEntity) ->
			create_object(Obj, [imports(ImplementationEntity)], [], []),
			Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
			abolish_object(Obj)
		),
		DeclarationEntity = Entity,
		entity_property(ImplementationEntity, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	% callers

	find_callers(Directory, Predicate, ReferenceFile0, ReferenceLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(ReferenceFile0, ReferenceFile)},
		atom_concat(Directory, '/.vscode_callers', Data),
		atom_concat(Directory, '/.vscode_callers_done', Marker),
		open(Data, write, DataStream),
		(	find_callers_(Predicate, ReferenceFile, ReferenceLine, Callers) ->
			forall(
				member(c(Name, CallerFile, CallerLine), Callers),
				{format(DataStream, 'Name:~w;File:~w;Line:~d~n', [Name, CallerFile, CallerLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	find_callers_(Predicate, ReferenceFile, ReferenceLine, Callers) :-
		find_definition(Predicate, ReferenceFile, ReferenceLine, DefinitionFile, DefinitionLine),
		entity(DefinitionFile, DefinitionLine, DefinitionEntity),
		entity_property(DefinitionEntity, _, defines(Name/Arity, DefinitionProperties)),
		memberchk(line_count(DefinitionLine), DefinitionProperties),
		!,
		findall(
			c(CallerPredicate, CallerFile, CallerLine),
			(	(	Callee = Name/Arity
				;	Callee = ^^Name/Arity
				;	Callee = ::Name/Arity
				;	Callee = _::Name/Arity
				),
				entity_property(CallerEntity, _, calls(Callee, CallsProperties)),
				entity_property(CallerEntity, _, file(CallerFile)),
				memberchk(line_count(CallerLine), CallsProperties),
				find_definition(Callee, CallerFile, CallerLine, DefinitionFile, DefinitionLine),
				memberchk(caller(CallerPredicate), CallsProperties)
			),
			Callers
		).

	% callees

	find_callees(Directory, Predicate, ReferenceFile0, ReferenceLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(ReferenceFile0, ReferenceFile)},
		atom_concat(Directory, '/.vscode_callees', Data),
		atom_concat(Directory, '/.vscode_callees_done', Marker),
		open(Data, write, DataStream),
		(	find_callees_(Predicate, ReferenceFile, ReferenceLine, Callees) ->
			forall(
				member(c(Name, CalleeFile, CalleeLine), Callees),
				{format(DataStream, 'Name:~w;File:~w;Line:~d~n', [Name, CalleeFile, CalleeLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	find_callees_(Predicate, ReferenceFile, ReferenceLine, Callees) :-
		find_definition(Predicate, ReferenceFile, ReferenceLine, DefinitionFile, DefinitionLine),
		entity(DefinitionFile, DefinitionLine, DefinitionEntity),
		entity_property(DefinitionEntity, _, defines(Name/Arity, DefinitionProperties)),
		memberchk(line_count(DefinitionLine), DefinitionProperties),
		!,
		findall(
			c(CalleePredicate, CalleeFile, CalleeLine),
			find_callee(DefinitionEntity, Name/Arity, CalleePredicate, CalleeFile, CalleeLine),
			Callees
		).

	find_callee(DefinitionEntity, Caller, CalleePredicate, CalleeFile, CalleeLine) :-
		entity_property(DefinitionEntity, _, calls(CalleePredicate, CallsProperties)),
		memberchk(caller(Caller), CallsProperties),
		memberchk(line_count(CallerLine), CallsProperties),
		(	find_definition_(CalleePredicate, DefinitionEntity, CallerLine, CalleeFile, CalleeLine) ->
			true
		;	% likely dynamic predicate with no clauses
			find_declaration_(CalleePredicate, DefinitionEntity, CallerLine, CalleeFile, CalleeLine)
		).

	% loader file

	find_parent_file(Directory, File0) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(File0, File)},
		atom_concat(Directory, '/.vscode_find_parent', Data),
		atom_concat(Directory, '/.vscode_find_parent_done', Marker),
		open(Data, write, DataStream),
		(	logtalk::loaded_file_property(File, parent(Loader)) ->
			{format(DataStream, '~w', [Loader])}
		;	true
		),
		close(DataStream),
		open(Marker, append, MarkerStream),
		close(MarkerStream).

	% auxiliary predicates

	entity(File, Line, Entity) :-
        (	object_property(Entity, file(File)),
        	object_property(Entity, lines(BeginLine, EndLine))
		;	category_property(Entity, file(File)),
        	category_property(Entity, lines(BeginLine, EndLine))
		;	protocol_property(Entity, file(File)),
        	protocol_property(Entity, lines(BeginLine, EndLine))
		),
        BeginLine =< Line, Line =< EndLine,
		!.

	entity_property(Object, object, Property) :-
		catch(object_property(Object, Property), _, fail).
	entity_property(Category, category, Property) :-
		catch(category_property(Category, Property), _, fail).
	entity_property(Protocol, protocol, Property) :-
		catch(protocol_property(Protocol, Property), _, fail).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

	% rewrite compiler error and warnings messages for parsing with Visual Studio Code

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% fail after processing to allow default processing of the messages
	logtalk::message_hook(_Message, error, core, Tokens) :-
		message_hook(Tokens, error),
		fail.
	logtalk::message_hook(_Message, error(Class), core, Tokens) :-
		message_hook(Tokens, error(Class)),
		fail.
	logtalk::message_hook(_Message, warning, core, Tokens) :-
		message_hook(Tokens, warning),
		fail.
	logtalk::message_hook(_Message, warning(Class), core, Tokens) :-
		message_hook(Tokens, warning(Class)),
		fail.

	message_hook(Tokens, Kind) :-
		logtalk::expand_library_path(logtalk_user('scratch/.messages'), File),
		open(File, append, Stream),
		logtalk::message_prefix_stream(Kind, core, Prefix, user_error),
		logtalk::print_message_tokens(Stream, Prefix, Tokens),
		close(Stream).

:- end_object.

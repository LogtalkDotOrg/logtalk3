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
		version is 0:21:0,
		author is 'Paulo Moura and Jacob Friedman',
		date is 2024-04-23,
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

	:- public(find_type_definition/2).
	:- mode(find_type_definition(+atom, @entity_identifier), one).
	:- info(find_type_definition/2, [
		comment is 'Find the referenced entity file and line.',
		argnames is ['Directory', 'Entity']
	]).

	:- public(find_references/4).
	:- mode(find_references(+atom, @callable, +atom, +integer), one).
	:- info(find_references/4, [
		comment is 'Find the called predicate references.',
		argnames is ['Directory', 'Call', 'CallFile', 'CallLine']
	]).

	:- public(find_implementations/5).
	:- mode(find_implementations(+atom, +atom, @predicate_indicator, +atom, +integer), one).
	:- info(find_implementations/5, [
		comment is 'Find predicate implementations predicate.',
		argnames is ['Directory', 'Kind', 'Resource', 'CallFile', 'CallLine']
	]).

	:- public(find_symbols/1).
	:- mode(find_symbols(+atom), one).
	:- info(find_symbols/1, [
		comment is 'Find workspace symbols.',
		argnames is ['Directory']
	]).

	% loading

	load(Directory, File) :-
		atom_concat(Directory, '/.loading_done', Marker),
		ignore(logtalk_load(File)),
		open(Marker, append, Stream),
		close(Stream).

	% documentation

	documentation(Directory, File) :-
		atom_concat(Directory, '/.xml_files_done', Marker),
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
		atom_concat(Directory, '/.dot_files_done', Marker),
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
		atom_concat(Directory, '/.declaration_done', Data),
		open(Data, write, Stream),
		(	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) ->
			{format(Stream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
		;	true
		),
		close(Stream).

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

	find_declaration_(Name/Arity, Entity, _, File, Line) :-
		% locally declared
		ground(Name/Arity),
		entity_property(Entity, Kind, declares(Name/Arity, Properties)),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties),
		!.

	find_declaration_(Name/Arity, Entity, _, File, Line) :-
		% non-local declaration
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
		entity_property(DeclarationEntity, _, file(File)),
		!.

	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		% non-terminal
		ExtArity is Arity + 2,
		entity_property(Entity, _, defines(Name/ExtArity, Properties)),
		memberchk(non_terminal(Name//Arity), Properties),
		!,
		find_declaration_(Name/ExtArity, Entity, CallerLine, File, Line).

	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		% predicate listed in a uses/2 directive
		ground(Name/Arity),
		(	entity_property(Entity, _, calls(Object::Name/Arity, Properties)) ->
			OriginalName = Name
		;	entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
			member(alias(Name/Arity), Properties)
		),
		callable(Object),
		memberchk(caller(Name/Arity), Properties),
		find_declaration_(Object::OriginalName/Arity, Entity, CallerLine, File, Line).

	% definitions

	find_definition(Directory, Call, CallFile0, CallLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(CallFile0, CallFile)},
		atom_concat(Directory, '/.definition_done', Data),
		open(Data, write, Stream),
		(	find_definition(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) ->
			{format(Stream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
		;	true
		),
		close(Stream).

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
		Object::predicate_property(Template, defined_in(Primary)),
		(	% local definitions
			entity_property(Primary, _, defines(Name/Arity, DefinitionProperties)),
			DefinitionEntity = Primary
		;	% multifile definitions
			entity_property(Primary, _, includes(Name/Arity, DefinitionEntity, DefinitionProperties))
		),
		(	memberchk(auxiliary, DefinitionProperties),
			entity_property(DefinitionEntity, _, calls(_, CallsProperties)),
			memberchk(alias(Name/Arity), CallsProperties) ->
			% predicate listed in a uses/2 directive
			entity_property(DefinitionEntity, _, file(File)),
			memberchk(line_count(Line), CallsProperties)
		;	entity_property(DefinitionEntity, _, file(File)),
			memberchk(line_count(Line), DefinitionProperties)
		),
		!.

	find_definition_(Object::Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
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
		memberchk(line_count(Line), Properties),
		!.

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
		entity_property(Entity, _, file(File)),
		!.

	find_definition_(^^Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
		ground(Name/Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(^^Name/ExtArity, Properties)),
		memberchk(line_count(CallLine), Properties),
		find_definition_(^^Name/ExtArity, Entity, CallLine, File, Line).

	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		% local predicate
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
			entity_property(Entity, _, calls(Name/ExtArity, CallsProperties)),
			memberchk(line_count(CallerLine), CallsProperties),
			find_definition_(Name/ExtArity, Entity, CallerLine, File, Line)
		),
		!.

	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		% predicate listed in a uses/2 directive
		ground(Name/Arity),
		(	entity_property(Entity, _, calls(Object::Name/Arity, Properties)) ->
			OriginalName = Name
		;	entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
			memberchk(alias(Name/Arity), Properties)
		),
		callable(Object),
		memberchk(caller(Name/Arity), Properties),
		find_definition_(Object::OriginalName/Arity, Entity, CallerLine, File, Line).

	% type definitions (entities)

	find_type_definition(Directory, Name/Arity) :-
		atom_concat(Directory, '/.type_definition_done', Data),
		open(Data, write, Stream),
		(	find_type_definition(Name/Arity, File, Line) ->
			{format(Stream, 'File:~w;Line:~d~n', [File, Line])}
		;	true
		),
		close(Stream).

	find_type_definition(Name/Arity, DefinitionFile, DefinitionLine) :-
		functor(Entity, Name, Arity),
		entity_property(Entity, Type, file(DefinitionFile)),
		entity_property(Entity, Type, lines(DefinitionLine, _)).

	% references

	find_references(Directory, Call, CallFile0, CallLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(CallFile0, CallFile)},
		atom_concat(Directory, '/.references_done', Data),
		open(Data, write, Stream),
		(	find_references_(Call, CallFile, CallLine, References) ->
			forall(
				member(DeclarationFile-DeclarationLine, References),
				{format(Stream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
			)
		;	true
		),
		close(Stream).

	find_references_(Name/Arity, File, Line, References) :-
		% predicate scope directive
		entity(File, Line, Entity),
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

	find_references_(Call, CallFile, CallLine, References) :-
		% predicate call
		entity(CallFile, CallLine, CallerEntity),
		findall(
			Reference,
			find_reference(Call, CallerEntity, Reference),
			References
		),
		% require at least one reference other than the selected one
		References = [_, _| _].

	find_reference(Alias::Name/Arity, Entity, File-Line) :-
		callable(Alias),
		ground(Name/Arity),
		(	entity_property(Entity, _, alias(Alias, Properties)),
			member(for(Object), Properties) ->
			true
		;	Object = Alias
		),
		entity_property(Other, Kind, calls(Object0::Name/Arity, Properties)),
		callable(Object0),
		Object0 = Object,
		entity_property(Other, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	find_reference(::Name/Arity, Entity, File-Line) :-
		% find only local references
		ground(Name/Arity),
		entity_property(Entity, Kind, file(File)),
		entity_property(Entity, Kind, calls(::Name/Arity, CallsProperties)),
		memberchk(line_count(Line), CallsProperties).

	find_reference(^^Name/Arity, Entity, File-Line) :-
		% find only local references
		ground(Name/Arity),
		entity_property(Entity, Kind, file(File)),
		entity_property(Entity, Kind, calls(^^Name/Arity, CallsProperties)),
		memberchk(line_count(Line), CallsProperties).

	find_reference(Name/Arity, Entity, File-Line) :-
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

	find_reference(Name/Arity, Entity, Reference) :-
		% predicate listed in a uses/2 directive
		ground(Name/Arity),
		(	entity_property(Entity, _, calls(Object::Name/Arity, _)) ->
			OriginalName = Name,
			ExtArity = Arity
		;	entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
			memberchk(alias(Name/Arity), Properties) ->
			ExtArity = Arity
		;	ExtArity is Arity + 2,
			entity_property(Entity, _, calls(Object::Name/ExtArity, Properties)),
			memberchk(non_terminal(Name//Arity), Properties),
			OriginalName = Name
		),
		callable(Object),
		find_reference(Object::OriginalName/ExtArity, Entity, Reference).

	% implementations

	find_implementations(Directory, Kind, Resource, ReferenceFile0, ReferenceLine) :-
		% workaround path downcasing on Windows
		{'$lgt_expand_path'(ReferenceFile0, ReferenceFile)},
		atom_concat(Directory, '/.implementations_done', Data),
		open(Data, write, Stream),
		(	find_implementations_(Kind, Resource, ReferenceFile, ReferenceLine, Implementations) ->
			forall(
				member(ImplementationFile-ImplementationLine, Implementations),
				{format(Stream, 'File:~w;Line:~d~n', [ImplementationFile, ImplementationLine])}
			)
		;	true
		),
		close(Stream).

	find_implementations_(predicate, Predicate, File, Line, Implementations) :-
		ground(Predicate),
		entity(File, Line, Entity),
		findall(
			Implementation,
			find_predicate_implementation(Predicate, Entity, Implementation),
			Implementations
		).

	find_implementations_(entity, Entity, _, _, Implementations) :-
		ground(Entity),
		findall(
			Implementation,
			find_entity_implementation(Entity, Implementation),
			Implementations
		).

	find_predicate_implementation(Name//Arity, Entity, File-Line) :-
		ExtArity is Arity + 2,
		find_predicate_implementation(Name/ExtArity, Entity, File-Line).
	find_predicate_implementation(Name/Arity, Entity, File-Line) :-
		entity_property(Entity, Kind, defines(Name/Arity, Properties)),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties).
	find_predicate_implementation(Name/Arity, Entity, File-Line) :-
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

	find_entity_implementation(Name/Arity, File-Line) :-
		functor(Template, Name, Arity),
		(	current_protocol(Name) ->
			implements_protocol(Entity, Name)
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

	% symbols

	find_symbols(Directory) :-
		atom_concat(Directory, '/.symbols_done', Data),
		open(Data, write, Stream),
		forall(
			find_symbol(Directory, File, Symbol, Kind, Line),
			{format(Stream, 'Symbol:~w;Kind:~d;Line:~d;File:~w~n', [Symbol, Kind, Line, File])}
		),
		close(Stream).

	find_symbol(Directory, File, Symbol, Kind, Line) :-
		logtalk::loaded_file(File),
		sub_atom(File, 0, _, _, Directory),
		find_symbol(File, Symbol, Kind, Line).

	% entities
	find_symbol(File, Object, 18, Line) :-
		logtalk::loaded_file_property(File, object(Object)),
		object_property(Object, lines(Line, _)).
	find_symbol(File, Protocol, 10, Line) :-
		logtalk::loaded_file_property(File, protocol(Protocol)),
		protocol_property(Protocol, lines(Line, _)).
	find_symbol(File, Category, 22, Line) :-
		logtalk::loaded_file_property(File, category(Category)),
		category_property(Category, lines(Line, _)).
	% predicates and non-terminals
	find_symbol(File, Resource, Kind, Line) :-
		(	logtalk::loaded_file_property(File, object(Object)),
			object_property(Object, declares(Predicate, Properties))
		;	logtalk::loaded_file_property(File, protocol(Protocol)),
			protocol_property(Protocol, declares(Predicate, Properties))
		;	logtalk::loaded_file_property(File, category(Category)),
			category_property(Category, declares(Predicate, Properties))
		),
		memberchk(line_count(Line), Properties),
		(	member(non_terminal(NonTerminal), Properties) ->
			Resource = NonTerminal,
			Kind = 7
		;	Resource = Predicate,
			Kind = 11
		).

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

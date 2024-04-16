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


:- category(vscode_message_streamer).

	:- info([
		version is 0:1:0,
		author is 'Jacob Friedman',
		date is 2021-08-28,
		comment is 'Rewrite compiler error and warnings messages for parsing with Visual Studio Code.'
	]).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% allow default processing of the messages
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

:- end_category.


:- object(vscode_reflection).

	:- info([
		version is 0:6:0,
		author is 'Paulo Moura',
		date is 2024-04-16,
		comment is 'Reflection support for Visual Studio Code programatic features.'
	]).

	:- public(entity/3).
	:- mode(entity(+atom, +integer, -entity_identifier), zero_or_one).
	:- info(entity/3, [
		comment is 'Find the entity (object or category) defined in the given file at the given line.',
		argnames is ['File', 'Line', 'Entity']
	]).

	:- public(find_declaration/5).
	:- mode(find_declaration(@callable, +atom, +integer, -atom, -integer), zero_or_one).
	:- info(find_declaration/5, [
		comment is 'Find the called predicate declaration file and line.',
		argnames is ['Call', 'CallFile', 'CallLine', 'DeclarationFile', 'DeclarationLine']
	]).

	:- public(find_declaration/4).
	:- mode(find_declaration(+atom, @callable, +atom, +integer), one).
	:- info(find_declaration/4, [
		comment is 'Find the called predicate declaration file and line.',
		argnames is ['Directory', 'Call', 'CallFile', 'CallLine']
	]).

	:- public(find_definition/5).
	:- mode(find_definition(@callable, +atom, +integer, -atom, -integer), zero_or_one).
	:- info(find_definition/5, [
		comment is 'Find the called predicate definition file and line.',
		argnames is ['Call', 'CallFile', 'CallLine', 'DefinitionFile', 'DefinitionLine']
	]).

	:- public(find_definition/4).
	:- mode(find_definition(+atom, @callable, +atom, +integer), one).
	:- info(find_definition/4, [
		comment is 'Find the called predicate definition file and line.',
		argnames is ['Directory', 'Call', 'CallFile', 'CallLine']
	]).

	:- public(find_type_definition/3).
	:- mode(find_type_definition(@entity_identifier, -atom, -integer), zero_or_one).
	:- info(find_type_definition/3, [
		comment is 'Find the referenced entity file and line.',
		argnames is ['Entity', 'DefinitionFile', 'DefinitionLine']
	]).

	:- public(find_type_definition/2).
	:- mode(find_type_definition(+atom, @entity_identifier), one).
	:- info(find_type_definition/2, [
		comment is 'Find the referenced entity file and line.',
		argnames is ['Directory', 'Entity']
	]).

%	:- public(find_references/5).
%	:- mode(find_references(@callable, +atom, +integer, -atom, -integer), zero_or_one).
%	:- info(find_references/5, [
%		comment is 'Find the called predicate references.',
%		argnames is ['Call', 'CallFile', 'CallLine', 'References']
%	]).

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

	% declarations

	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_declaration_(Call, CallerEntity, DeclarationFile, DeclarationLine).

	find_declaration(Directory, Call, CallFile, CallLine) :-
		atom_concat(Directory, '/.declaration_done', Data),
		open(Data, write, Stream),
		(	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) ->
			{format(Stream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
		;	true
		),
		close(Stream).

	find_declaration_(Object::Functor/Arity, _, File, Line) :-
		!,
		nonvar(Object),
		nonvar(Functor),
		nonvar(Arity),
		Object::current_predicate(Functor/Arity),
		functor(Template, Functor, Arity),
		Object::predicate_property(Template, declared_in(Entity)),
		entity_property(Entity, _, declares(Functor/Arity, Properties)),
		entity_property(Entity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_declaration_(::Functor/Arity, This, File, Line) :-
		!,
		ground(Functor/Arity),
		functor(Template, Functor, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This<<predicate_property(Template, declared_in(DeclarationEntity))
			;	create_object(Obj, [instantiates(This)], [], []),
				Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
				abolish_object(Obj)
			)
		;	%current_category(This) ->
			create_object(Obj, [imports(This)], [], []),
			Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
			abolish_object(Obj)
		),
		entity_property(DeclarationEntity, _, declares(Functor/Arity, Properties)),
		entity_property(DeclarationEntity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_declaration_(^^Functor/Arity, This, File, Line) :-
		!,
		ground(Functor/Arity),
		functor(Template, Functor, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This<<predicate_property(Template, declared_in(Entity))
			;	create_object(Obj, [instantiates(This)], [], []),
				Obj<<predicate_property(Template, declared_in(Entity)),
				abolish_object(Obj)
			)
		;	%current_category(This) ->
			create_object(Obj, [imports(This)], [], []),
			Obj<<predicate_property(Template, declared_in(Entity)),
			abolish_object(Obj)
		),
		entity_property(Entity, Kind, declares(Functor/Arity, Properties)),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	find_declaration_(Functor/Arity, This, File, Line) :-
		% predicate listed in a uses/2 directive
		ground(Functor/Arity),
		(	entity_property(This, _, calls(Object::Functor/Arity, _)) ->
			OriginalFunctor = Functor
		;	entity_property(This, _, calls(Object::OriginalFunctor/Arity, Properties)),
			member(as(Functor/Arity), Properties)
		),
		!,
		find_declaration_(Object::OriginalFunctor/Arity, This, File, Line).

	find_declaration_(Functor/Arity, This, File, Line) :-
		% local predicate
		ground(Functor/Arity),
		functor(Template, Functor, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This<<predicate_property(Template, declared_in(Entity))
			;	\+ instantiates_class(This, _) ->
				create_object(Obj, [instantiates(This)], [], []),
				Obj<<predicate_property(Template, declared_in(Entity)),
				abolish_object(Obj)
			;	This<<predicate_property(Template, declared_in(Entity))
			)
		;	%current_category(This) ->
			create_object(Obj, [imports(This)], [], []),
			Obj<<predicate_property(Template, declared_in(Entity)),
			abolish_object(Obj)
		),
		entity_property(Entity, Kind, declares(Functor/Arity, Properties)),
		entity_property(Entity, Kind, file(File)),
		memberchk(line_count(Line), Properties).

	% definitions

	find_definition(Call, CallFile, CallLine, DefinitionFile, DefinitionLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_definition_(Call, CallerEntity, DefinitionFile, DefinitionLine).

	find_definition(Directory, Call, CallFile, CallLine) :-
		atom_concat(Directory, '/.definition_done', Data),
		open(Data, write, Stream),
		(	find_definition(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) ->
			{format(Stream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
		;	true
		),
		close(Stream).

	find_definition_(Object::Functor/Arity, _, File, Line) :-
		!,
		nonvar(Object),
		nonvar(Functor),
		nonvar(Arity),
		Object::current_predicate(Functor/Arity),
		functor(Template, Functor, Arity),
		Object::predicate_property(Template, defined_in(Primary)),
		(	% local definitions
			entity_property(Primary, _, defines(Functor/Arity, Properties)),
			Entity = Primary
		;	% multifile definitions
			entity_property(Primary, _, includes(Functor/Arity, Entity, Properties))
		),
		(	memberchk(auxiliary, Properties),
			entity_property(Entity, _, calls(_, CallsProperties)),
			memberchk(alias(Functor/Arity), CallsProperties) ->
			% predicate listed in a uses/2 directive
			entity_property(Entity, _, file(File)),
			memberchk(line_count(Line), CallsProperties)
		;	entity_property(Entity, _, file(File)),
			memberchk(line_count(Line), Properties)
		).

	find_definition_(::Functor/Arity, This, File, Line) :-
		!,
		ground(Functor/Arity),
		functor(Template, Functor, Arity),
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
			entity_property(Primary, _, defines(Functor/Arity, Properties)),
			Entity = Primary
		;	% multifile definitions
			entity_property(Primary, _, includes(Functor/Arity, Entity, Properties))
		;	% local definition
			Entity = This,
			entity_property(This, _, defines(Functor/Arity, Properties))
		),
		entity_property(Entity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_definition_(^^Functor/Arity, This, File, Line) :-
		!,
		ground(Functor/Arity),
		functor(Template, Functor, Arity),
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

	find_definition_(Functor/Arity, This, File, Line) :-
		% predicate listed in a uses/2 directive
		ground(Functor/Arity),
		(	entity_property(This, _, calls(Object::Functor/Arity, _)) ->
			OriginalFunctor = Functor
		;	entity_property(This, _, calls(Object::OriginalFunctor/Arity, Properties)),
			memberchk(alias(Functor/Arity), Properties)
		),
		!,
		find_definition_(Object::OriginalFunctor/Arity, This, File, Line).

	find_definition_(Functor/Arity, This, File, Line) :-
		% local predicate
		ground(Functor/Arity),
		(	% definition
			Entity = This,
			entity_property(Entity, _, defines(Functor/Arity, Properties)) ->
			true
		;	% multifile definitions
			entity_property(This, _, includes(Functor/Arity, Entity, Properties))
		),
		entity_property(Entity, _, file(File)),
		memberchk(line_count(Line), Properties).

	% type definitions (entities)

	find_type_definition(Name/Arity, DefinitionFile, DefinitionLine) :-
		functor(Entity, Name, Arity),
		entity_property(Entity, Type, file(DefinitionFile)),
		entity_property(Entity, Type, lines(DefinitionLine, _)).

	find_type_definition(Directory, Name/Arity) :-
		atom_concat(Directory, '/.type_definition_done', Data),
		open(Data, write, Stream),
		(	find_type_definition(Name/Arity, File, Line) ->
			{format(Stream, 'File:~w;Line:~d~n', [File, Line])}
		;	true
		),
		close(Stream).

	% references

	find_references(Directory, Call, CallFile, CallLine) :-
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

	find_references_(Call, CallFile, CallLine, References) :-
		entity(CallFile, CallLine, CallerEntity),
		findall(
			Reference,
			find_reference(Call, CallerEntity, Reference),
			References
		).

	find_reference(Object::Name/Arity, _, File-Line) :-
		nonvar(Object),
		ground(Name/Arity),
		entity_property(Entity, Kind, file(File)),
		entity_property(Entity, Kind, calls(Object::Name/Arity, Properties)),
		memberchk(line_count(Line), Properties).

	% implementations

	find_implementations(Directory, Kind, Resource, ReferenceFile, ReferenceLine) :-
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
		entity(File, Line, Entity),
		findall(
			Implementation,
			find_predicate_implementation(Predicate, Entity, Implementation),
			Implementations
		).

	find_implementations_(entity, Entity, _, _, Implementations) :-
		findall(
			Implementation,
			find_entity_implementation(Entity, Implementation),
			Implementations
		).

	find_predicate_implementation(Name/Arity, Entity, File-Line) :-
		ground(Name/Arity),
		functor(Template, Name, Arity),
		entity_property(ImplementationEntity, Kind, file(File)),
		entity_property(ImplementationEntity, Kind, defines(Name/Arity, Properties)),
		(	current_object(ImplementationEntity) ->
			(	\+ instantiates_class(ImplementationEntity, _),
				\+ specializes_class(ImplementationEntity, _) ->
				ImplementationEntity<<predicate_property(Template, declared_in(DeclarationEntity))
			;	create_object(Obj, [instantiates(ImplementationEntity)], [], []),
				Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
				abolish_object(Obj)
			)
		;	%current_category(ImplementationEntity) ->
			create_object(Obj, [imports(ImplementationEntity)], [], []),
			Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
			abolish_object(Obj)
		),
		DeclarationEntity = Entity,
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

:- end_object.

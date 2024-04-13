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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2024-04-13,
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

	:- public(find_definition/5).
	:- mode(find_definition(@callable, +atom, +integer, -atom, -integer), zero_or_one).
	:- info(find_definition/5, [
		comment is 'Find the called predicate definition file and line.',
		argnames is ['Call', 'CallFile', 'CallLine', 'DefinitionFile', 'DefinitionLine']
	]).

	entity(File, Line, Entity) :-
        (	object_property(Entity, file(File)),
        	object_property(Entity, lines(BeginLine, EndLine))
		;	category_property(Entity, file(File)),
        	category_property(Entity, lines(BeginLine, EndLine))
		),
        BeginLine =< Line, Line =< EndLine,
		!.

	% declarations

	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_declaration(Call, CallerEntity, DeclarationFile, DeclarationLine).

	find_declaration(Object::Functor/Arity, _, File, Line) :-
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

	find_declaration(::Functor/Arity, This, File, Line) :-
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

	find_declaration(^^Functor/Arity, This, File, Line) :-
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

	find_declaration(Functor/Arity, This, File, Line) :-
		% predicate listed in a uses/2 directive
		ground(Functor/Arity),
		(	entity_property(This, _, calls(Object::Functor/Arity, _)) ->
			OriginalFunctor = Functor
		;	entity_property(This, _, calls(Object::OriginalFunctor/Arity, Properties)),
			member(as(Functor/Arity), Properties)
		),
		!,
		find_declaration(Object::OriginalFunctor/Arity, This, File, Line).

	find_declaration(Functor/Arity, This, File, Line) :-
		% local predicate
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

	% definitions

	find_definition(Call, CallFile, CallLine, DefinitionFile, DefinitionLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_definition(Call, CallerEntity, DefinitionFile, DefinitionLine).

	find_definition(Object::Functor/Arity, _, File, Line) :-
		!,
		nonvar(Object),
		nonvar(Functor),
		nonvar(Arity),
		Object::current_predicate(Functor/Arity),
		functor(Template, Functor, Arity),
		Object::predicate_property(Template, defined_in(Primary)),
		(	% local definitions
			Entity = Primary,
			entity_property(Primary, _, defines(Functor/Arity, Properties))
		;	% multifile definitions
			entity_property(Primary, _, includes(Functor/Arity, Entity, Properties))
		;	% local definition
			Entity = This,
			entity_property(This, _, defines(Functor/Arity, Properties))
		),
		entity_property(Entity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_definition(::Functor/Arity, This, File, Line) :-
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

	find_definition(^^Functor/Arity, This, File, Line) :-
		!,
		ground(Functor/Arity),
		functor(Template, Functor, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This<<predicate_property(Template, declared_in(DeclarationEntity)),
				This<<predicate_property(Template, redefined_from(Entity))
			;	create_object(Obj, [instantiates(This)], [], []),
				Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
				Obj<<predicate_property(Template, redefined_from(Entity)),
				abolish_object(Obj)
			)
		;	%current_category(This) ->
			create_object(Obj, [imports(This)], [], []),
			Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
			Obj<<predicate_property(Template, redefined_from(Entity)),
			abolish_object(Obj)
		),
		entity_property(Entity, _, defines(Functor/Arity, Properties)),
		entity_property(Entity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_definition(Functor/Arity, This, File, Line) :-
		% predicate listed in a uses/2 directive
		ground(Functor/Arity),
		(	entity_property(This, _, calls(Object::Functor/Arity, _)) ->
			OriginalFunctor = Functor
		;	entity_property(This, _, calls(Object::OriginalFunctor/Arity, Properties)),
			member(as(Functor/Arity), Properties)
		),
		!,
		find_definition(Object::OriginalFunctor/Arity, This, File, Line).

	find_definition(Functor/Arity, This, File, Line) :-
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

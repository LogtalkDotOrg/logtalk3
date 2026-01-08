%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2021-2026 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:87:0,
		author is 'Paulo Moura and Jacob Friedman',
		date is 2025-12-18,
		comment is 'Support for Visual Studio Code programatic features.'
	]).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- set_logtalk_flag(unknown_entities, silent).

	:- public(load/2).
	:- mode(load(+atom, +atom), one).
	:- info(load/2, [
		comment is 'Loads a file from a directory.',
		argnames is ['Directory', 'File']
	]).

	:- public(documentation/1).
	:- mode(documentation(+atom), one).
	:- info(documentation/1, [
		comment is 'Generates documentation given a loader file and a marker directory.',
		argnames is ['Directory']
	]).

	:- public(documentation_recursive/1).
	:- mode(documentation_recursive(+atom), one).
	:- info(documentation_recursive/1, [
		comment is 'Recursively generates documentation given a loader file and a marker directory.',
		argnames is ['Directory']
	]).

	:- public(diagrams/3).
	:- mode(diagrams(+atom, +atom, +atom), one).
	:- info(diagrams/3, [
		comment is 'Generates diagrams given a loader file and a marker directory.',
		argnames is ['Project', 'Directory', 'Format']
	]).

	:- public(diagrams_recursive/3).
	:- mode(diagrams_recursive(+atom, +atom, +atom), one).
	:- info(diagrams_recursive/3, [
		comment is 'Generates diagrams given a loader file and a marker directory.',
		argnames is ['Project', 'Directory', 'Format']
	]).

	:- public(dead_code/1).
	:- mode(dead_code(+atom), one).
	:- info(dead_code/1, [
		comment is 'Scans a directory for dead code.',
		argnames is ['Directory']
	]).

	:- public(dead_code_recursive/1).
	:- mode(dead_code_recursive(+atom), one).
	:- info(dead_code_recursive/1, [
		comment is 'Recursively scans a directory for dead code.',
		argnames is ['Directory']
	]).

	:- public(make/2).
	:- mode(make(+atom, +atom), one).
	:- info(make/2, [
		comment is 'Runs the make tool with the given target and marker directory.',
		argnames is ['Directory', 'Target']
	]).

	:- public(tests/2).
	:- mode(tests(+atom, +atom), one).
	:- info(tests/2, [
		comment is 'Runs the tests with the given tests driver file and marker directory.',
		argnames is ['Directory', 'Tester']
	]).

	:- public(test/3).
	:- mode(test(+atom, @callable, ++callable), one).
	:- info(test/3, [
		comment is 'Re-run a single test.',
		argnames is ['Directory', 'Object', 'Test']
	]).

	:- public(tests_file/2).
	:- mode(tests_file(+atom, +atom), one).
	:- info(tests_file/2, [
		comment is 'Runs the tests defined in the given tests file and marker directory.',
		argnames is ['Directory', 'File']
	]).

	:- public(tests_object/2).
	:- mode(tests_object(+atom, +atom), one).
	:- info(tests_object/2, [
		comment is 'Runs the tests defined in the given tests object and marker directory.',
		argnames is ['Directory', 'Object']
	]).

	:- public(metrics/1).
	:- mode(metrics(+atom), one).
	:- info(metrics/1, [
		comment is 'Computes metrics given a marker directory.',
		argnames is ['Directory']
	]).

	:- public(metrics_recursive/1).
	:- mode(metrics_recursive(+atom), one).
	:- info(metrics_recursive/1, [
		comment is 'Recursively computes code metrics given a marker directory.',
		argnames is ['Directory']
	]).

	:- public(doclet/2).
	:- mode(doclet(+atom, +atom), one).
	:- info(doclet/2, [
		comment is 'Loads the doclet file given the marker directory.',
		argnames is ['Directory', 'Doclet']
	]).

	:- public(find_entity_definition/2).
	:- mode(find_entity_definition(+atom, @atom), one).
	:- mode(find_entity_definition(+atom, @entity_indicator), one).
	:- info(find_entity_definition/2, [
		comment is 'Find the entity definition file and line.',
		argnames is ['Directory', 'Entity']
	]).

	:- public(find_predicate_definition/3).
	:- mode(find_predicate_definition(+atom, @atom, @predicate_indicator), one).
	:- mode(find_predicate_definition(+atom, @entity_indicator, @predicate_indicator), one).
	:- info(find_predicate_definition/3, [
		comment is 'Find the entity predicate definition file and line.',
		argnames is ['Directory', 'Entity', 'Predicate']
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
		comment is 'Find entity and called predicate references.',
		argnames is ['Directory', 'Call', 'CallFile', 'CallLine']
	]).

	:- public(find_implementations/4).
	:- mode(find_implementations(+atom, @predicate_indicator, +atom, +integer), one).
	:- info(find_implementations/4, [
		comment is 'Find protocol and predicate implementations.',
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

	:- public(find_ancestors/2).
	:- mode(find_ancestors(+atom, @callable), one).
	:- info(find_ancestors/2, [
		comment is 'Find the entity ancestors.',
		argnames is ['Directory', 'Entity']
	]).

	:- public(find_descendants/2).
	:- mode(find_descendants(+atom, @callable), one).
	:- info(find_descendants/2, [
		comment is 'Find the entity descendants.',
		argnames is ['Directory', 'Entity']
	]).

	:- public(find_entity_type/2).
	:- mode(find_entity_type(+atom, @callable), one).
	:- info(find_entity_type/2, [
		comment is 'Find the entity type.',
		argnames is ['Directory', 'Entity']
	]).

	:- public(find_parent_file/2).
	:- mode(find_parent_file(+atom, +atom), one).
	:- info(find_parent_file/2, [
		comment is 'Find the loader file.',
		argnames is ['Directory', 'File']
	]).

	:- public(infer_public_predicates/2).
	:- mode(infer_public_predicates(+atom, +atom), one).
	:- info(infer_public_predicates/2, [
		comment is 'Infer the public predicates for the given entity.',
		argnames is ['Directory', 'Entity']
	]).

	:- public(files_topological_sort/3).
	:- mode(files_topological_sort(+atom, +atom, +list(atom)), one).
	:- info(files_topological_sort/3, [
		comment is 'Sort files by dependencies.',
		argnames is ['Directory', 'LoaderDirectory', 'Files']
	]).

	:- public(debug/0).
	:- mode(debug, one).
	:- info(debug/0, [
		comment is 'Ensures the debugger tool is loaded and starts debugging for all defined breakpoints.'
	]).

	:- public(nodebug/0).
	:- mode(nodebug, one).
	:- info(nodebug/0, [
		comment is 'Ensures the debugger tool is loaded and stops debugging for all defined breakpoints. Also turns off tracing. Does not remove defined breakpoints.'
	]).

	:- public((spy)/1).
	:- mode(spy(@qualified_predicate_indicator), one).
	:- mode(spy(@qualified_non_terminal_indicator), one).
	:- mode(spy(@predicate_indicator), one).
	:- mode(spy(@non_terminal_indicator), one).
	:- info((spy)/1, [
		comment is 'Adds a spy point for the given (possibly qualified) predicate or non-terminal.',
		argnames is ['Predicate']
	]).

	:- public((spy)/2).
	:- mode(spy(+atom, +integer), one).
	:- info((spy)/2, [
		comment is 'Adds a breakpoint at the given file and line.',
		argnames is ['File', 'Line']
	]).

	:- public((spy)/3).
	:- mode(spy(+atom, +integer, @callable), one).
	:- info((spy)/3, [
		comment is 'Adds a conditional breakpoint at the given file and line.',
		argnames is ['File', 'Line', 'Condition']
	]).

	:- public((nospy)/1).
	:- mode(nospy(@qualified_directive_resource), one).
	:- mode(nospy(@predicate_indicator), one).
	:- info((nospy)/1, [
		comment is 'Removes a spy point for the given (possibly qualified) predicate.',
		argnames is ['Predicate']
	]).

	:- public((nospy)/2).
	:- mode(nospy(+atom, +integer), one).
	:- info((nospy)/2, [
		comment is 'Removes a breakpoint for the given file and line.',
		argnames is ['File', 'Line']
	]).

	:- public(log/3).
	:- mode(log(+atom, +integer, +atom), one).
	:- info(log/3, [
		comment is 'Adds a log point for the given file and line.',
		argnames is ['File', 'Line', 'Message']
	]).

	:- public(nolog/2).
	:- mode(nolog(+atom, +integer), one).
	:- info(nolog/2, [
		comment is 'Removes a log point for the given file and line.',
		argnames is ['File', 'Line']
	]).

	:- uses(user, [
		atomic_list_concat/2, numbervars/3
	]).

	% loading

	load(Directory, File) :-
		atom_concat(Directory, '/.vscode_loading_done', Marker),
		ignore(logtalk_load(File)),
		open(Marker, append, Stream),
		close(Stream).

	% documentation

	documentation(Directory) :-
		atom_concat(Directory, '/.vscode_xml_files_done', Marker),
		atom_concat(Directory, '/xml_docs', XMLDocs),
		ignore({
			logtalk_load(lgtdoc(loader)),
			lgtdoc::directory(Directory, [xml_docs_directory(XMLDocs)])
		}),
		open(Marker, append, Stream),
		close(Stream).

	documentation_recursive(Directory) :-
		atom_concat(Directory, '/.vscode_xml_files_done', Marker),
		atom_concat(Directory, '/xml_docs', XMLDocs),
		ignore({
			logtalk_load(lgtdoc(loader)),
			lgtdoc::rdirectory(Directory, [xml_docs_directory(XMLDocs)])
		}),
		open(Marker, append, Stream),
		close(Stream).

	% diagrams

	diagrams(Project, Directory, Format) :-
		atom_concat(Directory, '/.vscode_dot_files_done', Marker),
		atom_concat(Directory, '/dot_dias', DotDias),
		ignore({
			logtalk_load(diagrams(loader)),
			diagrams(Format)::directory(
				Project,
				Directory,
				[
					output_directory(DotDias),
					url_prefixes('vscode://file/', 'vscode://file/xml_docs/'),
					omit_path_prefixes(['$LOGTALKUSER', '$LOGTALKHOME']),
					predicate_url_target_format(other),
					zoom(true)
				]
			)
		}),
		open(Marker, append, Stream),
		close(Stream).

	diagrams_recursive(Project, Directory, Format) :-
		atom_concat(Directory, '/.vscode_dot_files_done', Marker),
		atom_concat(Directory, '/dot_dias', DotDias),
		ignore({
			logtalk_load(diagrams(loader)),
			diagrams(Format)::rdirectory(
				Project,
				Directory,
				[
					output_directory(DotDias),
					url_prefixes('vscode://file/', 'vscode://file/xml_docs/'),
					omit_path_prefixes(['$LOGTALKUSER', '$LOGTALKHOME']),
					predicate_url_target_format(other),
					zoom(true)
				]
			)
		}),
		open(Marker, append, Stream),
		close(Stream).

	% dead code

	dead_code(Directory) :-
		atom_concat(Directory, '/.vscode_dead_code_scanning_done', Marker),
		ignore({
			logtalk_load(dead_code_scanner(loader)),
			dead_code_scanner::directory(Directory)
		}),
		open(Marker, append, Stream),
		close(Stream).

	dead_code_recursive(Directory) :-
		atom_concat(Directory, '/.vscode_dead_code_scanning_done', Marker),
		ignore({
			logtalk_load(dead_code_scanner(loader)),
			dead_code_scanner::rdirectory(Directory)
		}),
		open(Marker, append, Stream),
		close(Stream).

	% make

	make(Directory, Target) :-
		atom_concat(Directory, '/.vscode_make_done', Marker),
		logtalk_make(Target),
		open(Marker, append, Stream),
		close(Stream),
		(	Target == debug ->
			ensure_debbugger
		;	true
		).

	% tests

	tests(Directory, Tester) :-
		atom_concat(Directory, '/.vscode_loading_done', Marker),
		atom_concat(Directory, '/.vscode_test_results', Data),
		setup_once_cleanup(
			open(Data, write, _, [alias(vscode_test_results)]),
			ignore({logtalk_load(Tester, [reload(always)])}),
			close(vscode_test_results)
		),
		open(Marker, append, Stream),
		close(Stream).

	test(Directory, Object, Test) :-
		atom_concat(Directory, '/.vscode_loading_done', Marker),
		atom_concat(Directory, '/.vscode_test_results', Data),
		setup_once_cleanup(
			open(Data, write, _, [alias(vscode_test_results)]),
			ignore({Object::run(Test)}),
			close(vscode_test_results)
		),
		open(Marker, append, Stream),
		close(Stream).

	tests_file(Directory, File) :-
		atom_concat(Directory, '/.vscode_loading_done', Marker),
		atom_concat(Directory, '/.vscode_test_results', Data),
		setup_once_cleanup(
			open(Data, write, _, [alias(vscode_test_results)]),
			forall(
				(	logtalk::loaded_file_property(File, object(Object)),
					extends_object(Object, lgtunit)
				),
				ignore({Object::run})
			),
			close(vscode_test_results)
		),
		open(Marker, append, Stream),
		close(Stream).

	tests_object(Directory, Object) :-
		atom_concat(Directory, '/.vscode_loading_done', Marker),
		atom_concat(Directory, '/.vscode_test_results', Data),
		setup_once_cleanup(
			open(Data, write, _, [alias(vscode_test_results)]),
			ignore({Object::run}),
			close(vscode_test_results)
		),
		open(Marker, append, Stream),
		close(Stream).

	% metrics

	metrics(Directory) :-
		atom_concat(Directory, '/.vscode_metrics_done', Marker),
		atom_concat(Directory, '/.vscode_metrics_results', Data),
		logtalk_load(code_metrics(loader)),
		setup_once_cleanup(
			open(Data, write, _, [alias(vscode_metrics_results)]),
			ignore({code_metrics::directory(Directory)}),
			close(vscode_metrics_results)
		),
		open(Marker, append, Stream),
		close(Stream).

	metrics_recursive(Directory) :-
		atom_concat(Directory, '/.vscode_metrics_done', Marker),
		atom_concat(Directory, '/.vscode_metrics_results', Data),
		logtalk_load(code_metrics(loader)),
		setup_once_cleanup(
			open(Data, write, _, [alias(vscode_metrics_results)]),
			ignore({code_metrics::rdirectory(Directory)}),
			close(vscode_metrics_results)
		),
		open(Marker, append, Stream),
		close(Stream).

	% doclet

	doclet(Directory, Doclet) :-
		atom_concat(Directory, '/.vscode_loading_done', Marker),
		ignore((
			logtalk_load(doclet(loader)),
			logtalk_load(Doclet)
		)),
		open(Marker, append, Stream),
		close(Stream).

	% definition from entity indentifier

	find_entity_definition(Directory, Entity0) :-
		(	Entity0 = Name/Arity ->
			functor(Entity, Name, Arity)
		;	Entity = Entity0
		),
		atom_concat(Directory, '/.vscode_entity_definition', Data),
		atom_concat(Directory, '/.vscode_entity_definition_done', Marker),
		open(Data, write, DataStream),
		(	entity_property(Entity, _, file(File)),
			entity_property(Entity, _, lines(Line, _)) ->
			{format(DataStream, 'File:~w;Line:~d~n', [File, Line])}
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	% definition from entity indentifier and predicate indicator

	find_predicate_definition(Directory, Entity0, Name//Arity) :-
		!,
		ExtArity is Arity + 2,
		find_predicate_definition(Directory, Entity0, Name/ExtArity).
	find_predicate_definition(Directory, Entity0, Predicate) :-
		(	Entity0 = Name/Arity ->
			functor(Entity, Name, Arity)
		;	Entity = Entity0
		),
		atom_concat(Directory, '/.vscode_predicate_definition', Data),
		atom_concat(Directory, '/.vscode_predicate_definition_done', Marker),
		open(Data, write, DataStream),
		(	entity_property(Entity, _, defines(Predicate, Properties)),
			(	member(include(File), Properties) ->
				true
			;	entity_property(Entity, _, file(File))
			),
			memberchk(line_count(Line), Properties) ->
			{format(DataStream, 'File:~w;Line:~d~n', [File, Line])}
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	% declarations

	find_declaration(Directory, Call, CallFile, CallLine) :-
		atom_concat(Directory, '/.vscode_declaration', Data),
		atom_concat(Directory, '/.vscode_declaration_done', Marker),
		open(Data, write, DataStream),
		(	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) ->
			{format(DataStream, 'File:~w;Line:~d~n', [DeclarationFile, DeclarationLine])}
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_declaration(Call, CallFile, CallLine, DeclarationFile, DeclarationLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_declaration_(Call, CallerEntity, CallLine, DeclarationFile, DeclarationLine).

	find_declaration_(Alias::Name/Arity, Entity, _, File, Line) :-
		callable(Alias),
		atom(Name),
		integer(Arity),
		(	entity_property(Entity, _, alias(Alias, Properties)),
			\+ member(predicate, Properties),
			member(for(Object), Properties) ->
			true
		;	current_object(Alias) ->
			Object = Alias
		;	false
		),
		Object \== user,
		functor(Template, Name, Arity),
		Object::predicate_property(Template, declared_in(DeclarationEntity, Line)),
		entity_property(DeclarationEntity, _, file(File)).

	% non-terminal
	find_declaration_(Object::Name/Arity, Entity, CallerLine, File, Line) :-
		callable(Object),
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(Object::Name/ExtArity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_declaration_(Object::Name/ExtArity, Entity, CallerLine, File, Line).

	% multifile predicate
	find_declaration_(Other::Name/Arity, Entity, _, File, Line) :-
		callable(Other),
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, provides(Name/Arity, Other, _)),
		entity_property(Other, Kind, declares(Name/Arity, Properties)),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).

	% multifile non-terminal
	find_declaration_(Other::Name/Arity, Entity, _, File, Line) :-
		callable(Other),
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, provides(Name/ExtArity, Other, _)),
		entity_property(Other, Kind, declares(Name/ExtArity, Properties)),
		memberchk(non_terminal(Name//Arity), Properties),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).

	find_declaration_(::Name/Arity, Entity, CallerLine, File, Line) :-
		(	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) ->
			true
		;	ExtArity is Arity + 2,
			entity_property(Entity, _, calls(::Name/ExtArity, Properties)),
			memberchk(lines(Start, End), Properties),
			Start =< CallerLine, CallerLine =< End,
			find_declaration_(::Name/ExtArity, Entity, CallerLine, File, Line)
		).

	find_declaration_(^^Name/Arity, Entity, CallerLine, File, Line) :-
		(	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) ->
			true
		;	ExtArity is Arity + 2,
			entity_property(Entity, _, calls(^^Name/ExtArity, Properties)),
			memberchk(lines(Start, End), Properties),
			Start =< CallerLine, CallerLine =< End,
			find_declaration_(^^Name/ExtArity, Entity, CallerLine, File, Line)
		).

	find_declaration_(@Name/Arity, Entity, CallerLine, File, Line) :-
		(	complements_object(Entity, Object) ->
			(	find_declaration_(Name/Arity, Object, _, File, Line) ->
				true
			;	ExtArity is Arity + 2,
				find_declaration_(Name/ExtArity, Object, _, File, Line)
			)
		;	% object or non-complementing category
			find_declaration_(Name/Arity, Entity, CallerLine, File, Line)
		).

	% locally declared
	find_declaration_(Name/Arity, Entity, _, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, Kind, declares(Name/Arity, Properties)),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Entity, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).

	% non-local declaration
	find_declaration_(Name/Arity, Entity, _, File, Line) :-
		atom(Name),
		integer(Arity),
		functor(Template, Name, Arity),
		(	current_object(Entity) ->
			Entity \== user,
			(	Entity<<predicate_property(Template, declared_in(DeclarationEntity, Line)) ->
				true
			;	once((
					instantiates_class(Entity, _)
				;	specializes_class(Entity, _)
				)),
				create_object(Obj, [instantiates(Entity)], [], []),
				Obj<<predicate_property(Template, declared_in(DeclarationEntity, Line))
			)
		;	current_category(Entity),
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
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, calls(Object::Name/Arity, Properties)),
		callable(Object),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_declaration_(Object::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in a uses/2 directive
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
		memberchk(alias(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_declaration_(Object::OriginalName/Arity, Entity, CallerLine, File, Line).

	% predicate listed in an alias/2 directive
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, alias(_/Arity, Properties)),
		memberchk(for(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(from(Entity), Properties),
		find_declaration_(Entity::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in an alias/2 directive
	find_declaration_(Name/Arity, Entity, CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, alias(Name/Arity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(for(OriginalName/Arity), Properties),
		memberchk(from(Entity), Properties),
		find_declaration_(Entity::OriginalName/Arity, Entity, CallerLine, File, Line).

	% definitions

	find_definition(Directory, Call, CallFile, CallLine) :-
		atom_concat(Directory, '/.vscode_definition', Data),
		atom_concat(Directory, '/.vscode_definition_done', Marker),
		open(Data, write, DataStream),
		(	find_definition(Call, CallFile, CallLine, DefinitionFile, DefinitionLine),
			% for dynamic predicates, the reflection API may return line 0
			DefinitionLine >= 1 ->
			{format(DataStream, 'File:~w;Line:~d~n', [DefinitionFile, DefinitionLine])}
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_definition(Call, CallFile, CallLine, DefinitionFile, DefinitionLine) :-
		entity(CallFile, CallLine, CallerEntity),
		find_definition_(Call, CallerEntity, CallLine, DefinitionFile, DefinitionLine).

	find_definition_(Alias::Name/Arity, Entity, _, File, Line) :-
		callable(Alias),
		atom(Name),
		integer(Arity),
		(	entity_property(Entity, _, alias(Alias, AliasProperties)),
			\+ member(predicate, AliasProperties),
			member(for(Object), AliasProperties) ->
			true
		;	Object = Alias
		),
		functor(Template, Name, Arity),
		current_object(Object),
		Object \== user,
		Object::predicate_property(Template, defined_in(Primary)),
		(	% local definitions
			entity_property(Primary, _, defines(Name/Arity, Properties)),
			\+ member(auxiliary, Properties),
			DefinitionEntity = Primary
		;	% multifile definitions
			entity_property(Primary, _, includes(Name/Arity, DefinitionEntity, Properties))
		;	% predicate listed in a uses/2 directive
			entity_property(Primary, _, defines(Name/Arity, DefinesProperties)),
			memberchk(auxiliary, DefinesProperties),
			entity_property(Primary, _, calls(_, Properties)),
			memberchk(caller(Name/Arity), Properties),
			DefinitionEntity = Primary
		),
		entity_property(DefinitionEntity, _, file(File)),
		memberchk(line_count(Line), Properties).

	find_definition_(Object::Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
		callable(Object),
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(Object::Name/ExtArity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallLine, CallLine =< End,
		find_definition_(Object::Name/ExtArity, Entity, CallLine, File, Line).

	find_definition_(::Name/Arity, This, _, File, Line) :-
		atom(Name),
		integer(Arity),
		functor(Template, Name, Arity),
		(	% definition
			(	current_object(This) ->
				(	\+ instantiates_class(This, _),
					\+ specializes_class(This, _) ->
					This \== user,
					This<<predicate_property(Template, declared_in(DeclarationEntity)),
					This<<predicate_property(Template, defined_in(Primary))
				;	create_object(Obj, [instantiates(This)], [], []),
					Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
					Obj<<predicate_property(Template, defined_in(Primary)),
					abolish_object(Obj)
				)
			;	current_category(This),
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
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(::Name/ExtArity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallLine, CallLine =< End,
		find_definition_(::Name/ExtArity, Entity, CallLine, File, Line).

	find_definition_(^^Name/Arity, This, _, File, Line) :-
		atom(Name),
		integer(Arity),
		functor(Template, Name, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This \== user,
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
		;	current_category(This),
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
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		entity_property(Entity, _, calls(^^Name/ExtArity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallLine, CallLine =< End,
		find_definition_(^^Name/ExtArity, Entity, CallLine, File, Line).

	find_definition_(@Name/Arity, Entity, CallLine, File, Line) :-
		(	complements_object(Entity, Object) ->
			(	find_definition_(Name/Arity, Object, _, File, Line),
				entity_property(Object, _, file(File)) ->
				true
			;	ExtArity is Arity + 2,
				find_definition_(Name/ExtArity, Object, _, File, Line),
				entity_property(Object, _, file(File))
			)
		;	current_object(Entity),
			find_definition_(Name/Arity, Entity, CallLine, File, Line)
		).

	% non-local definition
	find_definition_(Name/Arity, This, _, File, Line) :-
		atom(Name),
		integer(Arity),
		functor(Template, Name, Arity),
		(	current_object(This) ->
			(	\+ instantiates_class(This, _),
				\+ specializes_class(This, _) ->
				This<<predicate_property(Template, defined_in(Entity, Line))
			;	create_object(Obj, [instantiates(This)], [], []),
				Obj<<predicate_property(Template, defined_in(Entity, Line)),
				abolish_object(Obj)
			)
		;	current_category(This),
			create_object(Obj, [imports(This)], [], []),
			Obj<<predicate_property(Template, defined_in(Entity, Line)),
			abolish_object(Obj)
		),
		% ensure non-local definition as this clause cannot handle included files
		Entity \= This,
		entity_property(Entity, _, file(File)).

	% local predicate or non-terminal
	find_definition_(Name/Arity, Entity, _CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		(	% definition
			entity_property(Entity, _, defines(Name/Arity, Properties)) ->
			(	member(include(File), Properties) ->
				true
			;	entity_property(Entity, _, file(File))
			),
			memberchk(line_count(Line), Properties)
		;	% multifile definitions
			entity_property(Entity, _, includes(Name/Arity, DefinitionEntity, Properties)) ->
			entity_property(DefinitionEntity, _, file(File)),
			memberchk(line_count(Line), Properties)
		;	% non-terminal
			ExtArity is Arity + 2,
			entity_property(Entity, _, defines(Name/ExtArity, Properties)),
			(	member(include(File), Properties) ->
				true
			;	entity_property(Entity, _, file(File))
			),
			memberchk(line_count(Line), Properties)
		).

	% predicate listed in a uses/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, calls(Object::Name/Arity, Properties)),
		callable(Object),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_definition_(Object::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in a uses/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, calls(Object::OriginalName/Arity, Properties)),
		memberchk(alias(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		find_definition_(Object::OriginalName/Arity, Entity, CallerLine, File, Line).

	% predicate listed in an alias/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, alias(_/Arity, Properties)),
		memberchk(for(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(from(Entity), Properties),
		find_definition_(Entity::Name/Arity, Entity, CallerLine, File, Line).
	% predicate alias listed in an alias/2 directive
	find_definition_(Name/Arity, Entity, CallerLine, File, Line) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, alias(Name/Arity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< CallerLine, CallerLine =< End,
		memberchk(for(OriginalName/Arity), Properties),
		memberchk(from(Entity), Properties),
		find_definition_(Entity::OriginalName/Arity, Entity, CallerLine, File, Line).

	find_definition_(Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		(	entity_property(Entity, _, calls(Name/ExtArity, Properties)),
			memberchk(non_terminal(Name//Arity), Properties),
			OriginalName = Name
		;	entity_property(Entity, _, calls(OriginalName/ExtArity, Properties)),
			memberchk(alias(Name/ExtArity), Properties),
			memberchk(non_terminal(OriginalName//Arity), Properties)
		),
		find_definition_(OriginalName/ExtArity, Entity, CallLine, File, Line).

	find_definition_(Name/Arity, Entity, CallLine, File, Line) :-
		% non-terminal
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		(	entity_property(Entity, _, calls(Object::Name/ExtArity, Properties)),
			memberchk(non_terminal(Name//Arity), Properties),
			OriginalName = Name
		;	entity_property(Entity, _, calls(Object::OriginalName/ExtArity, Properties)),
			memberchk(alias(Name/ExtArity), Properties),
			memberchk(non_terminal(OriginalName//Arity), Properties)
		),
		callable(Object),
		find_definition_(Object::OriginalName/ExtArity, Entity, CallLine, File, Line).

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
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_type_definition_(Name/Arity, ReferenceEntity, DefinitionFile, DefinitionLine) :-
		atom(Name),
		integer(Arity),
		functor(Entity, Name, Arity),
		(	entity_property(ReferenceEntity, _, alias(Entity, Properties)),
			\+ member(predicate, Properties),
			memberchk(for(Original), Properties) ->
			% object alias
			entity_property(Original, Type, file(DefinitionFile)),
			entity_property(Original, Type, lines(DefinitionLine, _))
		;	% not an alias
			entity_property(Entity, Type, file(DefinitionFile)),
			entity_property(Entity, Type, lines(DefinitionLine, _))
		).

	% references

	find_references(Directory, Resource, ResourceFile, ResourceLine) :-
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
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_references_(Resource, File, Line, References) :-
		entity(File, Line, Entity),
		(	entity_property(Entity, _, directive(Start, End)),
			Start =< Line, Line =< End ->
			find_entity_references(Resource, References)
		;	find_predicate_references(Resource, Entity, File, Line, References)
		).

	% non-terminal
	find_predicate_references(Name//Arity, _, File, Line, References) :-
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		find_references_(Name/ExtArity, File, Line, References).

	% predicate scope directive
	find_predicate_references(Name/Arity, Entity, File, Line, References) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, declares(Name/Arity, Properties)),
		memberchk(line_count(Line), Properties),
		findall(
			CallerFile-CallerLine,
			(	entity_property(Caller, _, calls(Object::Name/Arity, CallsProperties)),
				callable(Object),
				memberchk(line_count(CallerLine), CallsProperties),
				find_declaration_(Object::Name/Arity, Caller, CallerLine, File, Line),
				(	member(include(CallerFile), CallsProperties) ->
					true
				;	entity_property(Caller, _, file(CallerFile))
				)
			),
			References0
		),
		findall(
			CallerFile-CallerLine,
			(	entity_property(Caller, _, calls(::Name/Arity, CallsProperties)),
				memberchk(line_count(CallerLine), CallsProperties),
				find_declaration_(::Name/Arity, Caller, CallerLine, File, Line),
				(	member(include(CallerFile), CallsProperties) ->
					true
				;	entity_property(Caller, _, file(CallerFile))
				)
			),
			References1,
			References0
		),
		findall(
			CallerFile-CallerLine,
			(	entity_property(Caller, _, calls(^^Name/Arity, CallsProperties)),
				memberchk(line_count(CallerLine), CallsProperties),
				find_declaration_(^^Name/Arity, Caller, CallerLine, File, Line),
				(	member(include(CallerFile), CallsProperties) ->
					true
				;	entity_property(Caller, _, file(CallerFile))
				)
			),
			References2,
			References1
		),
		findall(
			CallerFile-CallerLine,
			(	entity_property(Entity, _, calls(Name/Arity, CallsProperties)),
				memberchk(line_count(CallerLine), CallsProperties),
				(	member(include(CallerFile), CallsProperties) ->
					true
				;	CallerFile = File
				)
			),
			References3,
			References2
		),
		findall(
			UpdaterFile-UpdaterLine,
			(	entity_property(Updater, _, updates(Object::Name/Arity, UpdatesProperties)),
				callable(Object),
				memberchk(line_count(UpdaterLine), UpdatesProperties),
				find_declaration_(Object::Name/Arity, Updater, UpdaterLine, File, Line),
				(	member(include(UpdaterFile), UpdatesProperties) ->
					true
				;	entity_property(Updater, _, file(UpdaterFile))
				)
			),
			References4,
			References3
		),
		findall(
			UpdaterFile-UpdaterLine,
			(	entity_property(Updater, _, updates(::Name/Arity, UpdatesProperties)),
				memberchk(line_count(UpdaterLine), UpdatesProperties),
				find_declaration_(::Name/Arity, Updater, UpdaterLine, File, Line),
				(	member(include(UpdaterFile), UpdatesProperties) ->
					true
				;	entity_property(Updater, _, file(UpdaterFile))
				)
			),
			References5,
			References4
		),
		findall(
			UpdaterFile-UpdaterLine,
			(	entity_property(Entity, _, updates(Name/Arity, UpdatesProperties)),
				memberchk(line_count(UpdaterLine), UpdatesProperties),
				(	member(include(UpdaterFile), UpdatesProperties) ->
					true
				;	UpdaterFile = File
				)
			),
			References6,
			References5
		),
		findall(
			AliasFile-AliasLine,
			(	entity_property(Caller, _, alias(_, AliasProperties)),
				memberchk(predicate, AliasProperties),
				memberchk(for(Name/Arity), AliasProperties),
				memberchk(from(Entity), AliasProperties),
				memberchk(line_count(AliasLine), AliasProperties),
				(	member(include(AliasFile), AliasProperties) ->
					true
				;	entity_property(Caller, _, file(AliasFile))
				)
			),
			References7,
			References6
		),
		findall(
			ReferenceFile-ReferenceLine,
			(	entity_property(Other, Kind, references(Entity::Name/Arity, ReferenceProperties)),
				(	member(include(ReferenceFile), ReferenceProperties) ->
					true
				;	entity_property(Other, Kind, file(ReferenceFile))
				),
				memberchk(line_count(ReferenceLine), ReferenceProperties)
			),
			References,
			References7
		),
		References \== [].

	% predicate listed in a uses/2 directive
	find_predicate_references(Name/Arity, Entity, _, Line, References) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, calls(Object::Name/Arity, Properties)),
		callable(Object),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		find_declaration_(Object::Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).
	% predicate alias listed in a uses/2 directive; look for local calls to the alias
	find_predicate_references(Name/Arity, Entity, File, Line, References) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, calls(_::_/_, Properties)),
		memberchk(alias(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		find_predicate_local_references(Name/Arity, Entity, File, Line, References).

	% predicate listed in an alias/2 directive
	find_predicate_references(Name/Arity, Entity, _, Line, References) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, alias(_/Arity, Properties)),
		memberchk(for(Name/Arity), Properties),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		memberchk(from(Object), Properties),
		find_declaration_(Object::Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).
	% predicate alias listed in an alias/2 directive
	find_predicate_references(Name/Arity, Entity, _, Line, References) :-
		atom(Name),
		integer(Arity),
		entity_property(Entity, _, alias(Name/Arity, Properties)),
		memberchk(lines(Start, End), Properties),
		Start =< Line, Line =< End,
		memberchk(for(OriginalName/Arity), Properties),
		memberchk(from(Object), Properties),
		find_declaration_(Object::OriginalName/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(OriginalName/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).

	% local predicate call; declared
	find_predicate_references(Name/Arity, Entity, File, Line, References) :-
		atom(Name),
		integer(Arity),
		find_declaration_(Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		once((
			DeclarationFile \== File
		;	DeclarationLine \== Line
		)),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).

	% local predicate call; no declaration
	find_predicate_references(Name/Arity, Entity, File, Line, References) :-
		atom(Name),
		integer(Arity),
		find_predicate_local_references(Name/Arity, Entity, File, Line, References).

	% non-terminal
	find_predicate_references(Name/Arity, Entity, File, Line, References) :-
		atom(Name),
		integer(Arity),
		ExtArity is Arity + 2,
		once((
			(	entity_property(Entity, Kind, defines(Name/ExtArity, Properties))
			;	entity_property(Entity, Kind, calls(Name/ExtArity, Properties))
			),
			memberchk(non_terminal(Name//Arity), Properties)
		)),
		find_references_(Name/ExtArity, File, Line, References).

	find_predicate_references(Alias::Name/Arity, Entity, _, Line, References) :-
		callable(Alias),
		atom(Name),
		integer(Arity),
		(	entity_property(Entity, _, alias(Alias, Properties)),
			\+ member(predicate, Properties),
			member(for(Object), Properties) ->
			true
		;	Object = Alias
		),
		find_declaration_(Object::Name/Arity, Entity, Line, DeclarationFile, DeclarationLine),
		entity(DeclarationFile, DeclarationLine, DeclarationEntity),
		find_predicate_references(Name/Arity, DeclarationEntity, DeclarationFile, DeclarationLine, References).

	find_predicate_references(::Name/Arity, Entity, _, Line, References) :-
		atom(Name),
		integer(Arity),
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

	find_predicate_references(^^Name/Arity, Entity, _, Line, References) :-
		atom(Name),
		integer(Arity),
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

	find_predicate_local_references(Name/Arity, Entity, File, Line, References) :-
		findall(
			Reference,
			(	find_predicate_local_reference(Name/Arity, Entity, ReferenceFile, StartLine, EndLine),
				(	ReferenceFile \== File ->
					% included files case
					true
				;	\+ (StartLine =< Line, Line =< EndLine)
				),
				Reference = ReferenceFile-StartLine
			),
			References
		),
		References \== [].

	find_predicate_local_reference(Name/Arity, Entity, File, StartLine, EndLine) :-
		% local predicate
		atom(Name),
		integer(Arity),
		(	entity_property(Entity, Kind, calls(Name/Arity, _)) ->
			ExtArity = Arity
		;	ExtArity is Arity + 2,
			entity_property(Entity, Kind, defines(Name/ExtArity, DefinesProperties)),
			memberchk(non_terminal(Name//Arity), DefinesProperties)
		),
		entity_property(Entity, Kind, calls(Name/ExtArity, CallsProperties)),
		(	member(include(File), CallsProperties) ->
			true
		;	entity_property(Entity, Kind, file(File))
		),
		memberchk(lines(StartLine, EndLine), CallsProperties),
		StartLine > 0.

	find_entity_references(Name/Arity, References) :-
		atom(Name),
		integer(Arity),
		functor(Entity, Name, Arity),
		findall(
			Reference,
			find_entity_reference(Entity, Reference),
			References0
		),
		% there may be e.g. multiple explicit message sending calls in the same clause
		sort(References0, References).

	% entity opening directives
	find_entity_reference(Entity, File-Line) :-
		(	atom(Entity),
			current_protocol(Entity) ->
			(	extends_protocol(Other, Entity)
			;	implements_protocol(Other, Entity)
			)
		;	current_object(Entity) ->
			(	extends_object(Other, Entity)
			;	instantiates_class(Other, Entity)
			;	specializes_class(Other, Entity)
			;	complements_object(Other, Entity)
			)
		;	current_category(Entity),
			(	extends_category(Other, Entity)
			;	imports_category(Other, Entity)
			)
		),
		entity_property(Other, Kind, file(File)),
		entity_property(Other, Kind, lines(Line, _)).
	% uses/2 directives
	find_entity_reference(Entity, File-Line) :-
		current_object(Entity),
		% Object may not be bound
		entity_property(Other, Kind, calls(Object::Predicate, Properties)),
		callable(Object),
		Object = Entity,
		memberchk(caller(Predicate), Properties),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).
	% explicit messages
	find_entity_reference(Entity, File-Line) :-
		current_object(Entity),
		% Object may not be bound
		entity_property(Other, Kind, calls(Object::_, Properties)),
		callable(Object),
		Object = Entity,
		memberchk(caller(Caller), Properties),
		entity_property(Other, Kind, defines(Caller, DefinesProperties)),
		\+ member(auxiliary, DefinesProperties),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).
	% uses/1 and alias/2 directives
	find_entity_reference(Entity, File-Line) :-
		entity_property(Other, Kind, alias(_, Properties)),
		(	member(from(Entity), Properties) ->
			% predicate alias
			true
		;	% object alias
			memberchk(for(Entity), Properties)
		),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).
	% multifile/1 predicate clauses
	find_entity_reference(Entity, File-Line) :-
		entity_property(Other, Kind, provides(_, Entity, Properties)),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).
	% entity references including from multifile predicate directives
	find_entity_reference(Entity, File-Line) :-
		(	entity_property(Other, Kind, references(Entity, Properties))
		;	entity_property(Other, Kind, references(Entity::_, Properties))
		),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).

	% implementations

	find_implementations(Directory, Resource, ReferenceFile, ReferenceLine) :-
		atom_concat(Directory, '/.vscode_implementations', Data),
		atom_concat(Directory, '/.vscode_implementations_done', Marker),
		open(Data, write, DataStream),
		(	find_implementations_(Resource, ReferenceFile, ReferenceLine, Implementations) ->
			forall(
				member(ImplementationFile-ImplementationLine, Implementations),
				{format(DataStream, 'File:~w;Line:~d~n', [ImplementationFile, ImplementationLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_implementations_(Resource, File, Line, Implementations) :-
		entity(File, Line, Entity),
		(	entity_property(Entity, _, directive(Start, End)),
			Start =< Line, Line =< End ->
			find_protocol_implementations(Resource, Implementations)
		;	find_predicate_implementations(Resource, Entity, Implementations)
		).

	find_protocol_implementations(Resource, Implementations) :-
		ground(Resource),
		Resource = Protocol/0,
		current_protocol(Protocol),
		findall(
			File-Line,
			(	implements_protocol(Entity, Protocol),
				entity_property(Entity, _, file(File)),
				entity_property(Entity, _, lines(Line, _))
			),
			Implementations
		).

	find_predicate_implementations(Predicate, Entity, Implementations) :-
		ground(Predicate),
		findall(
			Implementation,
			find_predicate_implementation(Predicate, Entity, Implementation),
			Implementations
		).

	% non-terminal
	find_predicate_implementation(Name//Arity, Entity, File-Line) :-
		ExtArity is Arity + 2,
		find_predicate_implementation(Name/ExtArity, Entity, File-Line).
	% locally defined predicate
	find_predicate_implementation(Name/Arity, Entity, File-Line) :-
		entity_property(Entity, Kind, defines(Name/Arity, Properties)),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Entity, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).
	% multifile predicate
	find_predicate_implementation(Name/Arity, Entity, File-Line) :-
		entity_property(Entity, _, includes(Name/Arity, Other, Properties)),
		(	member(include(File), Properties) ->
			true
		;	entity_property(Other, _, file(File))
		),
		memberchk(line_count(Line), Properties).
	% descendant definitions
	find_predicate_implementation(Name/Arity, Entity, File-Line) :-
		functor(Template, Name, Arity),
		entity_property(ImplementationEntity, Kind, defines(Name/Arity, Properties)),
		ImplementationEntity \= Entity,
		(	current_object(ImplementationEntity) ->
			(	\+ instantiates_class(ImplementationEntity, _),
				\+ specializes_class(ImplementationEntity, _) ->
				ImplementationEntity \== user,
				ImplementationEntity<<predicate_property(Template, declared_in(DeclarationEntity))
			;	(	create_object(Obj, [instantiates(ImplementationEntity)], [], []),
					Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
					abolish_object(Obj)
				;	ImplementationEntity<<predicate_property(Template, declared_in(DeclarationEntity))
				)
			)
		;	current_category(ImplementationEntity),
			create_object(Obj, [imports(ImplementationEntity)], [], []),
			Obj<<predicate_property(Template, declared_in(DeclarationEntity)),
			abolish_object(Obj)
		),
		DeclarationEntity = Entity,
		(	member(include(File), Properties) ->
			true
		;	entity_property(ImplementationEntity, Kind, file(File))
		),
		memberchk(line_count(Line), Properties).

	% callers

	find_callers(Directory, Predicate, ReferenceFile, ReferenceLine) :-
		atom_concat(Directory, '/.vscode_callers', Data),
		atom_concat(Directory, '/.vscode_callers_done', Marker),
		open(Data, write, DataStream),
		(	find_callers_(Predicate, ReferenceFile, ReferenceLine, Callers0) ->
			% multiple clauses can result in repeated callers
			sort(Callers0, Callers),
			forall(
				member(c(Name, CallerFile, CallerLine), Callers),
				{format(DataStream, 'Name:~w;File:~w;Line:~d~n', [Name, CallerFile, CallerLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
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

	find_callees(Directory, Predicate, ReferenceFile, ReferenceLine) :-
		atom_concat(Directory, '/.vscode_callees', Data),
		atom_concat(Directory, '/.vscode_callees_done', Marker),
		open(Data, write, DataStream),
		(	find_callees_(Predicate, ReferenceFile, ReferenceLine, Callees0) ->
			% multiple clauses can result in repeated callees
			sort(Callees0, Callees),
			forall(
				member(c(Name, CalleeFile, CalleeLine), Callees),
				{format(DataStream, 'Name:~w;File:~w;Line:~d~n', [Name, CalleeFile, CalleeLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
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

	% ancestors

	find_ancestors(Directory, Entity) :-
		atom_concat(Directory, '/.vscode_ancestors', Data),
		atom_concat(Directory, '/.vscode_ancestors_done', Marker),
		open(Data, write, DataStream),
		(	find_ancestors_(Entity, Ancestors) ->
			forall(
				member(a(Type, Ancestor, AncestorFile, AncestorLine), Ancestors),
				{format(DataStream, 'Type:~w;Name:~w;File:~w;Line:~d~n', [Type, Ancestor, AncestorFile, AncestorLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_ancestors_(Entity, Ancestors) :-
		callable(Entity),
		(	current_object(Entity) ->
			findall(
				a(Type, Ancestor, File, Line),
				(	(	implements_protocol(Entity, Ancestor), Type = protocol
					;	imports_category(Entity, Ancestor), Type = category
					;	extends_object(Entity, Ancestor), Type = object
					;	instantiates_class(Entity, Ancestor), Type = object
					;	specializes_class(Entity, Ancestor), Type = object
					;	complements_object(Ancestor, Entity), Type = category
					),
					entity_property(Ancestor, _, file(File)),
					entity_property(Ancestor, _, lines(Line, _)),
					ground_entity(Ancestor)
				),
				Ancestors
			)
		;	atom(Entity),
			current_protocol(Entity) ->
			findall(
				a(Type, Ancestor, File, Line),
				(	extends_protocol(Entity, Ancestor), Type = protocol,
					entity_property(Ancestor, _, file(File)),
					entity_property(Ancestor, _, lines(Line, _)),
					ground_entity(Ancestor)
				),
				Ancestors
			)
		;	current_category(Entity),
			findall(
				a(Type, Ancestor, File, Line),
				(	(	extends_category(Entity, Ancestor), Type = category
					;	implements_protocol(Entity, Ancestor), Type = protocol
					),
					entity_property(Ancestor, _, file(File)),
					entity_property(Ancestor, _, lines(Line, _)),
					ground_entity(Ancestor)
				),
				Ancestors
			)
		).

	% descendants

	find_descendants(Directory, Entity) :-
		atom_concat(Directory, '/.vscode_descendants', Data),
		atom_concat(Directory, '/.vscode_descendants_done', Marker),
		open(Data, write, DataStream),
		(	find_descendants_(Entity, Descendants) ->
			forall(
				member(d(Type, Descendant, DescendantsFile, DescendantsLine), Descendants),
				{format(DataStream, 'Type:~w;Name:~w;File:~w;Line:~d~n', [Type, Descendant, DescendantsFile, DescendantsLine])}
			)
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_descendants_(Entity, Descendants) :-
		callable(Entity),
		(	current_object(Entity) ->
			findall(
				d(Type, Descendant, File, Line),
				(	(	extends_object(Descendant, Entity), Type = object
					;	instantiates_class(Descendant, Entity), Type = object
					;	specializes_class(Descendant, Entity), Type = object
					),
					entity_property(Descendant, _, file(File)),
					entity_property(Descendant, _, lines(Line, _)),
					ground_entity(Descendant)
				),
				Descendants
			)
		;	atom(Entity),
			current_protocol(Entity) ->
			findall(
				d(Type, Descendant, File, Line),
				(	(	extends_protocol(Descendant, Entity), Type = protocol
					;	implements_protocol(Descendant, Entity), (current_object(Descendant) -> Type = object; Type = category)
					),
					entity_property(Descendant, _, file(File)),
					entity_property(Descendant, _, lines(Line, _)),
					ground_entity(Descendant)
				),
				Descendants
			)
		;	current_category(Entity),
			findall(
				d(Type, Descendant, File, Line),
				(	(	extends_category(Descendant, Entity), Type = category
					;	imports_category(Descendant, Entity), Type = object
					;	complements_object(Entity, Descendant), Type = object
					),
					entity_property(Descendant, _, file(File)),
					entity_property(Descendant, _, lines(Line, _)),
					ground_entity(Descendant)
				),
				Descendants
			)
		).

	% entity type

	find_entity_type(Directory, Entity) :-
		atom_concat(Directory, '/.vscode_type', Data),
		atom_concat(Directory, '/.vscode_type_done', Marker),
		open(Data, write, DataStream),
		(	current_object(Entity) ->
			Type = object
		;	atom(Entity),
			current_protocol(Entity) ->
			Type = protocol
		;	% current_category(Entity),
			Type = category
		),
		{format(DataStream, '~w', [Type])},
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	% loader file

	find_parent_file(Directory, File) :-
		atom_concat(Directory, '/.vscode_find_parent', Data),
		atom_concat(Directory, '/.vscode_find_parent_done', Marker),
		open(Data, write, DataStream),
		(	find_parent_file_(File, Loader) ->
			{format(DataStream, '~w', [Loader])}
		;	true
		),
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	find_parent_file_(File, Loader) :-
		(	{'$lgt_environment_variable'('COMSPEC', _)} ->
			% assume running on Windows
			current_logtalk_flag(prolog_dialect, Dialect),
			% workaround backends downcasing file paths on Windows
			(	Dialect == swi ->
				{downcase_atom(File, FileAlt)}
			;	Dialect == sicstus ->
				downcase_atom_portable(File, FileAlt)
			;	Dialect == eclipse ->
				{'$lgt_prolog_os_file_name'(FileAlt, File)}
			;	FileAlt = File
			)
		;	FileAlt = File
		),
		(	logtalk::loaded_file_property(FileAlt, parent(Loader)) ->
			true
		;	logtalk::loaded_file_property(Loader, includes(FileAlt)) ->
			true
		;	fail
		).

	% infer public predicates

	infer_public_predicates(Directory, Entity) :-
		atom_concat(Directory, '/.vscode_infer_public_predicates', Data),
		atom_concat(Directory, '/.vscode_infer_public_predicates_done', Marker),
		open(Data, write, DataStream),
		(	current_object(Entity) ->
			findall(Predicate, (object_property(Entity, defines(Predicate, _)), \+ object_property(Entity, calls(Predicate, _))), Predicates)
		;	current_category(Entity) ->
			findall(Predicate, (category_property(Entity, defines(Predicate, _)), \+ category_property(Entity, calls(Predicate, _))), Predicates)
		;	Predicates = []
		),
		{format(DataStream, '~q', [Predicates])},
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	% sort files by dependencies

	files_topological_sort(Directory, LoaderDirectory, Files) :-
		atom_concat(Directory, '/.vscode_files_topological_sort', Data),
		atom_concat(Directory, '/.vscode_files_topological_sort_done', Marker),
		open(Data, write, DataStream),
		expand_files(Files, LoaderDirectory, FilePaths),
		findall(Path, member(_-Path, FilePaths), Paths),
		logtalk::loaded_files_topological_sort(Paths, SortedPaths),
		findall(File, (member(Path, SortedPaths), memberchk(File-Path, FilePaths)), SortedFiles),
		{format(DataStream, '~q', [SortedFiles])},
		close(DataStream),
		open(Marker, write, MarkerStream),
		close(MarkerStream).

	expand_files([], _, []).
	expand_files([File| Files], LoaderDirectory, [File-Path| FilePaths]) :-
		(	is_absolute_file_name(File) ->
			Path0 = File
		;	atomic_list_concat([LoaderDirectory, '/', File], Path0)
		),
		(	(	{'$lgt_file_extension'(logtalk, Extension)},
				atom_concat(Path0, Extension, Path1)
			;	{'$lgt_file_extension'(prolog, Extension)},
				atom_concat(Path0, Extension, Path1)
			;	Path1 = Path0
			),
			{'$lgt_expand_path'(Path1, Path)},
			{'$lgt_file_exists'(Path)} ->
			true
		;	{'$lgt_expand_path'(Path0, Path)}
		),
		expand_files(Files, LoaderDirectory, FilePaths).

	is_absolute_file_name(Path) :-
		(	{'$lgt_environment_variable'('COMSPEC', _)} ->
			sub_atom(Path, 1, 1, _, ':'),
			sub_atom(Path, 0, 1, _, Drive),
			(	a @=< Drive, Drive @=< z ->
				true
			;	'A' @=< Drive, Drive @=< 'Z'
			)
		;	% assume POSIX or POSIX compatible operating-system
			sub_atom(Path, 0, 1, _, '/')
		).

	% debugger support

	debug :-
		ensure_debbugger,
		{debugger::debug}.

	nodebug :-
		ensure_debbugger,
		{debugger::nodebug}.

	spy(Predicate) :-
		ensure_debbugger,
		spy_(Predicate), !.

	spy_(Name/Arity) :-
		{debugger::spy(Name/Arity)}.
	spy_(Name//Arity) :-
		{debugger::spy(Name//Arity)}.
	spy_(Entity::Name/Arity) :-
		{debugger::spy(Entity::Name/Arity)}.
	spy_(Entity::Name//Arity) :-
		{debugger::spy(Entity::Name//Arity)}.
	spy_((Sender, This, Self, Goal)) :-
		{debugger::spy(Sender, This, Self, Goal)}.

	spy(File, Line) :-
		ensure_debbugger,
		entity(File, Line, Entity),
		{debugger::spy(Entity-Line)}.

	spy(File, Line, Condition) :-
		ensure_debbugger,
		entity(File, Line, Entity),
		{debugger::spy(Entity, Line, Condition)}.

	nospy(Predicate) :-
		ensure_debbugger,
		nospy_(Predicate), !.

	nospy_(Name/Arity) :-
		{debugger::nospy(Name/Arity)}.
	nospy_(Name//Arity) :-
		{debugger::nospy(Name//Arity)}.
	nospy_(Entity::Name/Arity) :-
		{debugger::nospy(Entity::Name/Arity)}.
	nospy_(Entity::Name//Arity) :-
		{debugger::nospy(Entity::Name//Arity)}.
	nospy_((Sender, This, Self, Goal)) :-
		{debugger::nospy(Sender, This, Self, Goal)}.

	nospy(File, Line) :-
		ensure_debbugger,
		entity(File, Line, Entity),
		{debugger::nospy(Entity-Line)},
		{debugger::nospy(Entity, Line, _)}.

	log(File, Line, Message) :-
		ensure_debbugger,
		entity(File, Line, Entity),
		{debugger::log(Entity, Line, Message)}.

	nolog(File, Line) :-
		ensure_debbugger,
		entity(File, Line, Entity),
		{debugger::nolog(Entity, Line, _)}.

	ensure_debbugger :-
		(	current_object(debugger) ->
			true
		;	logtalk_load(debugger(loader))
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
	entity(File, Line, Entity) :-
		{'$lgt_environment_variable'('COMSPEC', _)},
		% assume running on Windows
		current_logtalk_flag(prolog_dialect, Dialect),
		% workaround backends downcasing file paths on Windows
		(	Dialect == swi ->
			{downcase_atom(File, FileAlt)}
		;	Dialect == sicstus ->
			downcase_atom_portable(File, FileAlt)
		;	Dialect == eclipse,
			{'$lgt_prolog_os_file_name'(FileAlt, File)}
		),
        (	object_property(Entity, file(FileAlt)),
        	object_property(Entity, lines(BeginLine, EndLine))
		;	category_property(Entity, file(FileAlt)),
        	category_property(Entity, lines(BeginLine, EndLine))
		;	protocol_property(Entity, file(FileAlt)),
        	protocol_property(Entity, lines(BeginLine, EndLine))
		),
        BeginLine =< Line, Line =< EndLine,
		!.
	entity(File, _, Entity) :-
		logtalk::loaded_file_property(Includer, includes(File, Line)),
		% this only works for directly included files
		entity(Includer, Line, Entity),
		!.
	entity(File, _, Entity) :-
		% handle the case of nested included files
        (	entity_property(Entity, _, declares(_, Properties))
		;	entity_property(Entity, _, defines(_, Properties))
		;	entity_property(Entity, _, provides(_, _, Properties))
		;	entity_property(Entity, _, calls(_, Properties))
		;	entity_property(Entity, _, updates(_, Properties))
		),
		memberchk(include(File), Properties),
		!.

	entity_property(Object, object, Property) :-
		catch(object_property(Object, Property), _, fail).
	entity_property(Category, category, Property) :-
		catch(category_property(Category, Property), _, fail).
	entity_property(Protocol, protocol, Property) :-
		catch(protocol_property(Protocol, Property), _, fail).

	ground_entity(Entity) :-
		(	atom(Entity) ->
			true
		;	entity_property(Entity, _, info(Info)),
			(	member(parameters(Parameters), Info) ->
				ground_entity_parameters(Entity, Parameters)
			;	member(parnames(Parnames), Info) ->
				ground_entity_parnames(Entity, Parnames)
			;	numbervars(Entity, 0, _)
			)
		).

	ground_entity_parameters(Entity, Parameters) :-
		Entity =.. [_| Arguments],
		ground_entity_parameters_(Arguments, Parameters).

	ground_entity_parameters_([], []).
	ground_entity_parameters_([Argument| Arguments], [Argument-_| Parameters]) :-
		!,
		ground_entity_parameters_(Arguments, Parameters).
	ground_entity_parameters_([_| Arguments], [_| Parameters]) :-
		ground_entity_parameters_(Arguments, Parameters).

	ground_entity_parnames(Entity, Parnames) :-
		Entity =.. [_| Arguments],
		ground_entity_parnames_(Arguments, Parnames).

	ground_entity_parnames_([], []).
	ground_entity_parnames_([Argument| Arguments], [Argument| Parnames]) :-
		!,
		ground_entity_parnames_(Arguments, Parnames).
	ground_entity_parnames_([_| Arguments], [_| Parnames]) :-
		ground_entity_parnames_(Arguments, Parnames).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

	downcase_atom_portable(AnyAtom, Lower) :-
		atom_chars(AnyAtom, AnyChars),
		downcase_atom_portable_(AnyChars, LowerChars),
		atom_chars(Lower, LowerChars).

	% ASCII only and avoiding 0'Char notation that would break some backends!
	downcase_atom_portable_([], []).
	downcase_atom_portable_([AnyChar| AnyChars], [LowerChar| LowerChars]) :-
		(	'A' @=< AnyChar, AnyChar @=< 'Z' ->
			char_code(AnyChar, AnyCode),
			LowerCode is AnyCode + 32,
			char_code(LowerChar, LowerCode)
		;	LowerChar = AnyChar
		),
		downcase_atom_portable_(AnyChars, LowerChars).

	:- meta_predicate(setup_once_cleanup(*, *, *)).
	setup_once_cleanup(Setup, Call, Cleanup) :-
		call(Setup),
		(   catch(Call, Error, (Cleanup, throw(Error))) ->
			call(Cleanup)
		;	call(Cleanup),
			fail
		).

	% rewrite compiler error and warnings messages for parsing with Visual Studio Code

	:- multifile(logtalk::message_prefix_file/6).
	:- dynamic(logtalk::message_prefix_file/6).

	logtalk::message_prefix_file(Kind, Component, Prefix, File, append, [alias(vscode_scratch_messages)]) :-
		message_prefix_file(Kind, Component, Prefix),
		logtalk::expand_library_path(logtalk_user('scratch/.messages'), File).

	% compiler warnings and errors
	message_prefix_file(error,      core,              '!     ').
	message_prefix_file(error(_),   core,              '!     ').
	message_prefix_file(warning,    core,              '*     ').
	message_prefix_file(warning(_), core,              '*     ').
	% dead_code_scanner tool warnings
	message_prefix_file(warning,    dead_code_scanner, '*     ').
	message_prefix_file(warning(_), dead_code_scanner, '*     ').
	% lgtdoc tool warnings
	message_prefix_file(warning,    lgtdoc,            '*     ').
	message_prefix_file(warning(_), lgtdoc,            '*     ').
	% lgtunit tool warning and errors
	message_prefix_file(error,      lgtunit,           '!     ').
	message_prefix_file(error(_),   lgtunit,           '!     ').
	message_prefix_file(warning,    lgtunit,           '*     ').
	message_prefix_file(warning(_), lgtunit,           '*     ').

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% fail after processing to allow default processing of the messages

	% compiling file messages
	logtalk::message_hook(compiling_file(File, _), _, core, _) :-
		(	stream_property(Stream, alias(vscode_scratch_messages)) ->
			true
		;	logtalk::expand_library_path(logtalk_user('scratch/.messages'), Messages),
			open(Messages, append, Stream, [alias(vscode_scratch_messages)])
		),
		logtalk::print_message_tokens(Stream, '% ', ['[ compiling ~w ... ]'-[File], nl, nl]),
		close(Stream),
		fail.
	% lgtunit test results
	logtalk::message_hook(tests_results_summary(Object, Total, Skipped, Passed, Failed, Flaky, Note), _, lgtunit, _) :-
		stream_property(_, alias(vscode_test_results)),
		entity_property(Object, Kind, file(File)),
		entity_property(Object, Kind, lines(Line, _)),
		(	Note == '' ->
			{format(vscode_test_results, 'File:~w;Line:~d;Object:~k;Status:~d tests: ~d skipped, ~d passed, ~d failed (~d flaky)~n', [File, Line, Object, Total, Skipped, Passed, Failed, Flaky])}
		;	{format(vscode_test_results, 'File:~w;Line:~d;Object:~k;Status:~d tests: ~d skipped, ~d passed, ~d failed (~d flaky; ~w~n)', [File, Line, Object, Total, Skipped, Passed, Failed, Flaky, Note])}
		),
		fail.
	:- if(current_logtalk_flag(prolog_dialect, ji)).
		logtalk::message_hook(passed_test(Object, Test, File, Start-_End, Flaky, _Note, CPUTime, WallTime), _, lgtunit, _) :-
			stream_property(_, alias(vscode_test_results)),
			flaky_text(Flaky, FlakyText),
			{format(vscode_test_results, 'File:~w;Line:~d;Object:~k;Test:~k;Status:passed~w(in ~f/~f cpu/wall seconds)~n', [File, Start, Object, Test, FlakyText, CPUTime, WallTime])},
			fail.
		logtalk::message_hook(failed_test(Object, Test, File, Start-_End, Reason, Flaky, _Note, CPUTime, WallTime), _, lgtunit, _) :-
			stream_property(_, alias(vscode_test_results)),
			flaky_text(Flaky, FlakyText),
			{format(vscode_test_results, 'File:~w;Line:~d;Object:~k;Test:~k;Status:failed~w(in ~f/~f cpu/wall seconds);Reason:', [File, Start, Object, Test, FlakyText, CPUTime, WallTime])},
			reason_format(Reason),
			nl(vscode_test_results),
			fail.
	:- else.
		logtalk::message_hook(passed_test(Object, Test, File, Start-_End, Flaky, _Note, CPUTime, WallTime), _, lgtunit, _) :-
			stream_property(_, alias(vscode_test_results)),
			flaky_text(Flaky, FlakyText),
			{format(vscode_test_results, 'File:~w;Line:~d;Object:~k;Test:~k;Status:passed~w(in ~9f/~9f cpu/wall seconds)~n', [File, Start, Object, Test, FlakyText, CPUTime, WallTime])},
			fail.
		logtalk::message_hook(failed_test(Object, Test, File, Start-_End, Reason, Flaky, _Note, CPUTime, WallTime), _, lgtunit, _) :-
			stream_property(_, alias(vscode_test_results)),
			flaky_text(Flaky, FlakyText),
			{format(vscode_test_results, 'File:~w;Line:~d;Object:~k;Test:~k;Status:failed~w(in ~9f/~9f cpu/wall seconds);Reason:', [File, Start, Object, Test, FlakyText, CPUTime, WallTime])},
			reason_format(Reason),
			nl(vscode_test_results),
			fail.
	:- endif.
	logtalk::message_hook(skipped_test(Object, Test, File, Start-_, Flaky, _Note), _, lgtunit, _) :-
		stream_property(_, alias(vscode_test_results)),
		flaky_text(Flaky, FlakyText),
		{format(vscode_test_results, 'File:~w;Line:~d;Object:~k;Test:~k;Status:skipped~w~n', [File, Start, Object, Test, FlakyText])},
		fail.
	logtalk::message_hook(entity_predicate_coverage(Entity, Predicate, Covered, Total, _Percentage, Clauses), _, lgtunit, _) :-
		stream_property(_, alias(vscode_test_results)),
		entity_property(Entity, Kind, file(File0)),
		(	Predicate = Name/Arity ->
			entity_property(Entity, Kind, defines(Name/Arity, Properties))
		;	Predicate = Name//Arity,
			ExtArity is Arity + 2,
			entity_property(Entity, Kind, defines(Name/ExtArity, Properties))
		),
		(	member(include(File), Properties) ->
			true
		;	File = File0
		),
		memberchk(lines(Line, _), Properties),
		(	Covered =:= Total ->
			% all clause are covered
			(	Total =:= 1 ->
				{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~w clause - ~w~n', [File, Line, Covered/Total, '(all)'])}
			;	{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~w clauses - ~w~n', [File, Line, Covered/Total, '(all)'])}
			)
		;	(	Total =:= 1 ->
				{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~w clause - ~w~n', [File, Line, Covered/Total, Clauses])}
			;	{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~w clauses - ~w~n', [File, Line, Covered/Total, Clauses])}
			)
		),
		fail.
	:- if(current_logtalk_flag(prolog_dialect, ji)).
		logtalk::message_hook(entity_coverage(Entity, Covered, Total, Percentage), _, lgtunit, _) :-
			stream_property(_, alias(vscode_test_results)),
			entity_property(Entity, Kind, file(File)),
			entity_property(Entity, Kind, lines(Line, _)),
			(	Total =:= 1 ->
				{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~d/~d clause (~f%)~n', [File, Line, Covered, Total, Percentage])}
			;	{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~d/~d clauses (~f%)~n', [File, Line, Covered, Total, Percentage])}
			),
			fail.
	:- else.
		logtalk::message_hook(entity_coverage(Entity, Covered, Total, Percentage), _, lgtunit, _) :-
			stream_property(_, alias(vscode_test_results)),
			entity_property(Entity, Kind, file(File)),
			entity_property(Entity, Kind, lines(Line, _)),
			(	Total =:= 1 ->
				{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~d/~d clause (~2f%)~n', [File, Line, Covered, Total, Percentage])}
			;	{format(vscode_test_results, 'File:~w;Line:~d;Status:Tests coverage: ~d/~d clauses (~2f%)~n', [File, Line, Covered, Total, Percentage])}
			),
			fail.
	:- endif.
	% code_metrics tool results
	logtalk::message_hook(entity_score(cc_metric, Entity, Score), _, code_metrics, _) :-
		stream_property(_, alias(vscode_metrics_results)),
		entity_property(Entity, Kind, file(File)),
		entity_property(Entity, Kind, lines(Line, _)),
		{format(vscode_metrics_results, 'File:~w;Line:~d;Score:~d~n', [File, Line, Score])},
		fail.
	% debugger messages
	logtalk::message_hook(fact(Entity, Head, _, File, Line), _, debugger, _) :-
		logtalk::expand_library_path(logtalk_user('scratch/.debug_info'), DebugInfo),
		open(DebugInfo, write, Stream),
		{format(Stream, 'File:~w;Line:~d;Head:~q @ ~q~n', [File, Line, Head, Entity])},
		close(Stream),
		fail.
	logtalk::message_hook(rule(Entity, Head, _, File, Line), _, debugger, _) :-
		logtalk::expand_library_path(logtalk_user('scratch/.debug_info'), DebugInfo),
		open(DebugInfo, write, Stream),
		{format(Stream, 'File:~w;Line:~d;Head:~q @ ~q~n', [File, Line, Head, Entity])},
		close(Stream),
		fail.

	flaky_text(true,  ' [flaky] ').
	flaky_text(false, ' ').

	reason_format(Reason) :-
		current_object(lgtunit),
		!,
		% avoid make check warning in the next line when lgtunit is not loaded
		Object = lgtunit,
		phrase(Object::failed_test_reason(Reason), Tokens0),
		replace_nl_tokens(Tokens0, Tokens),
		logtalk::print_message_tokens(vscode_test_results, '', Tokens).
	reason_format(Reason) :-
		writeq(vscode_test_results, Reason).

	replace_nl_tokens([], []).
	replace_nl_tokens([nl| Tokens0], Tokens) :-
		!,
		replace_nl_tokens(Tokens0, Tokens).
	replace_nl_tokens([Token| Tokens0], [Token| Tokens]) :-
		replace_nl_tokens(Tokens0, Tokens).

:- end_object.

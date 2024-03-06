%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2016-2024 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2016 Barry Evans <barryevans@kyndi.com>
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


:- object(dead_code_scanner,
	imports(options)).

	:- info([
		version is 0:13:0,
		author is 'Barry Evans and Paulo Moura',
		date is 2024-03-06,
		comment is 'A tool for detecting *likely* dead code in compiled Logtalk entities and Prolog modules compiled as objects.',
		remarks is [
			'Dead code' - 'A predicate or non-terminal that is not called (directly or indirectly) by any scoped predicate or non-terminal. These predicates and non-terminals are not used, cannot be called without breaking encapsulation, and are thus considered dead code.',
			'Known issues' - 'Use of local meta-calls with goal arguments only know at runtime can result in false positives. Calls from non-standard meta-predicates may be missed if the meta-calls are not optimized.',
			'Requirements' - 'Source files must be compiled with the ``source_data`` flag turned on. To avoid false positives do to meta-calls, compilation of source files with the ``optimized`` flag turned on is also advised.'
		]
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	% Use the structured printing mechanism in order to allow results to be
	% intercepted for alternative reporting by e.g. GUI IDEs
	:- uses(logtalk, [
		print_message/3
	]).

	:- uses(type, [
		valid/2
	]).

	:- public(entity/1).
	:- mode(entity(+entity_identifier), zero_or_one).
	:- info(entity/1, [
		comment is 'Scans a loaded entity for dead code. Fails if the entity does not exist.',
		argnames is ['Entity']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list(compound)), zero_or_one).
	:- info(file/2, [
		comment is 'Scans all entities in a loaded source file for dead code using the given options. The file can be given by name, basename, full path, or using library notation. Fails if the file is not loaded.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), zero_or_one).
	:- info(file/1, [
		comment is 'Scans all entities in a loaded source file for dead code using default options. The file can be given by name, basename, full path, or using library notation. Fails if the file is not loaded.',
		argnames is ['File']
	]).

	:- public(directory/2).
	:- mode(directory(+atom, +list(compound)), one).
	:- info(directory/2, [
		comment is 'Scans all entities in all loaded files from a given directory for dead code using the given options.',
		argnames is ['Directory', 'Options']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Scans all entities in all loaded files from a given directory for dead code using default options.',
		argnames is ['Directory']
	]).

	:- public(rdirectory/2).
	:- mode(rdirectory(+atom, +list(compound)), one).
	:- info(rdirectory/2, [
		comment is 'Scans all entities in all loaded files from a given directory and its sub-directories for dead code using the given options.',
		argnames is ['Directory', 'Options']
	]).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Scans all entities in all loaded files from a given directory and its sub-directories for dead code using default options.',
		argnames is ['Directory']
	]).

	:- public(library/2).
	:- mode(library(+atom, +list(compound)), one).
	:- info(library/2, [
		comment is 'Scans all entities in all loaded files from a given library for dead code using the given options.',
		argnames is ['Library', 'Options']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Scans all entities in all loaded files from a given library for dead code using default options.',
		argnames is ['Library']
	]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list(compound)), one).
	:- info(rlibrary/2, [
		comment is 'Scans all entities in all loaded files in a loaded library and its sub-libraries for dead code using the given options.',
		argnames is ['Library', 'Options']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Scans all entities in all loaded files in a loaded library and its sub-libraries for dead code using default options.',
		argnames is ['Library']
	]).

	:- public(all/1).
	:- mode(all(+list(compound)), one).
	:- info(all/1, [
		comment is 'Scans all entities for dead code using the given options.',
		argnames is ['Options']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Scans all entities for dead code using default options.'
	]).

	:- public(predicates/2).
	:- mode(predicates(+entity_identifier, -list(predicate_indicator)), one).
	:- info(predicates/2, [
		comment is 'Returns an ordered set of local predicates (and non-terminals) that are not used, directly or indirectly, by scoped predicates for a loaded entity.',
		argnames is ['Entity', 'Predicates']
	]).

	:- public(predicate/2).
	:- mode(predicate(+entity_identifier, ?predicate_indicator), zero_or_more).
	:- info(predicate/2, [
		comment is 'Enumerates, by backtracking, local predicates (and non-terminals) that are not used, directly or indirectly, by scoped predicates for a loaded entity.',
		argnames is ['Entity', 'Predicate']
	]).

	predicates(Entity, Predicates) :-
		(	setof(Predicate, predicate(Entity, Predicate), Predicates) ->
			true
		;	Predicates = []
		).

	predicate(Entity, Predicate) :-
		\+ (atom(Entity), current_protocol(Entity)),
		predicate(Entity, Predicate, _, _).

	% local predicates not called, directly or indirectly, by scoped predicates
	predicate(Entity, Predicate, File, Line) :-
		non_scoped_predicate(Entity, Predicate0, File, Line),
		\+ used_by_scoped_predicate(Entity, Predicate0),
		% likely dead predicate found; check if it resulted
		% from the compilation of a non-terminal
		(	entity_property(Entity, defines(Predicate0, Properties)),
			member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		).
	% unused predicates and non-terminals listed in the uses/2 directives
	predicate(Entity, Object::Resource, File, Line) :-
		entity_property(Entity, calls(Object::Original, CallsProperties)),
		(	member(caller(Original), CallsProperties) ->
			Predicate = Original,
			entity_property(Entity, defines(Predicate, DefinesProperties))
		;	memberchk(alias(Alias), CallsProperties),
			memberchk(caller(Alias), CallsProperties),
			Predicate = Alias,
			entity_property(Entity, defines(Alias, DefinesProperties))
		),
		memberchk(auxiliary, DefinesProperties),
		memberchk(number_of_clauses(1), DefinesProperties),
		% Predicate :- Object::Predicate linking clause that is generated when
		% processing uses/2 directives for allowing runtime use of listed resources
		\+ (
			entity_property(Entity, calls(Object::Original, OtherCallsProperties)),
			memberchk(caller(Caller), OtherCallsProperties),
			Caller \== Original,
			\+ member(alias(Caller), OtherCallsProperties)
		),
		% no other callers for Object::Predicate
		\+ entity_property(Entity, updates(Object::Original, _)),
		% not a predicate used as argument in calls to the database built-in methods
		\+ local_scope_directive(Entity, Predicate),
		\+ inherited_scope_directive(Entity, Predicate),
		% not a predicate (or non-terminal) made available (via a scope
		% directive) by the object containing the uses/2 directive
		(	memberchk(non_terminal(NonTerminal), CallsProperties),
			memberchk(non_terminal(NonTerminal), DefinesProperties) ->
			Resource = NonTerminal
		;	Resource = Predicate
		),
		(	member(include(File), CallsProperties) ->
			true
		;	entity_property(Entity, file(File))
		),
		memberchk(line_count(Line), CallsProperties).
	% unused predicates and non-terminals listed in the use_module/2 directives
	predicate(Entity, ':'(Module,Resource), File, Line) :-
		entity_property(Entity, calls(':'(Module,Original), CallsProperties)),
		(	member(caller(Original), CallsProperties),
			Predicate = Original,
			entity_property(Entity, defines(Predicate, DefinesProperties))
		;	memberchk(alias(Alias), CallsProperties),
			memberchk(caller(Alias), CallsProperties),
			Predicate = Alias,
			entity_property(Entity, defines(Alias, DefinesProperties))
		),
		memberchk(auxiliary, DefinesProperties),
		memberchk(number_of_clauses(1), DefinesProperties),
		% Predicate :- Module:Predicate linking clause that is generated when
		% processing uses/2 directives for allowing runtime use of listed resources
		\+ (
			entity_property(Entity, calls(':'(Module,Original), OtherCallsProperties)),
			memberchk(caller(Caller), OtherCallsProperties),
			Caller \== Original,
			\+ member(alias(Caller), OtherCallsProperties)
		),
		% no other callers for Module:Predicate
		\+ entity_property(Entity, updates(':'(Module,Original), _)),
		% not a predicate used as argument in calls to the database built-in methods
		\+ local_scope_directive(Entity, Predicate),
		\+ inherited_scope_directive(Entity, Predicate),
		% not a predicate (or non-terminal) made available (via a scope
		% directive) by the object containing the use_module/2 directive
		(	memberchk(non_terminal(NonTerminal), CallsProperties),
			memberchk(non_terminal(NonTerminal), DefinesProperties) ->
			Resource = NonTerminal
		;	Resource = Predicate
		),
		(	member(include(File), CallsProperties) ->
			true
		;	entity_property(Entity, file(File))
		),
		memberchk(line_count(Line), CallsProperties).

	non_scoped_predicate(Entity, Alias, File, Line) :-
		entity_property(Entity, defines(Alias, Properties)),
		Alias \= (_ :: _),
		% not a Logtalk multifile predicate definition
		Alias \= ':'(_, _),
		% not a Prolog multifile predicate definition
		\+ member(auxiliary, Properties),
		% not generated by the compiler or by the term-expansion mechanism
		(	entity_property(Entity, alias(Alias, AliasProperties)) ->
			memberchk(for(Predicate), AliasProperties)
		;	Predicate = Alias
		),
		\+ local_scope_directive(Entity, Predicate),
		% no local scope directive
		\+ inherited_scope_directive(Entity, Predicate),
		% no inherited scope directive
		(	current_category(Entity),
			complements_object(Entity, Object) ->
			non_scoped_predicate(Object, Predicate, _, _)
		;	true
		),
		% no scoped predicate in category complemented object
		(	member(include(File), Properties) ->
			true
		;	entity_property(Entity, file(File))
		),
		memberchk(line_count(Line), Properties).

	inherited_scope_directive(Entity, Predicate) :-
		(	current_category(Entity) ->
			inherited_scope_directive(category, Entity, Predicate)
		;	% current_object(Entity),
			\+ instantiates_class(Entity, _),
			\+ specializes_class(Entity, _) ->
			inherited_scope_directive(prototype, Entity, Predicate)
		;	\+ instantiates_class(Entity, _) ->
			inherited_scope_directive(class, Entity, Predicate)
		;	\+ specializes_class(Entity, _) ->
			inherited_scope_directive(instance, Entity, Predicate)
		;	(	inherited_scope_directive(instance, Entity, Predicate)
			;	inherited_scope_directive(class, Entity, Predicate)
			)
		).

	inherited_scope_directive(EntityKind, Entity, Predicate) :-
		ancestor(EntityKind, Entity, AncestorKind, Ancestor),
		(	local_scope_directive(Ancestor, Predicate) ->
			true
		;	inherited_scope_directive(AncestorKind, Ancestor, Predicate)
		).

	% protocol ancestors
	ancestor(protocol, Entity, protocol, Ancestor) :-
		extends_protocol(Entity, Ancestor).
	% category ancestors
	ancestor(category, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(category, Entity, category, Ancestor) :-
		extends_category(Entity, Ancestor).
	% prototype ancestors
	ancestor(prototype, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(prototype, Entity, category, Ancestor) :-
		imports_category(Entity, Ancestor).
	ancestor(prototype, Entity, prototype, Ancestor) :-
		extends_object(Entity, Ancestor).
	% instance ancestors
	ancestor(instance, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(instance, Entity, category, Ancestor) :-
		imports_category(Entity, Ancestor).
	ancestor(instance, Entity, class, Ancestor) :-
		instantiates_class(Entity, Ancestor),
		Entity \== Ancestor.
	% class ancestors
	ancestor(class, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(class, Entity, category, Ancestor) :-
		imports_category(Entity, Ancestor).
	ancestor(class, Entity, class, Ancestor) :-
		specializes_class(Entity, Ancestor).

	local_scope_directive(Entity, Predicate) :-
		entity_property(Entity, public(Public)),
		member(Predicate, Public).
	local_scope_directive(Entity, Predicate) :-
		entity_property(Entity, protected(Protected)),
		member(Predicate, Protected).
	local_scope_directive(Entity, Predicate) :-
		entity_property(Entity, private(Private)),
		member(Predicate, Private).

	used_by_scoped_predicate(Entity, Predicate) :-
		entity_property(Entity, public(Public)),
		entity_property(Entity, protected(Protected)),
		entity_property(Entity, private(Private)),
		used_by_scoped_predicate(Predicate, Public, Protected, Private, [], Entity).

	% already inspected
	used_by_scoped_predicate(Predicate, _Public, _Protected, _Private, Tested, _Entity) :-
		member(Predicate, Tested),
		!,
		fail.
	% called by a scoped predicate
	used_by_scoped_predicate(Predicate, Public, Protected, Private, Tested, Entity) :-
		entity_property(Entity, calls(Predicate, Properties)),
		memberchk(caller(Caller), Properties),
		(	Caller = (_ :: _)
			% called from a Logtalk multifile predicate clause
		;	Caller = ':'(_, _)
			% called from a Prolog multifile predicate clause
		;	Caller == (:-)/1
			% called from an initialization/1 directive
		;	member(Caller, Public)
		;	member(Caller, Protected)
		;	member(Caller, Private)
		;	inherited_scope_directive(Entity, Caller)
		;	used_by_scoped_predicate(Caller, Public, Protected, Private, [Predicate| Tested], Entity)
		).

	entity_property(Entity, Property) :-
		(	current_object(Entity) ->
			object_property(Entity, Property)
		;	current_category(Entity) ->
			category_property(Entity, Property)
		;	atom(Entity), current_protocol(Entity) ->
			protocol_property(Entity, Property)
		;	% entity is not loaded
			fail
		).

	rlibrary(Library, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	logtalk::expand_library_path(Library, TopPath) ->
			write_scan_header('Recursive library'),
			output_rlibrary(TopPath, Options),
			write_scan_footer('Recursive library')
		;	print_message(warning, dead_code_scanner, unknown(library,Library)),
			fail
		).

	rlibrary(Library) :-
		rlibrary(Library, []).

	output_rlibrary(TopPath, Options) :-
		^^option(exclude_libraries(ExcludedLibraries), Options),
		forall(
			(	sub_library(TopPath, Library, LibraryPath),
				\+ member(Library, ExcludedLibraries)
			),
			output_directory_files(LibraryPath, Options)
		).

	sub_library(TopPath, Library, LibraryPath) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(TopPath, _RelativePath, LibraryPath).

	library(Library, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	logtalk::expand_library_path(Library, Path) ->
			write_scan_header('Library'),
			output_directory_files(Path, Options),
			write_scan_footer('Library')
		;	print_message(warning, dead_code_scanner, unknown(library,Library)),
			fail
		).

	library(Library) :-
		library(Library, []).

	rdirectory(Directory, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	os::absolute_file_name(Directory, Path),
			os::directory_exists(Path) ->
			write_scan_header('Recursive directory'),
			output_rdirectory(Path, Options),
			write_scan_footer('Recursive directory')
		;	print_message(warning, dead_code_scanner, unknown(directory,Directory)),
			fail
		).

	rdirectory(Directory) :-
		rdirectory(Directory, []).

	output_rdirectory(Directory, Options) :-
		^^option(exclude_directories(ExcludedDirectories), Options),
		setof(
			SubDirectory,
			Length^After^(
				sub_directory(Directory, SubDirectory),
				\+ (
					member(ExcludedDirectory, ExcludedDirectories),
					sub_atom(SubDirectory, 0, Length, After, ExcludedDirectory)
				)
			),
			SubDirectories
		),
		forall(
			member(SubDirectory, SubDirectories),
			output_directory_files(SubDirectory, Options)
		).

	sub_directory(Directory, SubDirectory) :-
		logtalk::loaded_file(Path),
		os::decompose_file_name(Path, SubDirectory, _),
		atom_concat(Directory, _RelativePath, SubDirectory).

	directory(Directory, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	os::absolute_file_name(Directory, Path),
			os::directory_exists(Path) ->
			write_scan_header('Directory'),
			output_directory_files(Path, Options),
			write_scan_footer('Directory')
		;	print_message(warning, dead_code_scanner, unknown(directory,Directory)),
			fail
		).

	directory(Directory) :-
		directory(Directory, []).

	output_directory_files(Directory, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		print_message(silent, dead_code_scanner, scanning_directory(Directory)),
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		logtalk::loaded_file_property(Path, directory(DirectorySlash)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		not_excluded_file(ExcludedFiles, Path, Basename),
		process_file(Path, Options),
		fail.
	output_directory_files(_, _).

	entity(Entity) :-
		(	current_object(Entity) ->
			Kind = object
		;	current_category(Entity) ->
			Kind = category
		;	current_protocol(Entity) ->
			Kind = protocol
		;	print_message(warning, dead_code_scanner, unknown(entity,Entity)),
			fail
		),
		write_scan_header('Entity'),
		process_entity(Kind, Entity),
		write_scan_footer('Entity').

	process_entity(Kind, Entity) :-
		print_message(silent, dead_code_scanner, scanning_entity(Kind, Entity)),
		Kind \== protocol,
		predicate(Entity, Predicate, File, Line),
		print_message(warning, dead_code_scanner, dead_predicate(Entity, Predicate, File, Line)),
		fail.
	process_entity(_, _).

	file(Source, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		locate_file(Source, Path),
		write_scan_header('File'),
		process_file(Path, Options),
		write_scan_footer('File').

	file(Source) :-
		file(Source, []).

	% file given in library notation
	locate_file(LibraryNotation, Path) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Path).
	% file given using its name or basename
	locate_file(Source, Path) :-
		add_extension(Source, Basename),
		logtalk::loaded_file_property(Path, basename(Basename)),
		% check that there isn't another file with the same basename
		% from a different directory
		\+ (
			logtalk::loaded_file_property(OtherPath, basename(Basename)),
			Path \== OtherPath
		),
		!.
	% file given using a full path
	locate_file(Source, Path) :-
		add_extension(Source, SourceWithExtension),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		atom_concat(Directory, Basename, SourceWithExtension),
		!.

	add_extension(Source, SourceWithExtension) :-
		% ensure that Source is not specified using library notation
		atom(Source),
		os::decompose_file_name(Source, _, _, SourceExtension),
		(	logtalk::file_type_extension(source, SourceExtension) ->
			% source file extension present
			SourceWithExtension = Source
		;	% try possible source extensions
			logtalk::file_type_extension(source, Extension),
			atom_concat(Source, Extension, SourceWithExtension)
		).

	process_file(Path, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		print_message(silent, dead_code_scanner, scanning_file(Path)),
		(	logtalk::loaded_file_property(Path, object(Entity)),
			Kind = object
		;	logtalk::loaded_file_property(Path, category(Entity)),
			Kind = category
		),
		\+ member(Entity, ExcludedEntities),
		process_entity(Kind, Entity),
		fail.
	process_file(_, _).

	all(UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(exclude_entities(ExcludedEntities), Options),
		(	current_object(Entity),
			Kind = object
		;	current_category(Entity),
			Kind = category
		),
		\+ member(Entity, ExcludedEntities),
		process_entity(Kind, Entity),
		fail.
	all(_).

	all :-
		write_scan_header('All entities'),
		all([]),
		write_scan_footer('All entities').

	write_scan_header(Type) :-
		print_message(silent, dead_code_scanner, scan_started),
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(silent, dead_code_scanner, scan_start_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)),
		print_message(comment, dead_code_scanner, scanning_for_dead_code).

	write_scan_footer(Type) :-
		print_message(comment, dead_code_scanner, completed_scanning_for_dead_code),
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(silent, dead_code_scanner, scan_end_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)),
		print_message(silent, dead_code_scanner, scan_ended).

	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, exclude only the "startup" and "scratch_directory" libraries:
	default_option(exclude_libraries([startup, scratch_directory])).

	valid_option(exclude_directories(Directories)) :-
		valid(list(atom), Directories).
	valid_option(exclude_files(Files)) :-
		valid(list(atom), Files).
	valid_option(exclude_entities(Entities)) :-
		valid(list(atom), Entities).
	valid_option(exclude_libraries(Libraries)) :-
		valid(list(atom), Libraries).

	not_excluded_file([], _, _).
	not_excluded_file([ExcludedFile| ExcludedFiles], Path, Basename) :-
		% files in the exclusion list may be given by full path or by basename
		\+ member(Path, [ExcludedFile| ExcludedFiles]),
		\+ member(Basename, [ExcludedFile| ExcludedFiles]),
		% files in the exclusion list may be given with or without extension
		\+ (	logtalk::file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Path),
				member(Source, [ExcludedFile| ExcludedFiles])
		),
		\+ (	logtalk::file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Basename),
				member(Source, [ExcludedFile| ExcludedFiles])
		).

	fix_option(exclude_directories(Directories0), exclude_directories(Directories)) :-
		normalize_directory_paths(Directories0, Directories).

	normalize_directory_paths([], []).
	normalize_directory_paths([Directory0| Directories0], [Directory| Directories]) :-
		os::internal_os_path(Directory1, Directory0),
		os::absolute_file_name(Directory1, Directory2),
		(	sub_atom(Directory2, _, _, 0, '/') ->
			Directory = Directory2
		;	atom_concat(Directory2, '/', Directory)
		),
		normalize_directory_paths(Directories0, Directories).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with multifile predicates
	% that are called from a file but not defined in that file
	:- multifile(logtalk_library_path/2).
	:- dynamic(logtalk_library_path/2).
:- endif.

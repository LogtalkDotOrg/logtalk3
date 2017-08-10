%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2016 Barry Evans <barryevans@kyndi.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(dead_code_scanner).

	:- info([
		version is 0.11,
		author is 'Barry Evans and Paulo Moura',
		date is 2017/08/10,
		comment is 'A tool for detecting *likely* dead code in compiled Logtalk entities and Prolog modules compiled as objects.',
		remarks is [
			'Dead code' - 'A predicate or non-terminal that is not called (directly or indirectly) by any scoped predicate or non-terminal. These predicates and non-terminals are not used, cannot be called without breaking encapsulation, and are thus considered dead code.',
			'Know issues' - 'Use of local meta-calls with goal arguments only know at runtime can result in false positives. Calls from non-standard meta-predicates may be missed if the meta-calls are not optimized.',
			'Requirements' - 'Source files must be compiled with the source_data flag turned on. To avoid false positives do to meta-calls, compilation of source files with the optimized flag turned on is also advised.'
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

	:- public(entity/1).
	:- mode(entity(+entity_identifier), zero_or_one).
	:- info(entity/1, [
		comment is 'Scans a loaded entity for dead code. Fails if the entity does not exist.',
		argnames is ['Entity']
	]).

	:- public(file/1).
	:- mode(file(+atom), zero_or_one).
	:- info(file/1, [
		comment is 'Scans all entities in a loaded source file for dead code. The file can be given by name, basename, full path, or using library notation. Fails if the file is not loaded.',
		argnames is ['File']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Scans all entities in all loaded files from a given directory for dead code.',
		argnames is ['Directory']
	]).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Scans all entities in all loaded files from a given directory and its sub-directories for dead code.',
		argnames is ['Directory']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Scans all entities in all loaded files from a given library for dead code.',
		argnames is ['Library']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Scans all entities in all loaded files in a loaded library and its sub-libraries for dead code.',
		argnames is ['Library']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Scans all entities for dead code.'
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
		entity_property(Entity, calls(Object::Predicate, CallsProperties)),
		memberchk(caller(Predicate), CallsProperties),
		entity_property(Entity, defines(Predicate, DefinesProperties)),
		memberchk(auxiliary, DefinesProperties),
		memberchk(number_of_clauses(1), DefinesProperties),
		% Predicate :- Object::Predicate linking clause that is generated when
		% processing uses/2 directives for allowing runtime use of listed resources
		\+ (
			entity_property(Entity, calls(Object::Predicate, OtherCallsProperties)),
			memberchk(caller(Caller), OtherCallsProperties),
			Caller \== Predicate
		),
		% no other callers for Object::Predicate
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
		entity_property(Entity, calls(':'(Module,Predicate), CallsProperties)),
		memberchk(caller(Predicate), CallsProperties),
		entity_property(Entity, defines(Predicate, DefinesProperties)),
		memberchk(auxiliary, DefinesProperties),
		memberchk(number_of_clauses(1), DefinesProperties),
		% Predicate :- Module:Predicate linking clause that is generated when
		% processing uses/2 directives for allowing runtime use of listed resources
		\+ (
			entity_property(Entity, calls(':'(Module,Predicate), OtherCallsProperties)),
			memberchk(caller(Caller), OtherCallsProperties),
			Caller \== Predicate
		),
		% no other callers for Module:Predicate
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
		Alias \= _::_,
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
		(	Caller = _::_
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

	rlibrary(Library) :-
		(	logtalk::expand_library_path(Library, TopPath) ->
			write_scan_header('Recursive library'),
			output_rlibrary(TopPath),
			write_scan_footer('Recursive library')
		;	print_message(warning, dead_code_scanner, unknown(library,Library)),
			fail
		).

	output_rlibrary(TopPath) :-
		forall(
			sub_library(TopPath, LibraryPath),
			output_directory_files(LibraryPath)
		).

	sub_library(TopPath, LibraryPath) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(TopPath, _RelativePath, LibraryPath).

	library(Library) :-
		(	logtalk::expand_library_path(Library, Path) ->
			write_scan_header('Library'),
			output_directory_files(Path),
			write_scan_footer('Library')
		;	print_message(warning, dead_code_scanner, unknown(library,Library)),
			fail
		).

	rdirectory(Directory) :-
		(	os::expand_path(Directory, Path),
			os::directory_exists(Path) ->
			write_scan_header('Recursive directory'),
			output_rdirectory(Path),
			write_scan_footer('Recursive directory')
		;	print_message(warning, dead_code_scanner, unknown(directory,Directory)),
			fail
		).

	output_rdirectory(Directory) :-
		setof(
			SubDirectory,
			sub_directory(Directory, SubDirectory),
			SubDirectories
		),
		forall(
			member(SubDirectory, SubDirectories),
			output_directory_files(SubDirectory)
		).

	sub_directory(Directory, SubDirectory) :-
		logtalk::loaded_file(Path),
		os::decompose_file_name(Path, SubDirectory, _),
		atom_concat(Directory, _RelativePath, SubDirectory).

	directory(Directory) :-
		(	os::expand_path(Directory, Path),
			os::directory_exists(Path) ->
			write_scan_header('Directory'),
			output_directory_files(Path),
			write_scan_footer('Directory')
		;	print_message(warning, dead_code_scanner, unknown(directory,Directory)),
			fail
		).

	output_directory_files(Directory) :-
		print_message(silent, dead_code_scanner, scanning_directory(Directory)),
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		logtalk::loaded_file_property(Path, directory(DirectorySlash)),
		process_file(Path),
		fail.
	output_directory_files(_).

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

	file(Source) :-
		locate_file(Source, Path),
		write_scan_header('File'),
		process_file(Path),
		write_scan_footer('File').

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

	process_file(Path) :-
		print_message(silent, dead_code_scanner, scanning_file(Path)),
		(	logtalk::loaded_file_property(Path, object(Entity)),
			Kind = object
		;	logtalk::loaded_file_property(Path, category(Entity)),
			Kind = category
		),
		process_entity(Kind, Entity),
		fail.
	process_file(_).

	all :-
		write_scan_header('All entities'),
		current_object(Object),
		process_entity(object, Object),
		fail.
	all :-
		current_category(Category),
		process_entity(category, Category),
		fail.
	all :-
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

:- end_object.

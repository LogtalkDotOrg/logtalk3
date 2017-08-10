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


:- object(wrapper,
	implements(expanding)).

	:- info([
		version is 0.10,
		author is 'Paulo Moura',
		date is 2017/08/10,
		comment is 'Adviser tool for porting and wrapping plain Prolog applications.',
		remarks is [
			'prolog_extensions(Extensions) option' - 'list of file name extensions used to recognize Prolog source files (default is [''.pl'',''.pro'',''.prolog''])',
			'logtalk_extension(Extension) option' - 'Logtalk file name extension to be used for the generated wrapper files (default is ''.lgt'')',
			'exclude_files(Files) option' - 'list of Prolog source files names to exclude (default is [])',
			'exclude_directories(Files) option' - 'list of sub-directory names to exclude (default is [])',
			'include_wrapped_files(Boolean)' - 'generate include/1 directives for the wrapped Prolog source files (default is true)'
		]		
	]).

	:- uses(logtalk, [
		print_message/3
	]).

	:- public(rdirectory/2).
	:- mode(rdirectory(+atom, +list(compound)), one).
	:- info(rdirectory/2, [
		comment is 'Advises the user on missing directives for converting all plain Prolog files in a directory and its sub-directories to Logtalk objects using the specified options.',
		argnames is ['Directory', 'Options']
	]).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Advises the user on missing directives for converting all plain Prolog files in a directory and its sub-directories to Logtalk objects using default options.',
		argnames is ['Directory']
	]).

	:- public(directory/2).
	:- mode(directory(+atom, +list(compound)), one).
	:- info(directory/2, [
		comment is 'Advises the user on missing directives for converting all plain Prolog files in a directory to Logtalk objects using the specified options.',
		argnames is ['Directory', 'Options']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Advises the user on missing directives for converting all plain Prolog files in a directory to Logtalk objects using default options.',
		argnames is ['Directory']
	]).

	:- public(directories/2).
	:- mode(directories(+list(atom), +list(compound)), one).
	:- info(directories/2, [
		comment is 'Advises the user on missing directives for converting all Prolog files in a set of directories to Logtalk objects using the specified options.',
		argnames is ['Directories', 'Options']
	]).

	:- public(directories/1).
	:- mode(directories(+list(atom)), one).
	:- info(directories/1, [
		comment is 'Advises the user on missing directives for converting all Prolog files in a set of directories to Logtalk objects using default options.',
		argnames is ['Directories']
	]).

	:- public(files/2).
	:- mode(files(+list(atom), +list(compound)), one).
	:- info(files/2, [
		comment is 'Advises the user on missing directives for converting a list of plain Prolog files to Logtalk objects using the specified options.',
		argnames is ['Files', 'Options']
	]).

	:- public(files/1).
	:- mode(files(+list(atom)), one).
	:- info(files/1, [
		comment is 'Advises the user on missing directives for converting a list of plain Prolog files to Logtalk objects using default options.',
		argnames is ['Files']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list(compound)), one).
	:- info(file/2, [
		comment is 'Advises the user on missing directives for converting a plain Prolog file to Logtalk objects using the specified options.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Advises the user on missing directives for converting a plain Prolog file to Logtalk objects using default options.',
		argnames is ['File']
	]).

	:- public(save/1).
	:- mode(save(+list(compound)), one).
	:- info(save/1, [
		comment is 'Saves the generated wrapper objects (plus a loader file per directory) for all advised files using the specified options. The wrapper objects are saved to the same directories that contain the wrapped Prolog files.',
		argnames is ['Options']
	]).

	:- public(save/0).
	:- mode(save, one).
	:- info(save/0, [
		comment is 'Saves the generated wrapper objects (plus a loader file per directory) for all advised files using default options. The wrapper objects are saved to the same directories that contain the wrapped Prolog files.'
	]).

	:- public(default_option/1).
	:- mode(default_option(?compound), zero_or_more).
	:- info(default_option/1, [
		comment is 'Enumerates by backtracking the default options used when generating the wrapper objects.',
		argnames is ['DefaultOption']
	]).

	:- public(default_options/1).
	:- mode(default_options(-list(compound)), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options used when generating the wrapper objects.',
		argnames is ['DefaultOptions']
	]).

	:- private(merge_options/2).
	:- mode(merge_options(+list(compound), -list(compound)), one).
	:- info(merge_options/2, [
		comment is 'Merges the user options with the default options, returning the list of options used when generating the wrapper objects.',
		argnames is ['UserOptions', 'Options']
	]).

	:- private(predicate_called_but_not_defined_/2).
	:- dynamic(predicate_called_but_not_defined_/2).
	:- mode(predicate_called_but_not_defined_(?atom, ?predicate_indicator), zero_or_more).
	:- info(predicate_called_but_not_defined_/2, [
		comment is 'Table of called object predicates that are not locally defined.',
		argnames is ['Object', 'Predicate']
	]).

	:- private(object_predicate_called_/3).
	:- dynamic(object_predicate_called_/3).
	:- mode(object_predicate_called_(?atom, ?atom, ?predicate_indicator), zero_or_more).
	:- info(object_predicate_called_/3, [
		comment is 'Table of called object predicates.',
		argnames is ['Object', 'Other', 'Predicate']
	]).

	:- private(module_predicate_called_/3).
	:- dynamic(module_predicate_called_/3).
	:- mode(module_predicate_called_(?atom, ?atom, ?predicate_indicator), zero_or_more).
	:- info(module_predicate_called_/3, [
		comment is 'Table of called module predicates.',
		argnames is ['Object', 'Module', 'Predicate']
	]).

	:- private(unknown_predicate_called_/2).
	:- dynamic(unknown_predicate_called_/2).
	:- mode(unknown_predicate_called_(?atom, ?predicate_indicator), zero_or_more).
	:- info(unknown_predicate_called_/2, [
		comment is 'Table of predicates called but not defined.',
		argnames is ['Object', 'Predicate']
	]).

	:- private(missing_predicate_directive_/3).
	:- dynamic(missing_predicate_directive_/3).
	:- mode(missing_predicate_directive_(?atom, ?predicate_indicator, ?predicate_indicator), zero_or_more).
	:- info(missing_predicate_directive_/3, [
		comment is 'Table of missing predicate directives.',
		argnames is ['Object', 'Directive', 'Predicate']
	]).

	:- private(non_standard_predicate_call_/2).
	:- dynamic(non_standard_predicate_call_/2).
	:- mode(non_standard_predicate_call_(?atom, ?predicate_indicator), zero_or_more).
	:- info(non_standard_predicate_call_/2, [
		comment is 'Table of called non-standard predicates.',
		argnames is ['Object', 'Predicate']
	]).

	:- private(dynamic_directive_/3).
	:- dynamic(dynamic_directive_/3).
	:- mode(dynamic_directive_(?atom, ?integer, ?predicate_indicator), zero_or_more).
	:- info(dynamic_directive_/3, [
		comment is 'Table of declared dynamic predicates.',
		argnames is ['Object', 'Line', 'Predicate']
	]).

	:- private(multifile_directive_/3).
	:- dynamic(multifile_directive_/3).
	:- mode(multifile_directive_(?atom, ?integer, ?predicate_indicator), zero_or_more).
	:- info(multifile_directive_/3, [
		comment is 'Table of declared multifile predicates.',
		argnames is ['Object', 'Line', 'Predicate']
	]).

	:- private(add_directive_before_entity_/2).
	:- dynamic(add_directive_before_entity_/2).
	:- mode(add_directive_before_entity_(?atom, ?predicate_indicator), zero_or_more).
	:- info(add_directive_before_entity_/2, [
		comment is 'Table of directives to be added before the entity opening directive.',
		argnames is ['Object', 'Directive']
	]).

	:- private(add_directive_/2).
	:- dynamic(add_directive_/2).
	:- mode(add_directive_(?atom, ?predicate_indicator), zero_or_more).
	:- info(add_directive_/2, [
		comment is 'Table of directives to be added.',
		argnames is ['Object', 'Directive']
	]).

	:- private(add_directive_/3).
	:- dynamic(add_directive_/3).
	:- mode(add_directive_(?atom, ?predicate_indicator, ?predicate_indicator), zero_or_more).
	:- info(add_directive_/3, [
		comment is 'Table of directives to be added to complement existing directives.',
		argnames is ['Object', 'Directive', 'NewDirective']
	]).

	:- private(remove_directive_/2).
	:- dynamic(remove_directive_/2).
	:- mode(remove_directive_(?atom, ?predicate_indicator), zero_or_more).
	:- info(remove_directive_/2, [
		comment is 'Table of directives to be removed.',
		argnames is ['Object', 'Directive']
	]).

	:- private(file_being_advised_/4).
	:- dynamic(file_being_advised_/4).
	:- mode(file_being_advised_(?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(file_being_advised_/4, [
		comment is 'Table of files being advised are respective directories and names (basename without extension).',
		argnames is ['File', 'Path', 'Directory', 'Name']
	]).

	rdirectory(Directory, UserOptions) :-
		reset,
		merge_options(UserOptions, Options),
		memberchk(exclude_directories(ExcludedDirectories), Options),
		memberchk(prolog_extensions(Extensions), Options),
		rdirectory_directories(Directory, ExcludedDirectories, Directories),
		directories(Directories, Extensions, [], Files),
		wrap_files(Files, Options).

	rdirectory(Directory) :-
		rdirectory(Directory, []).

	rdirectory_directories(RootDirectory, ExcludedDirectories, [RootDirectory| AllSubDirectories]) :-
		sub_directories(RootDirectory, ExcludedDirectories, SubDirectories),
		findall(
			Directories,
			(	member(Directory, SubDirectories),
				rdirectory_directories(Directory, ExcludedDirectories, Directories)
			),
			DirectoriesList
		),
		append(DirectoriesList, AllSubDirectories).

	sub_directories(Directory, ExcludedDirectories, SubDirectories) :-
		os::expand_path(Directory, Path),
		os::directory_files(Path, Files),
		(	sub_atom(Path, _, 1, 0, '/') ->
			PathSlash = Path
		;	atom_concat(Path, '/', PathSlash)
		),
		findall(
			SubDirectory,
			(	member(File, Files),
				File \== '.',
				File \== '..',
				\+ member(File, ExcludedDirectories),
				atom_concat(PathSlash, File, SubDirectory),
				os::directory_exists(SubDirectory)
			),
			SubDirectories
		).

	directories(Directories, UserOptions) :-
		reset,
		merge_options(UserOptions, Options),
		memberchk(prolog_extensions(Extensions), Options),
		directories(Directories, Extensions, [], Files),
		wrap_files(Files, Options).

	directories([], _, Files, Files).
	directories([Directory| Directories], Extensions, Files0, Files) :-
		directory_prolog_files(Directory, Extensions, DirectoryFiles),
		append(Files0, DirectoryFiles, Files1),
		directories(Directories, Extensions, Files1, Files).

	directories(Directories) :-
		directories(Directories, []).		

	directory_prolog_files(Directory, Extensions, DirectoryFiles) :-
		os::directory_files(Directory, Files),
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		findall(
			DirectoryFile,
			(	member(File, Files),
				member(Extension, Extensions),
				sub_atom(File, _, _, 0, Extension),
				atom_concat(DirectorySlash, File, DirectoryFile)
			),
			DirectoryFiles
		).

	directory(Directory, UserOptions) :-
		reset,
		merge_options(UserOptions, Options),
		memberchk(prolog_extensions(Extensions), Options),
		directory_prolog_files(Directory, Extensions, Files),
		wrap_files(Files, Options).

	directory(Directory) :-
		directory(Directory, []).

	files(Files, UserOptions) :-
		reset,
		merge_options(UserOptions, Options),
		wrap_files(Files, Options).

	files(Files) :-
		files(Files, []).

	file(File, UserOptions) :-
		reset,
		merge_options(UserOptions, Options),
		wrap_files([File], Options).

	file(File) :-
		file(File, []).

	wrap_files(Files, Options) :-
		memberchk(exclude_files(ExcludedFiles), Options),
		forall(
			file_being_advised(Files, ExcludedFiles, File, Path, Directory, Name),
			assertz(file_being_advised_(File, Path, Directory, Name))
		),
		load_and_wrap_files(Options),
		generate_advise,
		print_advise.

	file_being_advised([File| _], ExcludedFiles, File, Path, Directory, Name) :-
		\+ member(File, ExcludedFiles),
		os::expand_path(File, Path),
		\+ member(Path, ExcludedFiles),
		os::decompose_file_name(File, Directory, Name, Extension),
		\+ member(Name, ExcludedFiles),
		atom_concat(Name, Extension, Basename),
		\+ member(Basename, ExcludedFiles).
	file_being_advised([_| Files], ExcludedFiles, File, Path, Directory, Name) :-
		file_being_advised(Files, ExcludedFiles, File, Path, Directory, Name).

	reset :-
		retractall(predicate_called_but_not_defined_(_, _)),
		retractall(missing_predicate_directive_(_, _, _)),
		retractall(non_standard_predicate_call_(_, _)),
		retractall(dynamic_directive_(_,_,_)),
		retractall(multifile_directive_(_,_,_)),
		retractall(file_being_advised_(_, _, _, _)),
		retractall(add_directive_before_entity_(_, _)),
		retractall(add_directive_(_, _)),
		retractall(add_directive_(_, _, _)),
		retractall(remove_directive_(_, _)).

	load_and_wrap_files(Options) :-
		file_being_advised_(_, Path, _, _),
		load_and_wrap_file(Path, Options),
		fail.
	load_and_wrap_files(_).

	load_and_wrap_file(File, Options) :-
		self(Self),
		(	os::file_exists(File) ->
			Path = File
		;	memberchk(prolog_extensions(Extensions), Options),
			member(Extension, Extensions),
			atom_concat(File, Extension, Path),
			os::file_exists(Path) ->
			true
		;	print_message(warning, wrapper, file_not_found(File))
		),
		logtalk_load_lint_options(LintOptions),
		logtalk_load(Path, [hook(Self), source_data(on), reload(always)| LintOptions]).

	logtalk_load_lint_options([
		portability(warning), 
		unknown_predicates(warning),
		undefined_predicates(warning),
		unknown_entities(warning),
		missing_directives(warning),
		redefined_built_ins(warning),
		singleton_variables(warning)
	]).

	generate_advise :-
		file_being_advised_(_, _, _, Object),
		predicates_called_but_not_defined(Object),
		fail.
	generate_advise :-
		file_being_advised_(_, _, _, Object),
		missing_public_directives_advise(Object),
		missing_private_directives_advise(Object),
		missing_predicate_directives_advise(Object),
		fail.
	generate_advise.		

	print_advise :-
		file_being_advised_(File, _, _, Object),
		print_message(information, wrapper, advise_for_file(File)),
		print_advise(Object),
		fail.
	print_advise.

	print_advise(Object) :-
		\+ \+ add_directive_before_entity_(Object, _),
		print_message(information(code), wrapper, add_directives_before_entity),
		add_directive_before_entity_(Object, Directive),
		print_message(information(code), wrapper, add_directive_before_entity(Directive)),
		fail.
	print_advise(Object) :-
		\+ \+ add_directive_(Object, _),
		print_message(information(code), wrapper, add_directives),
		add_directive_(Object, Directive),
		print_message(information(code), wrapper, add_directive(Directive)),
		fail.
	print_advise(Object) :-
		\+ \+ add_directive_(Object, _, _),
		add_directive_(Object, Directive, NewDirective),
		print_message(information(code), wrapper, add_directive(Directive, NewDirective)),
		fail.
	print_advise(Object) :-
		\+ \+ remove_directive_(Object, _),
		print_message(information(code), wrapper, remove_directives),
		remove_directive_(Object, Directive),
		print_message(information(code), wrapper, remove_directive(Directive)),
		fail.
	print_advise(_).

	save(UserOptions) :-
		merge_options(UserOptions, Options),
		save_wrapper_objects(Options).

	save :-
		save([]).

	save_wrapper_objects(Options) :-
		file_being_advised_(_, Path, Directory, Name),
		atom_concat(Directory, Name, Source0),
		memberchk(logtalk_extension(Extension), Options),
		atom_concat(Source0, Extension, Source),
		open(Source, write, Stream),
		set_output(Stream),
		save_wrapper_object(Name, Path, Options),
		close(Stream),
		fail.
	save_wrapper_objects(Options) :-
		setof(Object, File^Path^file_being_advised_(File, Path, Directory, Object), Objects),
		atom_concat(Directory, loader, Loader0),
		memberchk(logtalk_extension(Extension), Options),
		atom_concat(Loader0, Extension, Loader),
		open(Loader, write, Stream),
		set_output(Stream),
		print_message(raw, wrapper, add_directive(initialization(logtalk_load(Objects)))),
		close(Stream),
		fail.
	save_wrapper_objects(_).

	save_wrapper_object(Object, _, _) :-
		\+ \+ add_directive_before_entity_(Object, _),
		add_directive_before_entity_(Object, Directive),
		print_message(raw, wrapper, add_directive_before_entity(Directive)),
		fail.
	save_wrapper_object(Object, _, _) :-
		print_message(raw, wrapper, add_directive(object(Object))),
		fail.
	save_wrapper_object(Object, _, _) :-
		\+ \+ add_directive_(Object, _),
		add_directive_(Object, Directive),
		print_message(raw, wrapper, add_directive(Directive)),
		fail.
	save_wrapper_object(Object, _, _) :-
		\+ \+ add_directive_(Object, _, _),
		add_directive_(Object, Directive, NewDirective),
		print_message(raw, wrapper, add_directive(Directive, NewDirective)),
		fail.
	save_wrapper_object(_, Path, Options) :-
		memberchk(include_wrapped_files(true), Options),
		os::decompose_file_name(Path, _, Basename),
		print_message(raw, wrapper, add_directive(include(Basename))),
		fail.
	save_wrapper_object(_, _, _) :-
		print_message(raw, wrapper, add_directive(end_object)),
		fail.
	save_wrapper_object(_, _, _).

	% predicates called from other files wrapped as objects
	% must be declared public

	missing_public_directives_advise(Object) :-
		setof(
			Predicate,
			provides_used_predicate(Object, Predicate),
			Predicates
		),
		Directive =.. [(public), Predicates],
		assertz(add_directive_(Object, Directive)),
		fail.
	missing_public_directives_advise(_).

	provides_used_predicate(Object, Predicate) :-
		object_predicate_called_(_, Object, Predicate),
		object_property(Object, defines(Predicate, DefinitionProperties)),
		\+ member(auxiliary, DefinitionProperties),
		\+ (
			object_property(Object, declares(Predicate, Properties)),
			member((multifile), Properties)
		).

	% internal dynamic predicates should also be
	% declared private for improved performance

	missing_private_directives_advise(Object) :-
		setof(
			Predicate,
			internal_dynamic_predicate(Object, Predicate),
			Predicates
		),
		Directive =.. [(private), Predicates],
		assertz(add_directive_(Object, Directive)),
		fail.
	missing_private_directives_advise(_).

	internal_dynamic_predicate(Object, Predicate) :-
		dynamic_directive_(Object, _, Predicate),
		\+ predicate_called_but_not_defined_(_, Predicate).

	% missing public/1 directives are only generated for
	% predicates declared as multifile
	missing_predicate_directives_advise(Object) :-
		missing_predicate_directive_(Object, (public), Predicate),
		PublicDirective =.. [(public), Predicate],
		MultifileDirective =.. [(multifile), Predicate],
		assertz(add_directive_(Object, MultifileDirective, PublicDirective)),
		fail.
	% other missing directives
	missing_predicate_directives_advise(Object) :-
		missing_predicate_directive_(Object, DirectiveFunctor, Predicate),
		DirectiveFunctor \== (public),
		Directive =.. [DirectiveFunctor, Predicate],
		assertz(add_directive_(Object, Directive)),
		fail.
	missing_predicate_directives_advise(_).

	predicates_called_but_not_defined(Object) :-
		predicate_called_but_not_defined_(Object, Predicate),
		(	missing_predicate_directive_(Object, (dynamic), Predicate) ->
			true
		;	missing_predicate_directive_(Object, (multifile), Predicate) ->
			true
		;	object_property(Other, defines(Predicate, DefinitionProperties)),
			\+ member(auxiliary, DefinitionProperties),
			file_being_advised_(_, _, _, Other) ->
			retractall(object_predicate_called_(Object, Other, Predicate)),
			assertz(object_predicate_called_(Object, Other, Predicate))
		;	module_exported_predicate(Module, Predicate) ->
			retractall(module_predicate_called_(Object, Module, Predicate)),
			assertz(module_predicate_called_(Object, Module, Predicate))
		;	retractall(unknown_predicate_called_(Object, Predicate)),
			assertz(unknown_predicate_called_(Object, Predicate))
		),
		fail.
	predicates_called_but_not_defined(Object) :-
		missing_uses_directives_advise(Object),
		fail.
	predicates_called_but_not_defined(Object) :-
		missing_use_module_directives_advise(Object),
		fail.
	predicates_called_but_not_defined(Object) :-
		unknown_predicates_called_advise(Object),
		fail.
	predicates_called_but_not_defined(_).

	% generate uses/2 directives for resolving now implicitly
	% qualified calls to non-standard built-in predicates or to
	% predicates defined in other files that we are wrapping

	missing_uses_directives_advise(Object) :-
		missing_uses_directive(Object, Other, Predicates),
		Directive =.. [uses, Other, Predicates],
		assertz(add_directive_(Object, Directive)),
		fail.
	missing_uses_directives_advise(_).

	% for non-standard built-in predicates, just call them
	% in the context of the "user" pseudo-object
	missing_uses_directive(Object, user, Predicates) :-
		setof(
			Predicate,
			non_standard_predicate_call_(Object, Predicate),
			Predicates
		).

	% other called predicates that are not defined locally but
	% that there are also not built-in predicates
	missing_uses_directive(Object, Other, Predicates) :-
		setof(
			Predicate,
			object_predicate_called_(Object, Other, Predicate),
			Predicates
		).

	missing_use_module_directives_advise(Object) :-
		missing_use_module_directive(Object, Module, Predicates),
		Directive =.. [use_module, Module, Predicates],
		assertz(add_directive_(Object, Directive)),
		fail.
	missing_use_module_directives_advise(_).

	missing_use_module_directive(Object, Module, Predicates) :-
		setof(
			Predicate,
			module_predicate_called_(Object, Module, Predicate),
			Predicates
		).

	unknown_predicates_called_advise(Object) :-
		unknown_predicate_called_(Object, Predicate),
		print_message(warning, wrapper, unknown_predicate_called(Object, Predicate)),
		fail.
	unknown_predicates_called_advise(_).

	% options handling

	default_options(DefaultOptions) :-
		findall(DefaultOption, default_option(DefaultOption), DefaultOptions).

	merge_options(UserOptions, Options) :-
		findall(
			DefaultOption,
			(	default_option(DefaultOption),
				functor(DefaultOption, OptionName, Arity),
				functor(UserOption, OptionName, Arity),
				\+ member(UserOption, UserOptions)
			),
			DefaultOptions
		),
		append(UserOptions, DefaultOptions, Options).

	normalize_directory_paths([], []).
	normalize_directory_paths([Directory| Directories], [NormalizedDirectory| NormalizedDirectories]) :-
		os::expand_path(Directory, NormalizedDirectory0),
		(	sub_atom(NormalizedDirectory0, _, _, 0, '/') ->
			NormalizedDirectory = NormalizedDirectory0
		;	atom_concat(NormalizedDirectory0, '/', NormalizedDirectory)
		),
		normalize_directory_paths(Directories, NormalizedDirectories).

	% by default, use the most common Prolog source file extensions:
	default_option(prolog_extensions(['.pl', '.pro', '.prolog'])).
	% by default, use the common Logtalk source file extension:
	default_option(logtalk_extension('.lgt')).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, generate `include/1` directives for the wrapped Prolog source files
	default_option(include_wrapped_files(true)).

	% wrapper for the plain Prolog files source code

	term_expansion(begin_of_file, [(:- object(Name))]) :-
		logtalk_load_context(basename, Basename),
		os::decompose_file_name(Basename, _, Name, _).

	term_expansion(end_of_file, [(:- end_object), end_of_file]).

	% special cases

	% save the position of dynamic/1 directives
	term_expansion((:- dynamic(Predicates)), [(:- dynamic(Predicates))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename),
		logtalk_load_context(term_position, Line-_),
		flatten_to_list(Predicates, List),
		forall(
			member(Predicate, List),
			assertz(dynamic_directive_(Object, Line, Predicate))
		),
		fail.

	% save the position of multifile/1 directives
	term_expansion((:- multifile(Predicates)), [(:- multifile(Predicates))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename),
		logtalk_load_context(term_position, Line-_),
		flatten_to_list(Predicates, List),
		forall(
			member(Predicate, List),
			assertz(multifile_directive_(Object, Line, Predicate))
		),
		fail.

	% wrap set_prolog_flag/2 directives so that we don't get a compiler error
	term_expansion((:- set_prolog_flag(Flag,Value)), []) :-
		logtalk_load_context(entity_identifier, Object),
		assertz(add_directive_before_entity_(Object, set_prolog_flag(Flag,Value))),
		assertz(remove_directive_(Object, set_prolog_flag(Flag,Value))).

	% ensure that use_module/1-2 directives will load the module files
	% instead of being just processed by the Logtalk compiler to find
	% the module name and the exported/used predicates
	term_expansion((:- use_module(File)), _) :-
		logtalk_load_context(entity_identifier, Object),
		assertz(add_directive_before_entity_(Object, use_module(File))),
		{use_module(File)},
		fail.
	term_expansion((:- use_module(File,Exports)), _) :-
		logtalk_load_context(entity_identifier, Object),
		assertz(add_directive_before_entity_(Object, use_module(File,Exports))),
		{use_module(File,Exports)},
		fail.

	% discard include/1 directives for files being processed
	term_expansion((:- include(Include)), []) :-
		file_being_advised_(File, Path, _, Name),
		(	Include = File
		;	Include = Path
		;	Include = Name
		),
		logtalk_load_context(entity_identifier, Object),
		assertz(remove_directive_(Object, include(Include))).

	% discard ensure_loaded/1 directives for files already being processed
	term_expansion((:- ensure_loaded(LoadedFile)), []) :-
		file_being_advised_(File, Path, _, Name),
		(	LoadedFile = File
		;	LoadedFile = Path
		;	LoadedFile = Name
		),
		logtalk_load_context(entity_identifier, Object),
		assertz(remove_directive_(Object, ensure_loaded(LoadedFile))).

	% discard consult/1 directives for files already being processed
	term_expansion((:- [File|Files]), Expansion) :-
		term_expansion_consult_directive([File|Files], Expansion).

	term_expansion_consult_directive([], []).
	term_expansion_consult_directive([LoadedFile| LoadedFiles], Expansion) :-
		file_being_advised_(File, Path, _, Name),
		(	LoadedFile = File
		;	LoadedFile = Path
		;	LoadedFile = Name
		),
		!,
		logtalk_load_context(entity_identifier, Object),
		assertz(remove_directive_(Object, [LoadedFile])),
		term_expansion_consult_directive(LoadedFiles, Expansion).
	term_expansion_consult_directive([LoadedFile| LoadedFiles], [{:- [LoadedFile]}| Expansion]) :-
		term_expansion_consult_directive(LoadedFiles, Expansion).

	% hooks for intercepting relevant compiler lint messages

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(unknown_predicate_called_but_not_defined(_, _, _, Object, Predicate), _, core, _) :-
		assertz(predicate_called_but_not_defined_(Object, Predicate)).

	logtalk::message_hook(missing_predicate_directive(_, _, _, Object, DirectiveFunctor, Predicate), _, core, _) :-
		assertz(missing_predicate_directive_(Object, DirectiveFunctor, Predicate)).

	logtalk::message_hook(non_standard_predicate_call(_, _, _, Object, Predicate), _, core, _) :-
		assertz(non_standard_predicate_call_(Object, Predicate)).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, wrapper, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	message_prefix_stream(information,       '% ',     user_output).
	message_prefix_stream(information(code), '',       user_output).
	message_prefix_stream(warning,           '*     ', user_error).
	message_prefix_stream(raw,               '',       Stream) :-
		current_output(Stream).

	% wraper messages

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, wrapper) -->
		message_tokens(Message).

	message_tokens(file_not_found(File)) -->
		['File not found: ~w'-[File], nl].

	message_tokens(advise_for_file(File)) -->
		[nl, 'Advise for file: ~w'-[File], nl, nl].

	message_tokens(public_directive(Predicates)) -->
		[':- public(~q).'-[Predicates], nl, nl].

	message_tokens(add_directives) -->
		['% Add the following entity directives:'-[], nl, nl].

	message_tokens(add_directives_before_entity) -->
		['% Add the following directives before the entity opening directive:'-[], nl, nl].

	message_tokens(replace_directives) -->
		['% Replace the following directives:'-[], nl, nl].

	message_tokens(remove_directives) -->
		['% Remove the following directives:'-[], nl, nl].

	message_tokens(add_directive_before_entity(Directive)) -->
		[':- ~q.'-[Directive], nl, nl].

	message_tokens(add_directive(Directive)) -->
		[':- ~q.'-[Directive], nl, nl].

	message_tokens(add_directive(Directive, NewDirective)) -->
		[	
			'% before the directive:'-[], nl,
			'% :- ~q'-[Directive], nl,
			'% add the directive:'-[], nl, nl,
			':- ~q.'-[NewDirective], nl, nl
		].

	message_tokens(remove_directive(Directive)) -->
		[':- ~q.'-[Directive], nl, nl].

	message_tokens(uses_directive(Object, Predicates)) -->
		[':- uses(~q, ~q).'-[Object, Predicates], nl, nl].

	message_tokens(unknown_predicate_called(Object, Predicate)) -->
		['~q object calls ~q unknown predicate'-[Object, Predicate], nl].

	% modules support

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		module_exported_predicate(Module, Predicate) :-
			{current_module(Module),
			 module_property(Module, exports(Exports))},
			member(Predicate, Exports).

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		module_exported_predicate(Module, Predicate) :-
			{current_module(Module),
			 module_property(Module, exports(Exports))},
			member(Predicate, Exports).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		module_exported_predicate(Module, Functor/Arity) :-
			{current_module(Module),
			 predicate_property(':'(Module,Goal), exported)},
			functor(Goal, Functor, Arity).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		module_exported_predicate(Module, Predicate) :-
			{current_module(Module),
			 get_module_info(Module, interface, Exports)},
			member(export(Predicate), Exports).

	:- else.

		module_exported_predicate(_, _) :-
			fail.

	:- endif.

	% auxiliary predicates

	% flattens an item, a list of items, or a conjunction of items into a list
	flatten_to_list([A| B], [A| B]) :-
		!.
	flatten_to_list([], []) :-
		!.
	flatten_to_list((A, B), [A| BB]) :-
		!,
		flatten_to_list(B, BB).
	flatten_to_list(A, [A]).

	% we want to minimize any dependencies on other entities, including library objects

	append([], []).
	append([List| Lists], Concatenation) :-
		append(List, Tail, Concatenation),
		append(Lists, Tail).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

:- end_object.

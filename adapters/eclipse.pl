%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for ECLiPSe 6.1#143 and later versions
%  Last updated on October 11, 2016
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- pragma(system).
:- pragma(nodebug).

:- use_module(library(iso)).
:- ensure_loaded(library(iso_strict)).
:- import compare/3, term_variables/2 from iso_strict.

:- set_event_handler(134, '$lgt_eclipse_discontiguous_predicate_handler'/2).

'$lgt_eclipse_discontiguous_predicate_handler'(Err, Goal) :-
	'$lgt_increment_loadind_warnings_counter',
	error(default(Err), Goal).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  ISO Prolog Standard predicates that we must define because they are
%  not built-in
%
%  add a clause for '$lgt_iso_predicate'/1 declaring each ISO predicate that
%  we must define; there must be at least one clause for this predicate
%  whose call should fail if we don't define any ISO predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(_) :-
	fail.

:- local syntax_option(not(iso_restrictions)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer)

:- ensure_loaded(library(util)).
:- import between/3 from util.


% findall(?term, +callable, ?list, +list)

:- if(\+ get_flag(findall/4, type, built_in)).

	findall(Term, Goal, List, Tail) :-
		findall(Term, Goal, List0),
		append(List0, Tail, List).

:- endif.


% forall(+callable, +callable)

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% format(+stream_or_alias, +character_code_list_or_atom, +list)
% format(+character_code_list_or_atom, +list)

:- use_module(library(format)).


% numbervars(?term, +integer, ?integer)

:- use_module(library(numbervars)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate (and ideally meta_predicate/1
%  properties for built-in predicates and library predicates)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(':'(Module,Predicate), Property) :-
	!,
	functor(Predicate, Functor, Arity),
	'$lgt_eclipse_module_predicate_property'(Module, Functor/Arity, Property).
'$lgt_predicate_property'(Predicate, Property) :-
	!,
	functor(Predicate, Functor, Arity),
	'$lgt_eclipse_plain_predicate_property'(Functor/Arity, Property).

'$lgt_eclipse_module_predicate_property'(Module, Predicate, built_in) :-
	get_flag(Predicate, type, built_in)@Module.
'$lgt_eclipse_module_predicate_property'(Module, Predicate, dynamic) :-
	get_flag(Predicate, stability, dynamic)@Module.
'$lgt_eclipse_module_predicate_property'(Module, Predicate, static) :-
	get_flag(Predicate, stability, static)@Module.
'$lgt_eclipse_module_predicate_property'(Module, Predicate, meta_predicate(Template)) :-
	get_flag(Predicate, meta_predicate, Template)@Module.

'$lgt_eclipse_plain_predicate_property'(numbervars/3, built_in).
'$lgt_eclipse_plain_predicate_property'(Predicate, built_in) :-
	get_flag(Predicate, type, built_in).
'$lgt_eclipse_plain_predicate_property'(Predicate, dynamic) :-
	get_flag(Predicate, stability, dynamic).
'$lgt_eclipse_plain_predicate_property'(Predicate, static) :-
	get_flag(Predicate, stability, static).
'$lgt_eclipse_plain_predicate_property'(Predicate, meta_predicate(Template)) :-
	get_flag(Predicate, meta_predicate, Template).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable) -- not supported



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog non-standard built-in meta-predicates and meta-directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_meta_predicate'(@callable, ?callable, ?atom)
%
% table of meta-predicate patterns for proprietary built-in predicates;
% the third argument, which must be either "predicate" or "control_construct",
% is used to guide the compilation of these meta-predicates in debug mode

'$lgt_prolog_meta_predicate'(*->(_, _), *->(0, 0), control_construct).
'$lgt_prolog_meta_predicate'(~(_), ~(0), control_construct).
'$lgt_prolog_meta_predicate'(block(_, _, _), block(0, *, 0), predicate).
'$lgt_prolog_meta_predicate'(call_priority(_, _), call_priority(0, *), predicate).
'$lgt_prolog_meta_predicate'(coverof(_, _, _), coverof(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(do(_, _), do(*, 0), predicate).
'$lgt_prolog_meta_predicate'(event_create(_, _, _), event_create(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(fail_if(_), fail_if(0), predicate).
'$lgt_prolog_meta_predicate'(make_suspension(_, _, _), make_suspension(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(mutex(_, _), mutex(*, 0), predicate).
'$lgt_prolog_meta_predicate'(not(_), not(0), predicate).
'$lgt_prolog_meta_predicate'(subcall(_, _), subcall(0, *), predicate).
'$lgt_prolog_meta_predicate'(suspend(_, _, _), suspend(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(suspend(_, _, _, _), suspend(0, *, *, *), predicate).


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(demon(_), demon(/)).
'$lgt_prolog_meta_directive'(inline(_, _), inline(/, /)).
'$lgt_prolog_meta_directive'(set_error_handler(_, _), set_error_handler(*, /)).
'$lgt_prolog_meta_directive'(set_flag(_, _, _), set_flag(/, *, *)).
'$lgt_prolog_meta_directive'(skipped(_), skipped(/)).


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(++, *).	% ground normal argument


% '$lgt_prolog_database_predicate'(@callable)

'$lgt_prolog_database_predicate'(assert(_)).
'$lgt_prolog_database_predicate'(listing(_)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file name extension predicates
%
%  these extensions are used by Logtalk load/compile predicates
%
%  you may want to change the extension for the intermediate files 
%  generated by the Logtalk compiler ("object" files) to match the
%  extension expected by default by your Prolog compiler
%
%  there should only a single extension defined for object files but
%  but multiple extensions can be defined for Logtalk and Prolog source
%  files and for back-end specific temporary files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_file_extension'(?atom, ?atom)

'$lgt_file_extension'(logtalk, '.lgt').
'$lgt_file_extension'(logtalk, '.logtalk').
'$lgt_file_extension'(object, '.pl').
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(prolog, '.ecl').
'$lgt_file_extension'(prolog, '.prolog').
'$lgt_file_extension'(tmp, Extension) :-
	get_flag(eclipse_object_suffix, Extension0),	% '.eco' by default
	atom_string(Extension, Extension0).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog compiler features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, eclipse).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Build)) :-
	get_flag(version_as_list, [Major, Minor, Build]).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((6,1,143))).
'$lgt_prolog_feature'(prolog_conformance, lax).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, supported).
'$lgt_prolog_feature'(coinduction, supported).
'$lgt_prolog_feature'(unicode, unsupported).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_flag'(?atom, ?atom)
%
% default values for all flags

% startup flags:
'$lgt_default_flag'(settings_file, allow).
% lint compilation flags:
'$lgt_default_flag'(unknown_entities, warning).
'$lgt_default_flag'(unknown_predicates, warning).
'$lgt_default_flag'(undefined_predicates, warning).
'$lgt_default_flag'(unused_predicates, silent).
'$lgt_default_flag'(singleton_variables, warning).
'$lgt_default_flag'(portability, silent).
'$lgt_default_flag'(redefined_built_ins, silent).
'$lgt_default_flag'(missing_directives, warning).
'$lgt_default_flag'(underscore_variables, dont_care).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	(	getenv('COMSPEC', _) ->
		% Windows systems define this environment variable...
		ScratchDirectory = './lgt_tmp/'
	;	% ... but not POSIX systems
		ScratchDirectory = './.lgt_tmp/'
	).
'$lgt_default_flag'(report, on).
'$lgt_default_flag'(clean, on).
'$lgt_default_flag'(code_prefix, '$').
'$lgt_default_flag'(optimize, off).
'$lgt_default_flag'(source_data, on).
'$lgt_default_flag'(reload, changed).
'$lgt_default_flag'(debug, off).
% Prolog compiler and loader flags:
'$lgt_default_flag'(prolog_compiler, []).
'$lgt_default_flag'(prolog_loader, [debug:off]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  operating-system access predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_os_file_name'(+atom, -atom)
% '$lgt_prolog_os_file_name'(-atom, +atom)
%
% converts between Prolog internal file paths and operating-system paths

'$lgt_prolog_os_file_name'(PrologPath, OSPath) :-
	os_file_name(PrologPath, OSPath).


% '$lgt_expand_path'(+nonvar, -atom)
%
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	canonical_path_name(Path, ExpandedPath).


% '$lgt_file_exists'(+atom)
%
% checks if a directory exists

'$lgt_file_exists'(File) :-
	exists(File),
	get_file_info(File, type, file).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	(	exists(File) ->
		delete(File)
	;	true
	).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	canonical_path_name(Directory, Path),
	exists(Path),
	get_file_info(Path, type, directory).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	getcwd(DirectoryString),
	atom_string(Directory, DirectoryString).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	cd(Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	'$lgt_directory_exists'(Directory) ->
		true
	;	mkdir(Directory)
	).


% '$lgt_directory_hash_as_atom'(+atom, -atom)
%
% returns the directory hash as an atom

'$lgt_directory_hash_as_atom'(Directory, Hash) :-
	term_hash(Directory, 1, 2147483647, Hash0),
	number_codes(Hash0, Codes),
	atom_codes(Hash, Codes).


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_compile_prolog_code'(_, _, _).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_load_prolog_code'(File, _, Options) :-
	% remove the Prolog file name extension in order to support generating
	% and loading of .eco files when using the output:eco option
	'$lgt_file_extension'(object, Extension),
	atom_concat(Path, Extension, File),
	compile(Path, Options).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	get_file_info(File, mtime, Time).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	getenv(Variable, ValueString),
	atom_string(Value, ValueString).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory

'$lgt_startup_directory'(Directory) :-
	(	getenv('LOGTALK_STARTUP_DIRECTORY', DirectoryString) ->
		true
	;	getcwd(DirectoryString)
	),
	atom_string(Directory, DirectoryString).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	getenv('LOGTALKUSER', DirectoryString),
	atom_string(Directory, DirectoryString).


% '$lgt_home_directory'(-atom)
%
% returns the Logtalk home directory; fails if unknown

'$lgt_home_directory'(Directory) :-
	getenv('LOGTALKHOME', DirectoryString),
	atom_string(Directory, DirectoryString).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory must always end
% with a slash; the extension must start with a "." when defined and must
% be the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	pathname(File, DirectoryString, NameString, ExtensionString),
	atom_string(Directory0, DirectoryString),
	(	Directory0 = '' ->
		Directory = './'
	;	Directory = Directory0
	),
	atom_string(Name, NameString),
	atom_string(Extension, ExtensionString).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	get_stream_info(Stream, line, Last),
	Line is Last + 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  abstraction of the standard open/4 and close/1 predicates for dealing
%  with the alias/1 option in old non-standard compliant systems
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_open'(+atom, +atom, -stream, @list)

'$lgt_open'(File, Mode, Stream, Options) :-
	open(File, Mode, Stream, Options).


% '$lgt_close'(@stream)

'$lgt_close'(Stream) :-
	close(Stream).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages) and
%  the variable names (ideally using the standard variable_names/1 option)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position, -list)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd, Variables) :-
	get_stream_info(Stream, line, LineBegin),
	(	read_term(Stream, Term, [variable_names(Variables)| Options]) ->
		get_stream_info(Stream, line, LineEnd)
	;	throw(syntax_error)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_prolog_term_expansion'((:- Directive), Expanded) :-
	'$lgt_eclipse_directive_expansion'(Directive, Expanded0),
	(	Expanded0 == [] ->
		Expanded  == []
	;	Expanded0 =  {ExpandedDirective} ->
		Expanded  =  {(:- ExpandedDirective)}
	;	Expanded  =  (:- Expanded0)
	).


'$lgt_eclipse_directive_expansion'(mode(_), []).
'$lgt_eclipse_directive_expansion'(comment(_, _), []).

'$lgt_eclipse_directive_expansion'(export(chtab(Char, Class)), {export(chtab(Char, Class))}).
'$lgt_eclipse_directive_expansion'(export(domain(Domain)), {export(domain(Domain))}).
'$lgt_eclipse_directive_expansion'(export(struct(Struct)), {export(struct(Struct))}).
'$lgt_eclipse_directive_expansion'(export(syntax_option(SyntaxOption)), {export(syntax_option(SyntaxOption))}).

'$lgt_eclipse_directive_expansion'(op(Priority, Specifier, ':'(Module,Operators)), {op(Priority, Specifier, Operators)}) :-
	Module == user.

'$lgt_eclipse_directive_expansion'(pragma(Pragma), {pragma(Pragma)}).

'$lgt_eclipse_directive_expansion'(set_event_handler(Event, defers(Functor/Arity)), {set_event_handler(Event, defers(CFunctor/CArity))}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(Functor/Arity, _, CFunctor/CArity).
'$lgt_eclipse_directive_expansion'(set_event_handler(Event, Functor/Arity), {set_event_handler(Event, CFunctor/CArity)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(Functor/Arity, _, CFunctor/CArity).

'$lgt_eclipse_directive_expansion'(import(from(Conjunction, Module)), use_module(Module, Imports)) :-
	'$lgt_flatten_list'([Conjunction], Imports).

'$lgt_eclipse_directive_expansion'(local(Functor/Arity), private(Functor/Arity)).
'$lgt_eclipse_directive_expansion'(local(op(Priority, Spec, Operators)), op(Priority, Spec, Operators)).

'$lgt_eclipse_directive_expansion'(lib(Library), use_module(Module, Exports)) :-
	logtalk_load_context(entity_type, _),
	'$lgt_eclipse_list_of_exports'(library(Library), Module, Exports).

'$lgt_eclipse_directive_expansion'(reexport(File), reexport(Module, Exports)) :-
	atom(File),
	'$lgt_eclipse_list_of_exports'(File, Module, Exports).

'$lgt_eclipse_directive_expansion'(reexport(from(Conjunction, Module)), reexport(Module, Exports)) :-
	'$lgt_flatten_list'([Conjunction], Exports).

'$lgt_eclipse_directive_expansion'(use_module(File), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, _),
	'$lgt_eclipse_list_of_exports'(File, Module, Imports).


'$lgt_eclipse_list_of_exports'(File, Module, Exports) :-
	(	get_flag(prolog_suffix, Suffixes), existing_file(File, Suffixes, [], ExtRel) ->
		true
	;	% we may be compiling Prolog module files as Logtalk objects
		existing_file(File, [`.lgt`], [], ExtRel) ->
		true
	;	ExtRel = File
	),
	canonical_path_name(ExtRel, Path),
	open(Path, read, In),
	catch(read(In, ModuleDecl), _, (close(In), fail)),
	(	ModuleDecl = (:- module(Module, Interface)) ->
		true
	;	ModuleDecl = (:- module(Module)) ->
		(	current_module(Module) ->
			true
		;	ensure_loaded(File)
		),
		get_module_info(Module, interface, Interface)
	),
	'$lgt_eclipse_filter_exports'(Interface, Exports),
	!.


'$lgt_eclipse_filter_exports'([], []).

'$lgt_eclipse_filter_exports'([Functor/Arity| Interface], [Functor/Arity| Exports]) :-
	'$lgt_eclipse_filter_exports'(Interface, Exports).

'$lgt_eclipse_filter_exports'([op(Priority, Spec, Operators)| Interface], [op(Priority, Spec, Operators)| Exports]) :-
	'$lgt_eclipse_filter_exports'(Interface, Exports).

'$lgt_eclipse_filter_exports'([export(Functor/Arity)| Interface], [Functor/Arity| Exports]) :-
	!,
	'$lgt_eclipse_filter_exports'(Interface, Exports).

'$lgt_eclipse_filter_exports'([export(op(Priority, Spec, Operators))| Interface], [op(Priority, Spec, Operators)| Exports]) :-
	!,
	'$lgt_eclipse_filter_exports'(Interface, Exports).

'$lgt_eclipse_filter_exports'([export(_)| Interface], Exports) :-
	!,
	'$lgt_eclipse_filter_exports'(Interface, Exports).


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
	fail.


'$lgt_eclipse_meta_args1'([]).
'$lgt_eclipse_meta_args1'([::| MetaArgs]) :-
	'$lgt_eclipse_meta_args1'(MetaArgs).


'$lgt_eclipse_meta_args2'([*]) :- !.
'$lgt_eclipse_meta_args2'([::| MetaArgs]) :-
	'$lgt_eclipse_meta_args2'(MetaArgs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'(_, _, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  lambda expressions support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term(Term, Copy, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  hooks predicates for writing and asserting compiled entity terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_write_compiled_term'(@stream, @callable, +atom, +atom, +integer)
%
% the third argument is the term type: runtime (internal runtime clause),
% user (compiled user-defined term), or aux (auxiliary clause resulting
% e.g. from term-expansion)

'$lgt_write_compiled_term'(Stream, Term, _Kind, _Path, _Line) :-
	write_canonical(Stream, Term),
	write(Stream, '.\n').


% '$lgt_assertz_entity_clause'(@clause, +atom)

'$lgt_assertz_entity_clause'(Clause, _Kind) :-
	assertz(Clause).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  error term normalization (when exception terms don't follow the ISO
%  Prolog standard)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_normalize_error_term'(@callable, -callable)

'$lgt_normalize_error_term'(
	Error,
	Error
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message token printing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%:- multifile('$logtalk#0.print_message_token#4'/5).
%:- dynamic('$logtalk#0.print_message_token#4'/5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  term hashing (not currently used in the compiler/runtime)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% term_hash(@callable, +integer, +integer, -integer) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  string built-in type
%
%  define these predicates to trivially fail if no string type is available
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_string'(@term)

'$lgt_string'(Term) :-
	string(Term).


% '$lgt_string_codes'(+string, -list(codes))
% '$lgt_string_codes'(-string, +list(codes))

'$lgt_string_codes'(String, Codes) :-
	string_list(String, Codes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  module qualification to be used when calling Prolog meta-predicates
%  with meta-arguments that are calls to object or category predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_user_module_qualification'(@callable, -callable)

'$lgt_user_module_qualification'(Goal, Goal).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  shortcuts to the Logtalk built-in predicates logtalk_load/1 and
%  logtalk_make/1
%
%  defined in the adapter files to make it easier to comment them out in case
%  of conflict with some Prolog native feature; they require conformance with
%  the ISO Prolog standard regarding the definition of the {}/1 syntax
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


{*} :-
	!,
	logtalk_make(all).
{!} :-
	!,
	logtalk_make(clean).
{?} :-
	!,
	logtalk_make(missing).
{@} :-
	!,
	logtalk_make(circular).


{File, Files} :-
	!,
	logtalk_load(File),
	{Files}.
{File} :-
	logtalk_load(File).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

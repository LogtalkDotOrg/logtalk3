%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  Adapter file for ECLiPSe 6.1#143 and later versions
%  Last updated on August 4, 2013
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma(system).
:- pragma(nodebug).

:- use_module(library(iso)).

:- set_event_handler(134, '$lgt_eclipse_discontiguous_predicate_handler'/2).

'$lgt_eclipse_discontiguous_predicate_handler'(Err, Goal) :-
	'$lgt_increment_loadind_warnings_counter',
	error(default(Err), Goal).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  ISO Prolog Standard predicates that we must define because they are
%  not built-in
%
%  add a clause for '$lgt_iso_predicate'/1 declaring each ISO predicate that
%  we must define; there must be at least one clause for this predicate
%  whose call should fail if we don't define any ISO predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(_) :-
	fail.

:- local syntax_option(not(iso_restrictions)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer)

:- import between/3 from util.


% forall(+callable, +callable)

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% format(+stream_or_alias, +character_code_list_or_atom, +list)
% format(+character_code_list_or_atom, +list)

:- use_module(library(format)).


% numbervars(?term, +integer, ?integer) -- built-in

:- use_module(library(numbervars)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(':'(_,_), built_in) :-
	!.
'$lgt_predicate_property'(numbervars(_,_,_), built_in) :-
	!.

'$lgt_predicate_property'(Predicate, built_in) :-
	functor(Predicate, Functor, Arity),
	current_built_in(Functor/Arity).

'$lgt_predicate_property'(Predicate, dynamic) :-
	functor(Predicate, Functor, Arity),
	current_predicate(Functor/Arity),
	is_dynamic(Functor/Arity).

'$lgt_predicate_property'(Predicate, static) :-
	functor(Predicate, Functor, Arity),
	current_built_in(Functor/Arity).

'$lgt_predicate_property'(Predicate, static) :-
	functor(Predicate, Functor, Arity),
	current_predicate(Functor/Arity),
	\+ is_dynamic(Functor/Arity).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable)

setup_call_cleanup(_, _, _) :-
	throw(not_supported(setup_call_cleanup/3)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog non-standard built-in meta-predicates and meta-directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_meta_predicate'(+callable, ?callable, ?atom)
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


% '$lgt_prolog_meta_directive'(@callable, @callable)

'$lgt_prolog_meta_directive'(_, _) :-
	fail.


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file extension predicates
%
%  these extensions are used by Logtalk load/compile predicates
%
%  you may want to change the extension for Prolog files to match 
%  the one expected by default by your Prolog compiler
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_file_extension'(?atom, ?atom)

'$lgt_file_extension'(logtalk, '.lgt').
'$lgt_file_extension'(logtalk, '.logtalk').
'$lgt_file_extension'(prolog, '.pl').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, eclipse).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Build)) :-
	get_flag(version_as_list, [Major, Minor, Build]).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((6,1,143))).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, supported).
'$lgt_prolog_feature'(coinduction, supported).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_flag'(?atom, ?atom)
%
% default values for all flags

% lint compilation flags:
'$lgt_default_flag'(unknown_entities, warning).
'$lgt_default_flag'(misspelt_calls, warning).
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
'$lgt_default_flag'(debug, off).
% Prolog compiler and loader flags:
'$lgt_default_flag'(prolog_compiler, []).
'$lgt_default_flag'(prolog_loader, [debug:off]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
	exists(File).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	delete(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	canonical_path_name(Directory, Path),
	exists(Path).


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


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_compile_prolog_code'(_, _, _).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, _, Options) :-
	compile(File, Options).


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
% decomposes a file path in its components; the directory
% must always end with a slash and the extension must be
% the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	pathname(File, DirectoryString, NameString, ExtensionString),
	atom_string(Directory0, DirectoryString),
	(	Directory0 = '' ->
		Directory = './'
	;	Directory = Directory0
	),
	atom_string(Name, NameString),
	atom_string(Extension, ExtensionString).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(calendar)).


% '$lgt_current_date'(?integer, ?integer, ?integer)

'$lgt_current_date'(Year, Month, Day) :-
	mjd_now(MJD),
	mjd_to_date(MJD, Day/Month/Year).


% '$lgt_current_time'(?integer, ?integer, ?integer)

'$lgt_current_time'(Hours, Minutes, Seconds) :-
	mjd_now(MJD),
	mjd_to_time(MJD, Hours:Minutes:FloatSeconds),
	Seconds is integer(floor(FloatSeconds)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  timing predicate
%
%  if your Prolog compiler does not provide access to a timing predicate 
%  just write dummy definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_cpu_time'(-Seconds)

'$lgt_cpu_time'(Seconds) :-
	cputime(Seconds).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	get_stream_info(Stream, line, Last),
	Line is Last + 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd) :-
	get_stream_info(Stream, line, LineBegin),
	(	read_term(Stream, Term, Options) ->
		get_stream_info(Stream, line, LineEnd)
	;	throw(syntax_error)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

'$lgt_eclipse_directive_expansion'(demon(PIs), {demon(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_eclipse_directive_expansion'(export(chtab(Char, Class)), {export(chtab(Char, Class))}).
'$lgt_eclipse_directive_expansion'(export(domain(Domain)), {export(domain(Domain))}).
'$lgt_eclipse_directive_expansion'(export(struct(Struct)), {export(struct(Struct))}).
'$lgt_eclipse_directive_expansion'(export(syntax_option(SyntaxOption)), {export(syntax_option(SyntaxOption))}).

'$lgt_eclipse_directive_expansion'(inline(PI1, PI2), {inline(CPI1, CPI2)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PI1, CPI1),
	'$lgt_compile_predicate_indicators'(PI2, CPI2).

'$lgt_eclipse_directive_expansion'(pragma(Pragma), {pragma(Pragma)}).

'$lgt_eclipse_directive_expansion'(set_error_handler(Event, Functor/Arity), {set_error_handler(Event, CFunctor/CArity)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(Functor/Arity, CFunctor/CArity).
'$lgt_eclipse_directive_expansion'(set_event_handler(Event, defers(Functor/Arity)), {set_event_handler(Event, defers(CFunctor/CArity))}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(Functor/Arity, CFunctor/CArity).
'$lgt_eclipse_directive_expansion'(set_event_handler(Event, Functor/Arity), {set_event_handler(Event, CFunctor/CArity)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(Functor/Arity, CFunctor/CArity).

'$lgt_eclipse_directive_expansion'(set_flag(PI, Flag, Value), {set_flag(CPI, Flag, Value)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PI, CPI).

'$lgt_eclipse_directive_expansion'(skipped(PIs), {skipped(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Shortcut to the Logtalk built-in predicate logtalk_load/1
%
%  defined in the adapter files in order to be able to comment it out in case
%  of conflict with some Prolog native feature; it implies conformance with
%  the ISO Prolog standard regarding the definition of the {}/1 syntax
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{File, Files} :-
	!,
	logtalk_load(File),
	{Files}.
{File} :-
	logtalk_load(File).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'(_, _, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  experimental lambda support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term(Term, Copy, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  utility predicates used to construct execution context terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_exec_ctx'(c(This, r(Sender, Self, MetaCallCtx, Stack)), Sender, This, Self, MetaCallCtx, Stack).

'$lgt_exec_ctx_this_rest'(c(This, Ctx), This, Ctx).	% inheritance only requires updating "this"

'$lgt_exec_ctx_this'(c(This, _), This).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  hooks predicates for writing and assert compiled entity terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_write_term_and_source_location'(@stream, @callable, +atom, @callable)

'$lgt_write_term_and_source_location'(Stream, Term, _Kind, _Location) :-
	write_canonical(Stream, Term),
	write(Stream, '.\n').


% '$lgt_assertz_entity_clause'(@clause, +atom)

'$lgt_assertz_entity_clause'(Clause, _Kind) :-
	assertz(Clause).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  error term normalization
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_normalize_error_term'(@callable, -callable)

'$lgt_normalize_error_term'(
	Error,
	Error
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message token printing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%:- multifile('$lgt_logtalk.print_message_token'/3).
%:- dynamic('$lgt_logtalk.print_message_token'/3).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  term hashing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% term_hash(@callable, +integer, +integer, -integer) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

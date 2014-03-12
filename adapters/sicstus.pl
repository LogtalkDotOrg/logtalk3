%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  Adapter file for SICStus Prolog 4.1.0 and later versions
%  Last updated on March 12, 2014
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



:- use_module(library(file_systems)).
:- use_module(library(system)).
:- use_module(library(system3), [pid/1, shell/1, shell/2]).

% disable SICStus Prolog discontiguous predicate clauses warning
% as the Logtalk compiler does its own detection and there's no
% point in printing the same warning twice
%
%:- multifile(message_hook/3).
%message_hook(warning, clauses_not_together(_), _).



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

:- if((current_prolog_flag(version_data, sicstus(Major,Minor,_,_,_)), (Major,Minor) @>= (4,3))).

	'$lgt_iso_predicate'(_) :-
		fail.

:- else.

	'$lgt_iso_predicate'(acyclic_term(_)).
	'$lgt_iso_predicate'(subsumes_term(_, _)).
	'$lgt_iso_predicate'(term_variables(_, _)).

	:- use_module(library(terms), []).
	acyclic_term(Term) :-
		terms:acyclic_term(Term).

	subsumes_term(General, Specific) :-
		prolog:subsumes_chk(General, Specific).

	term_variables(Term, Variables) :-
		prolog:term_variables(Term, Variables).

:- endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer)

:- use_module(library(between), [between/3]).


% forall(+callable, +callable)

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% format(+stream_or_alias, +character_code_list_or_atom, +list) -- built-in


% format(+character_code_list_or_atom, +list) -- built-in


% numbervars(?term, +integer, ?integer) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)


'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable)

setup_call_cleanup(Setup, Call, Cleanup) :-
	call(Setup),
	call_cleanup(Call, Cleanup).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog non-standard built-in meta-predicates and meta-directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_meta_predicate'(@callable, ?callable, ?atom)
%
% table of meta-predicate patterns for proprietary built-in predicates;
% the third argument, which must be either "predicate" or "control_construct",
% is used to guide the compilation of these meta-predicates in debug mode

'$lgt_prolog_meta_predicate'(call_cleanup(_, _), call_cleanup(0, 0), predicate).
'$lgt_prolog_meta_predicate'(call_residue_vars(_, _), call_residue_vars(0, *), predicate).
'$lgt_prolog_meta_predicate'(do(_, _), do(*, 0), predicate) :-
	predicate_property(do(_, _), built_in).
'$lgt_prolog_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_prolog_meta_predicate'(freeze(_, _), freeze(*, 0), predicate).
'$lgt_prolog_meta_predicate'(if(_, _, _), if(0, 0, 0), predicate).
'$lgt_prolog_meta_predicate'(on_exception(_, _, _), on_exception(*, 0, 0), predicate).
'$lgt_prolog_meta_predicate'(save_predicates(_, _), save_predicates([/], *), predicate).
'$lgt_prolog_meta_predicate'(undo(_), undo(0), predicate).
'$lgt_prolog_meta_predicate'(when(_, _), when(*, 0), predicate).
% workaround problematic meta-predicate declarations:
'$lgt_prolog_meta_predicate'(compile(_), compile(*), predicate).
'$lgt_prolog_meta_predicate'(consult(_), consult(*), predicate).
'$lgt_prolog_meta_predicate'(ensure_loaded(_), ensure_loaded(*), predicate).
'$lgt_prolog_meta_predicate'(format(_, _), format(*, *), predicate).
'$lgt_prolog_meta_predicate'(format(_, _, _), format(*, *, *), predicate).
'$lgt_prolog_meta_predicate'(load_files(_), load_files(*), predicate).
'$lgt_prolog_meta_predicate'(load_files(_, _), load_files(*, *), predicate).
'$lgt_prolog_meta_predicate'(load_foreign_resource(_), load_foreign_resource(*), predicate).
'$lgt_prolog_meta_predicate'(use_module(_), use_module(*), predicate).
'$lgt_prolog_meta_predicate'(use_module(_, _), use_module(*, *), predicate).
'$lgt_prolog_meta_predicate'(use_module(_, _, _), use_module(*, *, *), predicate).


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(volatile(_), volatile(/)).


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


% '$lgt_prolog_database_predicate'(@callable)

'$lgt_prolog_database_predicate'(assert(_)).
'$lgt_prolog_database_predicate'(assert(_, _)).
'$lgt_prolog_database_predicate'(asserta(_, _)).
'$lgt_prolog_database_predicate'(assertz(_, _)).
'$lgt_prolog_database_predicate'(clause(_, _, _)).



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

'$lgt_prolog_feature'(prolog_dialect, sicstus).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	current_prolog_flag(version_data, sicstus(Major, Minor, Patch, _, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((4,1,0))).

'$lgt_prolog_feature'(encoding_directive, source).
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

% startup flags:
'$lgt_default_flag'(settings_file, allow).
% lint compilation flags:
'$lgt_default_flag'(unknown_entities, warning).
'$lgt_default_flag'(misspelt_calls, warning).
'$lgt_default_flag'(singleton_variables, warning).
'$lgt_default_flag'(portability, silent).
'$lgt_default_flag'(redefined_built_ins, silent).
'$lgt_default_flag'(missing_directives, warning).
:- if((current_prolog_flag(version_data, sicstus(Major,Minor,_,_,_)), (Major,Minor) @>= (4,3))).
	'$lgt_default_flag'(underscore_variables, dont_care).
:- else.
	'$lgt_default_flag'(underscore_variables, singletons).
:- endif.
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	(	environ('COMSPEC', _) ->
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
'$lgt_default_flag'(prolog_loader, [compilation_mode(compile)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_os_file_name'(+atom, -atom)
% '$lgt_prolog_os_file_name'(-atom, +atom)
%
% converts between Prolog internal file paths and operating-system paths

'$lgt_prolog_os_file_name'(Path, Path).


% '$lgt_expand_path'(+nonvar, -atom)
%
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	current_directory(Directory),
	absolute_file_name(Path, ExpandedPath, [relative_to(Directory)]).


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

'$lgt_file_exists'(File) :-
	current_directory(Directory),
	absolute_file_name(File, Path, [relative_to(Directory)]),
	file_exists(Path).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	absolute_file_name(Directory, Path),
	directory_exists(Path).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	current_directory(Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	current_directory(_, Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	current_directory(Current),
	absolute_file_name(Directory, Path, [relative_to(Current)]),
	(	directory_exists(Path) ->
		true
	;	make_directory(Path)
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
	load_files(File, Options).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	file_property(File, modify_timestamp, Time).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	environ(Variable, Value).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory 

'$lgt_startup_directory'(Directory) :-
	(	environ('LOGTALK_STARTUP_DIRECTORY', Directory) ->
		true
	;	current_directory(Directory)
	).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	environ('LOGTALKUSER', Directory).


% '$lgt_home_directory'(-atom)
%
% returns the Logtalk home directory; fails if unknown

'$lgt_home_directory'(Directory) :-
	environ('LOGTALKHOME', Directory).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory
% must always end with a slash and the extension must be
% the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	atom_codes(File, FileCodes),
	(	'$lgt_strrch'(FileCodes, 0'/, [_Slash| BasenameCodes]) ->
		atom_codes(Basename, BasenameCodes),
		atom_concat(Directory, Basename, File)
	;	Directory = './',
		atom_codes(Basename, FileCodes),
		BasenameCodes = FileCodes
	),
	(	'$lgt_strrch'(BasenameCodes, 0'., ExtensionCodes) ->
		atom_codes(Extension, ExtensionCodes),
		atom_concat(Name, Extension, Basename)
	;	Name = Basename,
		Extension = ''
	).

% the following auxiliar predicate was written by Per Mildner and 
% is used here (renamed just to avoid conflicts) with permission

'$lgt_strrch'(Xs, G, Ys) :-
	Xs = [X| Xs1],
	(	X == G ->
		'$lgt_strrch1'(Xs1, G, Xs, Ys)
	;	'$lgt_strrch'(Xs1, G, Ys)
	).

'$lgt_strrch1'(Xs, _G, _Prev, _Ys) :-
	var(Xs),
	!,
	fail.
'$lgt_strrch1'([], _G, Prev, Ys) :-
	Ys = Prev.
'$lgt_strrch1'(Xs, G, Prev, Ys) :-
	Xs = [X| Xs1],
	(	X == G ->
		'$lgt_strrch1'(Xs1, G, Xs, Ys)
	;	'$lgt_strrch1'(Xs1, G, Prev, Ys)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_current_date'(?integer, ?integer, ?integer)

'$lgt_current_date'(Year, Month, Day) :-
	datime(datime(Year, Month, Day, _, _, _)).


% '$lgt_current_time'(?integer, ?integer, ?integer)

'$lgt_current_time'(Hours, Minutes, Seconds) :-
	datime(datime(_, _, _, Hours, Minutes, Seconds)).



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
	statistics(runtime, [Miliseconds| _]),
	Seconds is Miliseconds / 1000.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	stream_property(Stream, position(Position)),
	stream_position_data(line_count, Position, LineCount),
	Line is LineCount + 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  abstraction of the standard open/4 and close/1 predicates for dealing
%  with the alias/1 option in old non-compliant systems
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_open'(+atom, +atom, -stream, @list)

'$lgt_open'(File, Mode, Stream, Options) :-
	open(File, Mode, Stream, Options).


% '$lgt_close'(@stream)

'$lgt_close'(Stream) :-
	close(Stream).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position, -list)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd, Variables) :-
	stream_position(Stream, PositionBegin),
	stream_position_data(line_count, PositionBegin, LineCountBegin),
	LineBegin is LineCountBegin + 1,
	read_term(Stream, Term, [variable_names(Variables)| Options]),
	stream_position(Stream, PositionEnd),
	stream_position_data(line_count, PositionEnd, LineCountEnd),
	LineEnd is LineCountEnd + 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'((:- Directive), Expanded) :-
	'$lgt_sicstus_directive_expansion'(Directive, Expanded0),
	(	Expanded0 == [] ->
		Expanded  == []
	;	Expanded0 =  {ExpandedDirective} ->
		Expanded  =  {(:- ExpandedDirective)}
	;	Expanded  =  (:- Expanded0)
	).


'$lgt_sicstus_directive_expansion'(mode(_), []).
'$lgt_sicstus_directive_expansion'(public(_), []) :-	% used to provide info to the cross-referencer
	logtalk_load_context(entity_type, module).			% only when we're compiling a module as an object!

'$lgt_sicstus_directive_expansion'(block(Heads), {block(THeads)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_heads'(Heads, _, THeads, '?').
'$lgt_sicstus_directive_expansion'(load_foreign_resource(Resource), {initialization(load_foreign_resource(Resource))}) :-
	load_foreign_resource(Resource).

'$lgt_sicstus_directive_expansion'(op(Priority, Specifier, ':'(Module,Operators)), {op(Priority, Specifier, Operators)}) :-
	Module == user.

'$lgt_sicstus_directive_expansion'(ensure_loaded(File), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, module),
	% ensure_loaded/1 directive used within a module (sloppy replacement for the use_module/1-2 directives)
	'$lgt_sicstus_list_of_exports'(File, Module, Imports).
'$lgt_sicstus_directive_expansion'(module(Module, Exports, _), module(Module, Exports)).
'$lgt_sicstus_directive_expansion'(use_module(File, Imports), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, _),
	'$lgt_sicstus_list_of_exports'(File, Module, _).
'$lgt_sicstus_directive_expansion'(use_module(File), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, _),
	'$lgt_sicstus_list_of_exports'(File, Module, Imports).
'$lgt_sicstus_directive_expansion'(use_module(Module, File, Imports), Directive) :-
	logtalk_load_context(entity_type, _),
	(	var(Module) ->
		'$lgt_sicstus_directive_expansion'(use_module(File, Imports), Directive)
	;	Directive = use_module(Module, Imports)
	).


'$lgt_sicstus_list_of_exports'(File, Module, Exports) :-
	nonvar(File),
	absolute_file_name(File, Path, [extensions(['.pl', '.pro']), access(read), file_errors(fail)]),
	current_module(Module, Path),	% this only succeeds for already loaded modules
	findall(
		Functor/Arity,
		(predicate_property(Module:Predicate, exported), functor(Predicate, Functor, Arity)),
		Exports),
	!.

'$lgt_sicstus_list_of_exports'(File, Module, Exports) :-
	(	absolute_file_name(File, Path, [extensions(['.pl', '.pro']), access(read), file_errors(fail)])
	;	% we may be compiling Prolog module files as Logtalk objects
		absolute_file_name(File, Path, [extensions(['.lgt']), access(read), file_errors(fail)])
	),
	open(Path, read, In),
	call_cleanup(read(In, ModuleDecl), close(In)),
	ModuleDecl = (:- module(Module, Exports)).


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('US-ASCII', 'ANSI_X3.4-1968', _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-1', 'ISO-8859-1', _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-2', 'ISO-8859-2', _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-15', 'ISO-8859-15', _).
'$lgt_logtalk_prolog_encoding'('UTF-8', 'UTF-8', _).
'$lgt_logtalk_prolog_encoding'('UCS-2', Encoding, Stream) :-	% UTF-16 subsumes UCS-2
	'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream).
'$lgt_logtalk_prolog_encoding'('UCS-2BE', 'UTF-16BE', _).
'$lgt_logtalk_prolog_encoding'('UCS-2LE', 'UTF-16LE', _).
'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream) :-	% BOM optional but strongly recommended
	(	stream_property(Stream, encoding('UTF-16BE')) ->
		Encoding = 'UTF-16BE'
	;	stream_property(Stream, encoding('UTF-16LE')) ->
		Encoding = 'UTF-16LE'
	).
'$lgt_logtalk_prolog_encoding'('UTF-16BE', 'UTF-16BE', _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-16LE', 'UTF-16LE', _).
'$lgt_logtalk_prolog_encoding'('UTF-32', Encoding, Stream) :-	% BOM mandatory
	(	stream_property(Stream, encoding('UTF-32BE')) ->
		Encoding = 'UTF-32BE'
	;	stream_property(Stream, encoding('UTF-32LE')) ->
		Encoding = 'UTF-32LE'
	).
'$lgt_logtalk_prolog_encoding'('UTF-32BE', 'UTF-32BE', _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-32LE', 'UTF-32LE', _).



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
	error(existence_error(procedure, ':'(user, Functor/Arity)), Context),
	error(existence_error(procedure, Functor/Arity), Context)
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message token printing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%:- multifile('$logtalk.print_message_token'/5).
%:- dynamic('$logtalk.print_message_token'/5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  term hashing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% term_hash(@callable, +integer, +integer, -integer)

:- use_module(library(terms), [term_hash/4]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  string built-in type
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_string'(@term)

'$lgt_string'(_) :-
	fail.


% '$lgt_string_codes'(+string, -list(codes))
% '$lgt_string_codes'(-string, +list(codes))

'$lgt_string_codes'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Shortcuts to the Logtalk built-in predicates logtalk_load/1 and
%  logtalk_make/1
%
%  defined in the adapter files to make it easier to comment them out in case
%  of conflict with some Prolog native feature; it implies conformance with
%  the ISO Prolog standard regarding the definition of the {}/1 syntax
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


{*} :-
	!,
	logtalk_make(all).
{!} :-
	!,
	logtalk_make(clean).


{File, Files} :-
	!,
	logtalk_load(File),
	{Files}.
{File} :-
	logtalk_load(File).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

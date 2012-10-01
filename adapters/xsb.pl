%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
%
%  Adapter file for XSB 3.3 or later versions
%  Last updated on August 2, 2012
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


:- import datime/1 from standard.
:- import expand_atom/2 from standard.



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

'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk specific predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable)

setup_call_cleanup(Setup, Call, Cleanup) :-
	call(Setup),
	call_cleanup(Call, Cleanup).


% setup_call_catcher_cleanup(+callable, +callable, ?term, +callable)

setup_call_catcher_cleanup(Setup, Call, Catcher, Cleanup) :-
	call(Setup),
	catch(Call, Catcher, Cleanup).


% forall(+callable, +callable) -- built-in


% call/2-7 -- built-in



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

'$lgt_prolog_meta_predicate'(fail_if(_), fail_if(0), predicate).
'$lgt_prolog_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_prolog_meta_predicate'(not(_), not(0), predicate).
'$lgt_prolog_meta_predicate'(sk_not(_), sk_not(0), predicate).
'$lgt_prolog_meta_predicate'(table_once(_), table_once(0), predicate).
'$lgt_prolog_meta_predicate'(tfindall(_, _, _), tfindall(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(tnot(_), tnot(0), predicate).

'$lgt_prolog_meta_predicate'(time(_), time(0), predicate).

'$lgt_prolog_meta_predicate'(abolish_table_call(_), abolish_table_call(0), predicate).
'$lgt_prolog_meta_predicate'(abolish_table_call(_, _), abolish_table_call(0, *), predicate).
'$lgt_prolog_meta_predicate'(abolish_table_pred(_), abolish_table_pred(0), predicate).
'$lgt_prolog_meta_predicate'(abolish_table_pred(_, _), abolish_table_pred(0, *), predicate).

'$lgt_prolog_meta_predicate'(thread_create(_, _, _), thread_create(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(thread_create(_, _), thread_create(0, *), predicate).
'$lgt_prolog_meta_predicate'(thread_create(_), thread_create(0), predicate).
'$lgt_prolog_meta_predicate'(thread_signal(_, _), thread_signal(*, 0), predicate).
'$lgt_prolog_meta_predicate'(with_mutex(_, _), with_mutex(*, 0), predicate).


% '$lgt_prolog_meta_directive'(@callable)

'$lgt_prolog_meta_directive'(_) :-
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
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(tmp, '.xwam').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, xsb).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	current_prolog_flag(version_data, xsb(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((3,3,0))).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, supported).
'$lgt_prolog_feature'(threads, Threads) :-
	(	xsb_configuration(engine_mode, 'multi-threading') ->
		Threads = supported
	;	Threads = unsupported
	).
'$lgt_prolog_feature'(modules, supported).
'$lgt_prolog_feature'(coinduction, unsupported).



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
'$lgt_default_flag'(underscore_variables, singletons).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	(	xsb_configuration(os_type, windows) ->
		ScratchDirectory = './lgt_tmp/'
	;	ScratchDirectory = './.lgt_tmp/'
	).
'$lgt_default_flag'(report, on).
'$lgt_default_flag'(clean, on).
'$lgt_default_flag'(smart_compilation, off).
'$lgt_default_flag'(reload, always).
'$lgt_default_flag'(code_prefix, '$').
'$lgt_default_flag'(optimize, on).
'$lgt_default_flag'(source_data, on).
'$lgt_default_flag'(debug, off).
% Prolog compiler and loader flags:
'$lgt_default_flag'(prolog_compiler, []).
'$lgt_default_flag'(prolog_loader, [optimize]).



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
	expand_atom(Path, EnvVarExpandedPath),
	path_sysop(expand, EnvVarExpandedPath, ExpandedPath).


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

'$lgt_file_exists'(File) :-
	'$lgt_expand_path'(File, Expanded),
	path_sysop(exists, Expanded).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	'$lgt_expand_path'(File, Expanded),
	path_sysop(rm, Expanded).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	'$lgt_expand_path'(Directory, Expanded),
	path_sysop(exists, Expanded),
	path_sysop(isdir, Expanded).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	path_sysop(cwd, Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	'$lgt_expand_path'(Directory, Expanded),
	path_sysop(chdir, Expanded).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	'$lgt_expand_path'(Directory, Expanded),
	(	path_sysop(exists, Expanded) ->
		true
	;	path_sysop(mkdir, Expanded)
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
	'$lgt_expand_path'(File, Expanded),
	reconsult(Expanded, Options).


% '$lgt_compare_file_modification_times'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_modification_times'(Result, File1, File2) :-
	path_sysop(modtime, File1, Time1),
	path_sysop(modtime, File2, Time2),
	compare(Result, Time1, Time2).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Name, Value) :-
	atom_concat('$', Name, Variable),
	expand_atom(Variable, Value).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory 

'$lgt_startup_directory'(Directory) :-
	(	expand_atom('$LOGTALK_STARTUP_DIRECTORY', Directory) ->
		true
	;	path_sysop(cwd, Directory)
	).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	expand_atom('$LOGTALKUSER', Directory).


% '$lgt_home_directory'(-atom)
%
% returns the Logtalk home directory; fails if unknown

'$lgt_home_directory'(Directory) :-
	expand_atom('$LOGTALKHOME', Directory).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory
% must always end with a slash and the extension must be
% the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	path_sysop(dirname, File, Directory0),
	(	Directory0 = '' ->
		Directory = './'
	;	sub_atom(Directory0, _, _, 0, '/') ->
		Directory = Directory0
	;	atom_concat(Directory0, '/', Directory)
	),
	path_sysop(basename, File, Name),
	path_sysop(extension, File, Extension0),
	(	Extension0 = '' ->
		Extension = Extension0
	;	atom_concat('.', Extension0, Extension)	
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_current_date'(?Year, ?Month, ?Day)

'$lgt_current_date'(Year, Month, Day) :-
	datime(datime(Year, Month, Day, _, _, _)).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	datime(datime(_, _, _, Hours, Mins, Secs)).



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

'$lgt_stream_current_line_number'(_, -1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, Options, '-'(-1, -1)) :-
	read_term(Stream, Term, Options).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_term_expansion'(@callable, -callable)

'$lgt_pl_term_expansion'((:- Directive), Expanded) :-
	'$lgt_xsb_directive_expansion'(Directive, Expanded0),
	(	Expanded0 == [] ->
		Expanded  == []
	;	Expanded0 =  {ExpandedDirective} ->
		Expanded  =  {(:- ExpandedDirective)}
	;	Expanded  =  (:- Expanded0)
	).


'$lgt_xsb_directive_expansion'(index(PI, IS), {index(CPI, IS)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PI, CPI).

'$lgt_xsb_directive_expansion'(table(PIs), {table(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_xsb_directive_expansion'(as(table(PIs),ShareMode), {as(table(CPIs),ShareMode)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_xsb_directive_expansion'(thread_private(PIs), {thread_private(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_xsb_directive_expansion'(thread_shared(PIs), {thread_shared(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_xsb_directive_expansion'(use_subsumptive_tabling(PIs), {use_subsumptive_tabling(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_xsb_directive_expansion'(use_variant_tabling(PIs), {use_variant_tabling(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_xsb_directive_expansion'(import(as(from(PI), Module), Functor), use_module(Module, [as(PI, Functor)])).

'$lgt_xsb_directive_expansion'(import(from(PIs1, Module)), use_module(Module, PIs2)) :-
	'$lgt_xsb_conjunction_to_list'(PIs1, PIs2).

'$lgt_xsb_directive_expansion'(local(PIs1), private(PIs2)) :-
	'$lgt_xsb_conjunction_to_list'(PIs1, PIs2).


'$lgt_xsb_conjunction_to_list'(Term, [Term]) :-
	var(Term),
	!.
'$lgt_xsb_conjunction_to_list'((Term, Conjunction), [Term| Terms]) :-
	!,
	'$lgt_xsb_conjunction_to_list'(Conjunction, Terms).
'$lgt_xsb_conjunction_to_list'(Term, [Term]).


% '$lgt_pl_goal_expansion'(@callable, -callable)

'$lgt_pl_goal_expansion'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  multi-threading predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% thread_property(+atom, ?nonvar) -- built-in


% thread_self(?atom) -- built-in


% thread_create(@callable, -thread_id, +list) -- built-in


% thread_join(+atom, -nonvar) -- built-in


% thread_detach(+atom) -- built-in


% thread_exit(@term) -- built-in


% thread_send_message(+atom, @callable) -- built-in


% thread_peek_message(+atom, ?callable) -- built-in


% thread_get_message(+atom, ?callable) -- built-in


% thread_get_message(?callable) -- built-in


% thread_signal(Thread, _Signal) -- built-in



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
	copy_term(Term, Copy).



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
	write(Stream, '.'),
	nl(Stream).


% '$lgt_assertz_entity_clause'(@clause, +atom)

'$lgt_assertz_entity_clause'(Clause, _Kind) :-
	assertz(Clause).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  annotations
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_value_annotation'(@callable, -atom, -callable, -callable, -callable)

'$lgt_default_value_annotation'(_, _, _, _, _) :-
	fail.


% '$lgt_default_goal_annotation'(@callable, -atom, -callable, -callable, -callable)

'$lgt_default_goal_annotation'(_, _, _, _, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  error term normalization
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_normalize_error_term'(@callable, -callable)

'$lgt_normalize_error_term'(error(error(Error, _, _), _, _), NormalizedError) :-
	'$lgt_normalize_error_term'(error(Error, _, _), NormalizedError).
'$lgt_normalize_error_term'(error(Error, _, _), error(NormalizedError, _)) :-
	'$lgt_normalize_error_term_aux'(Error, NormalizedError).


'$lgt_normalize_error_term_aux'(
	existence_error(procedure, ':'(usermod, Functor/Arity)),
	existence_error(procedure, Functor/Arity)
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

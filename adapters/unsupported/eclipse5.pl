%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  Adapter file for ECLiPSe 5.10#26 or later 5.10 versions
%  Last updated on May 4, 2013
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

:- use_module(library(numbervars)).


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


:- dynamic(lgt_exception_/1).


initialization(Goal) :-
	call(Goal).


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(assertz(_)).
'$lgt_iso_predicate'(atom_codes(_, _)).
'$lgt_iso_predicate'(atom_concat(_, _, _)).
'$lgt_iso_predicate'(catch(_, _, _)).
'$lgt_iso_predicate'(current_input(_)).
'$lgt_iso_predicate'(current_output(_)).
'$lgt_iso_predicate'(number_codes(_, _)).
'$lgt_iso_predicate'(throw(_)).
'$lgt_iso_predicate'(unify_with_occurs_check(_, _)).


assertz(Clause) :-
	assert(Clause).


atom_codes(Atom, List) :-
	var(Atom),
	string_list(String, List),
	atom_string(Atom, String).

atom_codes(Atom, List) :-
	nonvar(Atom),
	atom_string(Atom, String),
	string_list(String, List).


atom_concat(Atom1, Atom2, Atom3) :-
	nonvar(Atom1), nonvar(Atom2),
	!,
	concat_atoms(Atom1, Atom2, Atom3).

atom_concat(Atom1, Atom2, Atom3) :-
	nonvar(Atom3),
	name(Atom3, Codes3),
	'$lgt_append'(Codes1, Codes2, Codes3),
	name(Atom1, Codes1),
	name(Atom2, Codes2).


catch(Goal, Catcher, Recovery) :-
	block(Goal, Tag, lgt_catch_recovery(Tag, Catcher, Recovery)).


lgt_catch_recovery(Tag, Catcher, Recovery) :-
	(	Tag = lgt_error ->
		retract(lgt_exception_(Ball)),
		!,
		(	Catcher = Ball ->
			call(Recovery)
		;	exit_block(Tag)
		)
	;	exit_block(Tag)
	).


lgt_uncatched_exception :-
	(	retract(lgt_exception_(Ball)) ->
		writeq(Ball), nl
	;	write('uncatched exception'), nl
	),
	abort.


:- set_error_handler(230, lgt_uncatched_exception/0).


current_input(Stream) :-
	get_stream(input, Stream).


current_output(Stream) :-
	get_stream(output, Stream).


number_codes(Number, Codes) :-
	var(Number),
	string_list(String, Codes),
	number_string(Number, String).

number_codes(Number, Codes) :-
	nonvar(Number),
	number_string(Number, String),
	string_list(String, Codes).


throw(Ball) :-
	asserta(lgt_exception_(Ball)),
	exit_block(lgt_error).


:- set_flag(occur_check, on).
unify_with_occurs_check(X, X).
:- set_flag(occur_check, off).



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


% forall(+callable, +callable)

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% retractall(+callable)

retractall(Head) :-
	retract_all(Head).


% call/2-7

:- export call/2.	% avoid conflict with obsolete built-in predicate
call(F, A) :-
	Call =.. [F, A],
	call(Call).

call(F, A1, A2) :-
	Call =.. [F, A1, A2],
	call(Call).

call(F, A1, A2, A3) :-
	Call =.. [F, A1, A2, A3],
	call(Call).

call(F, A1, A2, A3, A4) :-
	Call =.. [F, A1, A2, A3, A4],
	call(Call).

call(F, A1, A2, A3, A4, A5) :-
	Call =.. [F, A1, A2, A3, A4, A5],
	call(Call).

call(F, A1, A2, A3, A4, A5, A6) :-
	Call =.. [F, A1, A2, A3, A4, A5, A6],
	call(Call).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog non-standard built-in meta-predicates and meta-directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_meta_predicate'(?callable, ?atom).

'$lgt_prolog_meta_predicate'(*->(0, 0), control_construct).
'$lgt_prolog_meta_predicate'(~(0), control_construct).
'$lgt_prolog_meta_predicate'(-?->(0), control_construct).
'$lgt_prolog_meta_predicate'(block(0, *, 0), predicate).
'$lgt_prolog_meta_predicate'(call_priority(0, *), predicate).
'$lgt_prolog_meta_predicate'(coverof(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(do(*, 0), predicate).
'$lgt_prolog_meta_predicate'(event_create(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(make_suspension(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(mutex(*, 0), predicate).
'$lgt_prolog_meta_predicate'(not(0), predicate).
'$lgt_prolog_meta_predicate'(suspend(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(suspend(0, *, *, *), predicate).



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
'$lgt_prolog_feature'(prolog_compatible_version, @>=((5,10,26))).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
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
	(	get_flag(hostarch, HostArch), atom_string(i386_nt, HostArch) ->
		ScratchDirectory = './lgt_tmp/'
	;	ScratchDirectory = './.lgt_tmp/'
	).
'$lgt_default_flag'(report, on).
'$lgt_default_flag'(clean, off).
'$lgt_default_flag'(code_prefix, '$').
'$lgt_default_flag'(optimize, off).
'$lgt_default_flag'(source_data, on).
'$lgt_default_flag'(debug, off).
% Prolog compiler and loader flags:
'$lgt_default_flag'(prolog_compiler, []).
'$lgt_default_flag'(prolog_loader, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_expand_path'(+nonvar, -atom)
%
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	atom_string(Path, PathString),
	canonical_path_name(PathString, ExpandedPathString),
	atom_string(ExpandedPath, ExpandedPathString).


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

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
	atom_string(Directory, DirectoryString),
	canonical_path_name(DirectoryString, Path),
	exists(Path).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	getcwd(Directory).


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

'$lgt_load_prolog_code'(File, _, _) :-
	get_flag(debug_compile, Current),
	set_flag(debug_compile, off),
	compile(File),
	set_flag(debug_compile, Current).


% '$lgt_compare_file_modification_times'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_modification_times'(Result, File1, File2) :-
	get_file_info(File1, mtime, Time1),
	get_file_info(File2, mtime, Time2),
	compare(Result, Time1, Time2).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	getenv(Variable, Value).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory 

'$lgt_startup_directory'(Directory) :-
	(	getenv('LOGTALK_STARTUP_DIRECTORY', Directory) ->
		true
	;	getcwd(Directory)
	).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	getenv('LOGTALKUSER', Directory).


% '$lgt_home_directory'(-atom)
%
% returns the Logtalk home directory; fails if unknown

'$lgt_home_directory'(Directory) :-
	getenv('LOGTALKHOME', Directory).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-  use_module(library(calendar)).


% '$lgt_current_date'(?Year, ?Month, ?Day)

'$lgt_current_date'(Year, Month, Day) :-
	mjd_now(MJD),
	mjd_to_date(MJD, Day/Month/Year).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	mjd_now(MJD),
	mjd_to_time(MJD, Hours:Mins:FloatSecs),
	Secs is integer(floor(FloatSecs)).



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
%  callable predicate
%
%  the usual callable/1 definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% callable(@term)

callable(Term) :-
	nonvar(Term),
	functor(Term, Functor, _),
	atom(Functor).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  read character predicate
%
%  read a single character echoing it and writing a newline after
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_read_single_char'(Char) :-
	flush(user), tyi(Code), put(Code), nl, char_code(Char, Code).



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


% '$lgt_prolog_meta_directive'(@callable, @callable)

'$lgt_prolog_meta_directive'(_, _) :-
	fail.


% '$lgt_ignore_pl_directive'(@callable)

'$lgt_ignore_pl_directive'(mode(_)).
'$lgt_ignore_pl_directive'(comment(_, _)).


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(demon(PIs), demon(CPIs)) :-
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_rewrite_and_copy_pl_directive'(inline(PI1, PI2), inline(CPI1, CPI2)) :-
	'$lgt_compile_predicate_indicators'(PI1, CPI1),
	'$lgt_compile_predicate_indicators'(PI2, CPI2).

'$lgt_rewrite_and_copy_pl_directive'(pragma(Pragma), pragma(Pragma)).

'$lgt_rewrite_and_copy_pl_directive'(set_flag(PI, Flag, Value), set_flag(CPI, Flag, Value)) :-
	'$lgt_compile_predicate_indicators'(PI, CPI).


% '$lgt_rewrite_and_recompile_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_recompile_pl_directive'(import(from(Conjunction, Module)), use_module(Module, Imports)) :-
	'$lgt_flatten_list'([Conjunction], Imports).

'$lgt_rewrite_and_recompile_pl_directive'(local(Functor/Arity), private(Functor/Arity)).

'$lgt_rewrite_and_recompile_pl_directive'(lib(Library), use_module(library(Library), Exports)) :-
	'$lgt_eclipse_list_of_exports'(library(Library), Exports).

'$lgt_rewrite_and_recompile_pl_directive'(reexport(Module), reexport(Module, Exports)) :-
	atom(Module),
	'$lgt_eclipse_list_of_exports'(Module, Exports).

'$lgt_rewrite_and_recompile_pl_directive'(reexport(from(Conjunction, Module)), reexport(Module, Exports)) :-
	'$lgt_flatten_list'([Conjunction], Exports).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(Library), use_module(Library, Imports)) :-
	'$lgt_eclipse_list_of_exports'(Library, Imports).


'$lgt_eclipse_list_of_exports'(Library, Exports) :-		% only works for already loaded modules
	(	atom(Library) ->
		Module = Library
	;	Library =..[_, Module]
	),
	current_module(Module),
	get_module_info(Module, interface, Interface),
	'$lgt_eclipse_filter_exports'(Interface, Exports).


'$lgt_eclipse_filter_exports'([], []).

'$lgt_eclipse_filter_exports'([export(Predicate)| Interface], [Predicate| Exports]) :-
	'$lgt_eclipse_filter_exports'(Interface, Exports).



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


'$lgt_exec_ctx'([This, Sender, Self, MetaCallCtx, Stack], Sender, This, Self, MetaCallCtx, Stack).

'$lgt_exec_ctx_this_rest'([This| Ctx], This, Ctx).	% inheritance only requires updating "this"

'$lgt_exec_ctx_this'([This| _], This).



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
%  annotations
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_value_annotation'(@callable, -term, -callable, -callable)

'$lgt_default_value_annotation'(_, _, _, _) :-
	fail.


% '$lgt_default_goal_annotation'(@callable, -callable, -callable, -callable)

'$lgt_default_goal_annotation'(_, _, _, _) :-
	fail.


% '$lgt_default_body_annotation'(@callable, -callable, -callable)

'$lgt_default_body_annotation'(_, _, _) :-
	fail.



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for YAP Prolog 6.3.4 and later versions
%  Last updated on July 11, 2020
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- no_source.
:- set_prolog_flag(generate_debug_info, false).

:- use_module(library(system)).
:- use_module(library(terms), [term_hash/4]).
:- use_module(library(lists), [member/2]).

:- set_prolog_flag(update_semantics, logical).
:- set_prolog_flag(unknown, error).
:- set_prolog_flag(syntax_errors, error).
%:-	set_prolog_flag(language, iso),		% commented due to all the YAP libraries that don't compile in "iso" mode!


% disable YAP discontiguous predicate clauses warning as
% the Logtalk compiler does its own detection and there's
% no point in printing the same warning twice
%
%:- multifile(message_hook/3).
%:- dynamic(message_hook/3).
%message_hook(clauses_not_together(_), _, _).



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer) -- built-in


% findall(?term, +callable, ?list, +list) -- built-in


% forall(+callable, +callable) -- built-in


% format(+stream_or_alias, +character_code_list_or_atom, +list) -- built-in

'$lgt_format'(Stream, Format, Arguments) :-
	format(Stream, Format, Arguments).

'$lgt_format'(Format, Arguments) :-
	format(Format, Arguments).


% format(+character_code_list_or_atom, +list) -- built-in


% numbervars(?term, +integer, ?integer) -- built-in



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

:- if((current_prolog_flag(version_data, yap(Major,Minor,_,_)), (Major,Minor) @< (6,3,4))).
	'$lgt_predicate_property'(Pred, Prop) :-
		predicate_property(Pred, Prop).
:- else.
	'$lgt_predicate_property'(Pred, Prop) :-
		current_prolog_flag(autoload, Value),
		setup_call_cleanup(
			set_prolog_flag(autoload, false),
			predicate_property(Pred, Prop),
			set_prolog_flag(autoload, Value)
		).
:- endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable) -- built-in



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
'$lgt_prolog_meta_predicate'(alarm(_, _, _), alarm(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(all(_, _, _), all(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(call_cleanup(_, _), call_cleanup(0, 0), predicate).
'$lgt_prolog_meta_predicate'(call_cleanup(_, _,_), call_cleanup(0, *, 0), predicate).
'$lgt_prolog_meta_predicate'(call_residue(_, _), call_residue(0, *), predicate).
:- if(predicate_property(call_residue_vars(_, _), built_in)).
	'$lgt_prolog_meta_predicate'(call_residue_vars(_, _), call_residue_vars(0, *), predicate).
:- endif.
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _, _, _, _, _, _, _, _), call_with_args(9, *, *, *, *, *, *, *, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _, _, _, _, _, _, _), call_with_args(8, *, *, *, *, *, *, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _, _, _, _, _, _), call_with_args(7, *, *, *, *, *, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _, _, _, _, _), call_with_args(6, *, *, *, *, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _, _, _, _), call_with_args(5, *, *, *, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _, _, _), call_with_args(4, *, *, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _, _), call_with_args(3, *, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _, _), call_with_args(2, *, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_, _), call_with_args(1, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_args(_), call_with_args(0), predicate).
:- if(predicate_property(depth_bound_call(_, _), built_in)).
	'$lgt_prolog_meta_predicate'(depth_bound_call(_, _), depth_bound_call(0, *), predicate).
:- endif.
'$lgt_prolog_meta_predicate'(if(_, _, _), if(0, 0, 0), predicate).
'$lgt_prolog_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_prolog_meta_predicate'(freeze(_, _), freeze(*, 0), predicate).
'$lgt_prolog_meta_predicate'(hide_predicate(_), hide_predicate(/), predicate).
'$lgt_prolog_meta_predicate'(incore(_), incore(0), predicate).
'$lgt_prolog_meta_predicate'(not(_), not(0), predicate).
'$lgt_prolog_meta_predicate'(on_exception(_, _, _), on_exception(*, 0, 0), predicate).
'$lgt_prolog_meta_predicate'(time_out(_, _, _), time_out(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(when(_, _), when(*, 0), predicate).
'$lgt_prolog_meta_predicate'(setup_call_cleanup(_, _, _), setup_call_cleanup(0, 0, 0), predicate).
'$lgt_prolog_meta_predicate'(setup_call_catcher_cleanup(_, _, _, _), setup_call_catcher_cleanup(0, 0, *, 0), predicate).
'$lgt_prolog_meta_predicate'(time(_), time(0), predicate).
'$lgt_prolog_meta_predicate'(thread_initialization(_), thread_initialization(0), predicate).
'$lgt_prolog_meta_predicate'(thread_at_exit(_), thread_at_exit(0), predicate).
'$lgt_prolog_meta_predicate'(thread_create(_, _, _), thread_create(0, *, *), predicate).
'$lgt_prolog_meta_predicate'(thread_create(_, _), thread_create(0, *), predicate).
'$lgt_prolog_meta_predicate'(thread_create(_), thread_create(0), predicate).
'$lgt_prolog_meta_predicate'(thread_signal(_, _), thread_signal(*, 0), predicate).
'$lgt_prolog_meta_predicate'(with_mutex(_, _), with_mutex(*, 0), predicate).
:- if(predicate_property(with_output_to(_, _), built_in)).
	'$lgt_prolog_meta_predicate'(with_output_to(_, _), with_output_to(*, 0), predicate).
:- endif.
% tabling meta-predicates:
'$lgt_prolog_meta_predicate'(abolish_table(_), abolish_table(/), predicate).
'$lgt_prolog_meta_predicate'(is_tabled(_), is_tabled(/), predicate).
'$lgt_prolog_meta_predicate'(table_statistics(_), table_statistics(/), predicate).
'$lgt_prolog_meta_predicate'(tabling_mode(_, _), tabling_mode(/, *), predicate).
'$lgt_prolog_meta_predicate'(show_table(_), show_table(/), predicate).
% workaround problematic meta-predicate declarations:
'$lgt_prolog_meta_predicate'(compile(_), compile(*), predicate).
'$lgt_prolog_meta_predicate'(consult(_), consult(*), predicate).
'$lgt_prolog_meta_predicate'(ensure_loaded(_), ensure_loaded(*), predicate).
'$lgt_prolog_meta_predicate'(format(_, _), format(*, *), predicate).
'$lgt_prolog_meta_predicate'(format(_, _, _), format(*, *, *), predicate).
'$lgt_prolog_meta_predicate'(load_files(_, _), load_files(*, *), predicate).
'$lgt_prolog_meta_predicate'(reconsult(_), reconsult(*), predicate).
'$lgt_prolog_meta_predicate'(use_module(_), use_module(*), predicate).
'$lgt_prolog_meta_predicate'(use_module(_, _), use_module(*, *), predicate).
'$lgt_prolog_meta_predicate'(use_module(_, _, _), use_module(*, *, *), predicate).


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(at_halt(_), at_halt(0)).
'$lgt_prolog_meta_directive'(initialization(_, _), initialization(0, *)).
'$lgt_prolog_meta_directive'(thread_initialization(_), thread_initialization(0)).
'$lgt_prolog_meta_directive'(thread_local(_), thread_local(/)).
'$lgt_prolog_meta_directive'(volatile(_), volatile(/)).


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


% '$lgt_candidate_tautology_or_falsehood_goal_hook'(@callable)
%
% valid candidates are proprietary built-in predicates with
% no side-effects when called with ground arguments

'$lgt_candidate_tautology_or_falsehood_goal_hook'(_ =@= _).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(atom_number(_, _)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(cyclic_term(_)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(name(_, _)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(rational(_)).


% '$lgt_prolog_database_predicate'(@callable)

'$lgt_prolog_database_predicate'(assert(_)).
'$lgt_prolog_database_predicate'(assert(_, _)).
'$lgt_prolog_database_predicate'(asserta(_, _)).
'$lgt_prolog_database_predicate'(assertz(_, _)).
'$lgt_prolog_database_predicate'(clause(_, _, _)).
'$lgt_prolog_database_predicate'(nth_clause(_, _, _)).
'$lgt_prolog_database_predicate'(assert_static(_)).
'$lgt_prolog_database_predicate'(asserta_static(_)).
'$lgt_prolog_database_predicate'(assertz_static(_)).
'$lgt_prolog_database_predicate'(listing(_)).


% '$lgt_prolog_predicate_property'(?callable)

'$lgt_prolog_predicate_property'(tabled).
'$lgt_prolog_predicate_property'(thread_local).
'$lgt_prolog_predicate_property'(volatile).



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
%  files and for backend specific temporary files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_file_extension'(?atom, ?atom)

'$lgt_file_extension'(logtalk, '.lgt').
'$lgt_file_extension'(logtalk, '.logtalk').
'$lgt_file_extension'(object, '.yap').
'$lgt_file_extension'(prolog, '.yap').
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(prolog, '.prolog').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  backend Prolog compiler features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% backend Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, yap).
'$lgt_prolog_feature'(prolog_version, v(Major, Minor, Patch)) :-
	current_prolog_flag(version_data, yap(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=(v(6,3,4))).
'$lgt_prolog_feature'(prolog_conformance, lax).

'$lgt_prolog_feature'(encoding_directive, full).
'$lgt_prolog_feature'(tabling, Tabling) :-
	(	current_prolog_flag(system_options, tabling) ->
		Tabling = supported
	;	current_prolog_flag(system_options, Options),
		member(tabling, Options) ->
		Tabling = supported
	;	Tabling = unsupported
	).
'$lgt_prolog_feature'(engines, Engines) :-
	(	current_prolog_flag(system_options, threads) ->
		Engines = supported
	;	current_prolog_flag(system_options, Options),
		member(threads, Options) ->
		Engines = supported
	;	Engines = unsupported
	).
'$lgt_prolog_feature'(threads, Threads) :-
	(	current_prolog_flag(system_options, threads) ->
		Threads = supported
	;	current_prolog_flag(system_options, Options),
		member(threads, Options) ->
		Threads = supported
	;	Threads = unsupported
	).
'$lgt_prolog_feature'(modules, supported).
'$lgt_prolog_feature'(coinduction, supported).
'$lgt_prolog_feature'(unicode, full).



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
'$lgt_default_flag'(singleton_variables, warning).
'$lgt_default_flag'(steadfastness, silent).
'$lgt_default_flag'(naming, silent).
'$lgt_default_flag'(duplicated_clauses, silent).
'$lgt_default_flag'(tail_recursive, silent).
'$lgt_default_flag'(portability, silent).
'$lgt_default_flag'(redefined_built_ins, silent).
'$lgt_default_flag'(redefined_operators, warning).
'$lgt_default_flag'(deprecated, warning).
'$lgt_default_flag'(missing_directives, warning).
'$lgt_default_flag'(duplicated_directives, warning).
'$lgt_default_flag'(trivial_goal_fails, warning).
'$lgt_default_flag'(always_true_or_false_goals, warning).
'$lgt_default_flag'(lambda_variables, warning).
'$lgt_default_flag'(suspicious_calls, warning).
'$lgt_default_flag'(underscore_variables, singletons).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	(	current_prolog_flag(unix, true) ->
		ScratchDirectory = './.lgt_tmp/'
	;	ScratchDirectory = './lgt_tmp/'
	).
'$lgt_default_flag'(report, Report) :-
	(	current_prolog_flag(verbose, normal) ->
		Report = on
	;	Report = warnings
	).
'$lgt_default_flag'(clean, on).
'$lgt_default_flag'(code_prefix, '$').
'$lgt_default_flag'(optimize, off).
'$lgt_default_flag'(source_data, on).
'$lgt_default_flag'(reload, changed).
'$lgt_default_flag'(debug, off).
% Prolog compiler and loader flags:
'$lgt_default_flag'(prolog_compiler, []).
'$lgt_default_flag'(prolog_loader, [silent(true), compilation_mode(compact)]).



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
	prolog_to_os_filename(PrologPath, OSPath).


% '$lgt_expand_path'(+nonvar, -atom)
%
% expands a file path to a full path

:- if((current_prolog_flag(version_data, yap(Major,Minor,_,_)), (Major,Minor) @< (6,3))).
	'$lgt_expand_path'(Path, ExpandedPath) :-
		working_directory(Current, Current),
		absolute_file_name(Path, [access(none), file_type(txt), relative_to(Current)], ExpandedPath).
:- else.
	'$lgt_expand_path'(Path, ExpandedPath) :-
		working_directory(Current, Current),
		(	absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath) ->
			true
		;	absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)
		).
:- endif.


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

'$lgt_file_exists'(File) :-
	file_exists(File).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	(	atom_concat(Directory2, '/', Directory) ->
		true
	;	Directory2 = Directory
	),
	'$lgt_expand_path'(Directory2, Path),
	file_exists(Path),
	file_property(Path, type(directory)).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Directory, Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	'$lgt_expand_path'(Directory, ExpandedPath),
	working_directory(_, ExpandedPath).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	'$lgt_directory_exists'(Directory) ->
		true
	;	make_directory(Directory)
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

'$lgt_load_prolog_code'(File, _Source, Options) :-
	load_files(File, Options).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	file_property(File, mod_time(Time)).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	environ(Variable, Value).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory must always end
% with a slash; the extension must start with a "." when defined and must
% be the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	file_directory_name(File, Directory0),
	atom_concat(Directory0, '/', Directory),
	file_base_name(File, Basename),
	file_name_extension(Name, Extension0, Basename),
	(	Extension0 = '' ->
		Extension = Extension0
	;	atom_concat('.', Extension0, Extension)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	stream_position(Stream, Position),
	stream_position_data(line_count, Position, Line).



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
	read_term(Stream, Term, [syntax_errors(error), term_position(PositionBegin), variable_names(Variables)| Options]),
	stream_position_data(line_count, PositionBegin, LineBegin),
	stream_position(Stream, PositionEnd),
	stream_position_data(line_count, PositionEnd, LineEnd).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'((:- Directive), Expanded) :-
	% allow first-argument indexing
	catch('$lgt_yap_directive_expansion'(Directive, Expanded), _, fail).


'$lgt_yap_directive_expansion'(style_check(_), []).

'$lgt_yap_directive_expansion'(create_prolog_flag(Key, Value, Options), {:- create_prolog_flag(Key, Value, Options)}).

'$lgt_yap_directive_expansion'(expects_dialect(Dialect), {:- expects_dialect(Dialect)}) :-
	'$expects_dialect'(Dialect).

'$lgt_yap_directive_expansion'(load_foreign_files(Files, Libs, InitRoutine), {:- initialization(load_foreign_files(Files, Libs, InitRoutine))}) :-
	load_foreign_files(Files, Libs, InitRoutine).

'$lgt_yap_directive_expansion'(public(PIs), {:- public(CPIs)}) :-	% used to make clause/2 work with static predicates
	logtalk_load_context(entity_type, module),					% only when we're compiling a module as an object!
	'$lgt_compile_predicate_indicators'(PIs, _, CPIs).

'$lgt_yap_directive_expansion'(encoding(Encoding1), (:- encoding(Encoding2))) :-
	nonvar(Encoding1),
	'$lgt_yap_encoding_to_logtalk_encoding'(Encoding1, Encoding2).

'$lgt_yap_directive_expansion'(ensure_loaded(File), Expanded) :-
	logtalk_load_context(entity_type, module),
	% ensure_loaded/1 directive used within a module
	% (sloppy replacement for the use_module/1-2 directives)
	'$lgt_yap_directive_expansion'(use_module(File), Expanded).

'$lgt_yap_directive_expansion'(module(Module,Exports0), [(:- module(Module,Exports))| Clauses]) :-
	'$lgt_yap_split_predicate_aliases'(Exports0, Exports, Clauses).

'$lgt_yap_directive_expansion'(reexport([]), []) :-
	!.
'$lgt_yap_directive_expansion'(reexport([File| Files]), [(:- use_module(Module, Exports)), (:- export(Exports))| Terms]) :-
	!,
	'$lgt_yap_list_of_exports'(File, Module, Exports0),
	'$lgt_yap_fix_predicate_aliases'(Exports0, Exports),
	'$lgt_yap_directive_expansion'(reexport(Files), Terms).
'$lgt_yap_directive_expansion'(reexport(File), [(:- use_module(Module, Exports)), (:- export(Exports))]) :-
	'$lgt_yap_list_of_exports'(File, Module, Exports0),
	'$lgt_yap_fix_predicate_aliases'(Exports0, Exports).

'$lgt_yap_directive_expansion'(reexport(File, Exports0), (:- reexport(Module, Exports))) :-
	'$lgt_yap_list_of_exports'(File, Module, OriginalExports),
	'$lgt_yap_filter_imports'(Exports0, OriginalExports, Exports1),
	'$lgt_yap_fix_predicate_aliases'(Exports1, Exports).

'$lgt_yap_directive_expansion'(yap_flag(Flag, Value), (:- set_prolog_flag(Flag, Value))).

'$lgt_yap_directive_expansion'(use_module(File, Imports0), (:- use_module(Module, Imports))) :-
	logtalk_load_context(entity_type, module),
	% we're compiling a module as an object; assume referenced modules are also compiled as objects
	!,
	'$lgt_yap_list_of_exports'(File, Module, Exports),
	'$lgt_yap_filter_imports'(Imports0, Exports, Imports1),
	'$lgt_yap_fix_predicate_aliases'(Imports1, Imports).

'$lgt_yap_directive_expansion'(use_module(File, Imports0), [{:- use_module(File, Imports0)}, (:- use_module(Module, Imports))]) :-
	logtalk_load_context(entity_type, _),
	% object or category using a Prolog module
	'$lgt_yap_list_of_exports'(File, Module, Exports),
	'$lgt_yap_filter_imports'(Imports0, Exports, Imports1),
	'$lgt_yap_fix_predicate_aliases'(Imports1, Imports),
	use_module(File, Imports0).

'$lgt_yap_directive_expansion'(use_module(File), (:- use_module(Module, Imports))) :-
	File \= [_| _],
	% not the Logtalk use_module/1 directive
	logtalk_load_context(entity_type, module),
	% we're compiling a module as an object; assume referenced modules are also compiled as objects
	!,
	'$lgt_yap_list_of_exports'(File, Module, Imports).

'$lgt_yap_directive_expansion'(use_module(File), [{:- use_module(File)}, (:- use_module(Module, Imports))]) :-
	File \= [_| _],
	% not the Logtalk use_module/1 directive
	logtalk_load_context(entity_type, _),
	% object or category using a Prolog module
	'$lgt_yap_list_of_exports'(File, Module, Imports),
	use_module(File).

'$lgt_yap_directive_expansion'(table(Predicates), {:- table(TPredicates)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_yap_table_directive_expansion'(Predicates, TPredicates).


'$lgt_yap_table_directive_expansion'([Predicate| Predicates], [TPredicate| TPredicates]) :-
	!,
	'$lgt_yap_table_directive_predicate'(Predicate, TPredicate),
	'$lgt_yap_table_directive_expansion'(Predicates, TPredicates).

'$lgt_yap_table_directive_expansion'((Predicate, Predicates), (TPredicate, TPredicates)) :-
	!,
	'$lgt_yap_table_directive_predicate'(Predicate, TPredicate),
	'$lgt_yap_table_directive_expansion'(Predicates, TPredicates).

'$lgt_yap_table_directive_expansion'(Predicate, TPredicate) :-
	'$lgt_yap_table_directive_predicate'(Predicate, TPredicate).


'$lgt_yap_table_directive_predicate'(F/A, TF/TA) :-
	!,
	'$lgt_compile_predicate_indicators'(F/A, _, TF/TA).

'$lgt_yap_table_directive_predicate'(F//A, TF/TA) :-
	!,
	A2 is A + 2,
	'$lgt_compile_predicate_indicators'(F/A2, _, TF/TA).

'$lgt_yap_table_directive_predicate'(Head, THead) :-
	'$lgt_compile_predicate_heads'(Head, _, THead, _).


'$lgt_yap_list_of_exports'(File, Module, Exports) :-
	(	logtalk_load_context(directory, Directory)
	;	logtalk_load_context(file, IncludeFile),
		file_directory_name(IncludeFile, Directory)
	),
	absolute_file_name(File, Path, [file_type(prolog), access(read), file_errors(fail), relative_to(Directory)]),
	(	module_property(Module, file(Path)),
		% only succeeds for loaded modules
		module_property(Module, exports(Exports)) ->
		true
	;	object_property(Module, file(Path)),
		object_property(Module, module),
		% module compiled as an object
		object_property(Module, public(Exports))
	),
	!.
'$lgt_yap_list_of_exports'(File, Module, Exports) :-
	(	absolute_file_name(File, Path, [file_type(prolog), access(read), file_errors(fail)]),
		file_property(Path, type(regular))
	;	% we may be compiling Prolog module files as Logtalk objects
		absolute_file_name(File, Path, [extensions(['.lgt','.logtalk']), access(read), file_errors(fail)])
	),
	open(Path, read, In),
	% deal with #! script; if not present assume that the
	% module declaration is the first directive on the file
	(	peek_char(In, #) ->
		skip(In, 10)
	;	true
	),
	setup_call_cleanup(true, '$lgt_yap_read_module_directive'(In, Module, Exports), close(In)),
	(	var(Module) ->
		file_base_name(Path, Base),
		file_name_extension(Module, _, Base)
	;	true
	).

'$lgt_yap_read_module_directive'(Stream, Module, Exports) :-
	% fragile hack as it ignores predicates exported via reexport/1-2 directives
	read(Stream, FirstTerm),
	(	FirstTerm  = (:- module(Module, Exports)) ->
		true
	;	FirstTerm = (:- encoding(_)) ->
		read(Stream, SecondTerm),
		SecondTerm = (:- module(Module, Exports))
	;	fail
	).


'$lgt_yap_filter_imports'([], _, []).
'$lgt_yap_filter_imports'([Import| Imports], _, [Import| Imports]).
'$lgt_yap_filter_imports'(except(Excluded), Exports, Imports) :-
	findall(
		Import,
		(	'$lgt_member'(Import, Exports),
			\+ '$lgt_member'(Import, Excluded)
		),
		Imports
	).


'$lgt_yap_split_predicate_aliases'([], [], []).
'$lgt_yap_split_predicate_aliases'([as(Functor/Arity, Alias)| Exports0], [Alias/Arity| Exports], [Clause| Clauses]) :-
	!,
	functor(Template, Functor, Arity),
	Template =.. [Functor| Arguments],
	AliasTemplate =.. [Alias| Arguments],
	Clause = (AliasTemplate :- Template),
	'$lgt_yap_split_predicate_aliases'(Exports0, Exports, Clauses).
'$lgt_yap_split_predicate_aliases'([Export| Exports0], [Export| Exports], Clauses) :-
	'$lgt_yap_split_predicate_aliases'(Exports0, Exports, Clauses).


'$lgt_yap_fix_predicate_aliases'([], []).
'$lgt_yap_fix_predicate_aliases'([Import0| Imports0], [Import| Imports]) :-
	'$lgt_yap_fix_predicate_aliases_aux'([Import0| Imports0], [Import| Imports]).
'$lgt_yap_fix_predicate_aliases'(except(Excluded), except(Excluded)).

'$lgt_yap_fix_predicate_aliases_aux'([], []).
'$lgt_yap_fix_predicate_aliases_aux'([as(Functor/Arity,Alias)| Imports0], [as(Functor/Arity,Alias/Arity)| Imports]) :-
	atom(Alias),
	!,
	'$lgt_yap_fix_predicate_aliases_aux'(Imports0, Imports).
'$lgt_yap_fix_predicate_aliases_aux'([Import| Imports0], [Import| Imports]) :-
	'$lgt_yap_fix_predicate_aliases_aux'(Imports0, Imports).


'$lgt_yap_encoding_to_logtalk_encoding'(ascii, 'US-ASCII').
'$lgt_yap_encoding_to_logtalk_encoding'(iso_latin_1, 'ISO-8859-1').
'$lgt_yap_encoding_to_logtalk_encoding'(utf8, 'UTF-8').
'$lgt_yap_encoding_to_logtalk_encoding'(utf16_be, 'UTF-16BE').
'$lgt_yap_encoding_to_logtalk_encoding'(utf16_le, 'UTF-16LE').
'$lgt_yap_encoding_to_logtalk_encoding'(utf32_be, 'UTF-32BE').
'$lgt_yap_encoding_to_logtalk_encoding'(utf32_le, 'UTF-32LE').


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  multi-threading predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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


% thread_sleep(+number) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('US-ASCII', ascii, _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-1', iso_latin_1, _).
'$lgt_logtalk_prolog_encoding'('UTF-8', utf8, _).				% BOM optional
'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream) :-	% BOM optional but strongly recommended
	(	stream_property(Stream, encoding(utf16_be)) ->
		Encoding = utf16_be
	;	stream_property(Stream, encoding(utf16_le)) ->
		Encoding = utf16_le
	).
'$lgt_logtalk_prolog_encoding'('UTF-16BE', utf16_be, _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-16LE', utf16_le, _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-32', Encoding, Stream) :-	% BOM optional but strongly recommended
	(	stream_property(Stream, encoding(utf32_be)) ->
		Encoding = utf32_be
	;	stream_property(Stream, encoding(utf32_le)) ->
		Encoding = utf32_le
	).
'$lgt_logtalk_prolog_encoding'('UTF-32BE', utf32_be, _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-32LE', utf32_le, _).		% BOM forbidden



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  lambda expressions support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term_nat(Term, Copy).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  goal_expansion/2 rules to allow calling the Prolog built-in predicates
%  phrase/2-3 with a Object::GRBody as the first argument and to optimize
%  ::/2 goals from within modules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic(user:goal_expansion/2).
:- multifile(user:goal_expansion/2).

% optimize portable format/2-3 calls
user:goal_expansion('$lgt_format'(Stream, Format, Arguments), format(Stream, Format, Arguments)).
user:goal_expansion('$lgt_format'(Format, Arguments), format(Format, Arguments)).

% support calls to phrase/2 that call object non-terminals
user:goal_expansion(phrase(Rule, Input, Rest), ExpandedGoal) :-
	nonvar(Rule),
	functor(Rule, '::', 2),
	!,
	'$lgt_execution_context'(ExCtx, user, user, user, user, [], []),
	'$lgt_user_module_qualification'('$lgt_phrase'(Rule, Input, Rest, ExCtx), ExpandedGoal).
% support calls to phrase/3 that call object non-terminals
user:goal_expansion(phrase(Rule, Input), ExpandedGoal) :-
	nonvar(Rule),
	functor(Rule, '::', 2),
	!,
	'$lgt_execution_context'(ExCtx, user, user, user, user, [], []),
	'$lgt_user_module_qualification'('$lgt_phrase'(Rule, Input, ExCtx), ExpandedGoal).
% optimize messages sent from modules (including "user")
user:goal_expansion('::'(Object, Message), ExpandedGoal) :-
	callable(Object),
	callable(Message),
	% find out in which module Logtalk was loaded (usually, "user")
	'$lgt_user_module_qualification'(xx, QualifiedGoal),
	QualifiedGoal = ':'(UserModule, xx),
	% this module plays the role of the Logtalk pseudo-object "user"
	(	prolog_load_context(term_position, Position),
		stream_position_data(line_count, Position, Line) ->
		% loading a file
		prolog_load_context(module, Module),
		Module \== UserModule,
		% loading a Prolog module file
		'$lgt_compiler_flag'(events, Events)
	;	% top-level goal
		Line = -1,
		% use default value of the "events" flag
		'$lgt_current_flag_'(events, Events)
	),
	'$lgt_comp_ctx'(Ctx, _, _, user, user, user, Obj, _, [], [], ExCtx, compile(aux,_,_), [], Line-Line, _),
	'$lgt_execution_context'(ExCtx, user, user, user, Obj, [], []),
	catch('$lgt_compile_message_to_object'(Message, Object, Goal, Events, Ctx), _, fail),
	'$lgt_user_module_qualification'(Goal, ExpandedGoal).



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


% term_hash(@callable, +integer, +integer, -integer) -- library



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  string built-in type
%
%  define these predicates to trivially fail if no string type is available
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_string'(@term)

'$lgt_string'(_) :-
	fail.


% '$lgt_string_codes'(+string, -list(codes))
% '$lgt_string_codes'(-string, +list(codes))

'$lgt_string_codes'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  module qualification to be used when calling Prolog meta-predicates
%  with meta-arguments that are calls to object or category predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_user_module_qualification'(@callable, -callable)

:- initialization((
	prolog_load_context(module, Module),
	assert_static('$lgt_user_module_qualification'(Goal, Module:Goal))
)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  auxiliary predicates for compiling modules as objects
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_find_module_predicate'(+atom, -atom, @callable)
%
% succeeds when Module:Predicate is visible in module Current

'$lgt_find_visible_module_predicate'(_Current, _Module, _Predicate) :-
	fail.



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


{X} :-
	var(X),
	throw(error(instantiation_error, logtalk({X}, _))).
{*} :-
	!,
	logtalk_make(all).
{!} :-
	!,
	logtalk_make(clean).
{?} :-
	!,
	logtalk_make(check).
{@} :-
	!,
	logtalk_make(circular).
{#} :-
	!,
	logtalk_make(documentation).
{+d} :-
	!,
	logtalk_make(debug).
{+n} :-
	!,
	logtalk_make(normal).
{+o} :-
	!,
	logtalk_make(optimal).
{$} :-
	!,
	logtalk_make(caches).

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

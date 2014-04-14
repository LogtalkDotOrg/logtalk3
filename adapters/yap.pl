%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  Adapter file for YAP Prolog 6.3.4 and later versions
%  Last updated on April 14, 2014
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


:- no_source.


:- initialization((
	use_module(library(system)),
	'$lgt_hide_predicates',
%	set_prolog_flag(language, iso),		% commented due to all the YAP libraries that don't compile in "iso" mode!
	set_prolog_flag(update_semantics, logical),
	set_prolog_flag(unknown, error),
	set_prolog_flag(syntax_errors, error))).


'$lgt_hide_predicates' :-
	(	predicate_property(hide_predicate(_), built_in) ->
		dynamic('$lgt_before_event_'/5), hide_predicate('$lgt_before_event_'/5),
		dynamic('$lgt_after_event_'/5), hide_predicate('$lgt_after_event_'/5),
		dynamic('$lgt_current_protocol_'/5), hide_predicate('$lgt_current_protocol_'/5),
		dynamic('$lgt_current_category_'/6), hide_predicate('$lgt_current_category_'/6),
		dynamic('$lgt_current_object_'/11), hide_predicate('$lgt_current_object_'/11),
		dynamic('$lgt_entity_property_'/2), hide_predicate('$lgt_entity_property_'/2),
		dynamic('$lgt_predicate_property_'/3), hide_predicate('$lgt_predicate_property_'/3),
		dynamic('$lgt_implements_protocol_'/3), hide_predicate('$lgt_implements_protocol_'/3),
		dynamic('$lgt_imports_category_'/3), hide_predicate('$lgt_imports_category_'/3),
		dynamic('$lgt_instantiates_class_'/3), hide_predicate('$lgt_instantiates_class_'/3),
		dynamic('$lgt_specializes_class_'/3), hide_predicate('$lgt_specializes_class_'/3),
		dynamic('$lgt_extends_protocol_'/3), hide_predicate('$lgt_extends_protocol_'/3),
		dynamic('$lgt_extends_object_'/3), hide_predicate('$lgt_extends_object_'/3),
		dynamic('$lgt_extends_category_'/3), hide_predicate('$lgt_extends_category_'/3),
		dynamic('$lgt_complemented_object_'/4), hide_predicate('$lgt_complemented_object_'/4),
		dynamic('$lgt_loaded_file_'/7), hide_predicate('$lgt_loaded_file_'/7),
		dynamic('$lgt_parent_file_'/2), hide_predicate('$lgt_parent_file_'/2),
		dynamic('$lgt_file_loading_stack_'/1), hide_predicate('$lgt_file_loading_stack_'/1),
		dynamic('$lgt_current_flag_'/2), hide_predicate('$lgt_current_flag_'/2),
		dynamic('$lgt_send_to_obj_static_binding_cache_'/4), hide_predicate('$lgt_send_to_obj_static_binding_cache_'/4),
		dynamic('$lgt_pp_warnings_top_goal_directory_'/2), hide_predicate('$lgt_pp_warnings_top_goal_directory_'/2),
		dynamic('$lgt_pp_compilation_warnings_counter_'/1), hide_predicate('$lgt_pp_compilation_warnings_counter_'/1),
		dynamic('$lgt_pp_loading_warnings_counter_'/1), hide_predicate('$lgt_pp_loading_warnings_counter_'/1),
		dynamic('$lgt_pp_aux_predicate_counter_'/1), hide_predicate('$lgt_pp_aux_predicate_counter_'/1),
		dynamic('$lgt_hook_term_expansion_'/2), hide_predicate('$lgt_hook_term_expansion_'/2),
		dynamic('$lgt_hook_goal_expansion_'/2), hide_predicate('$lgt_hook_goal_expansion_'/2),
		dynamic('$lgt_threaded_tag_counter_'/1), hide_predicate('$lgt_threaded_tag_counter_'/1),
		dynamic('$lgt_send_to_obj_'/3), hide_predicate('$lgt_send_to_obj_'/3),
		dynamic('$lgt_send_to_obj_ne_'/3), hide_predicate('$lgt_send_to_obj_ne_'/3),
		dynamic('$lgt_send_to_self_'/3), hide_predicate('$lgt_send_to_self_'/3),
		dynamic('$lgt_ctg_super_call_'/3), hide_predicate('$lgt_ctg_super_call_'/3),
		dynamic('$lgt_obj_super_call_'/3), hide_predicate('$lgt_obj_super_call_'/3),
		dynamic('$lgt_db_lookup_cache_'/5), hide_predicate('$lgt_db_lookup_cache_'/5),
		dynamic('$logtalk.message_tokens'/5), hide_predicate('$logtalk.message_tokens'/5),
		dynamic('$logtalk.message_prefix_stream'/5), hide_predicate('$logtalk.message_prefix_stream'/5),
		dynamic('$logtalk.print_message_token'/5), hide_predicate('$logtalk.print_message_token'/5),
		dynamic('$logtalk.trace_event'/3), hide_predicate('$logtalk.trace_event'/3)
	;	true
	).


% disable YAP discontiguous predicate clauses warning as
% the Logtalk compiler does its own detection and there's
% no point in printing the same warning twice
%
%:- multifile(message_hook/3).
%:- dynamic(message_hook/3).
%message_hook(clauses_not_together(_), _, _).



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
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer) -- built-in


% forall(+callable, +callable) -- built-in


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable) -- built-in



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
%  there should only a single extension defined for Prolog source
%  files but multiple extensions can be defined for Logtalk source
%  files and for back-end specific temporary files
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

'$lgt_prolog_feature'(prolog_dialect, yap).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	current_prolog_flag(version_data, yap(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((6,3,4))).

'$lgt_prolog_feature'(encoding_directive, full).
'$lgt_prolog_feature'(tabling, Tabling) :-
	(	current_prolog_flag(system_options, tabling) ->
		Tabling = supported
	;	Tabling = unsupported
	).
'$lgt_prolog_feature'(threads, Threads) :-
	(	current_prolog_flag(system_options, threads) ->
		Threads = supported
	;	Threads = unsupported
	).
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
'$lgt_default_flag'(underscore_variables, singletons).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(namespace, '').
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	(	current_prolog_flag(unix, true) ->
		ScratchDirectory = './.lgt_tmp/'
	;	ScratchDirectory = './lgt_tmp/'
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
'$lgt_default_flag'(prolog_loader, [silent(true)]).



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


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_compile_prolog_code'(_, _, _).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, Source, Options) :-
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


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory 

'$lgt_startup_directory'(Directory) :-
	(	environ('LOGTALK_STARTUP_DIRECTORY', Directory) ->
		true
	;	working_directory(Directory, Directory)
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
	file_directory_name(File, Directory0),
	atom_concat(Directory0, '/', Directory),
	file_base_name(File, Basename),
	file_name_extension(Name, Extension0, Basename),
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
	statistics(cputime, [Miliseconds, _]),
	Seconds is Miliseconds/1000.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	stream_position(Stream, Position),
	stream_position_data(line_count, Position, Line).



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
	read_term(Stream, Term, [term_position(PositionBegin), variable_names(Variables)| Options]),
	stream_position_data(line_count, PositionBegin, LineBegin),
	stream_position(Stream, PositionEnd),
	stream_position_data(line_count, PositionEnd, LineEnd).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'((:- Directive), Expanded) :-
	'$lgt_yap_directive_expansion'(Directive, Expanded0),
	(	Expanded0 == [] ->
		Expanded  == []
	;	Expanded0 =  {ExpandedDirective} ->
		Expanded  =  {(:- ExpandedDirective)}
	;	Expanded  =  (:- Expanded0)
	).

'$lgt_yap_directive_expansion'(style_check(_), []).

'$lgt_yap_directive_expansion'(create_prolog_flag(Key, Value, Options), {create_prolog_flag(Key, Value, Options)}).

'$lgt_yap_directive_expansion'(expects_dialect(Dialect), {expects_dialect(Dialect)}) :-
	'$expects_dialect'(Dialect).

'$lgt_yap_directive_expansion'(load_foreign_files(Files, Libs, InitRoutine), {initialization(load_foreign_files(Files, Libs, InitRoutine))}) :-
	load_foreign_files(Files, Libs, InitRoutine).

'$lgt_yap_directive_expansion'(public(PIs), {public(CPIs)}) :-	% used to make clause/2 work with static predicates
	logtalk_load_context(entity_type, module),					% only when we're compiling a module as an object!
	'$lgt_compile_predicate_indicators'(PIs, _, CPIs).

'$lgt_yap_directive_expansion'(table(F/A), {table(TF/TA)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(F/A, _, TF/TA).

'$lgt_yap_directive_expansion'(table([F/A| PIs]), {table(TPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'([F/A| PIs], _, TPIs).

'$lgt_yap_directive_expansion'(table(Head), {table(THead)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_heads'(Head, _, THead, _),
	functor(THead, _, Arity),
	arg(Arity, THead, first).

'$lgt_yap_directive_expansion'(encoding(Encoding1), encoding(Encoding2)) :-
	nonvar(Encoding1),
	'$lgt_yap_encoding_to_logtalk_encoding'(Encoding1, Encoding2).

'$lgt_yap_directive_expansion'(ensure_loaded(File), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, module),
	% ensure_loaded/1 directive used within a module (sloppy replacement for the use_module/1-2 directives)
	'$lgt_yap_list_of_exports'(File, Module, Imports).

'$lgt_yap_directive_expansion'(op(Priority, Specifier, ':'(Module,Operators)), {op(Priority, Specifier, Operators)}) :-
	Module == user.

'$lgt_yap_directive_expansion'(reexport(File), reexport(Module, Exports)) :-
	'$lgt_yap_list_of_exports'(File, Module, Exports).

'$lgt_yap_directive_expansion'(reexport(File, Exports), reexport(Module, Exports)) :-
	'$lgt_yap_list_of_exports'(File, Module, _).

'$lgt_yap_directive_expansion'(yap_flag(Flag, Value), set_prolog_flag(Flag, Value)).

'$lgt_yap_directive_expansion'(use_module(File, Imports), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, _),
	'$lgt_yap_list_of_exports'(File, Module, _).

'$lgt_yap_directive_expansion'(use_module(File), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, _),
	'$lgt_yap_list_of_exports'(File, Module, Imports).


'$lgt_yap_list_of_exports'(File, Module, Exports) :-
	(	absolute_file_name(File, Path, [file_type(prolog), access(read), file_errors(fail)]),
		file_property(Path, type(regular))
	;	% we may be compiling Prolog module files as Logtalk objects
		absolute_file_name(File, Path, [extensions(['.lgt']), access(read), file_errors(fail)])
	),
	open(Path, read, In),
	(	peek_char(In, #) ->		% deal with #! script; if not present
		skip(In, 10)			% assume that the module declaration
	;	true					% is the first directive on the file
	),
	setup_call_cleanup(true, read(In, ModuleDecl), close(In)),
	ModuleDecl = (:- module(Module, Exports)).


'$lgt_yap_encoding_to_logtalk_encoding'(ascii, 'US-ASCII').
'$lgt_yap_encoding_to_logtalk_encoding'(iso_latin_1, 'ISO-8859-1').
'$lgt_yap_encoding_to_logtalk_encoding'(utf8, 'UTF-8').
'$lgt_yap_encoding_to_logtalk_encoding'(unicode_be, 'UCS-2BE').
'$lgt_yap_encoding_to_logtalk_encoding'(unicode_le, 'UCS-2LE').


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
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


% thread_sleep(+number) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('US-ASCII', ascii, _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-1', iso_latin_1, _).
'$lgt_logtalk_prolog_encoding'('UTF-8', utf8, _).
'$lgt_logtalk_prolog_encoding'('UCS-2', Encoding, Stream) :-	% BOM mandatory
	(	stream_property(Stream, encoding(unicode_be)) ->
		Encoding = unicode_be
	;	stream_property(Stream, encoding(unicode_le)) ->
		Encoding = unicode_le
	).
'$lgt_logtalk_prolog_encoding'('UCS-2BE', unicode_be, _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UCS-2LE', unicode_le, _).
'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream) :-	% BOM optional but strongly recommended
	(	stream_property(Stream, encoding(unicode_be)) ->		% not true of course but usually we can get away with it
		Encoding = unicode_be
	;	stream_property(Stream, encoding(unicode_le)) ->
		Encoding = unicode_le
	).
'$lgt_logtalk_prolog_encoding'('UTF-16BE', unicode_be, _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-16LE', unicode_le, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  experimental lambda support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term_nat(Term, Copy).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  goal_expansion/2 rules to allow calling the Prolog built-in predicates
%  phrase/2-3 with a Object::GRBody as the first argument and to optimize
%  ::/2 goals from within modules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic(user:goal_expansion/2).
:- multifile(user:goal_expansion/2).

user:goal_expansion(phrase(Rule, Input, Rest), user:'$lgt_phrase'(Rule, Input, Rest, ExCtx)) :-
	nonvar(Rule),
	functor(Rule, '::', 2),
	!,
	'$logtalk.execution_context'(ExCtx, user, user, user, [], [], _).
user:goal_expansion(phrase(Rule, Input), user:'$lgt_phrase'(Rule, Input, ExCtx)) :-
	nonvar(Rule),
	functor(Rule, '::', 2),
	!,
	'$logtalk.execution_context'(ExCtx, user, user, user, [], [], _).

user:goal_expansion('::'(Object, Message), user:Goal) :-
	prolog_load_context(module, Module),
	Module \== user,
	'$lgt_compiler_flag'(events, Events),
	catch('$lgt_tr_msg'(Message, Object, Goal, user, _, Events), _, fail). 



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

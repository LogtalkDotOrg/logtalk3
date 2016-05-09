%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for Lean Prolog 4.5.7 and later versions
%  Last updated on May 9, 2016
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



:- set_quickfail(0, _).



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
%
% table of missing ISO predicates which are defined in this file

'$lgt_iso_predicate'(acyclic_term(_)).
'$lgt_iso_predicate'(close(_, _)).
'$lgt_iso_predicate'(get_byte(_, _)).
'$lgt_iso_predicate'(get_byte(_)).
'$lgt_iso_predicate'(put_byte(_, _)).
'$lgt_iso_predicate'(put_byte(_)).

acyclic_term(_).

close(Stream, _) :-
	catch(close(Stream), _, true).

get_byte(Stream, Byte) :-
	get_code(Stream, Byte).

get_byte(Byte) :-
	get_code(Byte).

put_byte(Stream, Byte) :-
	put_code(Stream, Byte).

put_byte(Byte) :-
	put_code(Byte).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer) -- built-in


% findall(?term, +callable, ?list, +list) -- built-in


% forall(+callable, +callable) -- built-in


% format(+stream_or_alias, +character_code_list_or_atom, +list)

format(Stream, Format, Arguments) :-
	atom_chars(Format, Chars),
	format_(Chars, Stream, Arguments).

format_([], _, []).
format_(['~', Spec| Chars], Stream, Arguments) :-
	!,
	format_spec_(Spec, Stream, Arguments, RemainingArguments),
	format_(Chars, Stream, RemainingArguments).
format_([Char| Chars], Stream, Arguments) :-
	char_code(Char, Code),
	put_code(Stream, Code),
	format_(Chars, Stream, Arguments).

format_spec_('a', Stream, [Argument| Arguments], Arguments) :-
%	atom(Argument),
	write(Stream, Argument).
format_spec_('c', Stream, [Argument| Arguments], Arguments) :-
	put_code(Stream, Argument).
format_spec_('s', Stream, [Argument| Arguments], Arguments) :-
	atom_codes(Atom, Argument),
	write(Stream, Atom).
format_spec_('w', Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_('q', Stream, [Argument| Arguments], Arguments) :-
	writeq(Stream, Argument).
format_spec_('k', Stream, [Argument| Arguments], Arguments) :-
	write_canonical(Stream, Argument).
format_spec_('d', Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_('D', Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_('f', Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_('g', Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_('G', Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_('i', _, [_| Arguments], Arguments).
format_spec_('n', Stream, Arguments, Arguments) :-
	nl(Stream).
format_spec_('~', Stream, Arguments, Arguments) :-
	char_code('~', Code),
	put_code(Stream, Code).


% format(+character_code_list_or_atom, +list)

format(Format, Arguments) :-
	current_output(Stream),
	format(Stream, Format, Arguments).


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

'$lgt_predicate_property'(Pred, static) :-
	predicate_property(Pred, compiled).
'$lgt_predicate_property'(Pred, ('dynamic')) :-
	predicate_property(Pred, interpreted).
'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable) -- not supported

setup_call_cleanup(_, _, _) :-
	throw(not_supported(setup_call_cleanup/3)).



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

'$lgt_prolog_meta_predicate'('?'(_), '?'(0), predicate).
'$lgt_prolog_meta_predicate'(':'(_, _), ':'(*, 0), predicate).
'$lgt_prolog_meta_predicate'('@'(_, _), '@'(*, 0), predicate).
'$lgt_prolog_meta_predicate'(add_task(_), add_task(0), predicate).
'$lgt_prolog_meta_predicate'(add_task(_, _), add_task(*, 0), predicate).
'$lgt_prolog_meta_predicate'(add_task(_, _, _), add_task(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(all(_, _), all(0, *), predicate).
'$lgt_prolog_meta_predicate'(all(_, _, _, _), all(*, *,0, *), predicate).
'$lgt_prolog_meta_predicate'(alt_findall(_, _, _), alt_findall(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(ask_agent(_, _), ask_agent(*, 0), predicate).
'$lgt_prolog_meta_predicate'(ask_broker(_), ask_broker(0), predicate).
'$lgt_prolog_meta_predicate'(ask_engine(_, _, _), ask_engine(* ,0, *), predicate).
'$lgt_prolog_meta_predicate'(ask_interactor(_, _), ask_interactor(*, 0), predicate).
'$lgt_prolog_meta_predicate'(ask_linda(_, _,_), ask_linda(*, *, 0), predicate).
'$lgt_prolog_meta_predicate'(ask_peer(_, _, _, _, _), ask_peer(*, *, *, 0, *), predicate).
'$lgt_prolog_meta_predicate'(bg(_, _), bg(0, *), predicate).
'$lgt_prolog_meta_predicate'(bg_clone(_, _), bg_clone(0, *), predicate).
'$lgt_prolog_meta_predicate'(bg_with(_, _, _), bg_with(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(call_ifdef(_, _), call_ifdef(0, 0), predicate).
'$lgt_prolog_meta_predicate'(call_with_time_limit(_, _), call_with_time_limit(*, 0), predicate).
'$lgt_prolog_meta_predicate'(ccall(_, _, _, _, _), ccall(*, *, *, 0, *), predicate).
'$lgt_prolog_meta_predicate'(det_call(_), det_call(0), predicate).
'$lgt_prolog_meta_predicate'(drop_at_least(_, _), drop_at_least(*, 0), predicate).
'$lgt_prolog_meta_predicate'(find_at_most(_, _, _, _), find_at_most(*, *, 0, *), predicate).
'$lgt_prolog_meta_predicate'(find_while(_, _, _, _), find_while(*, *, 0, *), predicate).
'$lgt_prolog_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_prolog_meta_predicate'(foreach(_, _), foreach(0, 0), predicate).
'$lgt_prolog_meta_predicate'(init_engine(_, _, _, _), init_engine(*, *, 0, *), predicate).
'$lgt_prolog_meta_predicate'(linda_client(_, _, _,_ , _), linda_client(*, *, *, *, 0), predicate).
'$lgt_prolog_meta_predicate'(mbg(_), mbg([0]), predicate).
'$lgt_prolog_meta_predicate'(new_engine(_, _, _), new_engine(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(nth_answer(_, _), nth_answer(*, 0), predicate).
'$lgt_prolog_meta_predicate'(rli_call(_, _, _, _, _), rli_call(*, *, _, 0, *), predicate).
'$lgt_prolog_meta_predicate'(take_at_most(_, _), take_at_most(*, 0), predicate).
'$lgt_prolog_meta_predicate'(tell_agent_at(_, _), tell_agent_at(*, 0), predicate).
'$lgt_prolog_meta_predicate'(time(_), time(0), predicate).
'$lgt_prolog_meta_predicate'(time(_, _), time(0, *), predicate).
'$lgt_prolog_meta_predicate'(to_engine(_, _, _), to_engine(*, *, 0), predicate).
'$lgt_prolog_meta_predicate'(topcall(_), topcall(0), predicate).
'$lgt_prolog_meta_predicate'(while(_, _), while(0, 0), predicate).

to_engine(Interactor, Pattern, Goal) :-
	to_engine(Interactor, (Pattern:-Goal)).


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(_, _) :-
	fail.


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


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
'$lgt_file_extension'(prolog, '.prolog').
'$lgt_file_extension'(prolog, '.pro').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog compiler features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, lean).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	current_prolog_flag(version_data, lprolog(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, '@>='((4, 5, 7))).
'$lgt_prolog_feature'(prolog_conformance, lax).

'$lgt_prolog_feature'(encoding_directive, source).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, unsupported).
'$lgt_prolog_feature'(coinduction, unsupported).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
%
%  if your Prolog compiler supports the ISO definition of the 
%  initialization/1 then change the default value below to true
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
'$lgt_default_flag'(prolog_loader, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  operating-system access predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_os_file_name'(+atom, -atom)
% '$lgt_prolog_os_file_name'(-atom, +atom)
%
% converts between Prolog internal file paths and operating-system paths

'$lgt_prolog_os_file_name'(Path, Path).


% '$lgt_expand_path'(+nonvar, -atom)
%
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	(	\+ atom_concat('/', _, Path),
		\+ atom_concat('$', _, Path),
		working_directory(Current, Current),
		atom_concat(Current, '/', ExpandedPath0),
		atom_concat(ExpandedPath0, Path, ExpandedPath1),
		absolute_file_name(ExpandedPath1, ExpandedPath)
	;	absolute_file_name(Path, ExpandedPath)
	).


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

'$lgt_file_exists'(File) :-
	exists_file(File).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	exists_dir(Directory).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Directory0, Directory0),
	atom_concat(Directory0, '/', Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	absolute_file_name(Directory, Path),
	working_directory(_, Path).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	exists_dir(Directory) ->
		true
	;	make_directory(Directory)
	).


% '$lgt_directory_hash_as_atom'(+atom, -atom)
%
% returns the directory hash as an atom

'$lgt_directory_hash_as_atom'(_, '').


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_compile_prolog_code'(File, _, Options) :-
	(	'$lgt_member'(wam, Options) ->
		'$lgt_file_extension'(object, Extension),
		atom_concat(Source, Extension, File),
		atomic_list_concat(['fcompile(\'', Source, '\')'], Arg),
		atom_concat(Source, '.wam', WamFile),
		bg(system([lprolog, Arg, halt], _)),
		repeat,
		exists_file(WamFile),
		!
	;	true
	).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_load_prolog_code'(File, _, _) :-
	consult(File).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	new_java_object('java.io.File'(File), Object),
	invoke_java_method(Object, lastModified, Time).


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
	;	working_directory(Directory, Directory)
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


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory must always end
% with a slash; the extension must start with a "." when defined and must
% be the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	abs2path_file(File, Directory0, Basename),
	(	Directory0 = '' ->
		Directory = './'
	;	sub_atom(Directory0, _, _, 0, '/') ->
		Directory = Directory0
	;	atom_concat(Directory0, '/', Directory)
	),
	(	sub_atom(Basename, Before, _, After, '.') ->
		sub_atom(Basename, 0, Before, _, Name),
		sub_atom(Basename, _, After, 0, Extension0),
		atom_concat('.', Extension0, Extension)
	;	Name = Basename,
		Extension = ''
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(_, -1).



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
	remove_alias(Stream),
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
	'$lgt_read_term_options'(Options),
	read_term(Stream, yes, Term, Variables, LineBegin0, LineEnd),
	LineBegin is LineBegin0 + 1.


% hack for the only two cases of list of options used by the compiler
'$lgt_read_term_options'([]).
'$lgt_read_term_options'([singletons([])]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'((:- index(Head)), {:- index(THead)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_heads'(Head, _, THead, 0).


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  multi-threading predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% thread_property(+atom, ?nonvar)

thread_property(_, _) :-
	fail.


% thread_self(?atom)

thread_self(_) :-
	fail.


% thread_create(@callable, -thread_id, +list)

thread_create(_, _, _) :-
	fail.


% thread_join(+atom, -nonvar)

thread_join(_, _) :-
	fail.


% thread_detach(+atom)

thread_detach(_) :-
	fail.


% thread_exit(@term)

thread_exit(_) :-
	fail.


% thread_send_message(+atom, @callable)

thread_send_message(_, _) :-
	fail.


% thread_peek_message(+atom, ?callable)

thread_peek_message(_, _) :-
	fail.


% thread_get_message(+atom, ?callable)

thread_get_message(_, _) :-
	fail.


% thread_get_message(?callable)

thread_get_message(_) :-
	fail.


% thread_sleep(+number)

thread_sleep(_) :-
	fail.


mutex_create(_, _) :-
	fail.


with_mutex(_, _) :-
	fail.


mutex_lock(_) :-
	fail.


mutex_unlock(_) :-
	fail.


mutex_property(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('UTF-8', 'UTF-8', _).
'$lgt_logtalk_prolog_encoding'('US-ASCII', 'US-ASCII', _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  lambda expressions support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term(Term, Copy).



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
	write(Stream, '.'), nl(Stream).


% '$lgt_assertz_entity_clause'(@clause, +atom)

'$lgt_assertz_entity_clause'(Clause, _Kind) :-
	assertz(Clause).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  dummy definitions just to avoid warnings at startup when using qcompile/1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


abort :- throw(abort).

break :- fail.
current_module(_) :- fail.
ensure_loaded(_) :- fail.
use_module(_) :- fail.
use_module(_, _) :- fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  index/1 directives that really don't belong on this file but we want to
%  keep the Logtalk main compiler/runtime file portable
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- index('$lgt_current_protocol_'(1, 0, 0, 0, 0)).
:- index('$lgt_current_category_'(1, 0, 0, 0, 0, 0)).
:- index('$lgt_current_object_'(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)).

:- index('$lgt_entity_property_'(1, 0)).
:- index('$lgt_predicate_property_'(1, 1, 0)).

:- index('$lgt_implements_protocol_'(1, 1, 0)).
:- index('$lgt_imports_category_'(1, 1, 0)).
:- index('$lgt_instantiates_class_'(1, 1, 0)).
:- index('$lgt_specializes_class_'(1, 1, 0)).
:- index('$lgt_extends_category_'(1, 1, 0)).
:- index('$lgt_extends_object_'(1, 1, 0)).
:- index('$lgt_extends_protocol_'(1, 1, 0)).
:- index('$lgt_complemented_object_'(1, 1, 0, 0, 0)).

:- index('$lgt_current_flag_'(1, 0)).

:- index('$lgt_loaded_file_'(1, 1, 0, 0, 0, 0, 0)).



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


:- multifile('$logtalk#0.print_message_token#4'/5).
:- dynamic('$logtalk#0.print_message_token#4'/5).

% nasty workaround for the lack of support for stream aliases in Lean Prolog
'$logtalk#0.print_message_token#4'(user_output, Prefix, Token, Tokens, _) :-
	!,
	current_output(Stream),
	'$lgt_lean_print_message_token'(Token, Tokens, Prefix, Stream).
'$logtalk#0.print_message_token#4'(user_error, Prefix, Token, Tokens, _) :-
	!,
	current_output(Stream),
	'$lgt_lean_print_message_token'(Token, Tokens, Prefix, Stream).
'$logtalk#0.print_message_token#4'(Alias, Prefix, Token, Tokens, _) :-
	atom(Alias),
	get_alias(Stream, Alias),
	'$lgt_lean_print_message_token'(Token, Tokens, Prefix, Stream).


'$lgt_lean_print_message_token'((-), _, _, _).
'$lgt_lean_print_message_token'(at_same_line, _, _, _).
'$lgt_lean_print_message_token'(nl, Tokens, Prefix, Stream) :-
	(	Tokens == [] ->
		nl(Stream)
	;	Tokens = [end(_)] ->
		nl(Stream)
	;	nl(Stream),
		write(Stream, Prefix)
	).
'$lgt_lean_print_message_token'(flush, _, _, Stream) :-
	flush_output(Stream).
'$lgt_lean_print_message_token'(Format-Arguments, _, _, Stream) :-
	format(Stream, Format, Arguments).
'$lgt_lean_print_message_token'(ansi(_, Format, Arguments), _, _, Stream) :-
	format(Stream, Format, Arguments).
'$lgt_lean_print_message_token'(begin(_, _), _, _, _).
'$lgt_lean_print_message_token'(end(_), _, _, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  term hashing (not currently used in the compiler/runtime)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% term_hash(@callable, +integer, +integer, -integer)

term_hash(_, _, _, _) :-
	fail.



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
{(?)} :-
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

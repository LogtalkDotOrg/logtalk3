%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for GNU Prolog 1.4.5 (and later versions)
%  Last updated on February 13, 2022
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- built_in.
:- set_prolog_flag(strict_iso, off).



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
% format(+character_code_list_or_atom, +list) -- built-in

'$lgt_format'(Stream, Format, Arguments) :-
	format(Stream, Format, Arguments).

'$lgt_format'(Format, Arguments) :-
	format(Format, Arguments).


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

:- dynamic('$lgt_gnu_foreign_'/1).

'$lgt_predicate_property'(Pred, foreign) :-
	\+ predicate_property(Pred, native_code),
	'$lgt_gnu_foreign_'(Pred).
'$lgt_predicate_property'(Pred, foreign) :-
	predicate_property(Pred, native_code),
	\+ predicate_property(Pred, built_in).
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

'$lgt_prolog_meta_predicate'(consult(_), consult(*), predicate) :- !.
'$lgt_prolog_meta_predicate'(Callable, Template, Kind) :-
	predicate_property(Callable, meta_predicate(Template)),
	predicate_property(Callable, built_in),
	(	predicate_property(Callable, control_construct) ->
		Kind = control_construct
	;	Kind = predicate
	),
	!.


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(built_in(_), built_in(/)).
'$lgt_prolog_meta_directive'(built_in_fd(_), built_in_fd(/)).
'$lgt_prolog_meta_directive'(ensure_linked(_), ensure_linked(/)).


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


% '$lgt_candidate_tautology_or_falsehood_goal_hook'(@callable)
%
% valid candidates are proprietary built-in predicates with
% no side-effects when called with ground arguments

'$lgt_candidate_tautology_or_falsehood_goal_hook'(is_list(_)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(list(_)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(list_or_partial_list(_)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(name(_, _)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(partial_list(_)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(succ(_, _)).


% '$lgt_prolog_database_predicate'(@callable)
%
% table of non-standard database built-in predicates

'$lgt_prolog_database_predicate'(listing(_)).


% '$lgt_prolog_predicate_property'(?callable)
%
% table of proprietary predicate properties; used by the
% compiler when checking if a predicate property is valid

'$lgt_prolog_predicate_property'(built_in_fd).
'$lgt_prolog_predicate_property'(control_construct).
'$lgt_prolog_predicate_property'(monofile).
'$lgt_prolog_predicate_property'(native_code).
'$lgt_prolog_predicate_property'(prolog_file(_)).
'$lgt_prolog_predicate_property'(prolog_line_).



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
'$lgt_file_extension'(object, '.pl').
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

'$lgt_prolog_feature'(prolog_dialect, gnu).
'$lgt_prolog_feature'(prolog_version, v(Major, Minor, Patch)) :-
	catch(current_prolog_flag(version_data, gprolog(Major, Minor, Patch, _)), _, fail).
'$lgt_prolog_feature'(prolog_compatible_version, @>=(v(1,4,5))).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(engines, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
:- if(predicate_property(current_module(_), built_in)).
	'$lgt_prolog_feature'(modules, supported).
:- else.
	'$lgt_prolog_feature'(modules, unsupported).
:- endif.
'$lgt_prolog_feature'(coinduction, unsupported).
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
'$lgt_default_flag'(singleton_variables, warning).
'$lgt_default_flag'(steadfastness, silent).
'$lgt_default_flag'(naming, silent).
'$lgt_default_flag'(duplicated_clauses, silent).
'$lgt_default_flag'(tail_recursive, silent).
'$lgt_default_flag'(disjunctions, warning).
'$lgt_default_flag'(catchall_catch, silent).
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
'$lgt_default_flag'(underscore_variables, dont_care).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	os_version(Version),
	(	atom_concat('Microsoft Windows', _, Version) ->
		ScratchDirectory = './lgt_tmp/'
	;	ScratchDirectory = './.lgt_tmp/'
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


% '$lgt_expand_path'(+atom, -atom)
%
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	absolute_file_name(Path, ExpandedPath).


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

'$lgt_file_exists'(File) :-
	file_exists(File),
	file_property(File, type(regular)).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	file_exists(Directory),
	file_property(Directory, type(directory)).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	change_directory(Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	file_exists(Directory),
		file_property(Directory, type(directory)) ->
		true
	;	make_directory(Directory)
	).


% '$lgt_directory_hash_dialect_as_atom'(+atom, -atom)
%
% returns the directory hash and dialect as an atom with the format _hash_dialect

'$lgt_directory_hash_dialect_as_atom'(Directory, Hash) :-
	term_hash(Directory, Hash0),
	'$lgt_prolog_feature'(prolog_dialect, Dialect),
	number_codes(Hash0, Hash0Codes),
	atom_codes(Dialect, DialectCodes),
	append([0'_| Hash0Codes], [0'_| DialectCodes], HashCodes),
	atom_codes(Hash, HashCodes).


% '$lgt_directory_hash_pid_as_atom'(+atom, -atom)
%
% returns the directory hash and PID as an atom with the format _hash_pid

'$lgt_directory_hash_pid_as_atom'(Directory, Hash) :-
	term_hash(Directory, Hash0),
	prolog_pid(PID),
	number_codes(Hash0, Hash0Codes),
	number_codes(PID, PIDCodes),
	append([0'_| Hash0Codes], [0'_| PIDCodes], HashCodes),
	atom_codes(Hash, HashCodes).


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_compile_prolog_code'(_, _, _).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_load_prolog_code'(File, _, _) :-
	consult(File).


% '$lgt_load_prolog_file'(+atom)
%
% compile and (re)load a Prolog file (used in standards compliance tests)

'$lgt_load_prolog_file'(File) :-
	consult(File).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	file_property(File, last_modification(Time)).


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
	decompose_file_name(File, Directory0, Name, Extension),
	(	Directory0 = '' ->
		Directory = './'
	;	sub_atom(Directory0, _, _, 0, '/') ->
		Directory = Directory0
	;	atom_concat(Directory0, '/', Directory)
	).


% '$lgt_directory_files'(+atom, -list(atom))
%
% returns a list of files in the given directory

'$lgt_directory_files'(Directory, Files) :-
	directory_files(Directory, Files).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	line_count(Stream, Last),
	Line is Last + 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  abstraction of the standard open/4 and close/1 predicates for dealing
%  with required proprietary actions when opening and closing streams
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


% '$lgt_read_term'(@stream, -term, +list, -pair(integer,integer))

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd) :-
	read_term(Stream, Term, [syntax_error(error)| Options]),
	last_read_start_line_column(LineBegin, _),
	stream_line_column(Stream, Line, Col),
	(	Col = 1 ->
		LineEnd is Line - 1
	;	LineEnd = Line
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'(begin_of_file, begin_of_file) :-
	retractall('$lgt_gnu_foreign_'(_)).

'$lgt_prolog_term_expansion'((:- Directive), Expanded) :-
	nonvar(Directive),
	% allow first-argument indexing
	'$lgt_gnu_directive_expansion'(Directive, Expanded).

'$lgt_gnu_directive_expansion'(built_in, {:- built_in}) :-
	\+ logtalk_load_context(entity_type, _).
'$lgt_gnu_directive_expansion'(built_in_fd, {:- built_in_fd}).
'$lgt_gnu_directive_expansion'(foreign(Template, Options), {:- foreign(Template, Options)}) :-
	functor(Template, Functor, Arity),
	functor(TTemplate, Functor, Arity),
	assertz('$lgt_gnu_foreign_'(TTemplate)).
'$lgt_gnu_directive_expansion'(foreign(Template), {:- foreign(Template)}) :-
	functor(Template, Functor, Arity),
	functor(TTemplate, Functor, Arity),
	assertz('$lgt_gnu_foreign_'(TTemplate)).
'$lgt_gnu_directive_expansion'(op(Priority, Specifier, ':'(Module,Operators)), {:- op(Priority, Specifier, Operators)}) :-
	Module == user.


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
	fail.



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
	write(Stream, '.\n').


% '$lgt_assertz_entity_clause'(@clause, +atom)

'$lgt_assertz_entity_clause'(Clause, _Kind) :-
	assertz(Clause).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  dummy definitions just to avoid errors when using gplc to generate
%  executables
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(\+ predicate_property(current_module(_), built_in)).
	current_module(_) :- fail.
:- endif.

:- if(\+ predicate_property(ensure_loaded(_), built_in)).
	ensure_loaded(_) :- fail.
:- endif.

:- if(\+ predicate_property(use_module(_), built_in)).
	use_module(_) :- fail.
:- endif.

:- if(\+ predicate_property(use_module(_, _), built_in)).
	use_module(_, _) :- fail.
:- endif.

asserta(_, _) :- fail.
assertz(_, _) :- fail.
clause(_, _, _) :- fail.
erase(_) :- fail.

mutex_create(_, _) :- fail.
mutex_lock(_) :- fail.
mutex_property(_, _) :- fail.
mutex_unlock(_) :- fail.
thread_create(_, _, _) :- fail.
thread_get_message(_) :- fail.
thread_get_message(_, _) :- fail.
thread_peek_message(_, _) :- fail.
thread_property(_, _) :- fail.
thread_self(_) :- fail.
thread_send_message(_, _) :- fail.
thread_join(_, _) :- fail.
with_mutex(_, _) :- fail.
message_queue_create(_) :- fail.
message_queue_destroy(_) :- fail.



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

% workaround non-standard format/3 predicate feature that uses "%" as a
% format control sequence

'$logtalk#0.print_message_token#4'(Stream, _, Format-Args, _, _) :-
	atom_codes(Format, FormatCodes),
	'$lgt_gnu_filter_format_codes'(FormatCodes, FilteredFormatCodes),
	format(Stream, FilteredFormatCodes, Args).

'$lgt_gnu_filter_format_codes'([], []).
'$lgt_gnu_filter_format_codes'([0'%| Codes], [0'%, 0'%| FilteredCodes]) :-
	!,
	'$lgt_gnu_filter_format_codes'(Codes, FilteredCodes).
'$lgt_gnu_filter_format_codes'([Code| Codes], [Code| FilteredCodes]) :-
	!,
	'$lgt_gnu_filter_format_codes'(Codes, FilteredCodes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  term hashing (not currently used in the compiler/runtime)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% term_hash(@callable, +integer, +integer, -integer) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  atomics concat (not currently used in the compiler/runtime)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% atomic_concat(+atomic, +atomic, ?atom)

:- if(\+ predicate_property(atomic_concat(_, _, _), built_in)).

	atomic_concat(Atomic1, Atomic2, Atom) :-
		(	var(Atomic1) ->
			throw(error(instantiation_error, atomic_concat/3))
		;	var(Atomic2) ->
			throw(error(instantiation_error, atomic_concat/3))
		;	\+ atomic(Atomic1) ->
			throw(error(type_error(atomic, Atomic1), atomic_concat/3))
		;	\+ atomic(Atomic2) ->
			throw(error(type_error(atomic, Atomic2), atomic_concat/3))
		;	'$lgt_gnu_atomic_atom'(Atomic1, Atom1),
			'$lgt_gnu_atomic_atom'(Atomic2, Atom2),
			atom_concat(Atom1, Atom2, Atom)
		).

	'$lgt_gnu_atomic_atom'(Atomic, Atom) :-
		(	atom(Atomic) ->
			Atom = Atomic
		;	number_atom(Atomic, Atom)
		).

:- endif.


% atomic_list_concat(@list(atomic), ?atom)

:- if(\+ predicate_property(atomic_list_concat(_, _), built_in)).

	atomic_list_concat([Atomic| Atomics], Atom) :-
		!,
		(	var(Atomic) ->
			throw(error(instantiation_error, atomic_list_concat/2))
		;	\+ atomic(Atomic) ->
			throw(error(type_error(atomic, Atomic), atomic_list_concat/2))
		;	'$lgt_gnu_atomic_atom'(Atomic, Atom0),
			'$lgt_gnu_atomic_list_concat'(Atomics, Atom0, Atom)
		).
	atomic_list_concat([], '').

	'$lgt_gnu_atomic_list_concat'([Next| Atomics], Atom0, Atom) :-
		!,
		atomic_list_concat([Next| Atomics], Atom1),
		atom_concat(Atom0, Atom1, Atom).
	'$lgt_gnu_atomic_list_concat'([], Atom, Atom).

:- endif.


% atomic_list_concat(@list(atomic), +atom, ?atom)

:- if(\+ predicate_property(atomic_list_concat(_, _, _), built_in)).

	atomic_list_concat([Atomic| Atomics], Separator, Atom) :-
		!,
		(	var(Atomic) ->
			throw(error(instantiation_error, atomic_list_concat/3))
		;	var(Separator) ->
			throw(error(instantiation_error, atomic_list_concat/3))
		;	\+ atomic(Atomic) ->
			throw(error(type_error(atomic, Atomic), atomic_list_concat/3))
		;	\+ atomic(Separator) ->
			throw(error(type_error(atomic, Separator), atomic_list_concat/3))
		;	'$lgt_gnu_atomic_atom'(Atomic, Atom0),
			'$lgt_gnu_atomic_atom'(Separator, SeparatorAtom),
			'$lgt_gnu_atomic_list_concat'(Atomics, Atom0, SeparatorAtom, Atom)
		).
	atomic_list_concat([], _, '').

	'$lgt_gnu_atomic_list_concat'([Next| Atomics], Atom0, SeparatorAtom, Atom) :-
		!,
		atomic_list_concat([Next| Atomics], SeparatorAtom, Atom2),
		atom_concat(SeparatorAtom, Atom2, Atom1),
		atom_concat(Atom0, Atom1, Atom).
	'$lgt_gnu_atomic_list_concat'([], Atom, _, Atom).

:- endif.



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

:- if(predicate_property(current_module(_), built_in)).
	'$lgt_user_module_qualification'(Goal, ':'(user,Goal)).
:- else.
	'$lgt_user_module_qualification'(Goal, Goal).
:- endif.



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

{Files} :-
	'$lgt_conjunction_to_list'(Files, List),
	logtalk_load(List).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

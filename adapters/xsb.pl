%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for XSB 3.8.0 or later versions
%  Last updated on October 31, 2023
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- import expand_atom/2 from standard.
:- import term_hash/3 from machine.
:- import sys_pid/1 from shell.
:- import list_directory/2 from shell.
:- import concat_atom/2 from string.



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


% '$lgt_iso_predicate'(?callable)

'$lgt_iso_predicate'(_) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer)

:- import between/3 from basics.


% findall(?term, +callable, ?list, +list) -- built-in


% forall(+callable, +callable) -- built-in


% format(+stream_or_alias, +character_code_list_or_atom, +list)
% format(+character_code_list_or_atom, +list)

:- import format/3, format/2 from format.

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

'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).
:- if(\+ predicate_property(setup_call_cleanup(_,_,_), _)).
	'$lgt_predicate_property'(setup_call_cleanup(_, _, _), built_in).
	'$lgt_predicate_property'(setup_call_cleanup(_, _, _), static).
	'$lgt_predicate_property'(setup_call_cleanup(_, _, _), meta_predicate(setup_call_cleanup(0,0,0))).
:- endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable)

:- if(\+ predicate_property(setup_call_cleanup(_,_,_), _)).

	setup_call_cleanup(Setup, Call, Cleanup) :-
		once(Setup),
		call_cleanup(Call, (Cleanup->true;true)).

:- endif.



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

'$lgt_prolog_meta_predicate'(call_cleanup(_, _), call_cleanup(0, 0), predicate).
'$lgt_prolog_meta_predicate'(fail_if(_), fail_if(0), predicate).
'$lgt_prolog_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_prolog_meta_predicate'(not(_), not(0), predicate).
'$lgt_prolog_meta_predicate'(sk_not(_), sk_not(0), predicate).
'$lgt_prolog_meta_predicate'(table_once(_), table_once(0), predicate).
'$lgt_prolog_meta_predicate'(tfindall(_, _, _), tfindall(*, 0, *), predicate).
'$lgt_prolog_meta_predicate'(tnot(_), tnot(0), predicate).

'$lgt_prolog_meta_predicate'(time(_), time(0), predicate).
'$lgt_prolog_meta_predicate'(timed_call(_, _), timed_call(0, *), predicate).

'$lgt_prolog_meta_predicate'(abolish_table_call(_), abolish_table_call(0), predicate).
'$lgt_prolog_meta_predicate'(abolish_table_call(_, _), abolish_table_call(0, *), predicate).
'$lgt_prolog_meta_predicate'(abolish_table_pred(_), abolish_table_pred(0), predicate).
'$lgt_prolog_meta_predicate'(abolish_table_pred(_, _), abolish_table_pred(0, *), predicate).


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(index(_, _), index(/, *)).
'$lgt_prolog_meta_directive'(use_subsumptive_tabling(_), use_subsumptive_tabling(/)).
'$lgt_prolog_meta_directive'(use_variant_tabling(_), use_variant_tabling(/)).


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


% '$lgt_candidate_tautology_or_falsehood_goal_hook'(@callable)
%
% valid candidates are proprietary built-in predicates with
% no side-effects when called with ground arguments

'$lgt_candidate_tautology_or_falsehood_goal_hook'(name(_, _)).


% '$lgt_prolog_database_predicate'(@callable)
%
% table of non-standard database built-in predicates

'$lgt_prolog_database_predicate'(assert(_)).
'$lgt_prolog_database_predicate'(listing(_)).


% '$lgt_prolog_predicate_property'(?callable)
%
% table of proprietary predicate properties; used by the
% compiler when checking if a predicate property is valid

'$lgt_prolog_predicate_property'(_) :-
	fail.



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
% there must be a single object file extension
'$lgt_file_extension'(object, '.P').
'$lgt_file_extension'(prolog, '.P').
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(prolog, '.prolog').
'$lgt_file_extension'(prolog, '.pro').
'$lgt_file_extension'(tmp, '.xwam').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  backend Prolog compiler features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% backend Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, xsb).
'$lgt_prolog_feature'(prolog_version, v(Major, Minor, Patch)) :-
	current_prolog_flag(version_data, xsb(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=(v(3,8,0))).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, supported).
'$lgt_prolog_feature'(engines, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, supported).
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
'$lgt_default_flag'(conditionals, warning).
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
'$lgt_default_flag'(grammar_rules, warning).
'$lgt_default_flag'(arithmetic_expressions, warning).
'$lgt_default_flag'(suspicious_calls, warning).
'$lgt_default_flag'(underscore_variables, singletons).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	(	expand_atom('$COMSPEC', _) ->
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
'$lgt_default_flag'(prolog_loader, [+optimize, -verbo, +canonical]).



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
	expand_atom(Path, EnvVarExpandedPath),
	path_sysop(expand, EnvVarExpandedPath, ExpandedPath0),
	ExpandedPath = ExpandedPath0.


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


% '$lgt_directory_hashes'(+atom, -atom, -atom)
%
% returns the directory hash and dialect as an atom with the format _hash_dialect
% plus the the directory hash and PID as an atom with the format _hash_pid

'$lgt_directory_hashes'(Directory, HashDialect, HashPid) :-
	term_hash(Directory, 2147483647, Hash),
	'$lgt_prolog_feature'(prolog_dialect, Dialect),
	sys_pid(PID),
	concat_atom(['_', Hash, '_', Dialect], HashDialect),
	concat_atom(['_', Hash, '_', PID], HashPid).


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_compile_prolog_code'(File, _, Options) :-
	compile(File, Options).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_load_prolog_code'(File, _, Options) :-
	'$lgt_expand_path'(File, Expanded),
	reconsult(Expanded, Options).


% '$lgt_load_prolog_file'(+atom)
%
% compile and (re)load a Prolog file (used in standards compliance tests)

'$lgt_load_prolog_file'(File) :-
	reconsult(File).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	path_sysop(modtime, File, Time).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Name, Value) :-
	atom_concat('$', Name, Variable),
	expand_atom(Variable, Value).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory must always end
% with a slash; the extension must start with a "." when defined and must
% be the empty atom when it does not exist

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


% '$lgt_directory_files'(+atom, -list(atom))
%
% returns a list of files in the given directory

'$lgt_directory_files'(Directory, Files) :-
	findall(File, list_directory(Directory, File), Files).



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
%  position (start and end lines; needed for improved error messages) due
%  to the lack of a standard option for this purpose
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -pair(integer,integer))

'$lgt_read_term'(Stream, Term, Options, '-'(-1, -1)) :-
	read_term(Stream, Term, Options).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'((:- Directive), Expanded) :-
	nonvar(Directive),
	% allow first-argument indexing
	catch('$lgt_xsb_directive_expansion'(Directive, Expanded), _, fail).


'$lgt_xsb_directive_expansion'(table(PIs), {:- table(CPIs)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, _, CPIs).

'$lgt_xsb_directive_expansion'(as(dynamic(PIs),ShareMode), {:- as(dynamic(CPIs),ShareMode)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, _, CPIs).

'$lgt_xsb_directive_expansion'(as(table(PIs),ShareMode), {:- as(table(CPIs),ShareMode)}) :-
	logtalk_load_context(entity_type, _),
	'$lgt_compile_predicate_indicators'(PIs, _, CPIs).

'$lgt_xsb_directive_expansion'(import(as(from(PI), Module), Functor), (:- use_module(Module, [as(PI, Functor)]))).

'$lgt_xsb_directive_expansion'(import(from(PIs1, Module)), (:- use_module(Module, PIs2))) :-
	'$lgt_xsb_conjunction_to_list'(PIs1, PIs2).

'$lgt_xsb_directive_expansion'(use_module(Module, Imports), [{:- use_module(Module, Imports)}, (:- use_module(Module, Imports))]) :-
	logtalk_load_context(entity_type, Type),
	Type \== module,
	use_module(Module, Imports).

'$lgt_xsb_directive_expansion'(local(PIs1), (:- private(PIs2))) :-
	'$lgt_xsb_conjunction_to_list'(PIs1, PIs2).

'$lgt_xsb_directive_expansion'(op(Priority, Specifier, ':'(Module,Operators)), {:- op(Priority, Specifier, Operators)}) :-
	Module == user.


'$lgt_xsb_conjunction_to_list'(Term, [Term]) :-
	var(Term),
	!.
'$lgt_xsb_conjunction_to_list'((Term, Conjunction), [Term| Terms]) :-
	!,
	'$lgt_xsb_conjunction_to_list'(Conjunction, Terms).
'$lgt_xsb_conjunction_to_list'(Term, [Term]).


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
%  error term normalization (when exception terms don't follow the ISO
%  Prolog standard)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_normalize_error_term'(@callable, -callable)

'$lgt_normalize_error_term'(error(error(Error, _, _), Context), error(NormalizedError, Context)) :-
	'$lgt_normalize_error_term_aux'(Error, NormalizedError).
'$lgt_normalize_error_term'(error(error(Error, _, _), _, _), error(NormalizedError, _)) :-
	'$lgt_normalize_error_term_aux'(Error, NormalizedError).
'$lgt_normalize_error_term'(error(Error, _, _), error(NormalizedError, _)) :-
	'$lgt_normalize_error_term_aux'(Error, NormalizedError).
'$lgt_normalize_error_term'(error(Error, Context), error(NormalizedError, Context)) :-
	'$lgt_normalize_error_term_aux'(Error, NormalizedError).

'$lgt_normalize_error_term_aux'(
	existence_error(predicate, ':'('usermod(?)', Functor/Arity)),
	existence_error(procedure, Functor/Arity)
).
'$lgt_normalize_error_term_aux'(
	existence_error(procedure, ':'('usermod(?)', Functor/Arity)),
	existence_error(procedure, Functor/Arity)
).
'$lgt_normalize_error_term_aux'(
	existence_error(predicate, ':'(usermod, Functor/Arity)),
	existence_error(procedure, Functor/Arity)
).
'$lgt_normalize_error_term_aux'(
	existence_error(procedure, ':'(usermod, Functor/Arity)),
	existence_error(procedure, Functor/Arity)
).
'$lgt_normalize_error_term_aux'(
	type_error(evaluable, Expression),
	type_error(evaluable, Functor/Arity)
) :-
	callable(Expression),
	Expression \= _/_,
	functor(Expression, Functor, Arity).
'$lgt_normalize_error_term_aux'(
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


% term_hash(@callable, +integer, +integer, -integer)

term_hash(_, _, _, _) :-
	fail.



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
		;	number_codes(Atomic, Codes),
			atom_codes(Atom, Codes)
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

'$lgt_user_module_qualification'(Goal, usermod:Goal).



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


% '$lgt_current_module_predicate'(+atom, +predicate_indicator)
%
% succeeds when Module defines Predicate

'$lgt_current_module_predicate'(Module, Predicate) :-
	current_predicate(Module:Predicate).



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

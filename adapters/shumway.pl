%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for Shumway (Prolog on .NET) 0.9.3 and later versions
%  Last updated on October 29, 2026
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Gustavo Brown
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


% SWI-style fixed-width unicode escapes (\uXXXX / \UXXXXXXXX) in quoted
% text, engine-wide for the Logtalk session — matching the reference
% backend the swi dialect announces. Strict ISO reads stay the default
% outside Logtalk.
:- set_prolog_flag(lenient_escapes, true).



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


% format(+character_code_list_or_atom, +list) -- built-in

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

'$lgt_predicate_property'(Predicate, Property) :-
	predicate_property(Predicate, Property).



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

'$lgt_prolog_meta_predicate'(freeze(_, _), freeze(*, 0), predicate).
'$lgt_prolog_meta_predicate'(when(_, _), when(*, 0), predicate).
'$lgt_prolog_meta_predicate'(call_cleanup(_, _), call_cleanup(0, 0), predicate).
'$lgt_prolog_meta_predicate'(call_det(_, _), call_det(0, *), predicate).
'$lgt_prolog_meta_predicate'(call_nth(_, _), call_det(0, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_error_context(_, _), call_with_error_context(0, *), predicate).
'$lgt_prolog_meta_predicate'(call_with_limit(_, _), call_with_limit(*, 0), predicate).
'$lgt_prolog_meta_predicate'(call_with_offset(_, _), call_with_limit(*, 0), predicate).
'$lgt_prolog_meta_predicate'(setup_call_cleanup(_, _, _), setup_call_cleanup(0, 0, 0), predicate).
'$lgt_prolog_meta_predicate'(time(_), time(0), predicate).
'$lgt_prolog_meta_predicate'(time_out(_, _, _), time_out(0, *, *), predicate).


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(table(_), table(/)).
'$lgt_prolog_meta_directive'(built_in(_), built_in(/)).
'$lgt_prolog_meta_directive'(ensure_linked(_), ensure_linked(/)).


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


% '$lgt_prolog_phrase_predicate'(@callable)
%
% table of predicates that call non-terminals
% (other than the de facto standard phrase/2-3 predicates)

'$lgt_prolog_phrase_predicate'(_) :-
	fail.



% '$lgt_candidate_tautology_or_falsehood_goal_hook'(@callable)
%
% valid candidates are proprietary built-in predicates with no
% side-effects when called with ground arguments

'$lgt_candidate_tautology_or_falsehood_goal_hook'(is_list(_)).
'$lgt_candidate_tautology_or_falsehood_goal_hook'(succ(_, _)).


% '$lgt_prolog_database_predicate'(@callable)
%
% table of non-standard database built-in predicates

'$lgt_prolog_database_predicate'(assert(_)).
'$lgt_prolog_database_predicate'(assert(_, _)).
'$lgt_prolog_database_predicate'(asserta(_, _)).
'$lgt_prolog_database_predicate'(assertz(_, _)).
'$lgt_prolog_database_predicate'(clause(_, _, _)).
'$lgt_prolog_database_predicate'(listing(_)).


% '$lgt_prolog_predicate_property'(?callable)
%
% table of proprietary predicate properties; used by the
% compiler when checking if a predicate property is valid

'$lgt_prolog_predicate_property'(built_in).
'$lgt_prolog_predicate_property'(dynamic).
'$lgt_prolog_predicate_property'(static).


% '$lgt_prolog_deprecated_built_in_predicate_hook'(?callable, ?callable)
%
% table of proprietary deprecated built-in predicates
% when there's a Prolog system advised alternative

'$lgt_prolog_deprecated_built_in_predicate_hook'(_, _) :-
	fail.


% '$lgt_prolog_deprecated_built_in_predicate_hook'(?callable)
%
% table of proprietary deprecated built-in predicates without
% a direct advised alternative

'$lgt_prolog_deprecated_built_in_predicate_hook'(_) :-
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
'$lgt_file_extension'(object, '.pl').
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(prolog, '.prolog').
'$lgt_file_extension'(prolog, '.pro').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  backend Prolog compiler features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% backend Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, shumway).
'$lgt_prolog_feature'(prolog_version, v(Major, Minor, Patch)) :-
	current_prolog_flag(version_data, shumway(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=(v(0, 9, 3))).
'$lgt_prolog_feature'(encoding_directive, full).
'$lgt_prolog_feature'(sockets, unsupported).
'$lgt_prolog_feature'(tabling, supported).
'$lgt_prolog_feature'(engines, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, unsupported).
'$lgt_prolog_feature'(coinduction, unsupported).
'$lgt_prolog_feature'(unicode, full).

% startup flags:
'$lgt_default_flag'(settings_file, allow).
% lint compilation flags:
'$lgt_default_flag'(linter, default).
'$lgt_default_flag'(general, warning).
'$lgt_default_flag'(encodings, warning).
'$lgt_default_flag'(unknown_entities, warning).
'$lgt_default_flag'(unknown_predicates, warning).
'$lgt_default_flag'(undefined_predicates, warning).
'$lgt_default_flag'(singleton_variables, warning).
'$lgt_default_flag'(steadfastness, silent).
'$lgt_default_flag'(naming, silent).
'$lgt_default_flag'(duplicated_clauses, silent).
'$lgt_default_flag'(left_recursion, warning).
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
'$lgt_default_flag'(underscore_variables, dont_care).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, ScratchDirectory) :-
	(	catch(getenv('COMSPEC', _), _, fail) ->
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

'$lgt_prolog_os_file_name'(PrologPath, OSPath) :-
	prolog_to_os_filename(PrologPath, OSPath).


% '$lgt_expand_path'(+atom, -atom)
%
% expands a file path to a full path

% Path expansion = $LOGTALKHOME/$LOGTALKUSER substitution, then absolute,
% then forward slashes throughout. The core assembles paths POSIX-style;
% .NET hands back backslashed Windows paths, and a mixed 'dir\/file' join
% breaks exists_file — so everything the adapter returns is '/'-separated.
'$lgt_expand_path'(Path, ExpandedPath) :-
	'$shumway_env_prefix'(Path, Substituted),
	(	catch(absolute_file_name(Substituted, Absolute), _, fail) ->
		true
	;	Absolute = Substituted
	),
	'$shumway_forward_slashes'(Absolute, ExpandedPath).

'$shumway_forward_slashes'(Path, Slashed) :-
	(	sub_atom(Path, _, _, _, '\\') ->
		atomic_list_concat(Segments, '\\', Path),
		atomic_list_concat(Segments, '/', Slashed)
	;	Slashed = Path
	).

'$shumway_env_prefix'(Path, Substituted) :-
	(	sub_atom(Path, 0, _, _, '$LOGTALKHOME/'),
		getenv('LOGTALKHOME', Value) ->
		sub_atom(Path, 13, _, 0, Tail),
		atomic_list_concat([Value, '/', Tail], Substituted)
	;	sub_atom(Path, 0, _, _, '$LOGTALKUSER/'),
		getenv('LOGTALKUSER', Value) ->
		sub_atom(Path, 13, _, 0, Tail),
		atomic_list_concat([Value, '/', Tail], Substituted)
	;	sub_atom(Path, 0, _, _, '$LOGTALK_STARTUP_DIRECTORY/'),
		getenv('LOGTALK_STARTUP_DIRECTORY', Value) ->
		sub_atom(Path, 27, _, 0, Tail),
		atomic_list_concat([Value, '/', Tail], Substituted)
	;	Substituted = Path
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
	catch(delete(File), _, true).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	exists_directory(Directory).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Current, Current),
	'$shumway_forward_slashes'(Current, Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	working_directory(_, Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	exists_directory(Directory) ->
		true
	;	catch(mkdir(Directory), _, true)
	).


% '$lgt_directory_hashes'(+atom, -atom, -atom)
%
% returns the directory hash and dialect as an atom with the format _hash_dialect
% plus the the directory hash and PID as an atom with the format _hash_pid

'$lgt_directory_hashes'(Directory, HashDialect, HashPid) :-
	'$shumway_hash_term'(Term, Hash),
	pid(PID),
	atomic_list_concat(['_', Hash, '_', shumway], HashDialect),
	atomic_list_concat(['_', Hash, '_', PID], HashPid).


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
	reconsult(File).


% '$lgt_load_prolog_file'(+atom)
%
% compile and (re)load a Prolog file (used in standards compliance tests)

'$lgt_load_prolog_file'(File) :-
	consult(File).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(_File, 0).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	catch(getenv(Variable, Value), _, fail).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory must always end
% with a slash; the extension must start with a "." when defined and must
% be the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	'$shumway_forward_slashes'(File, Path),
	(	'$shumway_last_occurrence'(Path, '/', SlashEnd) ->
		sub_atom(Path, 0, SlashEnd, _, Directory),
		sub_atom(Path, SlashEnd, _, 0, Base)
	;	Directory = './',
		Base = Path
	),
	(	'$shumway_last_occurrence'(Base, '.', DotStart0),
		DotStart0 > 1 ->
		DotStart is DotStart0 - 1,
		sub_atom(Base, 0, DotStart, _, Name),
		sub_atom(Base, DotStart, _, 0, Extension)
	;	Name = Base,
		Extension = ''
	).

% End position (1-based, past the character) of the LAST occurrence of a
% single-character atom; fails when absent. Driven by sub_atom backtracking.
'$shumway_last_occurrence'(Atom, Char, End) :-
	findall(After, sub_atom(Atom, _, 1, After, Char), Afters),
	Afters = [_| _],
	'$shumway_min_of'(Afters, MinAfter),
	atom_length(Atom, Length),
	End is Length - MinAfter.

'$shumway_min_of'([X| Xs], Min) :-
	'$shumway_min_of'(Xs, X, Min).

'$shumway_min_of'([], Min, Min).
'$shumway_min_of'([X| Xs], Acc, Min) :-
	(	X < Acc ->
		'$shumway_min_of'(Xs, X, Min)
	;	'$shumway_min_of'(Xs, Acc, Min)
	).


% '$lgt_directory_files'(+atom, -list(atom))
%
% returns a list of files in the given directory

'$lgt_directory_files'(_Directory, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(_Stream, 0).



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

'$lgt_read_term'(Stream, Term, Options, 0-0) :-
	read_term(Stream, Term, Options).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'((:- use_module(constraintLib, _)), []) :- !.
'$lgt_prolog_term_expansion'((:- use_module(Library)), Expansion) :- !,
	'$shumway_expand_use_module'(Library, Expansion).
'$lgt_prolog_term_expansion'((:- use_module(Library, Imports)), Expansion) :- !,
	(	'$shumway_provided_library'(Library) ->
		Expansion = []
	;	catch(use_module(Library), _, fail) ->
		Expansion = {:- use_module(Library)}
	;	nonvar(Imports), Imports = [_| _] ->
		% A module we cannot resolve, imported for SPECIFIC predicates
		% (json_path/types import unicode_property/2 from SWI's `unicode`):
		% route those names to `user`, where this adapter or the engine may
		% supply them. On this module-less backend that is what the import
		% MEANS; a predicate genuinely absent still raises a clear
		% existence_error at its call site, instead of the confusing
		% object-local '$obj#0.name#arity' one that dropping the directive
		% produced.
		Expansion = (:- uses(user, Imports))
	;	Expansion = []
	).

'$shumway_expand_use_module'(Library, []) :-
	'$shumway_provided_library'(Library),
	!.
'$shumway_expand_use_module'(Library, Expansion) :-
	(	catch(use_module(Library), _, fail) ->
		Expansion = {:- use_module(Library)}
	;	Expansion = []
	).

% Libraries whose predicates this adapter already supplies. These must be
% DROPPED rather than resolved: the engine's search path can carry another
% dialect's copy (the Scryer collection ships dif.pl), and loading it on top
% of ours collides on the shared public names.
'$shumway_provided_library'(library(dif)).      % library(coroutining), above
'$shumway_provided_library'(library(when)).
'$shumway_provided_library'(library(freeze)).
'$shumway_provided_library'(library(atts)).
'$shumway_provided_library'(library(time)).     % call_with_time_limit/2, below


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('US-ASCII', ascii, _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-1', iso_latin_1, _).
'$lgt_logtalk_prolog_encoding'('UTF-8', utf8, _).
'$lgt_logtalk_prolog_encoding'('UCS-2', Encoding, Stream) :-  % BOM mandatory
	(	stream_property(Stream, encoding(utf16le)) ->
		Encoding = utf16le
	;	stream_property(Stream, encoding(utf16be)) ->
		Encoding = utf16be
	).
'$lgt_logtalk_prolog_encoding'('UCS-2BE', utf16be, _).
'$lgt_logtalk_prolog_encoding'('UCS-2LE', utf16le, _).
'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream) :-  % BOM expected
	(	stream_property(Stream, encoding(utf16le)) ->
		Encoding = utf16le
	;	stream_property(Stream, encoding(utf16be)) ->
		Encoding = utf16be
	).
'$lgt_logtalk_prolog_encoding'('UTF-16BE', utf16be, _).
'$lgt_logtalk_prolog_encoding'('UTF-16LE', utf16le, _).
'$lgt_logtalk_prolog_encoding'('UTF-32', Encoding, Stream) :-  % BOM expected
	(	stream_property(Stream, encoding(utf32le)) ->
		Encoding = utf32le
	;	stream_property(Stream, encoding(utf32be)) ->
		Encoding = utf32be
	).
'$lgt_logtalk_prolog_encoding'('UTF-32BE', utf32be, _).
'$lgt_logtalk_prolog_encoding'('UTF-32LE', utf32le, _).



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

'$lgt_normalize_error_term'(Error, Error).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  term hashing (not currently used in the compiler/runtime)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% term_hash(@callable, +integer, +integer, -integer) -- built-in

term_hash(Term, _Depth, _Range, Hash) :-
	'$shumway_hash_term'(Term, Hash).

% Term hashing (scratch-name uniqueness, library term_hash users). Any
% stable hash works; fold the canonical spelling with the classic
% multiply-by-33 accumulator, capped to 30 bits.

'$shumway_hash_term'(Term, Hash) :-
	(	atom(Term) ->
		Spelling = Term
	;	term_to_atom(Term, Spelling)
	),
	atom_codes(Spelling, Codes),
	'$shumway_hash_fold'(Codes, 5381, Hash).

'$shumway_hash_fold'([], Hash, Hash).
'$shumway_hash_fold'([Code| Codes], Acc0, Hash) :-
	Acc is (Acc0 * 33 + Code) /\ 0x3fffffff,
	'$shumway_hash_fold'(Codes, Acc, Hash).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  atomics concat (not currently used in the compiler/runtime)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% atomic_concat(+atomic, +atomic, ?atom)

atomic_concat(A, B, Atom) :-
	(	var(A) ->
		throw(error(instantiation_error, atomic_concat/3))
	;	var(B) ->
		throw(error(instantiation_error, atomic_concat/3))
	;	\+ atomic(A) ->
		throw(error(type_error(atomic, A), atomic_concat/3))
	;	\+ atomic(B) ->
		throw(error(type_error(atomic, B), atomic_concat/3))
	;	'$shumway_as_atom'(A, AtomA),
		'$shumway_as_atom'(B, AtomB),
		atom_concat(AtomA, AtomB, Atom)
	).

'$shumway_as_atom'(Atomic, Atom) :-
	(	atom(Atomic) ->
		Atom = Atomic
	;	term_to_atom(Atomic, Atom)
	).


% atomic_list_concat(@list(atomic), ?atom) -- built-in


% atomic_list_concat(@list(atomic), +atom, ?atom) -- built-in



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
%  auxiliary predicates for compiling modules as objects
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_find_module_predicate'(+atom, -atom, @callable)
%
% succeeds when Module:Predicate is visible in module Current

'$lgt_find_visible_module_predicate'(_, _, _) :-
	fail.


% '$lgt_current_module_predicate'(+atom, +predicate_indicator)
%
% succeeds when Module defines Predicate

'$lgt_current_module_predicate'(_, _) :-
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
{+f} :-
	!,
	logtalk_make(force).
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

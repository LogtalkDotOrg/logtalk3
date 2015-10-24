%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for JIProlog 4.1.2.8 or later versions
%  Last updated on October 24, 2015
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- set_prolog_flag(unknown, error).



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
%  properties for an existing predicate (and ideally meta_predicate/1
%  properties for built-in predicates and library predicates)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer) -- built-in


% findall(?term, +callable, ?list, +list) -- built-in


% forall(+callable, +callable) -- built-in


% format(+stream_or_alias, +character_code_list_or_atom, +list)

format(Stream, Format, Arguments) :-
	atom_codes(Format, Codes),
	format_(Codes, Stream, Arguments).

format_([], _, _).
format_([0'~, Spec| Codes], Stream, Arguments) :-
	!,
	format_spec_(Spec, Stream, Arguments, RemainingArguments),
	format_(Codes, Stream, RemainingArguments).
format_([Code| Codes], Stream, Arguments) :-
	put_code(Stream, Code),
	format_(Codes, Stream, Arguments).

format_spec_(0'a, Stream, [Argument| Arguments], Arguments) :-
%	atom(Argument),
	write(Stream, Argument).
format_spec_(0'c, Stream, [Argument| Arguments], Arguments) :-
	put_code(Stream, Argument).
format_spec_(0's, Stream, [Argument| Arguments], Arguments) :-
	atom_codes(Atom, Argument),
	write(Stream, Atom).
format_spec_(0'w, Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_(0'q, Stream, [Argument| Arguments], Arguments) :-
	writeq(Stream, Argument).
format_spec_(0'k, Stream, [Argument| Arguments], Arguments) :-
	write_canonical(Stream, Argument).
format_spec_(0'd, Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_(0'D, Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_(0'f, Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_(0'g, Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_(0'G, Stream, [Argument| Arguments], Arguments) :-
	write(Stream, Argument).
format_spec_(0'i, _, [_| Arguments], Arguments).
format_spec_(0'n, Stream, Arguments, Arguments) :-
	nl(Stream).
format_spec_(0'~, Stream, Arguments, Arguments) :-
	put_code(Stream, 0'~).


% format(+character_code_list_or_atom, +list)

format(Format, Arguments) :-
	current_output(Stream),
	format(Stream, Format, Arguments).


% numbervars(?term, +integer, ?integer) -- built-in



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

'$lgt_prolog_meta_predicate'(Callable, Template, predicate) :-
	predicate_property(Callable, meta_predicate(Template)),
	predicate_property(Callable, built_in),
	!.


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(_, _) :-
	fail.


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


% '$lgt_prolog_database_predicate'(@callable)

'$lgt_prolog_database_predicate'(assert(_)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_file_extension'(?atom, ?atom)

'$lgt_file_extension'(logtalk, '.lgt').
'$lgt_file_extension'(logtalk, '.logtalk').
'$lgt_file_extension'(object, '.pl').
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(prolog, '.prolog').
'$lgt_file_extension'(tmp, '.jip').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog compiler features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, ji).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	current_prolog_flag(version_data, jiprolog(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((4,1,2))).
'$lgt_prolog_feature'(prolog_conformance, lax).

'$lgt_prolog_feature'(encoding_directive, source).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, unsupported).
'$lgt_prolog_feature'(coinduction, unsupported).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
%
%  if your Prolog compiler supports the ISO definition of the 
%  initialization/1 then change the default value below to true
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
'$lgt_default_flag'(underscore_variables, dont_care).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% other compilation flags:
'$lgt_default_flag'(scratch_directory, './').
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  operating-system access predicates
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
	% first expand any environment variable
	'$lgt_ji_expand_environment'(Path, ExpandedPath0),
	(	(	sub_atom(ExpandedPath0, 0, 1, _, '/')
			% assume POSIX full path 
		;	sub_atom(ExpandedPath0, 1, 1, _, ':')
			% assume Windows full Path starting with a drive letter followed by ":"
		) ->
		% assume full path
		ExpandedPath = ExpandedPath0
	;	% assume path relative to the current directory
		'$lgt_current_directory'(Directory),
		atom_concat(Directory, ExpandedPath0, ExpandedPath)
	).


'$lgt_ji_expand_environment'(Path, ExpandedPath) :-
	'$lgt_ji_convert_file_path'(Path, Path1),
	(	sub_atom(Path1, 0, 1, _, '$'),
		sub_atom(Path1, Before, _, _, '/') ->
		End is Before - 1,
		sub_atom(Path1, 1, End, _, Variable),
		sub_atom(Path1, Before, _, 0, Rest),
		'$lgt_environment_variable'(Variable, Value0),
		'$lgt_ji_convert_file_path'(Value0, Value),
		atom_concat(Value, Rest, ExpandedPath)
	;	Path1 = ExpandedPath
	).


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

'$lgt_file_exists'(File) :-
	exists_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	exists_directory(Directory).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Directory0, Directory0),
	'$lgt_ji_convert_file_path'(Directory0, Directory1),
	(	sub_atom(Directory1, _, 1, 0, '/') ->
		Directory = Directory1
	;	atom_concat(Directory1, '/', Directory)
	).


'$lgt_ji_convert_file_path'(File, Converted) :-
	atom_codes(File, FileCodes),
	char_code('\\', Backslash),
	char_code('/', Slah),
	'$lgt_ji_reverse_slashes'(FileCodes, Backslash, Slah, ConvertedCodes),
	atom_codes(Converted, ConvertedCodes).

'$lgt_ji_reverse_slashes'([], _, _, []).
'$lgt_ji_reverse_slashes'([Code| Codes], Backslash, Slah, [ConvertedCode| ConvertedCodes]) :-
	(	Code =:= Backslash ->
		ConvertedCode = Slah
	;	ConvertedCode = Code
	),
	'$lgt_ji_reverse_slashes'(Codes, Backslash, Slah, ConvertedCodes).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	'$lgt_expand_path'(Directory, Expanded),
	chdir(Expanded).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	'$lgt_expand_path'(Directory, Expanded),
	(	exists_directory(Expanded) ->
		true
	;	make_directory(Expanded)
	).


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_compile_prolog_code'(File, _, _) :-
	compile(File).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of flags

'$lgt_load_prolog_code'(PrologFile, _, _) :-
	consult(PrologFile).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	file_attributes(File, _, _, _, _, _, Time).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	(	invoke('java.lang.System', getenv('java.lang.String'), [Variable], Value),
		Value \== [] ->
		true
	;	% check if the environment variable value is passed as a property
		invoke('java.lang.System', getProperty('java.lang.String'), [Variable], Value),
		Value \== []
	).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory

'$lgt_startup_directory'(Directory) :-
	(	'$lgt_environment_variable'('LOGTALK_STARTUP_DIRECTORY', Directory0) ->
		true
	;	'$lgt_current_directory'(Directory0)
	),
	'$lgt_ji_convert_file_path'(Directory0, Directory).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	'$lgt_environment_variable'('LOGTALKUSER', Directory0),
	'$lgt_ji_convert_file_path'(Directory0, Directory).



% '$lgt_home_directory'(-atom)
%
% returns the Logtalk home directory; fails if unknown

'$lgt_home_directory'(Directory) :-
	'$lgt_environment_variable'('LOGTALKHOME', Directory0),
	'$lgt_ji_convert_file_path'(Directory0, Directory).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory must always end
% with a slash; the extension must start with a "." when defined and must
% be the empty atom when it does not exist

'$lgt_decompose_file_name'(File, Directory, Name, Extension) :-
	file_base_name(File, Name),
	file_name_extension(File, Extension0),
	(	Extension0 == [] ->
		Extension = ''
	;	atom_concat('.', Extension0, Extension)
	),
	atom_concat(Name, Extension, Basename),
	atom_concat(Directory, Basename, File).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	stream_property(Stream, position(Position)),
	stream_position_data(line_count, Position, Line).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  abstraction of the standard open/4 and close/1 predicates for dealing
%  with the alias/1 option in old non-standard compliant systems
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
%  position (start and end lines; needed for improved error messages) and
%  the variable names (ideally using the standard variable_names/1 option)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position, -list)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd, Variables) :-
	read_term(Stream, Term, [line_counts(LineBegin,LineEnd), variable_names(Variables)| Options]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'(_, _) :-
	fail.


% '$lgt_prolog_goal_expansion'(@callable, -callable)

'$lgt_prolog_goal_expansion'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('UTF-8', 'UTF-8', _).
'$lgt_logtalk_prolog_encoding'('US-ASCII', 'US-ASCII', _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  lambda expressions support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term(Term, Copy).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  hooks predicates for writing and asserting compiled entity terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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


%:- multifile('$logtalk#0.print_message_token#4'/5).
%:- dynamic('$logtalk#0.print_message_token#4'/5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  term hashing (not currently used in the compiler/runtime)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% term_hash(@callable, +integer, +integer, -integer)

term_hash(_, _, _, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  string built-in type
%
%  define these predicates to trivially fail if no string type is available
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_string'(@term)

'$lgt_string'(_) :-
	fail.


% '$lgt_string_codes'(+string, -list(codes))
% '$lgt_string_codes'(-string, +list(codes))

'$lgt_string_codes'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default user module
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_user_module_qualification'(@callable, -callable)

'$lgt_user_module_qualification'(Goal, Goal).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  shortcuts to the Logtalk built-in predicates logtalk_load/1 and
%  logtalk_make/1
%
%  defined in the adapter files to make it easier to comment them out in case
%  of conflict with some Prolog native feature; they require conformance with
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

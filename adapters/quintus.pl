%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Adapter file for Quintus Prolog 3.3~3.5
%  Last updated on February 10, 2016
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


:- [library(between)].
:- [library(foreach)].
:- [library(files)].
:- [library(directory)].
:- [library(strings)].
:- [library(environ)].
:- [library(subsumes)].
:- [library(unify)].
:- [library(math)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  ISO Prolog Standard predicates that we must define because they are
%  not built-in
%
%  add a clause for '$lgt_iso_predicate'/1 declaring each ISO predicate that
%  you must define; there must be at least one clause for this predicate
%  whose call should fail if you don't define any ISO predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- prolog_flag(character_escapes, _, on).


:- op(700, xfx, \=).
:- op(200, xfx, **).


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(_ \= _).
'$lgt_iso_predicate'(acyclic_term(_)).
'$lgt_iso_predicate'(at_end_of_stream).
'$lgt_iso_predicate'(at_end_of_stream(_)).
'$lgt_iso_predicate'(atom_codes(_, _)).
'$lgt_iso_predicate'(atom_concat(_, _, _)).
'$lgt_iso_predicate'(atom_length(_, _)).
'$lgt_iso_predicate'(catch(_, _, _)).
'$lgt_iso_predicate'(char_code(_, _)).
'$lgt_iso_predicate'(current_predicate(_)).
'$lgt_iso_predicate'(current_prolog_flag(_, _)).
'$lgt_iso_predicate'(flush_output).
'$lgt_iso_predicate'(get_code(_)).
'$lgt_iso_predicate'(get_code(_, _)).
'$lgt_iso_predicate'(number_codes(_, _)).
'$lgt_iso_predicate'(once(_)).
'$lgt_iso_predicate'(peek_code(_)).
'$lgt_iso_predicate'(peek_code(_, _)).
'$lgt_iso_predicate'(put_code(_)).
'$lgt_iso_predicate'(put_code(_, _)).
'$lgt_iso_predicate'(set_prolog_flag(_, _)).
'$lgt_iso_predicate'(stream_property(_, _)).
'$lgt_iso_predicate'(sub_atom(_, _, _, _, _)).
'$lgt_iso_predicate'(subsumes_term(_, _)).
'$lgt_iso_predicate'(term_variables(_, _)).
'$lgt_iso_predicate'(throw(_)).
'$lgt_iso_predicate'(unify_with_occurs_check(_, _)).


Term1 \= Term2 :-
	\+ (Term1 = Term2).


acyclic_term(_).


at_end_of_stream :-
	at_end_of_file.


at_end_of_stream(Stream) :-
	at_end_of_file(Stream).


atom_codes(Atom, Codes) :-
	atom_chars(Atom, Codes).


atom_concat(Atom1, Atom2, Atom3) :-
	nonvar(Atom1), nonvar(Atom2),
	!,
	atom_chars(Atom1, Codes1),
	atom_chars(Atom2, Codes2),
	append(Codes1, Codes2, Codes3),
	atom_chars(Atom3, Codes3).

atom_concat(Atom1, Atom2, Atom3) :-
	nonvar(Atom3),
	!,
	atom_chars(Atom3, Codes3),
	append(Codes1, Codes2, Codes3),
	atom_chars(Atom1, Codes1),
	atom_chars(Atom2, Codes2).


atom_length(Atom, Length) :-
	atom_chars(Atom, Codes),
	length(Codes, Length).


catch(Goal, Catcher, Recovery) :-
	on_exception(Catcher, Goal, Recovery).


char_code(Char, Code) :-
	atom_chars(Char, [Code]).


current_predicate(Functor/Arity) :-
	current_predicate(_, Callable),
	functor(Callable, Functor, Arity).
current_predicate(Module:Functor/Arity) :-
	current_predicate(_, Module:Callable),
	functor(Callable, Functor, Arity).


current_prolog_flag(Flag, Value) :-
	prolog_flag(Flag, Value).


flush_output :-
	current_output(Stream),
	flush_output(Stream).


get_code(Stream, Code) :-
	get0(Stream, Code).


get_code(Code) :-
	get0(Code).


number_codes(Number, Codes) :-
	number_chars(Number, Codes).


once(Goal) :-
	call(Goal),
	!.


peek_code(Stream, Code) :-
	peek_char(Stream, Code).


peek_code(Code) :-
	peek_char(Code).


put_code(Stream, Code) :-
	put(Stream, Code).


put_code(Code) :-
	put(Code).


stream_property(Stream, alias(Alias)) :-
	(	var(Stream) ->
		'$lgt_quintus_stream_alias'(Stream, Alias)
	;	'$lgt_quintus_stream_alias'(Stream, Alias),
		!
	).


set_prolog_flag(Flag, Value) :-
	prolog_flag(Flag, _, Value).


sub_atom(Atom, Before, Length, After, SubAtom) :-
	substring(Atom, SubAtom, Before, Length, After).


subsumes_term(General, Specific) :-
	subsumes(General, Specific).


term_variables(Term, Variables) :-
	'$lgt_quintus_term_variables'(Term, [], List),
	'$lgt_quintus_reverse'(List, [], Variables).

'$lgt_quintus_term_variables'(Term, Acc, Variables) :-
	(	var(Term) ->
		(	'$lgt_quintus_var_member_chk'(Term, Acc) ->
			Variables = Acc
		;	Variables = [Term| Acc]
		)
	;	Term =.. [_| Args],
		'$lgt_quintus_term_variables_list'(Args, Acc, Variables)
	).

'$lgt_quintus_term_variables_list'([], Variables, Variables).
'$lgt_quintus_term_variables_list'([Term| Terms], Acc, Variables) :-
	'$lgt_quintus_term_variables'(Term, Acc, Acc2),
	'$lgt_quintus_term_variables_list'(Terms, Acc2, Variables).

'$lgt_quintus_var_member_chk'(Variable, [Head| Tail]) :-
	(	Variable == Head ->
		true
	;	'$lgt_quintus_var_member_chk'(Variable, Tail)
	).

'$lgt_quintus_reverse'([], Reversed, Reversed).
'$lgt_quintus_reverse'([Head| Tail], List, Reversed) :-
	'$lgt_quintus_reverse'(Tail, [Head| List], Reversed).


throw(Ball) :-
	raise_exception(Ball).


unify_with_occurs_check(Term1, Term2) :-
	unify(Term1, Term2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  de facto standard Prolog predicates that might be missing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% between(+integer, +integer, ?integer) -- library predicate


% findall(?term, +callable, ?list, +list)

findall(Term, Goal, List, Tail) :-
	findall(Term, Goal, List0),
	append(List0, Tail, List).


% forall(+callable, +callable) -- library predicate


% format(+stream_or_alias, +character_code_list_or_atom, +list) -- built-in


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


'$lgt_predicate_property'(Pred, static) :-
	predicate_property(Pred, compiled).
'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable) -- not supported


% call/2-7

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog non-standard built-in meta-predicates and meta-directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_meta_predicate'(@callable, ?callable, ?atom)

'$lgt_prolog_meta_predicate'(not(_), not(0), predicate).
'$lgt_prolog_meta_predicate'(on_exception(_, _, _), on_exception(*, 0, 0), predicate).


% '$lgt_prolog_meta_directive'(@callable, -callable)

'$lgt_prolog_meta_directive'(_, _) :-
	fail.


% '$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(@nonvar, -atom)

'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(_, _) :-
	fail.


% '$lgt_prolog_database_predicate'(@callable)

'$lgt_prolog_database_predicate'(assert(_)).
'$lgt_prolog_database_predicate'(assert(_, _)).
'$lgt_prolog_database_predicate'(asserta(_, _)).
'$lgt_prolog_database_predicate'(assertz(_, _)).
'$lgt_prolog_database_predicate'(clause(_, _, _)).



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog compiler features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, quintus).
'$lgt_prolog_feature'(prolog_version, Version) :-
	prolog_flag(version, Version).
'$lgt_prolog_feature'(prolog_compatible_version, @>=(('3.3'))).
'$lgt_prolog_feature'(prolog_conformance, lax).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, supported).
'$lgt_prolog_feature'(coinduction, unsupported).



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
	% first expand any environment variable in the path
	expanded_file_name(Path, ExpandedPath0),
	'$lgt_quintus_convert_file_path'(ExpandedPath0, ExpandedPath1),
	(	absolute_file_name(ExpandedPath1, [file_errors(fail)], ExpandedPath) ->
		true
	;	absolute_file_name(ExpandedPath1, [file_type(directory), file_errors(fail)], ExpandedPath)
	).

'$lgt_quintus_convert_file_path'(File, Converted) :-
	atom_codes(File, FileCodes),
	'$lgt_quintus_reverse_slashes'(FileCodes, ConvertedCodes),
	atom_codes(Converted, ConvertedCodes).

'$lgt_quintus_reverse_slashes'([], []).
'$lgt_quintus_reverse_slashes'([Code| Codes], [ConvertedCode| ConvertedCodes]) :-
	(	char_code('\\', Code) ->
		char_code('/', ConvertedCode)
	;	ConvertedCode = Code
	),
	'$lgt_quintus_reverse_slashes'(Codes, ConvertedCodes).


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
	'$lgt_expand_path'(Directory, ExpandedPath),
	absolute_file_name(ExpandedPath, [access(exist), file_type(directory), file_errors(fail)], _).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	absolute_file_name('.', Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	unix(cd(Directory)).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	'$lgt_expand_path'(Directory, Path),
	(	absolute_file_name(Path, [access(exist), file_type(directory), file_errors(fail)], _) ->
		true
	;	atom_concat('mkdir ', Path, Command),
		unix(system(Command))
	).


% '$lgt_directory_hash_as_atom'(+atom, -atom)
%
% returns the directory hash as an atom

'$lgt_directory_hash_as_atom'(_, '').


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
	compile(File).


% '$lgt_file_modification_time'(+atom, -nonvar)
%
% gets a file modification time, assumed to be an opaque term but comparable

'$lgt_file_modification_time'(File, Time) :-
	file_property(File, modify_time, Time).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	environ(Variable, Value).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory

'$lgt_startup_directory'(Directory) :-
	environ('LOGTALK_STARTUP_DIRECTORY', Directory0),
	'$lgt_quintus_convert_file_path'(Directory0, Directory).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	environ('LOGTALKUSER', Directory0),
	'$lgt_quintus_convert_file_path'(Directory0, Directory).


% '$lgt_home_directory'(-atom)
%
% returns the Logtalk home directory; fails if unknown

'$lgt_home_directory'(Directory) :-
	environ('LOGTALKHOME', Directory).


% '$lgt_decompose_file_name'(+atom, ?atom, ?atom, ?atom)
%
% decomposes a file path in its components; the directory must always end
% with a slash; the extension must start with a "." when defined and must
% be the empty atom when it does not exist

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


% the following auxiliary predicate is simplified version of code
% written by Per Mildner and is used here with permission
'$lgt_strrch'(Xs, G, Ys) :-
	Xs = [X| Xs1],
	(	X == G ->
		'$lgt_strrch1'(Xs1, G, Xs, Ys)
	;	'$lgt_strrch'(Xs1, G, Ys)
	).

'$lgt_strrch1'([], _G, Ys, Ys).
'$lgt_strrch1'([X| Xs1], G, Prev, Ys) :-
	(	X == G ->
		'$lgt_strrch1'(Xs1, G, [X| Xs1], Ys)
	;	'$lgt_strrch1'(Xs1, G, Prev, Ys)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	line_count(Stream, Line).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  abstraction of the standard open/4 and close/1 predicates for dealing
%  with the alias/1 option in old non-standard compliant systems
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_open'(+atom, +atom, -stream, @list)

'$lgt_open'(File, Mode, Stream, Options) :-
	(	Options = [alias(Alias)| OtherOptions] ->
		open(File, Mode, OtherOptions, Stream),
		'$lgt_quintus_save_stream_alias'(Stream, Alias)
	;	open(File, Mode, Options, Stream)
	).


% '$lgt_close'(@stream)

'$lgt_close'(Stream) :-
	retractall('$lgt_quintus_stream_alias'(Stream, _)),
	close(Stream).


:- dynamic('$lgt_quintus_stream_alias'/2).


'$lgt_quintus_save_stream_alias'(Stream, Alias) :-
	retractall('$lgt_quintus_stream_alias'(Stream, _)),
	asserta('$lgt_quintus_stream_alias'(Stream, Alias)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages) and
%  the variable names (ideally using the standard variable_names/1 option)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position, -list)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd, Variables) :-
	line_count(Stream, LineBegin),
	read_term(Stream, [variable_names(Variables)| Options], Term),
	line_count(Stream, LineEnd).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_term_expansion'(@callable, -callable)

'$lgt_prolog_term_expansion'(_, _) :-
	fail.


% '$lgt_prolog_goal_expansion'(@callable, -callable)

% some built-in predicate use a argument order different from the one that become the standard
'$lgt_prolog_goal_expansion'(read_term(Term, Options), {read_term(Options, Term)}).
'$lgt_prolog_goal_expansion'(read_term(Stream, Term, Options), {read_term(Stream, Options, Term)}).
'$lgt_prolog_goal_expansion'(open(File, Mode, Stream, Options), {open(File, Mode, Options, Stream)}).
% most arithmetic functions are implemented as predicates
'$lgt_prolog_goal_expansion'(Result is Expression, {Head}) :-
	'$lgt_quintus_arithmetic_expression_to_head'(Result is Expression, Head),
	'$lgt_quintus_arithmetic_expression_to_goal'(Expression, Result, Goal),
	'$lgt_compile_aux_clauses'([({Head} :- {Goal})]).
'$lgt_prolog_goal_expansion'(X =:= Y, {Head}) :-
	'$lgt_quintus_arithmetic_expression_to_head'(X =:= Y, Head),
	'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
	'$lgt_quintus_arithmetic_expression_to_goal'(Y, ResultY, GoalY),
	'$lgt_compile_aux_clauses'([({Head} :- {GoalX, GoalY, ResultX =:= ResultY})]).
'$lgt_prolog_goal_expansion'(X =\= Y, {Head}) :-
	'$lgt_quintus_arithmetic_expression_to_head'(X =\= Y, Head),
	'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
	'$lgt_quintus_arithmetic_expression_to_goal'(Y, ResultY, GoalY),
	'$lgt_compile_aux_clauses'([({Head} :- {GoalX, GoalY, ResultX =\= ResultY})]).
'$lgt_prolog_goal_expansion'(X < Y, {Head}) :-
	'$lgt_quintus_arithmetic_expression_to_head'(X < Y, Head),
	'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
	'$lgt_quintus_arithmetic_expression_to_goal'(Y, ResultY, GoalY),
	'$lgt_compile_aux_clauses'([({Head} :- {GoalX, GoalY, ResultX < ResultY})]).
'$lgt_prolog_goal_expansion'(X =< Y, {Head}) :-
	'$lgt_quintus_arithmetic_expression_to_head'(X =< Y, Head),
	'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
	'$lgt_quintus_arithmetic_expression_to_goal'(Y, ResultY, GoalY),
	'$lgt_compile_aux_clauses'([({Head} :- {GoalX, GoalY, ResultX =< ResultY})]).
'$lgt_prolog_goal_expansion'(X > Y, {Head}) :-
	'$lgt_quintus_arithmetic_expression_to_head'(X > Y, Head),
	'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
	'$lgt_quintus_arithmetic_expression_to_goal'(Y, ResultY, GoalY),
	'$lgt_compile_aux_clauses'([({Head} :- {GoalX, GoalY, ResultX > ResultY})]).
'$lgt_prolog_goal_expansion'(X >= Y, {Head}) :-
	'$lgt_quintus_arithmetic_expression_to_head'(X >= Y, Head),
	'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
	'$lgt_quintus_arithmetic_expression_to_goal'(Y, ResultY, GoalY),
	'$lgt_compile_aux_clauses'([({Head} :- {GoalX, GoalY, ResultX >= ResultY})]).

'$lgt_quintus_arithmetic_expression_to_head'(Expression, Head) :-
	term_variables(Expression, Variables),
	gensym('$lgt_quintus_ae_', Functor),
	Head =.. [Functor| Variables].

'$lgt_quintus_arithmetic_expression_to_goal'(Expression, Result, ExpressionGoal) :-
	(	var(Expression) ->
		ExpressionGoal = (Result is Expression)
	;	number(Expression) ->
		ExpressionGoal = (Result is Expression)
	;	'$lgt_quintus_arithmetic_function_1'(Expression, Result, X, ResultX, Goal) ->
		'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
		ExpressionGoal = (GoalX, Goal)
	;	'$lgt_quintus_arithmetic_function_2'(Expression, Result, X, ResultX, Y, ResultY, Goal) ->
		'$lgt_quintus_arithmetic_expression_to_goal'(X, ResultX, GoalX),
		'$lgt_quintus_arithmetic_expression_to_goal'(Y, ResultY, GoalY),
		ExpressionGoal = (GoalX, GoalY, Goal)
	).

'$lgt_quintus_arithmetic_function_1'(abs(X), Result, X, ResultX, abs(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(atan(X), Result, X, ResultX, atan(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(ceiling(X), Result, X, ResultX, ceiling(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(cos(X), Result, X, ResultX, cos(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(exp(X), Result, X, ResultX, exp(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(float(X), Result, X, ResultX, float(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(floor(X), Result, X, ResultX, floor(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(log(X), Result, X, ResultX, log(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(round(X), Result, X, ResultX, round(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(sign(X), Result, X, ResultX, sign(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(sin(X), Result, X, ResultX, sin(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(sqrt(X), Result, X, ResultX, sqrt(ResultX, Result)).
'$lgt_quintus_arithmetic_function_1'(truncate(X), Result, X, ResultX, truncate(ResultX, Result)).

'$lgt_quintus_arithmetic_function_2'(X + Y, Result, X, ResultX, Y, ResultY, Result is ResultX + ResultY).
'$lgt_quintus_arithmetic_function_2'(X - Y, Result, X, ResultX, Y, ResultY, Result is ResultX - ResultY).
'$lgt_quintus_arithmetic_function_2'(X * Y, Result, X, ResultX, Y, ResultY, Result is ResultX * ResultY).
'$lgt_quintus_arithmetic_function_2'(X / Y, Result, X, ResultX, Y, ResultY, Result is ResultX / ResultY).
'$lgt_quintus_arithmetic_function_2'(X // Y, Result, X, ResultX, Y, ResultY, Result is ResultX // ResultY).
'$lgt_quintus_arithmetic_function_2'(X ** Y, Result, X, ResultX, Y, ResultY, pow(ResultX, ResultY, Result)).
'$lgt_quintus_arithmetic_function_2'(X mod Y, Result, X, ResultX, Y, ResultY, Result is ResultX mod ResultY).
'$lgt_quintus_arithmetic_function_2'(min(X, Y), Result, X, ResultX, Y, ResultY, min(ResultX, ResultY, Result)).
'$lgt_quintus_arithmetic_function_2'(max(X, Y), Result, X, ResultX, Y, ResultY, max(ResultX, ResultY, Result)).



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

'$lgt_normalize_error_term'(
	error(existence_error(_, _, procedure, ':'(user, Functor/Arity), _), _),
	error(existence_error(procedure, Functor/Arity), _)
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

'$lgt_user_module_qualification'(Goal, user:Goal).



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

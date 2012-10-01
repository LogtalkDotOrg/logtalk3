%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
%
%  Adapter file for LPA MacProlog32 1.25
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



:- prolog_flag(syntax_errors, _, error).



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


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(atom_codes(_, _)).
'$lgt_iso_predicate'(atom_concat(_, _, _)).
'$lgt_iso_predicate'(catch(_, _, _)).
'$lgt_iso_predicate'(nl(_)).
'$lgt_iso_predicate'(number_codes(_, _)).
'$lgt_iso_predicate'(once(_)).
'$lgt_iso_predicate'(open(_, _, _)).
'$lgt_iso_predicate'(open(_, _, _, _)).
'$lgt_iso_predicate'(read_term(_, _, _)).
'$lgt_iso_predicate'(throw(_)).
'$lgt_iso_predicate'(write_canonical(_, _)).
'$lgt_iso_predicate'(write_term(_, _, _)).


atom_codes(Atom, Codes) :-
	name(Atom, Codes).


atom_concat(Atom1, Atom2, Atom3) :-
	nonvar(Atom1), nonvar(Atom2),
	!,
	name(Atom1, Codes1),
	name(Atom2, Codes2),
	'$lgt_append'(Codes1, Codes2, Codes3),
	name(Atom3, Codes3).

atom_concat(Atom1, Atom2, Atom3) :-
	nonvar(Atom3),
	!,
	name(Atom3, Codes3),
	'$lgt_append'(Codes1, Codes2, Codes3),
	name(Atom1, Codes1),
	name(Atom2, Codes2).


lpa_catch(Error, Goal) :-
	catch(Error, Goal).


:- hide(catch).


catch(Error, Goal) :-
	lpa_catch(Error, Goal).


catch(Goal, Catcher, Recovery) :-
	lpa_catch(Error, Goal),
	(Error = 0 ->
		true
		;
		(Error = -1 ->
			fail
			;
			(Error = 999 ->
				retract(lgt_exception_(Ball)),
				!,
				(Catcher = Ball ->
					call(Recovery)
					;
					throw(Ball))
				;
				error_message(Error, Message),
				(Catcher = Message ->
					call(Recovery)
					;
					write(Message), nl, abort)))).


nl(Stream) :-
	nl ~> Stream.


number_codes(Number, Codes) :-
	name(Number, Codes).


once(Goal) :-
	one(Goal).


open(File, Mode, File) :-
	open(File, Mode).


open(File, Mode, File, []) :-
	open(File, Mode).


read_term(Stream, Term, [singletons([])]) :-
	!,
	read(Term) <~ Stream.

read_term(Stream, Term, _) :-
	read(Term) <~ Stream.


throw(Error) :-
	asserta(lgt_exception_(Error)),
	throw(999, Error).


write_canonical(Stream, Term) :-
	writeq(Term) ~> Stream.


write_term(Stream, Term, [quoted(true)]) :-
	!,
	writeq(Term) ~> Stream.

write_term(Stream, Term, _) :-
	write(Term) ~> Stream.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup_call_cleanup(+callable, +callable, +callable)

setup_call_cleanup(_, _, _) :-
	throw(not_supported(setup_call_cleanup/3)).


% forall(+callable, +callable) -- built-in


% call/2-7

call(F, A) :-
	F(A).

call(F, A1, A2) :-
	F(A1, A2).

call(F, A1, A2, A3) :-
	F(A1, A2, A3).

call(F, A1, A2, A3, A4) :-
	F(A1, A2, A3, A4).

call(F, A1, A2, A3, A4, A5) :-
	F(A1, A2, A3, A4, A5).

call(F, A1, A2, A3, A4, A5, A6) :-
	F(A1, A2, A3, A4, A5, A6).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog non-standard built-in meta-predicates and meta-directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_meta_predicate'(?callable, ?atom).

'$lgt_prolog_meta_predicate'(_, _) :-
	fail.


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, lpa).
'$lgt_prolog_feature'(prolog_version, _) :-
	fail.
'$lgt_prolog_feature'(prolog_compatible_version, ==((1,25))).

'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, unsupported).
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
'$lgt_default_flag'(scratch_directory, './lgt_tmp/').
'$lgt_default_flag'(report, on).
'$lgt_default_flag'(clean, off).
'$lgt_default_flag'(smart_compilation, off).
'$lgt_default_flag'(reload, always).
'$lgt_default_flag'(code_prefix, '$').
'$lgt_default_flag'(optimize, on).
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

'$lgt_expand_path'(_, _) :-
	fail.


% '$lgt_file_exists'(+atom)
%
% checks if a file exists

'$lgt_file_exists'(File) :-
	absolute_file_name(File, [access(exist)], _).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(_) :-
	fail.


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	absolute_file_name(Directory, [access(exist)], _),
	absolute_file_name(Directory, [file_type(directory)], _).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	var(Directory),
	dvol(Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	dvol(Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(_) :-
	throw(not_supported('$lgt_make_directory'/1)).


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
	reconsult(File).


% '$lgt_compare_file_modification_times'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_modification_times'(Result, File1, File2) :-
	fmdatetime(File1, Time1),
	fmdatetime(File2, Time2),
	compare(Result, Time1, Time2).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(_, _) :-
	fail.


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory 

'$lgt_startup_directory'(Directory) :-
	fail.


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(_) :-
	fail.


% '$lgt_home_directory'(-atom)
%
% returns the Logtalk home directory; fails if unknown

'$lgt_home_directory'(_) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  sorting predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_keysort'(+list, -list)

'$lgt_keysort'(List, Sorted) :-
	keysort(List, Sorted).


% '$lgt_sort'(+list, -list)

'$lgt_sort'(List, Sorted) :-
	sort(List, Sorted).



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
	date(Day, Month, Year).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	time(Hours, Mins, Secs, _).



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
	ticks(Ticks),
	Seconds is Ticks / 60.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  read character predicate
%
%  read a single character echoing it and writing a newline after
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_read_single_char'(Char) :-
	get(Code), name(Char, [Code]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, Options, -1) :-
	read_term(Stream, Term, Options).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog dialect specific term and goal expansion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_term_expansion'(@callable, -callable)

'$lgt_pl_term_expansion'(_, _) :-
	fail.


% '$lgt_pl_goal_expansion'(@callable, -callable)

'$lgt_pl_goal_expansion'(_, _) :-
	fail.



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

'$lgt_normalize_error_term'(
	Error,
	Error
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

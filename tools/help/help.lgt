%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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


:- object(help).

	:- info([
		version is 0.12,
		author is 'Paulo Moura',
		date is 2013/12/12,
		comment is 'Command-line help for Logtalk built-in control constructs, predicates, non-terminals, and methods.'
	]).

	:- initialization((nl, write('For help on Logtalk, type help::help.'), nl)).

	:- public(help/0).
	:- mode(help, one).
	:- info(help/0, [
		comment is 'Prints instructions on how to use help.'
	]).

	help :-
		write('On-line help is available for Logtalk built-in control constructs,'), nl,
		write('built-in directives, built-in predicates, built-in non-terminals,'), nl,
		write('built-in methods, and the standard library:'), nl, nl,
		write('  Type help::Functor/Arity.'), nl,
		write('  Or   help::Functor//Arity. '), nl,
		write('  Or   help::library.'), nl,
		write('  Or   help::library(Entity).'), nl, nl,
		write('The manual web page for the selected built-in feature will open in'), nl,
		write('your default web browser. To consult the Logtalk User and Reference'), nl,
		write('manuals:'), nl, nl,
		write('  Type help::manuals.'), nl, nl.

	:- public(('/')/2).
	:- mode('/'(+atom, +integer), zero_or_one).
	:- info(('/')/2, [
		comment is 'Provides help on the Functor/Arity built-in control construct, directive, predicate, or method.',
		argnames is ['Functor', 'Arity']
	]).

	Functor/Arity :-
		forall(
			(	built_in_directive(Functor, Arity, Path, File)
			;	built_in_method(Functor, Arity, Path, File)
			;	built_in_predicate(Functor, Arity, Path, File)
			;	control(Functor, Arity, Path, File)
			),
			open(Path, File)
		).

	:- public(('//')/2).
	:- mode('//'(+atom, +integer), zero_or_one).
	:- info(('//')/2, [
		comment is 'Provides help on the Functor//Arity built-in non-terminal.',
		argnames is ['Functor', 'Arity']
	]).

	NonTerminalFunctor//Arity :-
		forall(
			built_in_non_terminal(NonTerminalFunctor, Arity, Path, File),
			open(Path, File)
		).

	:- public(completion/2).
	:- mode(completion(+atom, -pair), zero_or_more).
	:- info(completion/2, [
		comment is 'Provides a completion pair (Completion-Page) for a given prefix.',
		argnames is ['Prefix', 'Completion']
	]).

	completion(Prefix, Completion-Page) :-
		(	built_in_directive(Functor, Arity, Path, File),
			Completion = Functor/Arity
		;	built_in_method(Functor, Arity, Path, File),
			Completion = Functor/Arity
		;	built_in_predicate(Functor, Arity, Path, File),
			Completion = Functor/Arity
		;	built_in_non_terminal(Functor, Arity, Path, File),
			Completion = Functor//Arity
		;	control(Functor, Arity, Path, File),
			Completion = Functor/Arity
		),
		sub_atom(Functor, 0, _, _, Prefix),
		atom_concat('$LOGTALKHOME', Path, Page0),
		atom_concat(Page0, File, Page1),
		os::expand_path(Page1, Page).

	:- public(completions/2).
	:- mode(completions(+atom, -lists(pair)), zero_or_more).
	:- info(completions/2, [
		comment is 'Provides a list of completions pairs (Completion-Page) for a given prefix.',
		argnames is ['Prefix', 'Completions']
	]).

	completions(Prefix, Completions) :-
		findall(Completion, completion(Prefix, Completion), Completions).

	:- public(built_in_directive/4).
	:- mode(built_in_directive(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_directive/4, [
		comment is 'Provides acess to the HTML documenting files describing built-in directives.',
		argnames is ['Functor', 'Arity', 'Directory', 'Basename']
	]).

	built_in_directive(encoding, 1, '/manuals/refman/directives/', 'encoding1.html').
	built_in_directive(initialization, 1, '/manuals/refman/directives/', 'initialization1.html').
	built_in_directive(op, 3, '/manuals/refman/directives/', 'op3.html').
	built_in_directive(set_logtalk_flag, 2, '/manuals/refman/directives/', 'set_logtalk_flag2.html').

	built_in_directive(if, 1, '/manuals/refman/directives/', 'if1.html').
	built_in_directive(elif, 1, '/manuals/refman/directives/', 'elif1.html').
	built_in_directive(else, 0, '/manuals/refman/directives/', 'else0.html').
	built_in_directive(endif, 0, '/manuals/refman/directives/', 'endif0.html').

	built_in_directive(calls, 1, '/manuals/refman/directives/', 'calls1.html').
	built_in_directive(category, N, '/manuals/refman/directives/', 'category1_3.html') :-
		integer::between(1, 3, N).
	built_in_directive(dynamic, 0, '/manuals/refman/directives/', 'dynamic0.html').
	built_in_directive(end_category, 0, '/manuals/refman/directives/', 'end_category0.html').
	built_in_directive(end_object, 0, '/manuals/refman/directives/', 'end_object0.html').
	built_in_directive(end_protocol, 0, '/manuals/refman/directives/', 'end_protocol0.html').
	built_in_directive(info, 1, '/manuals/refman/directives/', 'info1.html').
	built_in_directive(initialization, 1, '/manuals/refman/directives/', 'initialization1.html').
	built_in_directive(object, N, '/manuals/refman/directives/', 'object1_5.html') :-
		integer::between(1, 5, N).
	built_in_directive(protocol, N, '/manuals/refman/directives/', 'protocol1_2.html') :-
		integer::between(1, 2, N).
	built_in_directive(synchronized, 0, '/manuals/refman/directives/', 'synchronized0.html').
	built_in_directive(threaded, 0, '/manuals/refman/directives/', 'threaded0.html').
	built_in_directive(uses, 1, '/manuals/refman/directives/', 'uses1.html').

	built_in_directive(alias, 3, '/manuals/refman/directives/', 'alias3.html').
	built_in_directive(coinductive, 1, '/manuals/refman/directives/', 'coinductive1.html').
	built_in_directive(discontiguous, 1, '/manuals/refman/directives/', 'discontiguous1.html').
	built_in_directive(dynamic, 1, '/manuals/refman/directives/', 'dynamic1.html').
	built_in_directive(info, 2, '/manuals/refman/directives/', 'info2.html').
	built_in_directive(meta_predicate, 1, '/manuals/refman/directives/', 'meta_predicate1.html').
	built_in_directive(meta_non_terminal, 1, '/manuals/refman/directives/', 'meta_non_terminal1.html').
	built_in_directive(mode, 2, '/manuals/refman/directives/', 'mode2.html').
	built_in_directive(multifile, 1, '/manuals/refman/directives/', 'multifile1.html').
	built_in_directive(op, 3, '/manuals/refman/directives/', 'op3.html').
	built_in_directive(private, 1, '/manuals/refman/directives/', 'private1.html').
	built_in_directive(protected, 1, '/manuals/refman/directives/', 'protected1.html').
	built_in_directive(public, 1, '/manuals/refman/directives/', 'public1.html').
	built_in_directive(synchronized, 1, '/manuals/refman/directives/', 'synchronized1.html').
	built_in_directive(uses, 2, '/manuals/refman/directives/', 'uses2.html').
	built_in_directive(use_module, 2, '/manuals/refman/directives/', 'use_module2.html').

	:- public(built_in_predicate/4).
	:- mode(built_in_predicate(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_predicate/4, [
		comment is 'Provides acess to the HTML documenting files describing built-in predicates.',
		argnames is ['Functor', 'Arity', 'Directory', 'Basename']
	]).

	built_in_predicate(current_category, 1, '/manuals/refman/builtins/', 'current_category1.html').
	built_in_predicate(current_object, 1, '/manuals/refman/builtins/', 'current_object1.html').
	built_in_predicate(current_protocol, 1, '/manuals/refman/builtins/', 'current_protocol1.html').

	built_in_predicate(category_property, 2, '/manuals/refman/builtins/', 'category_property2.html').
	built_in_predicate(object_property, 2, '/manuals/refman/builtins/', 'object_property2.html').
	built_in_predicate(protocol_property, 2, '/manuals/refman/builtins/', 'protocol_property2.html').

	built_in_predicate(create_category, 4, '/manuals/refman/builtins/', 'create_category4.html').
	built_in_predicate(create_object, 4, '/manuals/refman/builtins/', 'create_object4.html').
	built_in_predicate(create_protocol, 3, '/manuals/refman/builtins/', 'create_protocol3.html').

	built_in_predicate(abolish_category, 1, '/manuals/refman/builtins/', 'abolish_category1.html').
	built_in_predicate(abolish_object, 1, '/manuals/refman/builtins/', 'abolish_object1.html').
	built_in_predicate(abolish_protocol, 1, '/manuals/refman/builtins/', 'abolish_protocol1.html').

	built_in_predicate(extends_object, 2, '/manuals/refman/builtins/', 'extends_object2_3.html').
	built_in_predicate(extends_object, 3, '/manuals/refman/builtins/', 'extends_object2_3.html').
	built_in_predicate(extends_protocol, 2, '/manuals/refman/builtins/', 'extends_protocol2_3.html').
	built_in_predicate(extends_protocol, 3, '/manuals/refman/builtins/', 'extends_protocol2_3.html').
	built_in_predicate(extends_category, 2, '/manuals/refman/builtins/', 'extends_category2_3.html').
	built_in_predicate(extends_category, 3, '/manuals/refman/builtins/', 'extends_category2_3.html').
	built_in_predicate(implements_protocol, 2, '/manuals/refman/builtins/', 'implements_protocol2_3.html').
	built_in_predicate(implements_protocol, 3, '/manuals/refman/builtins/', 'implements_protocol2_3.html').
	built_in_predicate(imports_category, 2, '/manuals/refman/builtins/', 'imports_category2_3.html').
	built_in_predicate(imports_category, 3, '/manuals/refman/builtins/', 'imports_category2_3.html').
	built_in_predicate(instantiates_class, 2, '/manuals/refman/builtins/', 'instantiates_class2_3.html').
	built_in_predicate(instantiates_class, 3, '/manuals/refman/builtins/', 'instantiates_class2_3.html').
	built_in_predicate(specializes_class, 2, '/manuals/refman/builtins/', 'specializes_class2_3.html').
	built_in_predicate(specializes_class, 3, '/manuals/refman/builtins/', 'specializes_class2_3.html').
	built_in_predicate(complements_object, 2, '/manuals/refman/builtins/', 'complements_object2.html').

	built_in_predicate(abolish_events, 5, '/manuals/refman/builtins/', 'abolish_events5.html').
	built_in_predicate(current_event, 5, '/manuals/refman/builtins/', 'current_event5.html').
	built_in_predicate(define_events, 5, '/manuals/refman/builtins/', 'define_events5.html').

	built_in_predicate(threaded, 1, '/manuals/refman/builtins/', 'threaded1.html').
	built_in_predicate(threaded_call, 1, '/manuals/refman/builtins/', 'threaded_call1_2.html').
	built_in_predicate(threaded_call, 2, '/manuals/refman/builtins/', 'threaded_call1_2.html').
	built_in_predicate(threaded_once, 1, '/manuals/refman/builtins/', 'threaded_once1_2.html').
	built_in_predicate(threaded_once, 2, '/manuals/refman/builtins/', 'threaded_once1_2.html').
	built_in_predicate(threaded_ignore, 1, '/manuals/refman/builtins/', 'threaded_ignore1.html').
	built_in_predicate(threaded_exit, 1, '/manuals/refman/builtins/', 'threaded_exit1_2.html').
	built_in_predicate(threaded_exit, 2, '/manuals/refman/builtins/', 'threaded_exit1_2.html').
	built_in_predicate(threaded_peek, 1, '/manuals/refman/builtins/', 'threaded_peek1_2.html').
	built_in_predicate(threaded_peek, 2, '/manuals/refman/builtins/', 'threaded_peek1_2.html').
	built_in_predicate(threaded_wait, 1, '/manuals/refman/builtins/', 'threaded_wait1.html').
	built_in_predicate(threaded_notify, 1, '/manuals/refman/builtins/', 'threaded_notify1.html').

	built_in_predicate(logtalk_compile, 1, '/manuals/refman/builtins/', 'logtalk_compile1.html').
	built_in_predicate(logtalk_compile, 2, '/manuals/refman/builtins/', 'logtalk_compile2.html').
	built_in_predicate(logtalk_load, 1, '/manuals/refman/builtins/', 'logtalk_load1.html').
	built_in_predicate(logtalk_load, 2, '/manuals/refman/builtins/', 'logtalk_load2.html').
	built_in_predicate(logtalk_library_path, 2, '/manuals/refman/builtins/', 'logtalk_library_path2.html').
	built_in_predicate(logtalk_load_context, 2, '/manuals/refman/builtins/', 'logtalk_load_context2.html').

	built_in_predicate(current_logtalk_flag, 2, '/manuals/refman/builtins/', 'current_logtalk_flag2.html').
	built_in_predicate(set_logtalk_flag, 2, '/manuals/refman/builtins/', 'set_logtalk_flag2.html').

	:- public(built_in_method/4).
	:- mode(built_in_method(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_method/4, [
		comment is 'Provides acess to the HTML documenting files describing built-in methods.',
		argnames is ['Functor', 'Arity', 'Directory', 'Basename']
	]).

	built_in_method(parameter, 2, '/manuals/refman/methods/', 'parameter2.html').
	built_in_method(self, 1, '/manuals/refman/methods/', 'self1.html').
	built_in_method(sender, 1, '/manuals/refman/methods/', 'sender1.html').
	built_in_method(this, 1, '/manuals/refman/methods/', 'this1.html').

	built_in_method(current_op, 3, '/manuals/refman/methods/', 'current_op3.html').
	built_in_method(current_predicate, 1, '/manuals/refman/methods/', 'current_predicate1.html').
	built_in_method(predicate_property, 2, '/manuals/refman/methods/', 'predicate_property2.html').

	built_in_method(abolish, 1, '/manuals/refman/methods/', 'abolish1.html').
	built_in_method(asserta, 1, '/manuals/refman/methods/', 'asserta1.html').
	built_in_method(assertz, 1, '/manuals/refman/methods/', 'assertz1.html').
	built_in_method(clause, 2, '/manuals/refman/methods/', 'clause2.html').
	built_in_method(retract, 1, '/manuals/refman/methods/', 'retract1.html').
	built_in_method(retractall, 1, '/manuals/refman/methods/', 'retractall1.html').

	built_in_method(call, N, '/manuals/refman/methods/', 'callN.html') :-
		integer::between(1, 8, N).
	built_in_method(once, 1, '/manuals/refman/methods/', 'once1.html').
	built_in_method((\+), 1, '/manuals/refman/methods/', 'not1.html').

	built_in_method(catch, 3, '/manuals/refman/methods/', 'catch3.html').
	built_in_method(throw, 1, '/manuals/refman/methods/', 'throw1.html').

	built_in_method(bagof, 3, '/manuals/refman/methods/', 'bagof3.html').
	built_in_method(findall, 3, '/manuals/refman/methods/', 'findall3.html').
	built_in_method(forall, 2, '/manuals/refman/methods/', 'forall2.html').
	built_in_method(setof, 3, '/manuals/refman/methods/', 'setof3.html').

	built_in_method(before, 3, '/manuals/refman/methods/', 'before3.html').
	built_in_method(after, 3, '/manuals/refman/methods/', 'after3.html').

	built_in_method(forward, 1, '/manuals/refman/methods/', 'forward1.html').

	built_in_method(phrase, 2, '/manuals/refman/methods/', 'phrase2.html').
	built_in_method(phrase, 3, '/manuals/refman/methods/', 'phrase3.html').

	built_in_method(expand_term, 2, '/manuals/refman/methods/', 'expand_term2.html').
	built_in_method(term_expansion, 2, '/manuals/refman/methods/', 'term_expansion2.html').
	built_in_method(expand_goal, 2, '/manuals/refman/methods/', 'expand_goal2.html').
	built_in_method(goal_expansion, 2, '/manuals/refman/methods/', 'goal_expansion2.html').

	:- public(control/4).
	:- mode(control(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(control/4, [
		comment is 'Provides acess to the HTML documenting files describing built-in control constructs.',
		argnames is ['Functor', 'Arity', 'Directory', 'Basename']
	]).

	control((::), 2, '/manuals/refman/control/', 'to_object2.html').
	control('[]', 1, '/manuals/refman/control/', 'delegate1.html').
	control((::), 1, '/manuals/refman/control/', 'to_self1.html').
	control((^^), 1, '/manuals/refman/control/', 'to_super1.html').
	control(({}), 1, '/manuals/refman/control/', 'external1.html').
	control((<<), 2, '/manuals/refman/control/', 'context2.html').

	:- public(built_in_non_terminal/4).
	:- mode(built_in_non_terminal(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_non_terminal/4, [
		comment is 'Provides acess to the HTML documenting files describing built-in DCG non-terminals.',
		argnames is ['Functor', 'Arity', 'Directory', 'Basename']
	]).

	built_in_non_terminal(call, N, '/manuals/refman/methods/', 'call1.html') :-
		integer::between(1, 6, N).
	built_in_non_terminal(phrase, 1, '/manuals/refman/methods/', 'phrase1.html').

	:- public(library/0).
	:- mode(library, one).
	:- info(library/0, [
		comment is 'Provides help on the standard Logtalk library.'
	]).

	library :-
		open('/docs/', 'library.html').

	:- public(library/1).
	:- mode(library(+entity_identifier), zero_or_one).
	:- info(library/1, [
		comment is 'Provides help on the standard Logtalk library.',
		argnames is ['Entity']
	]).

	library(Entity) :-
		nonvar(Entity),
		callable(Entity),
		functor(Entity, Functor, Arity),
		atom_concat(Functor, '_', File0),
		number_chars(Arity, ArityChars),
		atom_chars(ArityAtom, ArityChars),
		atom_concat(File0, ArityAtom, File1),
		atom_concat(File1, '.html', File),
		open('/docs/', File).

	:- public(manuals/0).
	:- mode(manuals, one).
	:- info(manuals/0, [
		comment is 'Provides access to the Logtalk User and Reference manuals.'
	]).

	manuals :-
		open('/manuals/', 'index.html').

	open(Path, File) :-
		(	\+ os::environment_variable('LOGTALKHOME', _) ->
			write('The environment variable LOGTALKHOME must be defined and pointing to your'), nl,
			write('Logtalk installation folder in order for on-line help to be available.'), nl, nl
		;	os::environment_variable('COMSPEC', _) ->
			% assume we're running on Windows
			convert_file_path(Path, ConvertedPath),
			atom_concat('%LOGTALKHOME%', ConvertedPath, FullPath),
			atom_concat('cmd /c start /D"', FullPath, Command0),
			atom_concat(Command0, '" ', Command1),
			atom_concat(Command1, File, Command),
			os::shell(Command)
		;	os::shell('uname -s | grep Darwin 1> /dev/null') ->
			% assume we're running on MacOS X
			atom_concat('open "$LOGTALKHOME', Path, Command0),
			atom_concat(Command0, File, Command1),
			atom_concat(Command1, '" > /dev/null 2>&1', Command),
			os::shell(Command)
		;	os::shell('uname -s | grep Linux 1> /dev/null') ->
			% assume we're running on Linux
			atom_concat('xdg-open "$LOGTALKHOME', Path, Command0),
			atom_concat(Command0, File, Command1),
			atom_concat(Command1, '" > /dev/null 2>&1', Command),
			os::shell(Command)
		;	% we couldn't find which operating-system are we running on
			write('Unsupported operating-system.'), nl
		).

	convert_file_path(File, Converted) :-
		atom_chars(File, FileChars),
		reverse_slashes(FileChars, ConvertedChars),
		atom_chars(Converted, ConvertedChars).

	reverse_slashes([], []).
	reverse_slashes([Char| Chars], [ConvertedChar| ConvertedChars]) :-
		(	Char == ('/') ->
			ConvertedChar = ('\\')
		;	ConvertedChar = Char
		),
		reverse_slashes(Chars, ConvertedChars).

:- end_object.

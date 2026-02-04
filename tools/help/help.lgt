%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(help,
	implements(forwarding)).

	:- info([
		version is 0:43:0,
		author is 'Paulo Moura',
		date is 2026-02-04,
		comment is 'Command-line help for Logtalk tools, libraries, entities, predicates, and non-terminals.'
	]).

	% only allow complementing categories to add new functionality:
	:- set_logtalk_flag(complements, allow).

	:- initialization((
		nl, write('For help on Logtalk, type help::help.'), nl, nl,
		ignore(cache_epub_xhtml_files)
	)).

	forward(Message) :-
		writeq(Message), write(' is not a valid help request.'), nl,
		help.

	:- public(help/0).
	:- mode(help, one).
	:- info(help/0, [
		comment is 'Provides instructions on how to use the help tool.'
	]).

	:- public(handbook/0).
	:- mode(handbook, one).
	:- info(handbook/0, [
		comment is 'Provides access to the Handbook.'
	]).

	:- public(apis/0).
	:- mode(apis, one).
	:- info(apis/0, [
		comment is 'Provides access to the APIs documentation.'
	]).

	:- public(apis/1).
	:- mode(apis(+predicate_indicator), one).
	:- mode(apis(+non_terminal_indicator), one).
	:- info(apis/1, [
		comment is 'Provides help on the given predicate or non-terminal.',
		argnames is ['Indicator']
	]).

	:- public(tools/0).
	:- mode(tools, one).
	:- info(tools/0, [
		comment is 'Provides access to the developer tools documentation.'
	]).

	:- public(tool/1).
	:- mode(tool(+atom), zero_or_one).
	:- info(tool/1, [
		comment is 'Provides help on the given developer tool.',
		argnames is ['Tool']
	]).

	:- public(libraries/0).
	:- mode(libraries, one).
	:- info(libraries/0, [
		comment is 'Provides access to the standard libraries documentation.'
	]).

	:- public(library/1).
	:- mode(library(+atom), zero_or_one).
	:- info(library/1, [
		comment is 'Provides help on the given standard library.',
		argnames is ['Library']
	]).

	:- public(entity/1).
	:- mode(entity(+entity_identifier), zero_or_one).
	:- info(entity/1, [
		comment is 'Provides help on the given built-in, tool, and library entity (object, protocol, or category).',
		argnames is ['Entity']
	]).

	:- public(('/')/2).
	:- mode('/'(+atom, +integer), zero_or_one).
	:- info(('/')/2, [
		comment is 'Provides help on the ``Name/Arity`` built-in control construct, directive, predicate, or method.',
		argnames is ['Name', 'Arity']
	]).

	:- public(('//')/2).
	:- mode('//'(+atom, +integer), zero_or_one).
	:- info(('//')/2, [
		comment is 'Provides help on the ``Name//Arity`` built-in non-terminal.',
		argnames is ['Name', 'Arity']
	]).

	:- public(man/1).
	:- mode(man(+atom), one).
	:- info(man/1, [
		comment is 'Opens the man page of the given script. On POSIX systems, the page is open inline. On Windows system, the HTML version of the man page is open on the operating-system default browser.',
		argnames is ['Page']
	]).

	:- public(completion/2).
	:- mode(completion(+atom, -pair), zero_or_more).
	:- info(completion/2, [
		comment is 'Provides a completion pair, ``Completion-Page``, for a given prefix.',
		argnames is ['Prefix', 'Completion']
	]).

	:- public(completions/2).
	:- mode(completions(+atom, -lists(pair)), zero_or_more).
	:- info(completions/2, [
		comment is 'Provides a list of completions pairs, ``Completion-Page``, for a given prefix.',
		argnames is ['Prefix', 'Completions']
	]).

	:- public(built_in_directive/4).
	:- mode(built_in_directive(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_directive/4, [
		comment is 'Provides access to the HTML documenting files describing built-in directives.',
		argnames is ['Name', 'Arity', 'Directory', 'Basename']
	]).

	:- public(built_in_predicate/4).
	:- mode(built_in_predicate(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_predicate/4, [
		comment is 'Provides access to the HTML documenting files describing built-in predicates.',
		argnames is ['Name', 'Arity', 'Directory', 'Basename']
	]).

	:- public(built_in_method/4).
	:- mode(built_in_method(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_method/4, [
		comment is 'Provides access to the HTML documenting files describing built-in methods.',
		argnames is ['Name', 'Arity', 'Directory', 'Basename']
	]).

	:- public(control_construct/4).
	:- mode(control_construct(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(control_construct/4, [
		comment is 'Provides access to the HTML documenting files describing built-in control constructs.',
		argnames is ['Name', 'Arity', 'Directory', 'Basename']
	]).

	:- public(built_in_non_terminal/4).
	:- mode(built_in_non_terminal(?atom, ?integer, -atom, -atom), zero_or_more).
	:- info(built_in_non_terminal/4, [
		comment is 'Provides access to the HTML documenting files describing built-in DCG non-terminals.',
		argnames is ['Name', 'Arity', 'Directory', 'Basename']
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(os, [
		absolute_file_name/2, copy_file/2, decompose_file_name/4,
		environment_variable/2, file_exists/1, file_modification_time/2,
		make_directory_path/1, shell/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	help :-
		nl,
		write('Help tool predicates open documentation either inline using a terminal-based'), nl,
		write('browser (when available) or in the operating-system default browser.'), nl, nl,
		write('To consult the Logtalk Handbook and APIs documentation:'), nl, nl,
		write('    help::handbook.          help::apis.'), nl, nl,
		write('To consult the documentation of Logtalk built-in control constructs, methods,'), nl,
		write('directives, predicates, and non-terminals:'), nl, nl,
		write('    help::Name/Arity.        help::Name//Arity.'), nl, nl,
		write('To consult the documentation of Logtalk developer tools and libraries plus'), nl,
		write('their APIs and entities:'), nl, nl,
		write('    help::tools.             help::tool(Tool).'), nl,
		write('    help::libraries.         help::library(Library).'), nl,
		write('    help::apis(Name/Arity).  help::apis(Name//Arity).'), nl,
		write('    help::entity(Entity).'), nl, nl,
		write('To compile and load source files, the following shortcut can be used:'), nl, nl,
		write('    {File1, File2, ...}.'), nl, nl,
		write('To recompile and reload modified files, the following shortcut can be used:'), nl, nl,
		write('    {*}.'), nl, nl,
		write('To recompile source files for debugging, the following shortcut can be used:'), nl, nl,
		write('    {+d}.'), nl, nl,
		write('Next, load the debugger and start tracing:'), nl, nl,
		write('    {debugger(loader)}, debugger::trace.'), nl, nl,
		write('To learn more about the available top-level shortcuts:'), nl, nl,
		write('    help::logtalk_load/1.            help::logtalk_make/1.'), nl, nl,
		write('Hint: you can preload the debugger (and other developer tools) from your'), nl,
		write('settings file (see the samples/settings-sample.lgt file for instructions).'), nl, nl.

	Name/Arity :-
		atom(Name),
		findall(
			Path-File,
			(	built_in_directive(Name, Arity, Path, File)
			;	built_in_method(Name, Arity, Path, File)
			;	built_in_predicate(Name, Arity, Path, File)
			;	control_construct(Name, Arity, Path, File)
			),
			Hits
		),
		show(Hits, Name/Arity).

	Name//Arity :-
		atom(Name),
		findall(
			Path-File,
			built_in_non_terminal(Name, Arity, Path, File),
			Hits
		),
		show(Hits, Name//Arity).

	show([], Search) :-
		fuzzy_matching(Search).
	show([Path-File| Hits], Search) :-
		(	Hits == [] ->
			open(Path, File)
		;	fuzzy_matching(Search)
		).

	fuzzy_matching(Name/Arity) :-
		findall(
			ExpandedName/Arity,
			completion(Name, ExpandedName/Arity-_),
			ExpandedPIs
		),
		alternatives(ExpandedPIs, Name/Arity).
	fuzzy_matching(Name//Arity) :-
		findall(
			ExpandedName//Arity,
			completion(Name, ExpandedName//Arity-_),
			ExpandedNTIs
		),
		alternatives(ExpandedNTIs, Name//Arity).

	alternatives([], Search) :-
		write('No help available for '), writeq(Search), write('.'), nl,
		write('Not a Logtalk built-in directive, predicate, non-terminal, or control construct.'), nl,
		write('Looking for a tool or library predicate instead? Try help::apis/1.'), nl,
		fail.
	alternatives([Alternative| Alternatives], _) :-
		(	Alternatives == [] ->
			write('Do you mean instead:'), nl
		;	write('Do you mean instead one of:'), nl
		),
		write_alternatives([Alternative| Alternatives]).

	write_alternatives([]).
	write_alternatives([Alternative| Alternatives]) :-
		write('  '), writeq(Alternative), nl,
		write_alternatives(Alternatives).

	completion(Prefix, Completion-Page) :-
		(	built_in_directive(Name, Arity, Path, File),
			Completion = Name/Arity
		;	built_in_method(Name, Arity, Path, File),
			Completion = Name/Arity
		;	built_in_predicate(Name, Arity, Path, File),
			Completion = Name/Arity
		;	built_in_non_terminal(Name, Arity, Path, File),
			Completion = Name//Arity
		;	control_construct(Name, Arity, Path, File),
			Completion = Name/Arity
		),
		sub_atom(Name, 0, _, _, Prefix),
		atom_concat('$LOGTALKHOME', Path, Page0),
		atom_concat(Page0, File, Page1),
		absolute_file_name(Page1, Page).

	completions(Prefix, Completions) :-
		findall(Completion, completion(Prefix, Completion), Completions).

	built_in_directive(encoding, 1, '/docs/handbook/refman/directives/', 'encoding_1.html').
	built_in_directive(set_logtalk_flag, 2, '/docs/handbook/refman/directives/', 'set_logtalk_flag_2.html').

	built_in_directive(if, 1, '/docs/handbook/refman/directives/', 'if_1.html').
	built_in_directive(elif, 1, '/docs/handbook/refman/directives/', 'elif_1.html').
	built_in_directive(else, 0, '/docs/handbook/refman/directives/', 'else_0.html').
	built_in_directive(endif, 0, '/docs/handbook/refman/directives/', 'endif_0.html').

	built_in_directive(category, N, '/docs/handbook/refman/directives/', 'category_1_4.html') :-
		between(1, 4, N).
	built_in_directive(dynamic, 0, '/docs/handbook/refman/directives/', 'dynamic_0.html').
	built_in_directive(end_category, 0, '/docs/handbook/refman/directives/', 'end_category_0.html').
	built_in_directive(end_object, 0, '/docs/handbook/refman/directives/', 'end_object_0.html').
	built_in_directive(end_protocol, 0, '/docs/handbook/refman/directives/', 'end_protocol_0.html').
	built_in_directive(built_in, 0, '/docs/handbook/refman/directives/', 'built_in_0.html').
	built_in_directive(include, 1, '/docs/handbook/refman/directives/', 'include_1.html').
	built_in_directive(info, 1, '/docs/handbook/refman/directives/', 'info_1.html').
	built_in_directive(initialization, 1, '/docs/handbook/refman/directives/', 'initialization_1.html').
	built_in_directive(object, N, '/docs/handbook/refman/directives/', 'object_1_5.html') :-
		between(1, 5, N).
	built_in_directive(protocol, N, '/docs/handbook/refman/directives/', 'protocol_1_2.html') :-
		between(1, 2, N).
	built_in_directive(threaded, 0, '/docs/handbook/refman/directives/', 'threaded_0.html').
	built_in_directive(uses, 1, '/docs/handbook/refman/directives/', 'uses_1.html').

	built_in_directive(alias, 2, '/docs/handbook/refman/directives/', 'alias_2.html').
	built_in_directive(coinductive, 1, '/docs/handbook/refman/directives/', 'coinductive_1.html').
	built_in_directive(discontiguous, 1, '/docs/handbook/refman/directives/', 'discontiguous_1.html').
	built_in_directive(dynamic, 1, '/docs/handbook/refman/directives/', 'dynamic_1.html').
	built_in_directive(info, 2, '/docs/handbook/refman/directives/', 'info_2.html').
	built_in_directive(meta_predicate, 1, '/docs/handbook/refman/directives/', 'meta_predicate_1.html').
	built_in_directive(meta_non_terminal, 1, '/docs/handbook/refman/directives/', 'meta_non_terminal_1.html').
	built_in_directive(mode, 2, '/docs/handbook/refman/directives/', 'mode_2.html').
	built_in_directive(mode_non_terminal, 2, '/docs/handbook/refman/directives/', 'mode_non_terminal_2.html').
	built_in_directive(multifile, 1, '/docs/handbook/refman/directives/', 'multifile_1.html').
	built_in_directive(op, 3, '/docs/handbook/refman/directives/', 'op_3.html').
	built_in_directive(private, 1, '/docs/handbook/refman/directives/', 'private_1.html').
	built_in_directive(protected, 1, '/docs/handbook/refman/directives/', 'protected_1.html').
	built_in_directive(public, 1, '/docs/handbook/refman/directives/', 'public_1.html').
	built_in_directive(synchronized, 1, '/docs/handbook/refman/directives/', 'synchronized_1.html').
	built_in_directive(uses, 2, '/docs/handbook/refman/directives/', 'uses_2.html').
	built_in_directive(use_module, 1, '/docs/handbook/refman/directives/', 'use_module_1.html').
	built_in_directive(use_module, 2, '/docs/handbook/refman/directives/', 'use_module_2.html').

	built_in_predicate(current_category, 1, '/docs/handbook/refman/predicates/', 'current_category_1.html').
	built_in_predicate(current_object, 1, '/docs/handbook/refman/predicates/', 'current_object_1.html').
	built_in_predicate(current_protocol, 1, '/docs/handbook/refman/predicates/', 'current_protocol_1.html').

	built_in_predicate(category_property, 2, '/docs/handbook/refman/predicates/', 'category_property_2.html').
	built_in_predicate(object_property, 2, '/docs/handbook/refman/predicates/', 'object_property_2.html').
	built_in_predicate(protocol_property, 2, '/docs/handbook/refman/predicates/', 'protocol_property_2.html').

	built_in_predicate(create_category, 4, '/docs/handbook/refman/predicates/', 'create_category_4.html').
	built_in_predicate(create_object, 4, '/docs/handbook/refman/predicates/', 'create_object_4.html').
	built_in_predicate(create_protocol, 3, '/docs/handbook/refman/predicates/', 'create_protocol_3.html').

	built_in_predicate(abolish_category, 1, '/docs/handbook/refman/predicates/', 'abolish_category_1.html').
	built_in_predicate(abolish_object, 1, '/docs/handbook/refman/predicates/', 'abolish_object_1.html').
	built_in_predicate(abolish_protocol, 1, '/docs/handbook/refman/predicates/', 'abolish_protocol_1.html').

	built_in_predicate(extends_object, 2, '/docs/handbook/refman/predicates/', 'extends_object_2_3.html').
	built_in_predicate(extends_object, 3, '/docs/handbook/refman/predicates/', 'extends_object_2_3.html').
	built_in_predicate(extends_protocol, 2, '/docs/handbook/refman/predicates/', 'extends_protocol_2_3.html').
	built_in_predicate(extends_protocol, 3, '/docs/handbook/refman/predicates/', 'extends_protocol_2_3.html').
	built_in_predicate(extends_category, 2, '/docs/handbook/refman/predicates/', 'extends_category_2_3.html').
	built_in_predicate(extends_category, 3, '/docs/handbook/refman/predicates/', 'extends_category_2_3.html').
	built_in_predicate(implements_protocol, 2, '/docs/handbook/refman/predicates/', 'implements_protocol_2_3.html').
	built_in_predicate(implements_protocol, 3, '/docs/handbook/refman/predicates/', 'implements_protocol_2_3.html').
	built_in_predicate(imports_category, 2, '/docs/handbook/refman/predicates/', 'imports_category_2_3.html').
	built_in_predicate(imports_category, 3, '/docs/handbook/refman/predicates/', 'imports_category_2_3.html').
	built_in_predicate(instantiates_class, 2, '/docs/handbook/refman/predicates/', 'instantiates_class_2_3.html').
	built_in_predicate(instantiates_class, 3, '/docs/handbook/refman/predicates/', 'instantiates_class_2_3.html').
	built_in_predicate(specializes_class, 2, '/docs/handbook/refman/predicates/', 'specializes_class_2_3.html').
	built_in_predicate(specializes_class, 3, '/docs/handbook/refman/predicates/', 'specializes_class_2_3.html').
	built_in_predicate(complements_object, 2, '/docs/handbook/refman/predicates/', 'complements_object_2.html').
	built_in_predicate(conforms_to_protocol, N, '/docs/handbook/refman/predicates/', 'conforms_to_protocol_2_3.html') :-
		between(2, 3, N).

	built_in_predicate(abolish_events, 5, '/docs/handbook/refman/predicates/', 'abolish_events_5.html').
	built_in_predicate(current_event, 5, '/docs/handbook/refman/predicates/', 'current_event_5.html').
	built_in_predicate(define_events, 5, '/docs/handbook/refman/predicates/', 'define_events_5.html').

	built_in_predicate(threaded, 1, '/docs/handbook/refman/predicates/', 'threaded_1.html').
	built_in_predicate(threaded_call, 1, '/docs/handbook/refman/predicates/', 'threaded_call_1_2.html').
	built_in_predicate(threaded_call, 2, '/docs/handbook/refman/predicates/', 'threaded_call_1_2.html').
	built_in_predicate(threaded_once, 1, '/docs/handbook/refman/predicates/', 'threaded_once_1_2.html').
	built_in_predicate(threaded_once, 2, '/docs/handbook/refman/predicates/', 'threaded_once_1_2.html').
	built_in_predicate(threaded_ignore, 1, '/docs/handbook/refman/predicates/', 'threaded_ignore_1.html').
	built_in_predicate(threaded_exit, 1, '/docs/handbook/refman/predicates/', 'threaded_exit_1_2.html').
	built_in_predicate(threaded_exit, 2, '/docs/handbook/refman/predicates/', 'threaded_exit_1_2.html').
	built_in_predicate(threaded_peek, 1, '/docs/handbook/refman/predicates/', 'threaded_peek_1_2.html').
	built_in_predicate(threaded_peek, 2, '/docs/handbook/refman/predicates/', 'threaded_peek_1_2.html').
	built_in_predicate(threaded_cancel, 1, '/docs/handbook/refman/predicates/', 'threaded_cancel_1.html').
	built_in_predicate(threaded_wait, 1, '/docs/handbook/refman/predicates/', 'threaded_wait_1.html').
	built_in_predicate(threaded_notify, 1, '/docs/handbook/refman/predicates/', 'threaded_notify_1.html').

	built_in_predicate(threaded_engine, 1, '/docs/handbook/refman/predicates/', 'threaded_engine_1.html').
	built_in_predicate(threaded_engine_create, 3, '/docs/handbook/refman/predicates/', 'threaded_engine_create_3.html').
	built_in_predicate(threaded_engine_destroy, 1, '/docs/handbook/refman/predicates/', 'threaded_engine_destroy_1.html').
	built_in_predicate(threaded_engine_self, 1, '/docs/handbook/refman/predicates/', 'threaded_engine_self_1.html').
	built_in_predicate(threaded_engine_next, 2, '/docs/handbook/refman/predicates/', 'threaded_engine_next_2.html').
	built_in_predicate(threaded_engine_next_reified, 2, '/docs/handbook/refman/predicates/', 'threaded_engine_next_reified_2.html').
	built_in_predicate(threaded_engine_yield, 1, '/docs/handbook/refman/predicates/', 'threaded_engine_yield_1.html').
	built_in_predicate(threaded_engine_post, 2, '/docs/handbook/refman/predicates/', 'threaded_engine_post_2.html').
	built_in_predicate(threaded_engine_fetch, 1, '/docs/handbook/refman/predicates/', 'threaded_engine_fetch_1.html').

	built_in_predicate(logtalk_compile, 1, '/docs/handbook/refman/predicates/', 'logtalk_compile_1.html').
	built_in_predicate(logtalk_compile, 2, '/docs/handbook/refman/predicates/', 'logtalk_compile_2.html').
	built_in_predicate(logtalk_load, 1, '/docs/handbook/refman/predicates/', 'logtalk_load_1.html').
	built_in_predicate(logtalk_load, 2, '/docs/handbook/refman/predicates/', 'logtalk_load_2.html').
	built_in_predicate(logtalk_make, 0, '/docs/handbook/refman/predicates/', 'logtalk_make_0.html').
	built_in_predicate(logtalk_make, 1, '/docs/handbook/refman/predicates/', 'logtalk_make_1.html').
	built_in_predicate(logtalk_make_target_action, 1, '/docs/handbook/refman/predicates/', 'logtalk_make_target_action_1.html').
	built_in_predicate(logtalk_library_path, 2, '/docs/handbook/refman/predicates/', 'logtalk_library_path_2.html').
	built_in_predicate(logtalk_load_context, 2, '/docs/handbook/refman/predicates/', 'logtalk_load_context_2.html').

	built_in_predicate(current_logtalk_flag, 2, '/docs/handbook/refman/predicates/', 'current_logtalk_flag_2.html').
	built_in_predicate(set_logtalk_flag, 2, '/docs/handbook/refman/predicates/', 'set_logtalk_flag_2.html').
	built_in_predicate(create_logtalk_flag, 3, '/docs/handbook/refman/predicates/', 'create_logtalk_flag_3.html').

	built_in_predicate(logtalk_linter_hook, 7, '/docs/handbook/refman/predicates/', 'logtalk_linter_hook_7.html').

	built_in_method(!, 0, '/docs/handbook/refman/methods/', 'cut_0.html').
	built_in_method(true, 0, '/docs/handbook/refman/methods/', 'true_0.html').
	built_in_method(fail, 0, '/docs/handbook/refman/methods/', 'fail_0.html').
	built_in_method(false, 0, '/docs/handbook/refman/methods/', 'false_0.html').
	built_in_method(repeat, 0, '/docs/handbook/refman/methods/', 'repeat_0.html').

	built_in_method(parameter, 2, '/docs/handbook/refman/methods/', 'parameter_2.html').
	built_in_method(self, 1, '/docs/handbook/refman/methods/', 'self_1.html').
	built_in_method(sender, 1, '/docs/handbook/refman/methods/', 'sender_1.html').
	built_in_method(this, 1, '/docs/handbook/refman/methods/', 'this_1.html').
	built_in_method(context, 1, '/docs/handbook/refman/methods/', 'context_1.html').

	built_in_method(current_op, 3, '/docs/handbook/refman/methods/', 'current_op_3.html').
	built_in_method(current_predicate, 1, '/docs/handbook/refman/methods/', 'current_predicate_1.html').
	built_in_method(predicate_property, 2, '/docs/handbook/refman/methods/', 'predicate_property_2.html').

	built_in_method(abolish, 1, '/docs/handbook/refman/methods/', 'abolish_1.html').
	built_in_method(asserta, 1, '/docs/handbook/refman/methods/', 'asserta_1.html').
	built_in_method(assertz, 1, '/docs/handbook/refman/methods/', 'assertz_1.html').
	built_in_method(clause, 2, '/docs/handbook/refman/methods/', 'clause_2.html').
	built_in_method(retract, 1, '/docs/handbook/refman/methods/', 'retract_1.html').
	built_in_method(retractall, 1, '/docs/handbook/refman/methods/', 'retractall_1.html').

	built_in_method(call, N, '/docs/handbook/refman/methods/', 'call_N.html') :-
		between(1, 8, N).
	built_in_method(once, 1, '/docs/handbook/refman/methods/', 'once_1.html').
	built_in_method((\+), 1, '/docs/handbook/refman/methods/', 'not_1.html').
	built_in_method(ignore, 1, '/docs/handbook/refman/methods/', 'ignore_1.html').

	built_in_method(catch, 3, '/docs/handbook/refman/methods/', 'catch_3.html').
	built_in_method(throw, 1, '/docs/handbook/refman/methods/', 'throw_1.html').

	built_in_method(instantiation_error, 0, '/docs/handbook/refman/methods/', 'instantiation_error_0.html').
	built_in_method(uninstantiation_error, 1, '/docs/handbook/refman/methods/', 'uninstantiation_error_1.html').
	built_in_method(type_error, 2, '/docs/handbook/refman/methods/', 'type_error_2.html').
	built_in_method(domain_error, 2, '/docs/handbook/refman/methods/', 'domain_error_2.html').
	built_in_method(consistency_error, 3, '/docs/handbook/refman/methods/', 'consistency_error_3.html').
	built_in_method(existence_error, 2, '/docs/handbook/refman/methods/', 'existence_error_2.html').
	built_in_method(permission_error, 3, '/docs/handbook/refman/methods/', 'permission_error_3.html').
	built_in_method(representation_error, 1, '/docs/handbook/refman/methods/', 'representation_error_1.html').
	built_in_method(evaluation_error, 1, '/docs/handbook/refman/methods/', 'evaluation_error_1.html').
	built_in_method(resource_error, 1, '/docs/handbook/refman/methods/', 'resource_error_1.html').
	built_in_method(syntax_error, 1, '/docs/handbook/refman/methods/', 'syntax_error_1.html').
	built_in_method(system_error, 0, '/docs/handbook/refman/methods/', 'system_error_0.html').

	built_in_method(bagof, 3, '/docs/handbook/refman/methods/', 'bagof_3.html').
	built_in_method(findall, 3, '/docs/handbook/refman/methods/', 'findall_3.html').
	built_in_method(findall, 4, '/docs/handbook/refman/methods/', 'findall_4.html').
	built_in_method(forall, 2, '/docs/handbook/refman/methods/', 'forall_2.html').
	built_in_method(setof, 3, '/docs/handbook/refman/methods/', 'setof_3.html').

	built_in_method(before, 3, '/docs/handbook/refman/methods/', 'before_3.html').
	built_in_method(after, 3, '/docs/handbook/refman/methods/', 'after_3.html').

	built_in_method(forward, 1, '/docs/handbook/refman/methods/', 'forward_1.html').

	built_in_method(phrase, 2, '/docs/handbook/refman/methods/', 'phrase_2.html').
	built_in_method(phrase, 3, '/docs/handbook/refman/methods/', 'phrase_3.html').

	built_in_method(expand_term, 2, '/docs/handbook/refman/methods/', 'expand_term_2.html').
	built_in_method(term_expansion, 2, '/docs/handbook/refman/methods/', 'term_expansion_2.html').
	built_in_method(expand_goal, 2, '/docs/handbook/refman/methods/', 'expand_goal_2.html').
	built_in_method(goal_expansion, 2, '/docs/handbook/refman/methods/', 'goal_expansion_2.html').

	built_in_method(coinductive_success_hook, N, '/docs/handbook/refman/methods/', 'coinductive_success_hook_1_2.html') :-
		between(1, 2, N).

	built_in_method(ask_question, 5, '/docs/handbook/refman/methods/', 'ask_question_5.html').
	built_in_method(message_hook, 4, '/docs/handbook/refman/methods/', 'message_hook_4.html').
	built_in_method(message_prefix_stream, 4, '/docs/handbook/refman/methods/', 'message_prefix_stream_4.html').
	built_in_method(message_prefix_file, 6, '/docs/handbook/refman/methods/', 'message_prefix_file_6.html').
	built_in_method(print_message, 3, '/docs/handbook/refman/methods/', 'print_message_3.html').
	built_in_method(print_message_tokens, 3, '/docs/handbook/refman/methods/', 'print_message_tokens_3.html').
	built_in_method(print_message_token, 4, '/docs/handbook/refman/methods/', 'print_message_token_4.html').
	built_in_method(question_hook, 6, '/docs/handbook/refman/methods/', 'question_hook_6.html').
	built_in_method(question_prompt_stream, 4, '/docs/handbook/refman/methods/', 'question_prompt_stream_4.html').

	control_construct((::), 2, '/docs/handbook/refman/control/', 'send_to_object_2.html').
	control_construct('[]', 1, '/docs/handbook/refman/control/', 'delegate_message_1.html').
	control_construct((::), 1, '/docs/handbook/refman/control/', 'send_to_self_1.html').
	control_construct((^^), 1, '/docs/handbook/refman/control/', 'call_super_1.html').
	control_construct((@),  1, '/docs/handbook/refman/control/', 'call_in_this_1.html').
	control_construct(({}), 1, '/docs/handbook/refman/control/', 'external_call_1.html').
	control_construct((<<), 2, '/docs/handbook/refman/control/', 'context_switch_2.html').

	built_in_non_terminal(call, N, '/docs/handbook/refman/methods/', 'call_1.html') :-
		between(1, 6, N).
	built_in_non_terminal(eos, 0, '/docs/handbook/refman/methods/', 'eos_0.html').
	built_in_non_terminal(phrase, 1, '/docs/handbook/refman/methods/', 'phrase_1.html').

	built_in_non_terminal(message_tokens, 2, '/docs/handbook/refman/methods/', 'message_tokens_2.html').

	tools :-
		open('/docs/handbook/devtools/', 'index.html').

	tool(Tool) :-
		var(Tool),
		!,
		open('/docs/handbook/devtools/', 'index.html').
	tool(Tool) :-
		logtalk_library_path(Tool, _),
		atom_concat(Tool,  '.html', Path),
		open('/docs/handbook/devtools/', Path),
		!.
	tool(Tool) :-
		write('Unknown library or no help available for '), writeq(Tool), write('.'), nl,
		write('Showing index of all developer tools.'), nl,
		open('/docs/handbook/devtools/', 'index.html').

	libraries :-
		open('/docs/handbook/libraries/', 'index.html').

	library(Topic) :-
		var(Topic),
		!,
		open('/docs/handbook/libraries/', 'index.html').
	library(Library) :-
		logtalk_library_path(Library, _),
		atom_concat(Library,  '.html', Path),
		open('/docs/handbook/libraries/', Path),
		!.
	library(Library) :-
		write('Unknown library or no help available for '), writeq(Library), write('.'), nl,
		write('Showing index of all libraries.'), nl,
		open('/docs/handbook/libraries/', 'index.html').

	apis :-
		open('/docs/apis/', 'index.html').

	apis(Name/Arity) :-
		atom(Name),
		integer(Arity),
		atom::replace_sub_atom('_', '-', Name, NameDashes),
		atomic_list_concat(['predicate_index.html#', NameDashes, '-', Arity], File),
		open('/docs/apis/', File),
		!.
	apis(Name/Arity) :-
		!,
		write('Unknown predicate or no help available for '), writeq(Name/Arity), write('.'), nl,
		write('Showing index of all documented predicates.'), nl,
		open('/docs/apis/', 'predicate_index.html').
	apis(Name//Arity) :-
		atom(Name),
		integer(Arity),
		atom::replace_sub_atom('_', '-', Name, NameDashes),
		atomic_list_concat(['predicate_index.html#', NameDashes, '-', Arity], File),
		open('/docs/apis/', File),
		!.
	apis(Name//Arity) :-
		!,
		write('Unknown non-terminal or no help available for '), writeq(Name//Arity), write('.'), nl,
		write('Showing index of all documented predicates.'), nl,
		open('/docs/apis/', 'predicate_index.html').

	entity(Entity) :-
		callable(Entity),
		functor(Entity, Functor, Arity),
		atom_concat(Functor, '_', File0),
		number_chars(Arity, ArityChars),
		atom_chars(ArityAtom, ArityChars),
		atom_concat(File0, ArityAtom, File1),
		atom_concat(File1, '.html', File),
		open('/docs/apis/', File),
		!.
	entity(Entity) :-
		write('Unknown entity or no help available for '), writeq(Entity), write('.'), nl,
		write('Showing index of all documented entities.'), nl,
		open('/docs/apis/', 'entity_index.html').

	handbook :-
		open('/docs/handbook/', 'index.html').

	open(_, _) :-
		\+ environment_variable('LOGTALKHOME', _),
		!,
		write('The environment variable LOGTALKHOME must be defined and pointing to your'), nl,
		write('Logtalk installation folder in order for on-line help to be available.'), nl, nl,
		fail.
	open(Path, File) :-
		% the JIProlog uses a Java-based console that isn't compatible with inline browsers
		\+ current_logtalk_flag(prolog_dialect, ji),
		inline_browser(Browser),
		inline_browser_executable(Browser, Executable),
		open_in_inline_browser(Browser, Executable, Path, File),
		!.
	open(Path, File) :-
		open_in_default_browser(Path, File).

	inline_browser(Browser) :-
		current_logtalk_flag(help_default_browser, Browser),
		(	Browser == default ->
			!,
			fail
		;	Browser == '' ->
			fail
		;	atom(Browser),
			memberchk(Browser, [lynx, w3m, links, cha])
		).
	inline_browser(lynx).
	inline_browser(w3m).
	inline_browser(links).
	inline_browser(cha).

	inline_browser_executable(Browser, Executable) :-
		inline_browser_command_path(Browser,  Executable),
		file_exists(Executable),
		!.

	inline_browser_command_path(lynx,  '/usr/bin/lynx').
	inline_browser_command_path(lynx,  '/usr/local/bin/lynx').
	inline_browser_command_path(lynx,  '/opt/local/bin/lynx').
	inline_browser_command_path(lynx,  '/opt/homebrew/bin/lynx').
	inline_browser_command_path(w3m,   '/usr/bin/w3m').
	inline_browser_command_path(w3m,   '/usr/local/bin/w3m').
	inline_browser_command_path(w3m,   '/opt/local/bin/w3m').
	inline_browser_command_path(w3m,   '/opt/homebrew/bin/w3m').
	inline_browser_command_path(links, '/usr/bin/links').
	inline_browser_command_path(links, '/usr/local/bin/links').
	inline_browser_command_path(links, '/opt/local/bin/links').
	inline_browser_command_path(links, '/opt/homebrew/bin/links').
	inline_browser_command_path(links, '/usr/bin/links2').
	inline_browser_command_path(links, '/usr/local/bin/links2').
	inline_browser_command_path(links, '/opt/local/bin/links2').
	inline_browser_command_path(cha,   '/usr/bin/cha').
	inline_browser_command_path(cha,   '/usr/local/bin/cha').
	inline_browser_command_path(cha,   '/opt/local/bin/cha').
	inline_browser_command_path(cha,   '/opt/homebrew/bin/cha').

	open_in_inline_browser(Browser, Executable, Directory, File) :-
		(	Browser == lynx
			% lynx doesn't understand (by default) .xhtml files
		;	current_logtalk_flag(help_default_files, html)
			% use the HTML files generated by Sphinx for web browsing
		),
		!,
		environment_variable('LOGTALKHOME', LOGTALKHOME),
		atomic_list_concat([Executable, ' ', LOGTALKHOME, Directory, File], Command),
		shell(Command).
	open_in_inline_browser(_, Executable, Directory, File) :-
		% for other terminal-based browsers or when the help_default_files flag is set to xhtml
		environment_variable('LOGTALKUSER', LOGTALKUSER),
		atomic_list_concat([LOGTALKUSER, '/tools/help/.docs_cache', Directory, File], Path0),
		decompose_file_name(Path0, Directory0, Name0, _),
		atomic_list_concat([Directory0, Name0, '.xhtml'], Path),
		file_exists(Path),
		!,
		atomic_list_concat([Executable, ' ', Path], Command),
		shell(Command).
	open_in_inline_browser(_, Executable, Directory, File) :-
		% resort to .html files when the .xhtml files are not cached
		open_in_inline_browser(lynx, Executable, Directory, File).

	open_in_default_browser(Path, File) :-
		(	environment_variable('COMSPEC', _) ->
			% assume we're running on Windows
			atomic_list_concat(['cmd /c start "" "file:///%LOGTALKHOME%', Path, File, '"'], Command),
			shell(Command)
		;	shell('uname -s | grep Darwin 1> /dev/null') ->
			% assume we're running on macOS
			atomic_list_concat(['open "file://$LOGTALKHOME', Path, File, '" > /dev/null 2>&1'], Command),
			shell(Command)
		;	shell('command -v xdg-open > /dev/null 2>&1') ->
			% assume we're running on Linux or BSD
			atomic_list_concat(['xdg-open "file://$LOGTALKHOME', Path, File, '" > /dev/null 2>&1'], Command),
			shell(Command)
		;	% we couldn't find which operating-system are we running on
			write('Unsupported operating-system.'), nl
		).

	cache_epub_xhtml_files :-
		environment_variable('COMSPEC', _),
		% assume we're running on Windows
		!.
	cache_epub_xhtml_files :-
		environment_variable('LOGTALKUSER', LOGTALKUSER),
		atomic_list_concat([LOGTALKUSER, '/VERSION.txt'], OriginalFile),
		atomic_list_concat([LOGTALKUSER, '/tools/help/.docs_cache/VERSION.txt'], CopyFile),
		file_exists(CopyFile),
		file_modification_time(OriginalFile, OriginalTime),
		file_modification_time(CopyFile, CopyTime),
		CopyTime @>= OriginalTime,
		atom_concat(LOGTALKUSER, '/tools/help/.docs_cache/docs/handbook/index.xhtml', HandbookIndex),
		file_exists(HandbookIndex),
		atom_concat(LOGTALKUSER, '/tools/help/.docs_cache/docs/apis/index.xhtml', APIsIndex),
		file_exists(APIsIndex),
		% assume .xhtml files already cached
		!.
	cache_epub_xhtml_files :-
		% cache the .xhtml files by extracting thme from the .epub archives if they exist
		current_logtalk_flag(version_data, logtalk(Major,Minor,Patch, _)),
		environment_variable('LOGTALKUSER', LOGTALKUSER),
		atomic_list_concat([LOGTALKUSER, '/tools/help/.docs_cache/docs/handbook'], HandbookDirectory),
		make_directory_path(HandbookDirectory),
		atomic_list_concat([LOGTALKUSER, '/tools/help/.docs_cache/docs/apis'], APIsDirectory),
		make_directory_path(APIsDirectory),
		environment_variable('LOGTALKHOME', LOGTALKHOME),
		atomic_list_concat([LOGTALKHOME, '/docs/handbook/TheLogtalkHandbook-', Major, '.', Minor, '.', Patch, '.epub'], HandbookFile),
		atomic_list_concat([LOGTALKHOME, '/docs/apis/LogtalkAPIs-', Major, '.', Minor, '.', Patch, '.epub'], APIsFile),
		(	file_exists(HandbookFile),
			file_exists(APIsFile) ->
			copy_file(HandbookFile, HandbookDirectory),
			copy_file(APIsFile, APIsDirectory),
			atomic_list_concat([LOGTALKUSER, '/tools/help/.docs_cache/docs/handbook/TheLogtalkHandbook-', Major, '.', Minor, '.', Patch, '.epub'], HandbookFileCopy),
			atomic_list_concat(['bsdtar -xf "', HandbookFileCopy, '" --directory "', HandbookDirectory, '"'], Command1),
			shell(Command1),
			atomic_list_concat([LOGTALKUSER, '/tools/help/.docs_cache/docs/apis/LogtalkAPIs-', Major, '.', Minor, '.', Patch, '.epub'], HandbookAPIsCopy),
			atomic_list_concat(['bsdtar -xf "', HandbookAPIsCopy, '" --directory "', APIsDirectory, '"'], Command2),
			shell(Command2),
			atomic_list_concat(['find "', LOGTALKUSER, '/tools/help/.docs_cache/docs', '" -type f -name "*.xhtml" -exec sed -i.bak \'s/\\.html#/.xhtml#/g\' {} +'], Command3),
			shell(Command3),
			atomic_list_concat(['find "', LOGTALKUSER, '/tools/help/.docs_cache/docs', '" -name "*.bak" -delete'], Command4),
			shell(Command4),
			atomic_list_concat([LOGTALKUSER, '/VERSION.txt'], OriginalFile),
			atomic_list_concat([LOGTALKUSER, '/tools/help/.docs_cache/VERSION.txt'], CopyFile),
			copy_file(OriginalFile, CopyFile)
		;	true
		).

	man(Page) :-
		\+ man_page_url(Page, _),
		!,
		write('Unknown script or no help available for '), writeq(Page), write('.'), nl.
	man(Page) :-
		environment_variable('COMSPEC', _),
		% assume we're running on Windows
		!,
		man_page_url(Page, URL),
		atomic_list_concat(['cmd /c start "" "', URL, '"'], Command),
		shell(Command).
	man(Page) :-
		atomic_list_concat([man, ' ', Page], Command),
		shell(Command).

	man_page_url(bplgt,                  'https://logtalk.org/man/bplgt.html').
	man_page_url(ciaolgt,                'https://logtalk.org/man/ciaolgt.html').
	man_page_url(cxlgt,                  'https://logtalk.org/man/cxlgt.html').
	man_page_url(eclipselgt,             'https://logtalk.org/man/eclipselgt.html').
	man_page_url(gplgt,                  'https://logtalk.org/man/gplgt.html').
	man_page_url(jiplgt,                 'https://logtalk.org/man/jiplgt.html').
	man_page_url(quintuslgt,             'https://logtalk.org/man/quintuslgt.html').
	man_page_url(sicstuslgt,             'https://logtalk.org/man/sicstuslgt.html').
	man_page_url(swilgt,                 'https://logtalk.org/man/swilgt.html').
	man_page_url(taulgt,                 'https://logtalk.org/man/taulgt.html').
	man_page_url(tplgt,                  'https://logtalk.org/man/tplgt.html').
	man_page_url(xsblgt,                 'https://logtalk.org/man/xsblgt.html').
	man_page_url(xvmlgt,                 'https://logtalk.org/man/xvmlgt.html').
	man_page_url(yaplgt,                 'https://logtalk.org/man/yaplgt.html').
	man_page_url(logtalk_user_setup,     'https://logtalk.org/man/logtalk_user_setup.html').
	man_page_url(logtalk_backend_select, 'https://logtalk.org/man/logtalk_backend_select.html').
	man_page_url(logtalk_version_select, 'https://logtalk.org/man/logtalk_version_select.html').
	man_page_url(logtalk_tester,         'https://logtalk.org/man/logtalk_tester.html').
	man_page_url(logtalk_allure_report,  'https://logtalk.org/man/logtalk_allure_report.html').
	man_page_url(logtalk_doclet,         'https://logtalk.org/man/logtalk_doclet.html').
	man_page_url(lgt2html,               'https://logtalk.org/man/lgt2html.html').
	man_page_url(lgt2md,                 'https://logtalk.org/man/lgt2md.html').
	man_page_url(lgt2pdf,                'https://logtalk.org/man/lgt2pdf.html').
	man_page_url(lgt2rst,                'https://logtalk.org/man/lgt2rst.html').
	man_page_url(lgt2txt,                'https://logtalk.org/man/lgt2txt.html').
	man_page_url(lgt2xml,                'https://logtalk.org/man/lgt2xml.html').
	man_page_url(lgt2svg,                'https://logtalk.org/man/lgt2svg.html').
	man_page_url(lgtenv,                 'https://logtalk.org/man/lgtenv.html').

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with multifile predicates
	% that are called from a file but not defined in that file
	:- multifile(logtalk_library_path/2).
	:- dynamic(logtalk_library_path/2).
:- endif.

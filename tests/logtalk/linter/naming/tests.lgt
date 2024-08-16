%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-08-16,
		comment is 'Unit tests for the ``naming`` linter flag.'
	]).

	:- private(file_and_entity_names_differ/2).
	:- dynamic(file_and_entity_names_differ/2).

	:- private(camel_case_entity_name/4).
	:- dynamic(camel_case_entity_name/4).

	:- private(entity_name_with_digits_in_the_middle/4).
	:- dynamic(entity_name_with_digits_in_the_middle/4).

	:- private(camel_case_predicate_name/5).
	:- dynamic(camel_case_predicate_name/5).

	:- private(predicate_name_with_digits_in_the_middle/5).
	:- dynamic(predicate_name_with_digits_in_the_middle/5).

	:- private(camel_case_non_terminal_name/5).
	:- dynamic(camel_case_non_terminal_name/5).

	:- private(non_terminal_name_with_digits_in_the_middle/5).
	:- dynamic(non_terminal_name_with_digits_in_the_middle/5).

	:- private(non_camel_case_variable_name/5).
	:- dynamic(non_camel_case_variable_name/5).

	:- private(non_camel_case_variable_name/3).
	:- dynamic(non_camel_case_variable_name/3).

	:- private(variable_name_with_digits_in_the_middle/5).
	:- dynamic(variable_name_with_digits_in_the_middle/5).

	:- private(variable_name_with_digits_in_the_middle/3).
	:- dynamic(variable_name_with_digits_in_the_middle/3).

	:- private(variable_names_differ_only_on_case/6).
	:- dynamic(variable_names_differ_only_on_case/6).

	:- private(variable_names_differ_only_on_case/4).
	:- dynamic(variable_names_differ_only_on_case/4).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [naming(warning)]).

	cleanup :-
		retractall(file_and_entity_names_differ(_, _)),
		retractall(camel_case_entity_name(_, _, _, _)),
		retractall(entity_name_with_digits_in_the_middle(_, _, _, _)),
		retractall(camel_case_predicate_name(_, _, _, _, _)),
		retractall(predicate_name_with_digits_in_the_middle(_, _, _, _, _)),
		retractall(camel_case_non_terminal_name(_, _, _, _, _)),
		retractall(non_terminal_name_with_digits_in_the_middle(_, _, _, _, _)),
		retractall(non_camel_case_variable_name(_, _, _, _, _)),
		retractall(non_camel_case_variable_name(_, _, _)),
		retractall(variable_name_with_digits_in_the_middle(_, _, _, _, _)),
		retractall(variable_name_with_digits_in_the_middle(_, _, _)),
		retractall(variable_names_differ_only_on_case(_, _, _, _, _, _)),
		retractall(variable_names_differ_only_on_case(_, _, _, _)).

	test(naming_linter_flag_01, exists(Name == 'Corge42Grault')) :-
		variable_name_with_digits_in_the_middle(_, _, object, naming, Name).

	test(naming_linter_flag_02, exists(Name == 'Corge_Grault')) :-
		non_camel_case_variable_name(_, _, object, naming, Name).

	test(naming_linter_flag_03, exists(Name1-Name2 == 'List'-'LIST')) :-
		variable_names_differ_only_on_case(_, _, object, naming, Name1, Name2).

	test(naming_linter_flag_04, exists(Predicate == fooBar/0)) :-
		camel_case_predicate_name(_, _, object, naming, Predicate).

	test(naming_linter_flag_05, exists(Predicate == foo42bar/0)) :-
		predicate_name_with_digits_in_the_middle(_, _, object, naming, Predicate).

	test(naming_linter_flag_06, exists(NonTerminal == nonTerminal//0)) :-
		camel_case_non_terminal_name(_, _, object, naming, NonTerminal).

	test(naming_linter_flag_07, exists(NonTerminal == non42terminal//0)) :-
		non_terminal_name_with_digits_in_the_middle(_, _, object, naming, NonTerminal).

	test(naming_linter_flag_08, exists(Predicate == bazQux/0)) :-
		camel_case_predicate_name(_, _, object, naming, Predicate).

	test(naming_linter_flag_09, exists(Predicate == baz42qux/0)) :-
		predicate_name_with_digits_in_the_middle(_, _, object, naming, Predicate).

	test(naming_linter_flag_10, exists(NonTerminal == noMoreTokens//0)) :-
		camel_case_non_terminal_name(_, _, object, naming, NonTerminal).

	test(naming_linter_flag_11, exists(NonTerminal == no42tokens//0)) :-
		non_terminal_name_with_digits_in_the_middle(_, _, object, naming, NonTerminal).

	test(naming_linter_flag_12, exists(Name == someObject)) :-
		camel_case_entity_name(_, _, object, Name).

	test(naming_linter_flag_13, exists(Name == foo42bar)) :-
		entity_name_with_digits_in_the_middle(_, _, protocol, Name).

	test(naming_linter_flag_14, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(file_and_entity_names_differ(file, entity), core), Tokens).

	test(naming_linter_flag_15, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(camel_case_entity_name(file, 1-2, object, fooBar), core), Tokens).

	test(naming_linter_flag_16, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(entity_name_with_digits_in_the_middle(file, 1-2, object, foo42bar), core), Tokens).

	test(naming_linter_flag_17, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(camel_case_predicate_name(file, 1-2, object, naming, fooBar/1), core), Tokens).

	test(naming_linter_flag_18, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(predicate_name_with_digits_in_the_middle(file, 1-2, object, naming, foo42bar/1), core), Tokens).

	test(naming_linter_flag_19, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(camel_case_non_terminal_name(file, 1-2, object, naming, fooBar//1), core), Tokens).

	test(naming_linter_flag_20, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_terminal_name_with_digits_in_the_middle(file, 1-2, object, naming, foo42bar//1), core), Tokens).

	test(naming_linter_flag_21, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_camel_case_variable_name(file, 1-2, object, naming, 'Foo_Bar'), core), Tokens).

	test(naming_linter_flag_22, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_camel_case_variable_name(file, 1-2, 'Foo_Bar'), core), Tokens).

	test(naming_linter_flag_23, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(variable_name_with_digits_in_the_middle(file, 1-2, object, naming, 'Foo42Bar'), core), Tokens).

	test(naming_linter_flag_24, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(variable_name_with_digits_in_the_middle(file, 1-2, 'Foo42Bar'), core), Tokens).

	test(naming_linter_flag_25, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(variable_names_differ_only_on_case(file, 1-2, object, naming, 'FooBar', 'Foobar'), core), Tokens).

	test(naming_linter_flag_26, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(variable_names_differ_only_on_case(file, 1-2, 'FooBar', 'Foobar'), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(file_and_entity_names_differ(SourceFile, Single), warning(naming), core, _) :-
		assertz(file_and_entity_names_differ(SourceFile, Single)).
	logtalk::message_hook(camel_case_entity_name(File, Lines, Type, Entity), warning(naming), core, _) :-
		assertz(camel_case_entity_name(File, Lines, Type, Entity)).
	logtalk::message_hook(entity_name_with_digits_in_the_middle(File, Lines, Type, Entity), warning(naming), core, _) :-
		assertz(entity_name_with_digits_in_the_middle(File, Lines, Type, Entity)).
	logtalk::message_hook(camel_case_predicate_name(File, Lines, Type, Entity, Predicate), warning(naming), core, _) :-
		assertz(camel_case_predicate_name(File, Lines, Type, Entity, Predicate)).
	logtalk::message_hook(predicate_name_with_digits_in_the_middle(File, Lines, Type, Entity, Predicate), warning(naming), core, _) :-
		assertz(predicate_name_with_digits_in_the_middle(File, Lines, Type, Entity, Predicate)).
	logtalk::message_hook(camel_case_non_terminal_name(File, Lines, Type, Entity, NonTerminal), warning(naming), core, _) :-
		assertz(camel_case_non_terminal_name(File, Lines, Type, Entity, NonTerminal)).
	logtalk::message_hook(non_terminal_name_with_digits_in_the_middle(File, Lines, Type, Entity, NonTerminal), warning(naming), core, _) :-
		assertz(non_terminal_name_with_digits_in_the_middle(File, Lines, Type, Entity, NonTerminal)).
	logtalk::message_hook(non_camel_case_variable_name(File, Lines, Type, Entity, Name), warning(naming), core, _) :-
		assertz(non_camel_case_variable_name(File, Lines, Type, Entity, Name)).
	logtalk::message_hook(non_camel_case_variable_name(File, Lines, Name), warning(naming), core, _) :-
		assertz(non_camel_case_variable_name(File, Lines, Name)).
	logtalk::message_hook(variable_name_with_digits_in_the_middle(File, Lines, Type, Entity, Name), warning(naming), core, _) :-
		assertz(variable_name_with_digits_in_the_middle(File, Lines, Type, Entity, Name)).
	logtalk::message_hook(variable_name_with_digits_in_the_middle(File, Lines, Name), warning(naming), core, _) :-
		assertz(variable_name_with_digits_in_the_middle(File, Lines, Name)).
	logtalk::message_hook(variable_names_differ_only_on_case(File, Lines, Type, Entity, Name, OtherName), warning(naming), core, _) :-
		assertz(variable_names_differ_only_on_case(File, Lines, Type, Entity, Name, OtherName)).
	logtalk::message_hook(variable_names_differ_only_on_case(File, Lines, Name, OtherName), warning(naming), core, _) :-
		assertz(variable_names_differ_only_on_case(File, Lines, Name, OtherName)).

:- end_object.

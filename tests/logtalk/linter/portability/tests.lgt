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
		date is 2024-08-20,
		comment is 'Unit tests for the ``portability`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(prolog_dialect_goal_expansion/6).
	:- dynamic(prolog_dialect_goal_expansion/6).

	:- private(prolog_dialect_goal_expansion/4).
	:- dynamic(prolog_dialect_goal_expansion/4).

	:- private(prolog_dialect_term_expansion/6).
	:- dynamic(prolog_dialect_term_expansion/6).

	:- private(prolog_dialect_term_expansion/4).
	:- dynamic(prolog_dialect_term_expansion/4).

	:- private(compiling_proprietary_prolog_directive/5).
	:- dynamic(compiling_proprietary_prolog_directive/5).

	:- private(compiling_query_as_initialization_goal/5).
	:- dynamic(compiling_query_as_initialization_goal/5).

	:- private(logtalk_built_in_predicate_as_directive/3).
	:- dynamic(logtalk_built_in_predicate_as_directive/3).

	:- private(top_level_shortcut_as_directive/3).
	:- dynamic(top_level_shortcut_as_directive/3).

	:- private(non_standard_file_directive/3).
	:- dynamic(non_standard_file_directive/3).

	:- private(non_standard_prolog_flag/5).
	:- dynamic(non_standard_prolog_flag/5).

	:- private(non_standard_prolog_flag/3).
	:- dynamic(non_standard_prolog_flag/3).

	:- private(non_standard_prolog_flag_value/6).
	:- dynamic(non_standard_prolog_flag_value/6).

	:- private(non_standard_prolog_flag_value/4).
	:- dynamic(non_standard_prolog_flag_value/4).

	:- private(non_standard_predicate_option/6).
	:- dynamic(non_standard_predicate_option/6).

	:- private(non_standard_predicate_call/5).
	:- dynamic(non_standard_predicate_call/5).

	:- private(non_standard_arithmetic_function_call/5).
	:- dynamic(non_standard_arithmetic_function_call/5).

	:- private(missing_function/5).
	:- dynamic(missing_function/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [portability(warning), deprecated(silent)]).

	cleanup :-
		retractall(prolog_dialect_goal_expansion(_, _, _, _, _, _)),
		retractall(prolog_dialect_goal_expansion(_, _, _, _)),
		retractall(prolog_dialect_term_expansion(_, _, _, _, _, _)),
		retractall(prolog_dialect_term_expansion(_, _, _, _)),
		retractall(compiling_proprietary_prolog_directive(_, _, _, _, _)),
		retractall(compiling_query_as_initialization_goal(_, _, _, _, _)),
		retractall(logtalk_built_in_predicate_as_directive(_, _, _)),
		retractall(top_level_shortcut_as_directive(_, _, _)),
		retractall(non_standard_file_directive(_, _, _)),
		retractall(non_standard_prolog_flag(_, _, _, _, _)),
		retractall(non_standard_prolog_flag(_, _, _)),
		retractall(non_standard_prolog_flag_value(_, _, _, _, _, _)),
		retractall(non_standard_prolog_flag_value(_, _, _, _)),
		retractall(non_standard_predicate_option(_, _, _, _, _, _)),
		retractall(non_standard_predicate_call(_, _, _, _, _)),
		retractall(non_standard_arithmetic_function_call(_, _, _, _, _)),
		retractall(missing_function(_, _, _, _, _)).

	test(portability_linter_flag_01, exists(Directive == [file])) :-
		top_level_shortcut_as_directive(_, _, Directive).

	test(portability_linter_flag_02, exists(Directive == logtalk_load(library))) :-
		logtalk_built_in_predicate_as_directive(_, _, Directive).

	test(portability_linter_flag_03, exists(Directive == write(hello))) :-
		non_standard_file_directive(_, _, Directive).

	test(portability_linter_flag_04, exists(Predicate-Option == (open/4)-encoding('UTF-16'))) :-
		non_standard_predicate_option(_, _, object, portability, Predicate, Option).

	test(portability_linter_flag_05, exists(variant(Predicate-Option, (read_term/2)-lines(_, _)))) :-
		non_standard_predicate_option(_, _, object, portability, Predicate, Option).

	test(portability_linter_flag_06, exists(Flag == encoding)) :-
		non_standard_prolog_flag(_, _, object, portability, Flag).

	test(portability_linter_flag_07, exists(Flag-Value == double_quotes-string)) :-
		non_standard_prolog_flag_value(_, _, object, portability, Flag, Value).

	test(portability_linter_flag_08, exists(Predicate == tell/1), [condition(predicate_property(tell(_), built_in))]) :-
		non_standard_predicate_call(_, _, object, portability, Predicate).

	test(portability_linter_flag_09, exists(Function == popcount/1), [condition(catch(_ is popcount(42), _, fail))]) :-
		non_standard_arithmetic_function_call(_, _, object, portability, Function).

	test(portability_linter_flag_10, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(prolog_dialect_goal_expansion(file, 1-2, object, portability, foo, bar), core), Tokens).

	test(portability_linter_flag_11, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(prolog_dialect_goal_expansion(file, 1-2, foo, bar), core), Tokens).

	test(portability_linter_flag_12, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(prolog_dialect_term_expansion(file, 1-2, object, portability, foo, bar), core), Tokens).

	test(portability_linter_flag_13, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(prolog_dialect_term_expansion(file, 1-2, foo, bar), core), Tokens).

	test(portability_linter_flag_14, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(compiling_proprietary_prolog_directive(file, 1-2, object, portability, foo), core), Tokens).

	test(portability_linter_flag_15, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(compiling_query_as_initialization_goal(file, 1-2, object, portability, foo), core), Tokens).

	test(portability_linter_flag_16, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(logtalk_built_in_predicate_as_directive(file, 1-2, logtalk_load(foo)), core), Tokens).

	test(portability_linter_flag_17, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(top_level_shortcut_as_directive(file, 1-2, [foo]), core), Tokens).

	test(portability_linter_flag_18, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_file_directive(file, 1-2, foo), core), Tokens).

	test(portability_linter_flag_19, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_prolog_flag(file, 1-2, object, portability, foo), core), Tokens).

	test(portability_linter_flag_20, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_prolog_flag(file, 1-2, foo), core), Tokens).

	test(portability_linter_flag_21, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_prolog_flag_value(file, 1-2, object, portability, foo, bar), core), Tokens).

	test(portability_linter_flag_22, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_prolog_flag_value(file, 1-2, foo, bar), core), Tokens).

	test(portability_linter_flag_23, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_predicate_option(file, 1-2, object, portability, read_term/3, foo(bar)), core), Tokens).

	test(portability_linter_flag_24, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_predicate_call(file, 1-2, object, portability, foo/1), core), Tokens).

	test(portability_linter_flag_25, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_standard_arithmetic_function_call(file, 1-2, object, portability, foo/1), core), Tokens).

	test(portability_linter_flag_26, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(missing_function(file, 1-2, object, portability, foo/1), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(prolog_dialect_goal_expansion(File, Lines, Type, Entity, Goal, ExpandedGoal), warning(portability), core, _) :-
		assertz(prolog_dialect_goal_expansion(File, Lines, Type, Entity, Goal, ExpandedGoal)).
	logtalk::message_hook(prolog_dialect_goal_expansion(File, Lines, Goal, ExpandedGoal), warning(portability), core, _) :-
		assertz(prolog_dialect_goal_expansion(File, Lines, Goal, ExpandedGoal)).
	logtalk::message_hook(prolog_dialect_term_expansion(File, Lines, Type, Entity, Term, ExpandedTerms), warning(portability), core, _) :-
		assertz(prolog_dialect_term_expansion(File, Lines, Type, Entity, Term, ExpandedTerms)).
	logtalk::message_hook(prolog_dialect_term_expansion(File, Lines, Term, ExpandedTerms), warning(portability), core, _) :-
		assertz(prolog_dialect_term_expansion(File, Lines, Term, ExpandedTerms)).
	logtalk::message_hook(compiling_proprietary_prolog_directive(File, Lines, Type, Entity, Directive), warning(portability), core, _) :-
		assertz(compiling_proprietary_prolog_directive(File, Lines, Type, Entity, Directive)).
	logtalk::message_hook(compiling_query_as_initialization_goal(File, Lines, Type, Entity, Directive), warning(portability), core, _) :-
		assertz(compiling_query_as_initialization_goal(File, Lines, Type, Entity, Directive)).
	logtalk::message_hook(logtalk_built_in_predicate_as_directive(File, Lines, Directive), warning(portability), core, _) :-
		assertz(logtalk_built_in_predicate_as_directive(File, Lines, Directive)).
	logtalk::message_hook(top_level_shortcut_as_directive(File, Lines, Directive), warning(portability), core, _) :-
		assertz(top_level_shortcut_as_directive(File, Lines, Directive)).
	logtalk::message_hook(non_standard_file_directive(File, Lines, Directive), warning(portability), core, _) :-
		assertz(non_standard_file_directive(File, Lines, Directive)).
	logtalk::message_hook(non_standard_prolog_flag(File, Lines, Type, Entity, Flag), warning(portability), core, _) :-
		assertz(non_standard_prolog_flag(File, Lines, Type, Entity, Flag)).
	logtalk::message_hook(non_standard_prolog_flag(File, Lines, Flag), warning(portability), core, _) :-
		assertz(non_standard_prolog_flag(File, Lines, Flag)).
	logtalk::message_hook(non_standard_prolog_flag_value(File, Lines, Type, Entity, Flag, Value), warning(portability), core, _) :-
		assertz(non_standard_prolog_flag_value(File, Lines, Type, Entity, Flag, Value)).
	logtalk::message_hook(non_standard_prolog_flag_value(File, Lines, Flag, Value), warning(portability), core, _) :-
		assertz(non_standard_prolog_flag_value(File, Lines, Flag, Value)).
	logtalk::message_hook(non_standard_predicate_option(File, Lines, Type, Entity, Predicate, Option), warning(portability), core, _) :-
		assertz(non_standard_predicate_option(File, Lines, Type, Entity, Predicate, Option)).
	logtalk::message_hook(non_standard_predicate_call(File, Lines, Type, Entity, Predicate), warning(portability), core, _) :-
		assertz(non_standard_predicate_call(File, Lines, Type, Entity, Predicate)).
	logtalk::message_hook(non_standard_arithmetic_function_call(File, Lines, Type, Entity, Function), warning(portability), core, _) :-
		assertz(non_standard_arithmetic_function_call(File, Lines, Type, Entity, Function)).
	logtalk::message_hook(missing_function(File, Lines, Type, Entity, Function), warning(portability), core, _) :-
		assertz(missing_function(File, Lines, Type, Entity, Function)).

:- end_object.

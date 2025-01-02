%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-08-14,
		comment is 'Unit tests for the ``grammar_rules`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(calls_non_terminal_as_predicate/5).
	:- dynamic(calls_non_terminal_as_predicate/5).

	:- private(calls_predicate_as_non_terminal/5).
	:- dynamic(calls_predicate_as_non_terminal/5).

	:- private(unsound_construct_in_grammar_rule/5).
	:- dynamic(unsound_construct_in_grammar_rule/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [grammar_rules(warning)]).

	cleanup :-
		retractall(calls_non_terminal_as_predicate(_, _, _, _, _)),
		retractall(calls_predicate_as_non_terminal(_, _, _, _, _)).

	test(grammar_rules_linter_flag_01, exists(Term == bar//0)) :-
		calls_non_terminal_as_predicate(_, _, object, non_terminal_called_as_a_predicate, Term).

	test(grammar_rules_linter_flag_02, exists(Term == p/3)) :-
		calls_predicate_as_non_terminal(_, _, object, predicate_called_as_a_non_terminal, Term).

	test(grammar_rules_linter_flag_03, exists(variant(Term, \+ p(_)))) :-
		unsound_construct_in_grammar_rule(_, _, object, unsound_construct_in_grammar_rule, Term).

	test(grammar_rules_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(calls_non_terminal_as_predicate(file, 1-2, object, grammar_rules, a//0), core), Tokens).

	test(grammar_rules_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(calls_predicate_as_non_terminal(file, 1-2, object, grammar_rules, a/0), core), Tokens).

	test(grammar_rules_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(unsound_construct_in_grammar_rule(file, 1-2, object, grammar_rules, \+ a), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(calls_non_terminal_as_predicate(File, Lines, Type, Entity, NonTerminal), warning(grammar_rules), core, _) :-
		assertz(calls_non_terminal_as_predicate(File, Lines, Type, Entity, NonTerminal)).
	logtalk::message_hook(calls_predicate_as_non_terminal(File, Lines, Type, Entity, Predicate), warning(grammar_rules), core, _) :-
		assertz(calls_predicate_as_non_terminal(File, Lines, Type, Entity, Predicate)).
	logtalk::message_hook(unsound_construct_in_grammar_rule(File, Lines, Type, Entity, Construct), warning(grammar_rules), core, _) :-
		assertz(unsound_construct_in_grammar_rule(File, Lines, Type, Entity, Construct)).

:- end_object.

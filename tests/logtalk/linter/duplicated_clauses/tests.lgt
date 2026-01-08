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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-08-13,
		comment is 'Unit tests for the ``duplicated_clauses`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(duplicated_clause/7).
	:- dynamic(duplicated_clause/7).

	:- private(duplicated_grammar_rule/7).
	:- dynamic(duplicated_grammar_rule/7).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [duplicated_clauses(warning)]).

	cleanup :-
		retractall(duplicated_clause(_, _, _, _, _, _, _)),
		retractall(duplicated_grammar_rule(_, _, _, _, _, _, _)).

	test(duplicated_clauses_linter_flag_01, exists(Term == a(1))) :-
		duplicated_clause(_, _, object, duplicated_clauses, Term, _, _).

	test(duplicated_clauses_linter_flag_02, exists(variant(Term, (b(X, Y) :- a(X), a(Y))))) :-
		duplicated_clause(_, _, object, duplicated_clauses, Term, _, _).

	test(duplicated_clauses_linter_flag_03, exists(Term == (c --> [1], d))) :-
		duplicated_grammar_rule(_, _, object, duplicated_clauses, Term, _, _).

	test(duplicated_clauses_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(duplicated_clause(file, 1-2, object, duplicated_clauses, a, original, 3-4), core), Tokens).

	test(duplicated_clauses_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(duplicated_grammar_rule(file, 1-2, object, duplicated_clauses, a, original, 3-4), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(duplicated_clause(File, Lines, Type, Entity, Original, OriginalFile, OriginalLines), warning(duplicated_clauses), core, _) :-
		assertz(duplicated_clause(File, Lines, Type, Entity, Original, OriginalFile, OriginalLines)).
	logtalk::message_hook(duplicated_grammar_rule(File, Lines, Type, Entity, Original, OriginalFile, OriginalLines), warning(duplicated_clauses), core, _) :-
		assertz(duplicated_grammar_rule(File, Lines, Type, Entity, Original, OriginalFile, OriginalLines)).

:- end_object.

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
		date is 2026-02-14,
		comment is 'Unit tests for the "c45" library.'
	]).

	cover(c45).

	cleanup :-
		^^clean_file('test_output.pl').

	% learn/2 tests - play_tennis dataset

	test(c45_learn_2_play_tennis, deterministic) :-
		c45::learn(play_tennis, Tree),
		ground(Tree).

	test(c45_learn_2_play_tennis_root_is_tree, true(Attribute == outlook)) :-
		c45::learn(play_tennis, tree(Attribute, _)).

	test(c45_learn_2_play_tennis_overcast_is_leaf, true(Class == yes)) :-
		c45::learn(play_tennis, tree(outlook, Subtrees)),
		list::member(overcast-leaf(Class), Subtrees).

	% learn/2 tests - contact_lenses dataset

	test(c45_learn_2_contact_lenses, deterministic) :-
		c45::learn(contact_lenses, Tree),
		ground(Tree).

	test(c45_learn_2_contact_lenses_root_is_tree, true) :-
		c45::learn(contact_lenses, tree(_, _)).

	test(c45_learn_2_contact_lenses_reduced_tears_none, true(Class == none)) :-
		c45::learn(contact_lenses, tree(tear_production_rate, Subtrees)),
		list::member(reduced-leaf(Class), Subtrees).

	% tree_to_clauses/4 tests

	test(c45_tree_to_clauses_4_play_tennis, true) :-
		c45::learn(play_tennis, Tree),
		c45::tree_to_clauses(play_tennis, Tree, classify, Clauses),
		list::length(Clauses, N),
		N > 0.

	test(c45_tree_to_clauses_4_clauses_are_ground, true) :-
		c45::learn(play_tennis, Tree),
		c45::tree_to_clauses(play_tennis, Tree, classify, Clauses),
		forall(
			list::member(Clause, Clauses),
			(Clause =.. [classify| Args], list::length(Args, 5))
		).

	test(c45_tree_to_clauses_4_contact_lenses, true) :-
		c45::learn(contact_lenses, Tree),
		c45::tree_to_clauses(contact_lenses, Tree, recommend, Clauses),
		list::length(Clauses, N),
		N > 0.

	% tree_to_file/4 tests

	test(c45_tree_to_file_4_play_tennis, deterministic) :-
		^^file_path('test_output.pl', File),
		c45::learn(play_tennis, Tree),
		c45::tree_to_file(play_tennis, Tree, classify, File).

	% print_tree/1 tests

	test(c45_print_tree_1_play_tennis, deterministic) :-
		^^suppress_text_output,
		c45::learn(play_tennis, Tree),
		c45::print_tree(Tree).

	test(c45_print_tree_1_leaf, deterministic) :-
		^^suppress_text_output,
		c45::print_tree(leaf(yes)).

	test(c45_print_tree_1_contact_lenses, deterministic) :-
		^^suppress_text_output,
		c45::learn(contact_lenses, Tree),
		c45::print_tree(Tree).

	% learn/2 tests - iris dataset (continuous attributes)

	test(c45_learn_2_iris, deterministic) :-
		c45::learn(iris, Tree),
		ground(Tree).

	test(c45_learn_2_iris_root_is_continuous, true) :-
		c45::learn(iris, tree(_, threshold(_), _, _)).

	% tree_to_clauses/4 tests - iris dataset

	test(c45_tree_to_clauses_4_iris, true(N > 0)) :-
		c45::learn(iris, Tree),
		c45::tree_to_clauses(iris, Tree, classify, Clauses),
		list::length(Clauses, N).

	% print_tree/1 tests - iris dataset

	test(c45_print_tree_1_iris, deterministic) :-
		^^suppress_text_output,
		c45::learn(iris, Tree),
		c45::print_tree(Tree).

	% learn/2 tests - breast_cancer dataset (missing values)

	test(c45_learn_2_breast_cancer, deterministic) :-
		c45::learn(breast_cancer, Tree),
		ground(Tree).

	test(c45_learn_2_breast_cancer_root_is_tree, true) :-
		c45::learn(breast_cancer, tree(_, _)).

	% tree_to_clauses/4 tests - breast_cancer dataset

	test(c45_tree_to_clauses_4_breast_cancer, true(N > 0)) :-
		c45::learn(breast_cancer, Tree),
		c45::tree_to_clauses(breast_cancer, Tree, classify, Clauses),
		list::length(Clauses, N).

	% print_tree/1 tests - breast_cancer dataset

	test(c45_print_tree_1_breast_cancer, deterministic) :-
		^^suppress_text_output,
		c45::learn(breast_cancer, Tree),
		c45::print_tree(Tree).

:- end_object.


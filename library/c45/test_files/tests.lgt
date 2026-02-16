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
		date is 2026-02-16,
		comment is 'Unit tests for the "c45" library.'
	]).

	cover(c45).

	cleanup :-
		^^clean_file('test_output.pl').

	% learn/2 tests - play_tennis dataset

	test(c45_learn_2_play_tennis, deterministic(ground(Tree))) :-
		c45::learn(play_tennis, Tree).

	test(c45_learn_2_play_tennis_root_is_tree, true(Attribute == outlook)) :-
		c45::learn(play_tennis, tree(Attribute, _)).

	test(c45_learn_2_play_tennis_overcast_is_leaf, true(Class == yes)) :-
		c45::learn(play_tennis, tree(outlook, Subtrees)),
		list::member(overcast-leaf(Class), Subtrees).

	% learn/2 tests - contact_lenses dataset

	test(c45_learn_2_contact_lenses, deterministic(ground(Tree))) :-
		c45::learn(contact_lenses, Tree).

	test(c45_learn_2_contact_lenses_root_is_tree, true) :-
		c45::learn(contact_lenses, tree(_, _)).

	test(c45_learn_2_contact_lenses_reduced_tears_none, true(Class == none)) :-
		c45::learn(contact_lenses, tree(tear_production_rate, Subtrees)),
		list::member(reduced-leaf(Class), Subtrees).

	% classifier_to_clauses/4 tests

	test(c45_classifier_to_clauses_4_play_tennis, true(N > 0)) :-
		c45::learn(play_tennis, Tree),
		c45::classifier_to_clauses(play_tennis, Tree, classify, Clauses),
		list::length(Clauses, N).

	test(c45_classifier_to_clauses_4_clauses_are_ground, true) :-
		c45::learn(play_tennis, Tree),
		c45::classifier_to_clauses(play_tennis, Tree, classify, Clauses),
		forall(
			list::member(Clause, Clauses),
			(Clause =.. [classify| Arguments], list::length(Arguments, 5))
		).

	test(c45_classifier_to_clauses_4_contact_lenses, true(N > 0)) :-
		c45::learn(contact_lenses, Tree),
		c45::classifier_to_clauses(contact_lenses, Tree, recommend, Clauses),
		list::length(Clauses, N).

	% classifier_to_file/4 tests

	test(c45_classifier_to_file_4_play_tennis, deterministic) :-
		^^file_path('test_output.pl', File),
		c45::learn(play_tennis, Tree),
		c45::classifier_to_file(play_tennis, Tree, classify, File).

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

	test(c45_learn_2_iris, deterministic(ground(Tree))) :-
		c45::learn(iris, Tree).

	test(c45_learn_2_iris_root_is_continuous, true) :-
		c45::learn(iris, tree(_, threshold(_), _, _)).

	% classifier_to_clauses/4 tests - iris dataset

	test(c45_classifier_to_clauses_4_iris, true(N > 0)) :-
		c45::learn(iris, Tree),
		c45::classifier_to_clauses(iris, Tree, classify, Clauses),
		list::length(Clauses, N).

	% print_tree/1 tests - iris dataset

	test(c45_print_tree_1_iris, deterministic) :-
		^^suppress_text_output,
		c45::learn(iris, Tree),
		c45::print_tree(Tree).

	% learn/2 tests - breast_cancer dataset (missing values)

	test(c45_learn_2_breast_cancer, deterministic(ground(Tree))) :-
		c45::learn(breast_cancer, Tree).

	test(c45_learn_2_breast_cancer_root_is_tree, true) :-
		c45::learn(breast_cancer, tree(_, _)).

	% classifier_to_clauses/4 tests - breast_cancer dataset

	test(c45_classifier_to_clauses_4_breast_cancer, true(N > 0)) :-
		c45::learn(breast_cancer, Tree),
		c45::classifier_to_clauses(breast_cancer, Tree, classify, Clauses),
		list::length(Clauses, N).

	% print_tree/1 tests - breast_cancer dataset

	test(c45_print_tree_1_breast_cancer, deterministic) :-
		^^suppress_text_output,
		c45::learn(breast_cancer, Tree),
		c45::print_tree(Tree).

	% prune/3 tests - play_tennis dataset

	test(c45_prune_3_play_tennis, deterministic(ground(PrunedTree))) :-
		c45::learn(play_tennis, Tree),
		c45::prune(play_tennis, Tree, PrunedTree).

	test(c45_prune_3_play_tennis_clauses, true(N > 0)) :-
		c45::learn(play_tennis, Tree),
		c45::prune(play_tennis, Tree, PrunedTree),
		c45::classifier_to_clauses(play_tennis, PrunedTree, classify, Clauses),
		list::length(Clauses, N).

	% prune/3 tests - breast_cancer dataset (larger, more likely to be pruned)

	test(c45_prune_3_breast_cancer, deterministic(ground(PrunedTree))) :-
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, PrunedTree).

	test(c45_prune_3_breast_cancer_different, true(Tree \== PrunedTree)) :-
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, PrunedTree).

	test(c45_prune_3_breast_cancer_clauses, true(N > 0)) :-
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, PrunedTree),
		c45::classifier_to_clauses(breast_cancer, PrunedTree, classify, Clauses),
		list::length(Clauses, N).

	% prune/3 tests - iris dataset (continuous attributes)

	test(c45_prune_3_iris, deterministic(ground(PrunedTree))) :-
		c45::learn(iris, Tree),
		c45::prune(iris, Tree, PrunedTree).

	test(c45_prune_3_iris_clauses, true(N > 0)) :-
		c45::learn(iris, Tree),
		c45::prune(iris, Tree, PrunedTree),
		c45::classifier_to_clauses(iris, PrunedTree, classify, Clauses),
		list::length(Clauses, N).

	% prune/5 tests - custom confidence factor and minimum instances

	test(c45_prune_5_breast_cancer, deterministic(ground(PrunedTree))) :-
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, 0.25, 2, PrunedTree).

	test(c45_prune_5_breast_cancer_high_min_instances, deterministic(ground(PrunedTree))) :-
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, 0.25, 5, PrunedTree).

	test(c45_prune_5_iris, deterministic(ground(PrunedTree))) :-
		c45::learn(iris, Tree),
		c45::prune(iris, Tree, 0.25, 2, PrunedTree).

	test(c45_prune_5_play_tennis, deterministic(ground(PrunedTree))) :-
		c45::learn(play_tennis, Tree),
		c45::prune(play_tennis, Tree, 0.25, 2, PrunedTree).

	test(c45_prune_5_breast_cancer_low_cf, deterministic(ground(PrunedTree))) :-
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, 0.1, 2, PrunedTree).

	test(c45_prune_5_breast_cancer_high_cf, deterministic(ground(PrunedTree))) :-
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, 0.5, 2, PrunedTree).

	test(c45_prune_5_low_cf_more_aggressive, true(N1 =< N2)) :-
		% Lower confidence factor should result in more aggressive pruning
		% More aggressive pruning means fewer or equal clauses
		c45::learn(breast_cancer, Tree),
		c45::prune(breast_cancer, Tree, 0.05, 2, PrunedTree1),
		c45::prune(breast_cancer, Tree, 0.5,  2, PrunedTree2),
		c45::classifier_to_clauses(breast_cancer, PrunedTree1, classify, Clauses1),
		c45::classifier_to_clauses(breast_cancer, PrunedTree2, classify, Clauses2),
		list::length(Clauses1, N1),
		list::length(Clauses2, N2).

	% Classification verification tests - play_tennis dataset
	% These tests verify that generated clauses correctly classify training examples

	test(c45_classify_play_tennis_example_1, true(Class == no)) :-
		% Example 1: sunny, hot, high, weak -> no
		c45::learn(play_tennis, Tree),
		c45::classifier_to_clauses(play_tennis, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(sunny, hot, high, weak, Class),
		abolish_object(Classifier).

	test(c45_classify_play_tennis_example_3, true(Class == yes)) :-
		% Example 3: overcast, hot, high, weak -> yes (overcast always means play)
		c45::learn(play_tennis, Tree),
		c45::classifier_to_clauses(play_tennis, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(overcast, hot, high, weak, Class),
		abolish_object(Classifier).

	% Classification verification tests - contact_lenses dataset

	test(c45_classify_contact_lenses_example_2, true(Class == none)) :-
		% Example 2: young, myope, no astigmatism, reduced tears -> none
		c45::learn(contact_lenses, Tree),
		c45::classifier_to_clauses(contact_lenses, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(young, myope, no, reduced, Class),
		abolish_object(Classifier).

	test(c45_classify_contact_lenses_example_5, true(Class == soft)) :-
		% Example 5: young, hypermetrope, no astigmatism, normal tears -> soft
		c45::learn(contact_lenses, Tree),
		c45::classifier_to_clauses(contact_lenses, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(young, hypermetrope, no, normal, Class),
		abolish_object(Classifier).

	% Classification verification tests - iris dataset (continuous attributes)

	test(c45_classify_iris_setosa, true(Class == setosa)) :-
		% Example 1: sepal_length=5.1, sepal_width=3.5, petal_length=1.4, petal_width=0.2 -> setosa
		c45::learn(iris, Tree),
		c45::classifier_to_clauses(iris, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(5.1, 3.5, 1.4, 0.2, Class),
		abolish_object(Classifier).

	test(c45_classify_iris_virginica, true(Class == virginica)) :-
		% Example 150: sepal_length=5.9, sepal_width=3.0, petal_length=5.1, petal_width=1.8 -> virginica
		c45::learn(iris, Tree),
		c45::classifier_to_clauses(iris, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(5.9, 3.0, 5.1, 1.8, Class),
		abolish_object(Classifier).

	% Classification verification tests - breast_cancer dataset

	test(c45_classify_breast_cancer_example_1, true(Class == no_recurrence_events)) :-
		% Example 1: 30_39, premeno, 30_34, 0_2, no, 3, left, left_low, no -> no_recurrence_events
		c45::learn(breast_cancer, Tree),
		c45::classifier_to_clauses(breast_cancer, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/10)], Clauses),
		Classifier::classify('30_39', premeno, '30_34', '0_2', no, '3', left, left_low, no, Class),
		abolish_object(Classifier).

	test(c45_classify_breast_cancer_example_225, true(Class == recurrence_events)) :-
		% Example 225: 30_39, premeno, 25_29, 3_5, yes, 3, left, left_low, yes -> recurrence_events
		c45::learn(breast_cancer, Tree),
		c45::classifier_to_clauses(breast_cancer, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/10)], Clauses),
		Classifier::classify('30_39', premeno, '25_29', '3_5', yes, '3', left, left_low, yes, Class),
		abolish_object(Classifier).

	% predict/3 tests

	test(c45_predict_3_play_tennis_no, true(Class == no)) :-
		% Example 1: outlook=sunny, temperature=hot, humidity=high, wind=weak -> no
		c45::learn(play_tennis, Tree),
		c45::predict(Tree, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(c45_predict_3_play_tennis_yes, true(Class == yes)) :-
		% Example 3: outlook=overcast, temperature=hot, humidity=high, wind=weak -> yes
		c45::learn(play_tennis, Tree),
		c45::predict(Tree, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Class).

	test(c45_predict_3_contact_lenses_none, true(Class == none)) :-
		% Example 2: age=young, spectacle_prescription=myope, astigmatism=no, tear_production_rate=reduced -> none
		c45::learn(contact_lenses, Tree),
		c45::predict(Tree, [age-young, spectacle_prescription-myope, astigmatism-no, tear_production_rate-reduced], Class).

	test(c45_predict_3_contact_lenses_soft, true(Class == soft)) :-
		% Example 5: age=young, spectacle_prescription=hypermetrope, astigmatism=no, tear_production_rate=normal -> soft
		c45::learn(contact_lenses, Tree),
		c45::predict(Tree, [age-young, spectacle_prescription-hypermetrope, astigmatism-no, tear_production_rate-normal], Class).

	test(c45_predict_3_iris_setosa, true(Class == setosa)) :-
		% Example 1: sepal_length=5.1, sepal_width=3.5, petal_length=1.4, petal_width=0.2 -> setosa
		c45::learn(iris, Tree),
		c45::predict(Tree, [sepal_length-5.1, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	test(c45_predict_3_iris_virginica, true(Class == virginica)) :-
		% Example 150: sepal_length=5.9, sepal_width=3.0, petal_length=5.1, petal_width=1.8 -> virginica
		c45::learn(iris, Tree),
		c45::predict(Tree, [sepal_length-5.9, sepal_width-3.0, petal_length-5.1, petal_width-1.8], Class).

	test(c45_predict_3_breast_cancer_no_recurrence, true(Class == no_recurrence_events)) :-
		% Example 1: 30_39, premeno, 30_34, 0_2, no, 3, left, left_low, no -> no_recurrence_events
		c45::learn(breast_cancer, Tree),
		c45::predict(Tree, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no], Class).

	test(c45_predict_3_breast_cancer_recurrence, true(Class == recurrence_events)) :-
		% Example 225: 30_39, premeno, 25_29, 3_5, yes, 3, left, left_low, yes -> recurrence_events
		c45::learn(breast_cancer, Tree),
		c45::predict(Tree, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes], Class).

:- end_object.


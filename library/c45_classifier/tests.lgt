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
		version is 3:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Unit tests for the "c45_classifier" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	cover(c45_classifier).

	cleanup :-
		^^clean_file('test_output.pl').

	% learn/2 tests - play_tennis dataset

	test(c45_learn_2_play_tennis, deterministic(ground(Tree))) :-
		c45_classifier::learn(play_tennis, Tree).

	test(c45_valid_classifier_1, deterministic(c45_classifier::valid_classifier(Tree))) :-
		c45_classifier::learn(play_tennis, Tree).

	test(c45_invalid_classifier_1, fail) :-
		c45_classifier::valid_classifier(tree(outlook, [sunny-leaf(no), sunny-leaf(yes)])).

	test(c45_learn_2_play_tennis_root_is_tree, true(Attribute == outlook)) :-
		c45_classifier::learn(play_tennis, c45_model(tree(Attribute, _), _)).

	test(c45_learn_2_play_tennis_overcast_is_leaf, deterministic(Class == yes)) :-
		c45_classifier::learn(play_tennis, c45_model(tree(outlook, Subtrees), _)),
		memberchk(overcast-leaf(Class), Subtrees).

	% learn/2 tests - contact_lenses dataset

	test(c45_learn_2_contact_lenses, deterministic(ground(Tree))) :-
		c45_classifier::learn(contact_lenses, Tree).

	test(c45_learn_2_contact_lenses_root_is_tree, true) :-
		c45_classifier::learn(contact_lenses, c45_model(tree(_, _), _)).

	test(c45_learn_2_contact_lenses_reduced_tears_none, deterministic(Class == none)) :-
		c45_classifier::learn(contact_lenses, c45_model(tree(tear_production_rate, Subtrees), _)),
		memberchk(reduced-leaf(Class), Subtrees).

	% export_to_clauses/4 tests

	test(c45_export_to_clauses_4_play_tennis, deterministic(N > 0)) :-
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::export_to_clauses(play_tennis, Tree, classify, Clauses),
		length(Clauses, N).

	test(c45_export_to_clauses_4_clauses_are_ground, deterministic) :-
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::export_to_clauses(play_tennis, Tree, classify, Clauses),
		forall(
			member(Clause, Clauses),
			(	(	Clause = (Head :- _) ->
					true
				;	Head = Clause
				),
				Head =.. [classify| Arguments],
				length(Arguments, 5)
			)
		).

	test(c45_export_to_clauses_4_contact_lenses, deterministic(N > 0)) :-
		c45_classifier::learn(contact_lenses, Tree),
		c45_classifier::export_to_clauses(contact_lenses, Tree, recommend, Clauses),
		length(Clauses, N).

	% export_to_file/4 tests

	test(c45_export_to_file_4_play_tennis, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::export_to_file(play_tennis, Tree, classify, File).

	test(c45_diagnostics_2, deterministic(Diagnostics == [model(c45_classifier), classes([no, yes])])) :-
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::diagnostics(Tree, Diagnostics).

	% print_classifier/1 tests

	test(c45_print_classifier_1_play_tennis, deterministic) :-
		^^suppress_text_output,
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::print_classifier(Tree).

	test(c45_print_classifier_1_leaf, deterministic) :-
		^^suppress_text_output,
		c45_classifier::print_classifier(leaf(yes)).

	test(c45_print_classifier_1_contact_lenses, deterministic) :-
		^^suppress_text_output,
		c45_classifier::learn(contact_lenses, Tree),
		c45_classifier::print_classifier(Tree).

	% learn/2 tests - iris dataset (continuous attributes)

	test(c45_learn_2_iris, deterministic(ground(Tree))) :-
		c45_classifier::learn(iris, Tree).

	test(c45_learn_2_iris_root_is_continuous, deterministic) :-
		c45_classifier::learn(iris, c45_model(tree(_, threshold(_), _, _), _)).

	% export_to_clauses/4 tests - iris dataset

	test(c45_export_to_clauses_4_iris, deterministic(N > 0)) :-
		c45_classifier::learn(iris, Tree),
		c45_classifier::export_to_clauses(iris, Tree, classify, Clauses),
		length(Clauses, N).

	% print_classifier/1 tests - iris dataset

	test(c45_print_classifier_1_iris, deterministic) :-
		^^suppress_text_output,
		c45_classifier::learn(iris, Tree),
		c45_classifier::print_classifier(Tree).

	% learn/2 tests - breast_cancer dataset (missing values)

	test(c45_learn_2_breast_cancer, deterministic(ground(Tree))) :-
		c45_classifier::learn(breast_cancer, Tree).

	test(c45_learn_2_breast_cancer_root_is_tree, true) :-
		c45_classifier::learn(breast_cancer, c45_model(tree(_, _), _)).

	% export_to_clauses/4 tests - breast_cancer dataset

	test(c45_export_to_clauses_4_breast_cancer, deterministic(N > 0)) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::export_to_clauses(breast_cancer, Tree, classify, Clauses),
		length(Clauses, N).

	% print_classifier/1 tests - breast_cancer dataset

	test(c45_print_classifier_1_breast_cancer, deterministic) :-
		^^suppress_text_output,
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::print_classifier(Tree).

	% prune/3 tests - play_tennis dataset

	test(c45_prune_3_play_tennis, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::prune(play_tennis, Tree, PrunedTree).

	test(c45_prune_3_play_tennis_clauses, deterministic(N > 0)) :-
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::prune(play_tennis, Tree, PrunedTree),
		c45_classifier::export_to_clauses(play_tennis, PrunedTree, classify, Clauses),
		length(Clauses, N).

	% prune/3 tests - breast_cancer dataset (larger, more likely to be pruned)

	test(c45_prune_3_breast_cancer, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, PrunedTree).

	test(c45_prune_3_breast_cancer_different, deterministic(Tree \== PrunedTree)) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, PrunedTree).

	test(c45_prune_3_breast_cancer_clauses, deterministic(N > 0)) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, PrunedTree),
		c45_classifier::export_to_clauses(breast_cancer, PrunedTree, classify, Clauses),
		length(Clauses, N).

	% prune/3 tests - iris dataset (continuous attributes)

	test(c45_prune_3_iris, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(iris, Tree),
		c45_classifier::prune(iris, Tree, PrunedTree).

	test(c45_prune_3_iris_clauses, deterministic(N > 0)) :-
		c45_classifier::learn(iris, Tree),
		c45_classifier::prune(iris, Tree, PrunedTree),
		c45_classifier::export_to_clauses(iris, PrunedTree, classify, Clauses),
		length(Clauses, N).

	% prune/5 tests - custom confidence factor and minimum instances

	test(c45_prune_5_breast_cancer, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, 0.25, 2, PrunedTree).

	test(c45_prune_5_breast_cancer_high_min_instances, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, 0.25, 5, PrunedTree).

	test(c45_prune_5_iris, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(iris, Tree),
		c45_classifier::prune(iris, Tree, 0.25, 2, PrunedTree).

	test(c45_prune_5_play_tennis, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::prune(play_tennis, Tree, 0.25, 2, PrunedTree).

	test(c45_prune_5_breast_cancer_low_cf, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, 0.1, 2, PrunedTree).

	test(c45_prune_5_breast_cancer_high_cf, deterministic(ground(PrunedTree))) :-
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, 0.5, 2, PrunedTree).

	test(c45_prune_5_low_cf_more_aggressive, deterministic(N1 =< N2)) :-
		% Lower confidence factor should result in more aggressive pruning
		% More aggressive pruning means fewer or equal clauses
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::prune(breast_cancer, Tree, 0.05, 2, PrunedTree1),
		c45_classifier::prune(breast_cancer, Tree, 0.5,  2, PrunedTree2),
		tree_node_count(PrunedTree1, N1),
		tree_node_count(PrunedTree2, N2).

	% Classification verification tests - play_tennis dataset
	% These tests verify that generated clauses correctly classify training examples

	test(c45_classify_play_tennis_example_1, true(Class == no)) :-
		% Example 1: sunny, hot, high, weak -> no
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::export_to_clauses(play_tennis, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(sunny, hot, high, weak, Class),
		abolish_object(Classifier).

	test(c45_classify_play_tennis_example_3, true(Class == yes)) :-
		% Example 3: overcast, hot, high, weak -> yes (overcast always means play)
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::export_to_clauses(play_tennis, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(overcast, hot, high, weak, Class),
		abolish_object(Classifier).

	% Classification verification tests - contact_lenses dataset

	test(c45_classify_contact_lenses_example_2, true(Class == none)) :-
		% Example 2: young, myope, no astigmatism, reduced tears -> none
		c45_classifier::learn(contact_lenses, Tree),
		c45_classifier::export_to_clauses(contact_lenses, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(young, myope, no, reduced, Class),
		abolish_object(Classifier).

	test(c45_classify_contact_lenses_example_5, true(Class == soft)) :-
		% Example 5: young, hypermetrope, no astigmatism, normal tears -> soft
		c45_classifier::learn(contact_lenses, Tree),
		c45_classifier::export_to_clauses(contact_lenses, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(young, hypermetrope, no, normal, Class),
		abolish_object(Classifier).

	% Classification verification tests - iris dataset (continuous attributes)

	test(c45_classify_iris_setosa, true(Class == setosa)) :-
		% Example 1: sepal_length=5.1, sepal_width=3.5, petal_length=1.4, petal_width=0.2 -> setosa
		c45_classifier::learn(iris, Tree),
		c45_classifier::export_to_clauses(iris, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(5.1, 3.5, 1.4, 0.2, Class),
		abolish_object(Classifier).

	test(c45_classify_iris_virginica, true(Class == virginica)) :-
		% Example 150: sepal_length=5.9, sepal_width=3.0, petal_length=5.1, petal_width=1.8 -> virginica
		c45_classifier::learn(iris, Tree),
		c45_classifier::export_to_clauses(iris, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/5)], Clauses),
		Classifier::classify(5.9, 3.0, 5.1, 1.8, Class),
		abolish_object(Classifier).

	% Classification verification tests - breast_cancer dataset

	test(c45_classify_breast_cancer_example_1, true(Class == no_recurrence_events)) :-
		% Example 1: 30_39, premeno, 30_34, 0_2, no, 3, left, left_low, no -> no_recurrence_events
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::export_to_clauses(breast_cancer, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/10)], Clauses),
		Classifier::classify('30_39', premeno, '30_34', '0_2', no, '3', left, left_low, no, Class),
		abolish_object(Classifier).

	test(c45_classify_breast_cancer_example_225, true(Class == recurrence_events)) :-
		% Example 225: 30_39, premeno, 25_29, 3_5, yes, 3, left, left_low, yes -> recurrence_events
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::export_to_clauses(breast_cancer, Tree, classify, Clauses),
		create_object(Classifier, [], [public(classify/10)], Clauses),
		Classifier::classify('30_39', premeno, '25_29', '3_5', yes, '3', left, left_low, yes, Class),
		abolish_object(Classifier).

	% predict/3 tests

	test(c45_predict_3_play_tennis_no, deterministic(Class == no)) :-
		% Example 1: outlook=sunny, temperature=hot, humidity=high, wind=weak -> no
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::predict(Tree, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(c45_predict_3_play_tennis_yes, deterministic(Class == yes)) :-
		% Example 3: outlook=overcast, temperature=hot, humidity=high, wind=weak -> yes
		c45_classifier::learn(play_tennis, Tree),
		c45_classifier::predict(Tree, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Class).

	test(c45_predict_3_contact_lenses_none, deterministic(Class == none)) :-
		% Example 2: age=young, spectacle_prescription=myope, astigmatism=no, tear_production_rate=reduced -> none
		c45_classifier::learn(contact_lenses, Tree),
		c45_classifier::predict(Tree, [age-young, spectacle_prescription-myope, astigmatism-no, tear_production_rate-reduced], Class).

	test(c45_predict_3_contact_lenses_soft, deterministic(Class == soft)) :-
		% Example 5: age=young, spectacle_prescription=hypermetrope, astigmatism=no, tear_production_rate=normal -> soft
		c45_classifier::learn(contact_lenses, Tree),
		c45_classifier::predict(Tree, [age-young, spectacle_prescription-hypermetrope, astigmatism-no, tear_production_rate-normal], Class).

	test(c45_predict_3_iris_setosa, deterministic(Class == setosa)) :-
		% Example 1: sepal_length=5.1, sepal_width=3.5, petal_length=1.4, petal_width=0.2 -> setosa
		c45_classifier::learn(iris, Tree),
		c45_classifier::predict(Tree, [sepal_length-5.1, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	test(c45_predict_3_iris_virginica, deterministic(Class == virginica)) :-
		% Example 150: sepal_length=5.9, sepal_width=3.0, petal_length=5.1, petal_width=1.8 -> virginica
		c45_classifier::learn(iris, Tree),
		c45_classifier::predict(Tree, [sepal_length-5.9, sepal_width-3.0, petal_length-5.1, petal_width-1.8], Class).

	test(c45_predict_3_breast_cancer_no_recurrence, deterministic(Class == no_recurrence_events)) :-
		% Example 1: 30_39, premeno, 30_34, 0_2, no, 3, left, left_low, no -> no_recurrence_events
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::predict(Tree, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no], Class).

	test(c45_predict_3_breast_cancer_recurrence, deterministic(Class == recurrence_events)) :-
		% Example 225: 30_39, premeno, 25_29, 3_5, yes, 3, left, left_low, yes -> recurrence_events
		c45_classifier::learn(breast_cancer, Tree),
		c45_classifier::predict(Tree, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes], Class).

	test(c45_classifier_fixture_integrity, deterministic) :-
		forall(
			member(Kind, [integer_numeric, extreme_numeric, constant_xor, missing, all_missing, categorical, singleton, attribute_free]),
			(
				Dataset = c45_fixture(Kind),
				Dataset::class_values(Classes),
				findall(example(Id, Class, Values), Dataset::example(Id, Class, Values), Examples),
				assertion(Examples \== []),
				forall(member(example(_, Class, _), Examples), assertion(list::memberchk(Class, Classes)))
			)
		).

	test(c45_classifier_singleton_baseline, deterministic(Tree == leaf(yes))) :-
		c45_classifier::learn(c45_fixture(singleton), c45_model(Tree, _)).

	test(c45_classifier_attribute_free_baseline, deterministic(Tree == leaf(yes))) :-
		c45_classifier::learn(c45_fixture(attribute_free), c45_model(Tree, _)).

	test(c45_classifier_empty_internal_tree, deterministic(Tree == leaf(unknown))) :-
		c45_classifier<<build_tree([], [], [], Tree).

	test(c45_classifier_empty_known_examples, deterministic(Known == [])) :-
		c45_classifier<<known_examples([], x, Known).

	test(c45_classifier_check_variable, error(instantiation_error)) :-
		c45_classifier::check_classifier(_).

	test(c45_classifier_check_invalid, error(domain_error(classifier, invalid))) :-
		c45_classifier::check_classifier(invalid).

	test(c45_classifier_open_subtrees, deterministic(var(Tail))) :-
		assertion(\+ c45_classifier::valid_classifier(tree(a, [ordinary-leaf(yes)| Tail]))).

	test(c45_classifier_variable_leaf, deterministic(var(Class))) :-
		assertion(\+ c45_classifier::valid_classifier(leaf(Class))).

	test(c45_classifier_malformed_models, all(\+ c45_classifier::valid_classifier(Tree))) :-
		member(Tree, [tree(a, []), tree(a, [invalid]), tree(a, [ordinary-leaf(yes)| invalid]), tree(a, threshold(1), leaf(yes), leaf(no))]).

	test(c45_classifier_prune_invalid_confidence, all(Formal == domain_error(confidence_factor, Factor))) :-
		member(Factor, [0.0, -0.1, 0.9, 1.0, 2.0, 1, invalid]),
		catch(c45_classifier::prune(play_tennis, leaf(yes), Factor, 2, _), error(Formal, _), true).

	test(c45_classifier_prune_invalid_minimum, all(Formal == domain_error(positive_integer, Minimum))) :-
		member(Minimum, [0, -1, 2.0, invalid]),
		catch(c45_classifier::prune(play_tennis, leaf(yes), 0.25, Minimum, _), error(Formal, _), true).

	test(c45_classifier_prune_variable_confidence, error(instantiation_error)) :-
		c45_classifier::prune(play_tennis, leaf(yes), _, 2, _).

	test(c45_classifier_prune_variable_minimum, error(instantiation_error)) :-
		c45_classifier::prune(play_tennis, leaf(yes), 0.25, _, _).

	test(c45_classifier_prune_invalid_tree, error(domain_error(classifier, invalid))) :-
		c45_classifier::prune(play_tennis, invalid, _).

	test(c45_classifier_prune_empty_dataset, error(domain_error(non_empty_dataset, c45_fixture(empty)))) :-
		c45_classifier::prune(c45_fixture(empty), leaf(yes), _).

	test(c45_classifier_prune_incomplete_dataset, error(existence_error(attribute, x))) :-
		c45_classifier::prune(c45_fixture(incomplete), leaf(yes), _).

	test(c45_classifier_duplicate_attribute_declarations, error(domain_error(attribute_declarations, x))) :-
		c45_classifier::learn(c45_fixture(duplicate_attributes), _).

	test(c45_classifier_confidence_half, deterministic) :-
		c45_classifier<<confidence_to_zscore(0.5, ZScore),
		assertion(ZScore =~= 0.0),
		c45_classifier<<upper_confidence_bound(1, 10, ZScore, Bound),
		assertion(Bound =~= 0.1).

	test(c45_classifier_confidence_tiny, deterministic) :-
		c45_classifier<<confidence_to_zscore(1.0e-300, ZScore),
		assertion(ZScore > 30.0),
		c45_classifier<<upper_confidence_bound(1, 10, ZScore, Bound),
		assertion(Bound >= 0.1),
		assertion(Bound =< 1.0).

	test(c45_classifier_integer_numeric_threshold, deterministic) :-
		c45_classifier::learn(c45_fixture(integer_numeric), Tree),
		Tree = c45_model(tree(x, threshold(Threshold), leaf(yes), leaf(no)), _),
		assertion(Threshold =~= 1.5),
		assertion(c45_classifier::valid_classifier(Tree)),
		forall(c45_fixture(integer_numeric)::example(_, Class, Values), assertion(c45_classifier::predict(Tree, Values, Class))).

	test(c45_classifier_integer_midpoint_float, deterministic) :-
		c45_classifier<<candidate_thresholds([0, 2], [Threshold]),
		assertion(float(Threshold)),
		assertion(Threshold =~= 1.0),
		assertion(c45_classifier::valid_classifier(tree(x, threshold(Threshold), leaf(yes), leaf(no)))).

	test(c45_classifier_numeric_equal_values, deterministic(Thresholds == [1.5])) :-
		sort([2.0, 1.0, 2.0, 1.0], Sorted),
		c45_classifier<<candidate_thresholds(Sorted, Thresholds).

	test(c45_classifier_extreme_numeric_threshold, deterministic) :-
		c45_classifier::learn(c45_fixture(extreme_numeric), Tree),
		Tree = c45_model(tree(x, threshold(Threshold), leaf(yes), leaf(no)), _),
		assertion(Threshold >= 1.0e308),
		assertion(Threshold < 1.1e308),
		assertion(c45_classifier::valid_classifier(Tree)).

	test(c45_classifier_opposite_sign_midpoint, deterministic) :-
		c45_classifier<<candidate_thresholds([-1.0e308, 1.0e308], [Threshold]),
		assertion(Threshold =~= 0.0).

	test(c45_classifier_adjacent_float_midpoint, deterministic) :-
		Lower is 1.0000000000000002,
		Upper is 1.0000000000000004,
		c45_classifier<<candidate_thresholds([Lower, Upper], [Threshold]),
		assertion(Threshold >= Lower),
		assertion(Threshold < Upper).

	test(c45_classifier_empty_thresholds, deterministic(Thresholds == [])) :-
		c45_classifier<<candidate_thresholds([], Thresholds).

	test(c45_classifier_singleton_thresholds, deterministic(Thresholds == [])) :-
		c45_classifier<<candidate_thresholds([2.0], Thresholds).

	test(c45_classifier_equal_value_midpoint, deterministic(Thresholds == [])) :-
		c45_classifier<<candidate_thresholds([1.0, 1.0], Thresholds).

	test(c45_classifier_negative_midpoint, deterministic) :-
		c45_classifier<<candidate_thresholds([-4, -2.0], [Threshold]),
		assertion(Threshold =~= -3.0).

	test(c45_classifier_small_midpoint, deterministic) :-
		c45_classifier<<candidate_thresholds([1.0e-300, 2.0e-300], [Threshold]),
		assertion(Threshold >= 1.0e-300),
		assertion(Threshold < 2.0e-300).

	test(c45_classifier_constant_feature_xor, deterministic) :-
		c45_classifier::learn(c45_fixture(constant_xor), Tree),
		assertion(c45_classifier::valid_classifier(Tree)),
		forall(c45_fixture(constant_xor)::example(_, Class, Values), assertion(c45_classifier::predict(Tree, Values, Class))).

	test(c45_classifier_continuous_zero_gain_xor, deterministic) :-
		c45_classifier::learn(c45_fixture(continuous_xor), Tree),
		Tree = c45_model(tree(x, threshold(_), _, _), _),
		assertion(c45_classifier::valid_classifier(Tree)),
		forall(c45_fixture(continuous_xor)::example(_, Class, Values), assertion(c45_classifier::predict(Tree, Values, Class))).

	test(c45_classifier_unusable_attributes, all(Tree == leaf(no))) :-
		member(Kind, [constant, all_missing, missing_discrete]),
		c45_classifier::learn(c45_fixture(Kind), c45_model(Tree, _)).

	test(c45_classifier_candidate_ties, deterministic(Best == a)) :-
		c45_classifier<<select_best([none-2.0, a-0.0, none-1.0, b-0.0], Best).

	test(c45_classifier_empty_candidates, deterministic(Best == none)) :-
		c45_classifier<<select_best([], Best).

	test(c45_classifier_unusable_candidates, deterministic(Best == none)) :-
		c45_classifier<<select_best([none-0.0, none-0.0], Best).

	test(c45_classifier_unknown_internal_attribute, deterministic(Best == none)) :-
		Examples = [weighted_example(1, yes, [], 1.0), weighted_example(2, no, [], 1.0)],
		c45_classifier<<best_attribute(Examples, [absent], [], Best).

	test(c45_classifier_zero_internal_population, deterministic(Best == none)) :-
		c45_classifier<<best_attribute([], [x], [x-continuous], Best).

	test(c45_classifier_attribute_order_xor, deterministic) :-
		findall(Id-Class-Values, c45_fixture(constant_xor)::example(Id, Class, Values), Examples),
		c45_classifier<<build_tree(Examples, [b, a, x], [x-continuous, a-[0, 1], b-[0, 1]], Tree),
		forall(c45_fixture(constant_xor)::example(_, Class, Values), assertion(c45_classifier::predict(Tree, Values, Class))).

	test(c45_classifier_weighted_examples, deterministic) :-
		c45_classifier<<weight_examples([1-yes-[x-Value], 2-no-[x-1.0]], Weighted),
		Weighted = [weighted_example(1, yes, [x-Value], First), weighted_example(2, no, [x-1.0], Second)],
		assertion(First =~= 1.0),
		assertion(Second =~= 1.0),
		assertion(var(Value)).

	test(c45_classifier_weighted_class_counts, deterministic) :-
		Examples = [weighted_example(1, yes, [], 0.5), weighted_example(2, no, [], 2.0), weighted_example(3, yes, [], 0.5)],
		c45_classifier<<weighted_class_counts(Examples, [no-No, yes-Yes]),
		assertion(No =~= 2.0),
		assertion(Yes =~= 1.0),
		c45_classifier<<example_mass(Examples, Total),
		assertion(Total =~= 3.0).

	test(c45_classifier_fractional_discrete_partition, deterministic(var(Missing))) :-
		Examples = [weighted_example(1, yes, [a-first], 3.0), weighted_example(2, no, [a-second], 1.0), weighted_example(3, no, [a-Missing], 4.0)],
		c45_classifier<<partition_weighted_value(Examples, a, first, 0.5, First),
		c45_classifier<<partition_weighted_value(Examples, a, second, 0.5, Second),
		c45_classifier<<example_mass(First, FirstMass),
		c45_classifier<<example_mass(Second, SecondMass),
		assertion(FirstMass =~= 6.0),
		assertion(SecondMass =~= 2.0).

	test(c45_classifier_fractional_continuous_partition, deterministic(var(Missing))) :-
		Examples = [weighted_example(1, yes, [x-0.0], 3.0), weighted_example(2, no, [x-1.0], 1.0), weighted_example(3, no, [x-Missing], 4.0)],
		c45_classifier<<partition_weighted_threshold(Examples, x, 0.5, Left, Right),
		c45_classifier<<example_mass(Left, LeftMass),
		c45_classifier<<example_mass(Right, RightMass),
		assertion(LeftMass =~= 6.0),
		assertion(RightMass =~= 2.0).

	test(c45_classifier_all_missing_weighted_partition, deterministic(var(Value))) :-
		Examples = [weighted_example(1, yes, [x-Value], 2.0)],
		c45_classifier<<partition_weighted_threshold(Examples, x, 0.5, Left, Right),
		c45_classifier<<example_mass(Left, LeftMass),
		c45_classifier<<example_mass(Right, RightMass),
		assertion(LeftMass =~= 1.0),
		assertion(RightMass =~= 1.0),
		c45_classifier<<partition_weighted_value(Examples, x, first, 0.5, Discrete),
		c45_classifier<<example_mass(Discrete, DiscreteMass),
		assertion(DiscreteMass =~= 1.0).

	test(c45_classifier_zero_fraction_excluded, deterministic(Filtered == [])) :-
		Examples = [weighted_example(1, yes, [a-first], 1.0), weighted_example(2, no, [a-_], 2.0)],
		c45_classifier<<partition_weighted_value(Examples, a, absent, 0.0, Filtered).

	test(c45_classifier_empty_weighted_partitions, deterministic) :-
		c45_classifier<<partition_weighted_threshold([], x, 0.5, Left, Right),
		assertion(Left == []),
		assertion(Right == []),
		c45_classifier<<partition_weighted_value([], a, first, 0.0, Discrete),
		assertion(Discrete == []),
		c45_classifier<<weighted_class_counts([], Counts),
		assertion(Counts == []).

	test(c45_classifier_missing_continuous_gain, deterministic) :-
		findall(Id-Class-Values, c45_fixture(missing)::example(Id, Class, Values), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<threshold_gain_ratio(0.5, x, Weighted, 4.0, 1.0, Ratio),
		Expected is 1.0 / 3.0,
		assertion(Ratio =~= Expected),
		c45_classifier::learn(c45_fixture(missing), Tree),
		Tree = c45_model(RawTree, _),
		assertion(RawTree == tree(x, threshold(0.5), leaf(yes), leaf(no))).

	test(c45_classifier_missing_discrete_gain, deterministic) :-
		findall(Id-Class-Values, c45_fixture(missing_categorical)::example(Id, Class, Values), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<compute_gain_ratios([a], [a-[first, second]], Weighted, 4.0, 1.0, [candidate(a, _, Ratio)]),
		Expected is 1.0 / 3.0,
		assertion(Ratio =~= Expected),
		c45_classifier::learn(c45_fixture(missing_categorical), Tree),
		Tree = c45_model(RawTree, _),
		assertion(RawTree == tree(a, [first-leaf(yes), second-leaf(no)])).

	test(c45_classifier_weighted_majority, deterministic(Class == yes)) :-
		Examples = [weighted_example(1, yes, [], 2.0), weighted_example(2, no, [], 0.5), weighted_example(3, no, [], 0.5)],
		c45_classifier<<majority_class(Examples, Class).

	test(c45_classifier_weighted_entropy, deterministic) :-
		Examples = [weighted_example(1, yes, [], 0.5), weighted_example(2, no, [], 0.5)],
		c45_classifier<<entropy(Examples, 1.0, Entropy),
		assertion(Entropy =~= 1.0).

	test(c45_classifier_fractional_pruning_error, deterministic) :-
		findall(Id-Class-Values, c45_fixture(missing)::example(Id, Class, Values), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<subtree_error(tree(x, threshold(0.5), leaf(yes), leaf(no)), 2.0, Weighted, Error),
		assertion(Error >= 0.0),
		assertion(Error =< 1.0).

	test(c45_classifier_all_missing_pruning_mass, deterministic(var(Value))) :-
		Examples = [weighted_example(1, yes, [a-Value], 2.0)],
		Tree = tree(a, [first-leaf(yes), second-leaf(no)]),
		c45_classifier<<subtree_error(Tree, 2.0, Examples, Error),
		assertion(Error >= 0.0),
		assertion(Error =< 1.0).

	test(c45_classifier_weighted_error_count, deterministic) :-
		Examples = [weighted_example(1, yes, [], 0.75), weighted_example(2, no, [], 0.25)],
		c45_classifier<<count_errors(Examples, yes, 0.0, Error),
		assertion(Error =~= 0.25).

	test(c45_classifier_missing_continuous_prediction, deterministic(var(Value))) :-
		Tree = tree(x, threshold(0.5), leaf(yes), leaf(yes)),
		c45_classifier::predict(Tree, [x-Value], Class),
		assertion(Class == yes).

	test(c45_classifier_missing_discrete_prediction, deterministic(var(Value))) :-
		Tree = tree(a, [first-leaf(yes), second-leaf(yes), third-leaf(no)]),
		c45_classifier::predict(Tree, [a-Value], Class),
		assertion(Class == yes).

	test(c45_classifier_singleton_vote, deterministic(Class == yes)) :-
		c45_classifier<<majority_vote([yes], Class).

	test(c45_classifier_empty_leaf_estimate, deterministic) :-
		c45_classifier<<leaf_error([], 2.0, 2, Error, Class),
		assertion(Error =~= 1.0),
		assertion(Class == unknown).

	test(c45_classifier_degenerate_threshold_gain, all(Ratio =~= 0.0)) :-
		member(Threshold, [-1.0, 2.0]),
		Examples = [weighted_example(1, yes, [x-0.0], 1.0), weighted_example(2, no, [x-1.0], 1.0)],
		c45_classifier<<threshold_gain_ratio(Threshold, x, Examples, 2.0, 1.0, Ratio).

	test(c45_classifier_all_missing_threshold_gain, deterministic) :-
		Examples = [weighted_example(1, yes, [x-_], 1.0)],
		c45_classifier<<threshold_gain_ratio(0.5, x, Examples, 1.0, 0.0, Ratio),
		assertion(Ratio =~= 0.0).

	test(c45_classifier_zero_count_entropy, deterministic) :-
		c45_classifier<<compute_entropy([yes-0.0], 1.0, 0.0, Entropy),
		assertion(Entropy =~= 0.0).

	test(c45_classifier_annotated_continuous_prediction, deterministic(var(Value))) :-
		Tree = tree(x, threshold(0.5), leaf(yes), leaf(no)),
		Statistics = threshold_stats([no-2.0, yes-8.0], 8.0, leaf_stats([yes-8.0]), 2.0, leaf_stats([no-2.0])),
		Model = c45_model(Tree, Statistics),
		assertion(c45_classifier::valid_classifier(Model)),
		c45_classifier::predict(Model, [x-Value], Class),
		assertion(Class == yes),
		c45_classifier::predict(Model, [], AbsentClass),
		assertion(AbsentClass == yes),
		c45_classifier::predict(Model, [x-1.0], KnownClass),
		assertion(KnownClass == no).

	test(c45_classifier_annotated_discrete_prediction, deterministic(var(Value))) :-
		Tree = tree(a, [first-leaf(yes), second-leaf(no)]),
		Statistics = discrete_stats([no-2.0, yes-8.0], [branch(first, 8.0, leaf_stats([yes-8.0])), branch(second, 2.0, leaf_stats([no-2.0]))]),
		Model = c45_model(Tree, Statistics),
		assertion(c45_classifier::valid_classifier(Model)),
		c45_classifier::predict(Model, [a-Value], Class),
		assertion(Class == yes),
		c45_classifier::predict(Model, [a-unseen], UnseenClass),
		assertion(UnseenClass == yes),
		c45_classifier::predict(Model, [a-second], KnownClass),
		assertion(KnownClass == no).

	test(c45_classifier_annotated_training_statistics, deterministic) :-
		findall(Id-Class-Values, c45_fixture(missing)::example(Id, Class, Values), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		Tree = tree(x, threshold(0.5), leaf(yes), leaf(no)),
		c45_classifier<<tree_statistics(Tree, Weighted, Statistics),
		assertion(c45_classifier::valid_classifier(c45_model(Tree, Statistics))),
		Statistics = threshold_stats([no-No, yes-Yes], LeftMass, leaf_stats(_), RightMass, leaf_stats(_)),
		assertion(No =~= 2.0),
		assertion(Yes =~= 2.0),
		assertion(LeftMass =~= 2.0),
		assertion(RightMass =~= 2.0).

	test(c45_classifier_annotated_empty_branch, deterministic) :-
		Tree = tree(a, [first-leaf(yes), second-leaf(no), unused-leaf(no)]),
		Examples = [weighted_example(1, yes, [a-first], 1.0), weighted_example(2, no, [a-second], 1.0)],
		c45_classifier<<tree_statistics(Tree, Examples, Statistics),
		assertion(c45_classifier::valid_classifier(c45_model(Tree, Statistics))),
		c45_classifier::predict(c45_model(Tree, Statistics), [], Class),
		assertion(Class == no).

	test(c45_classifier_annotated_invalid_statistics, all(\+ c45_classifier::valid_classifier(Model))) :-
		member(Model, [c45_model(leaf(yes), leaf_stats([])), c45_model(leaf(yes), leaf_stats([yes-0.0])), c45_model(leaf(yes), leaf_stats([no-2.0, yes-1.0])), c45_model(tree(a, [first-leaf(yes)]), discrete_stats([yes-1.0], [branch(other, 1.0, leaf_stats([yes-1.0]))]))]).

	test(c45_classifier_annotated_open_statistics, deterministic(var(Tail))) :-
		Model = c45_model(leaf(yes), leaf_stats([yes-1.0| Tail])),
		assertion(\+ c45_classifier::valid_classifier(Model)).

	test(c45_classifier_annotated_print, deterministic) :-
		^^suppress_text_output,
		c45_classifier::print_classifier(c45_model(leaf(yes), leaf_stats([yes-1.0]))).

	test(c45_classifier_annotated_prune, deterministic) :-
		Tree = tree(x, threshold(0.5), leaf(yes), leaf(no)),
		findall(Id-Class-Values, c45_fixture(missing)::example(Id, Class, Values), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<tree_statistics(Tree, Weighted, Statistics),
		c45_classifier::prune(c45_fixture(missing), c45_model(Tree, Statistics), Pruned),
		assertion(c45_classifier::valid_classifier(Pruned)).

	test(c45_classifier_annotated_export, deterministic(var(Value))) :-
		Model = c45_model(leaf(yes), leaf_stats([yes-1.0])),
		c45_classifier::export_to_clauses(c45_fixture(singleton), Model, classify, Clauses),
		create_object(Object, [], [public(classify/2)], Clauses),
		Object::classify(Value, Class),
		abolish_object(Object),
		assertion(Class == yes).

	test(c45_classifier_annotated_learn_validity, deterministic) :-
		forall(
			member(Kind, [integer_numeric, extreme_numeric, constant_xor, continuous_xor, missing, missing_categorical, all_missing, categorical, singleton, attribute_free]),
			(
				c45_classifier::learn(c45_fixture(Kind), Model),
				assertion(c45_classifier::valid_classifier(Model)),
				assertion(functor(Model, c45_model, 2))
			)
		).

	test(c45_classifier_annotated_nested_missing, deterministic) :-
		Tree = tree(x, threshold(0.5), leaf(yes), tree(a, [first-leaf(no), second-leaf(yes)])),
		RightStats = discrete_stats([no-2.0, yes-1.0], [branch(first, 2.0, leaf_stats([no-2.0])), branch(second, 1.0, leaf_stats([yes-1.0]))]),
		Statistics = threshold_stats([no-2.0, yes-2.0], 1.0, leaf_stats([yes-1.0]), 3.0, RightStats),
		Model = c45_model(Tree, Statistics),
		assertion(c45_classifier::valid_classifier(Model)),
		c45_classifier::predict(Model, [], Class),
		assertion(Class == no),
		c45_classifier::predict(Model, [x-1.0, a-second], KnownClass),
		assertion(KnownClass == yes).

	test(c45_classifier_legacy_categorical_export, deterministic) :-
		First = =<(2),
		Second = >(2),
		Tree = tree(a, [First-leaf(yes), Second-leaf(no), ordinary-leaf(yes)]),
		c45_classifier::export_to_clauses(c45_fixture(categorical), Tree, classify, Clauses),
		create_object(Object, [], [public(classify/2)], Clauses),
		findall(Class, Object::classify(First, Class), FirstClasses),
		findall(Class, Object::classify(Second, Class), SecondClasses),
		abolish_object(Object),
		assertion(FirstClasses == [yes]),
		assertion(SecondClasses == [no]).

	test(c45_classifier_legacy_continuous_export, deterministic) :-
		Tree = tree(x, threshold(1.5), leaf(yes), tree(x, threshold(2.5), leaf(no), leaf(yes))),
		c45_classifier::export_to_clauses(c45_fixture(integer_numeric), Tree, classify, Clauses),
		create_object(Object, [], [public(classify/2)], Clauses),
		findall(Class, Object::classify(1.0, Class), FirstClasses),
		findall(Class, Object::classify(2, Class), SecondClasses),
		findall(Class, Object::classify(3.0, Class), ThirdClasses),
		abolish_object(Object),
		assertion(FirstClasses == [yes]),
		assertion(SecondClasses == [no]),
		assertion(ThirdClasses == [yes]).

	test(c45_classifier_legacy_leaf_export, deterministic(Clauses == [classify(yes)])) :-
		c45_classifier::export_to_clauses(c45_fixture(attribute_free), leaf(yes), classify, Clauses).

	test(c45_classifier_legacy_print_templates, deterministic) :-
		^^suppress_text_output,
		c45_classifier::print_classifier(tree(x, threshold(0.5), leaf(yes), leaf(no))),
		c45_classifier::print_classifier(tree(a, [first-leaf(yes), second-leaf(no)])).

	test(c45_classifier_annotated_missing_export, deterministic(var(Value))) :-
		Tree = tree(x, threshold(0.5), leaf(yes), leaf(no)),
		Statistics = threshold_stats([no-2.0, yes-8.0], 8.0, leaf_stats([yes-8.0]), 2.0, leaf_stats([no-2.0])),
		Model = c45_model(Tree, Statistics),
		c45_classifier::export_to_clauses(c45_fixture(missing), Model, classify, Clauses),
		create_object(Object, [], [public(classify/2)], Clauses),
		Object::classify(Value, Class),
		abolish_object(Object),
		assertion(Class == yes).

	test(c45_classifier_prune_minimum_population, deterministic) :-
		c45_classifier::learn(c45_fixture(integer_numeric), Model),
		c45_classifier::prune(c45_fixture(integer_numeric), Model, 0.25, 5, Pruned),
		Pruned = c45_model(leaf(no), _),
		assertion(c45_classifier::valid_classifier(Pruned)).

	test(c45_classifier_prune_minimum_boundary, deterministic) :-
		c45_classifier::learn(c45_fixture(integer_numeric), Model),
		c45_classifier::prune(c45_fixture(integer_numeric), Model, 0.25, 4, Pruned),
		Pruned = c45_model(tree(x, threshold(_), _, _), _),
		assertion(c45_classifier::valid_classifier(Pruned)).

	test(c45_classifier_prune_empty_branch_preserved, deterministic(Pruned == Tree)) :-
		Tree = tree(x, threshold(0.5), leaf(yes), leaf(no)),
		c45_classifier<<prune_tree(Tree, 2.0, 2, [], Pruned).

	test(c45_classifier_prune_empty_discrete_preserved, deterministic(Pruned == Tree)) :-
		Tree = tree(a, [first-leaf(yes), second-leaf(no)]),
		c45_classifier<<prune_tree(Tree, 2.0, 2, [], Pruned).

	test(c45_classifier_leaf_estimate_independent_of_minimum, deterministic) :-
		Examples = [weighted_example(1, yes, [], 1.0), weighted_example(2, no, [], 0.5)],
		c45_classifier<<leaf_error(Examples, 2.0, 1, First, yes),
		c45_classifier<<leaf_error(Examples, 2.0, 100, Second, yes),
		assertion(First =~= Second),
		assertion(First < 1.0).

	test(c45_classifier_wilson_reference, deterministic) :-
		c45_classifier<<upper_confidence_bound(1, 10, 1.0, Bound),
		Expected is (0.1 + 1.0 / 20.0 + sqrt(0.1 * 0.9 / 10.0 + 1.0 / 400.0)) / 1.1,
		assertion(Bound =~= Expected).

	test(c45_classifier_wilson_boundaries, deterministic) :-
		c45_classifier<<upper_confidence_bound(0, 0, 2.0, Empty),
		assertion(Empty =~= 1.0),
		c45_classifier<<upper_confidence_bound(10, 10, 2.0, All),
		assertion(All =~= 1.0),
		c45_classifier<<upper_confidence_bound(0, 10, 2.0, None),
		Expected is 4.0 / 14.0,
		assertion(None =~= Expected).

	test(c45_classifier_wilson_tiny_population, deterministic) :-
		c45_classifier<<upper_confidence_bound(5.0e-301, 1.0e-300, 2.0, Bound),
		assertion(Bound >= 0.5),
		assertion(Bound =< 1.0).

	test(c45_classifier_histogram_threshold_reference, deterministic) :-
		forall(
			member(Kind, [integer_numeric, extreme_numeric, constant, continuous_xor, missing, all_missing]),
			(
				findall(Id-Class-Values, c45_fixture(Kind)::example(Id, Class, Values), Examples),
				c45_classifier<<weight_examples(Examples, Weighted),
				c45_classifier<<example_mass(Weighted, Total),
				c45_classifier<<best_threshold(x, Weighted, Total, 0.0, Gain, Ratio, Threshold),
				reference_best_threshold(Weighted, Total, ReferenceGain, ReferenceRatio, ReferenceThreshold),
				assertion(Gain =~= ReferenceGain),
				assertion(Ratio =~= ReferenceRatio),
				assertion(Threshold == ReferenceThreshold)
			)
		).

	test(c45_classifier_information_gain_eligibility, deterministic(Ratios == [useful-0.5])) :-
		Candidates = [candidate(weak, 0.01, 9.0), candidate(useful, 0.5, 0.5), candidate(none, 0.0, 0.0)],
		c45_classifier<<candidate_gain_totals(Candidates, 0.0, 0, Gain, Count),
		Average is Gain / Count,
		c45_classifier<<eligible_ratios(Candidates, Average, Ratios).

	test(c45_classifier_zero_gain_eligibility, deterministic(Ratios == [a-0.0, b-0.0])) :-
		c45_classifier<<eligible_ratios([candidate(a, 0.0, 0.0), candidate(b, 0.0, 0.0)], 0.0, Ratios).

	test(c45_classifier_histogram_count_updates, deterministic) :-
		c45_classifier<<adjust_class_counts([a-0.0, b-0.0], [b-2.0], 1, [a-First, b-Second]),
		assertion(First =~= 0.0),
		assertion(Second =~= 2.0),
		c45_classifier<<adjust_class_counts([a-0.3], [a-0.30000000000000004], -1, [a-Zero]),
		assertion(Zero =~= 0.0).

	test(c45_classifier_discrete_histogram_reference, deterministic) :-
		findall(Id-Class-Values, c45_fixture(missing_categorical)::example(Id, Class, Values), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<discrete_candidate(a, Weighted, 4.0, a, Gain, Ratio),
		c45_classifier<<known_examples(Weighted, a, Known),
		c45_classifier<<entropy(Known, 2.0, Entropy),
		c45_classifier<<information_gain([first, second], a, Known, 2.0, Entropy, ReferenceGain),
		c45_classifier<<split_info([first, second], a, Known, 4.0, Info0),
		c45_classifier<<missing_split_info(2.0, 4.0, Info0, Info),
		ExpectedGain is 0.5 * ReferenceGain,
		ExpectedRatio is ExpectedGain / Info,
		assertion(Gain =~= ExpectedGain),
		assertion(Ratio =~= ExpectedRatio).

	test(c45_classifier_cached_tree_statistics, deterministic) :-
		c45_classifier::learn(c45_fixture(unused_branch), Model),
		Model = c45_model(Tree, Statistics),
		assertion(c45_classifier::valid_classifier(Model)),
		findall(Id-Class-Values, c45_fixture(unused_branch)::example(Id, Class, Values), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<tree_statistics(Tree, Weighted, Reference),
		assertion(Statistics == Reference).

	test(c45_classifier_entropy_rounding_boundary, deterministic) :-
		c45_classifier<<compute_entropy([major-1.0000000000000002, minor-1.0e-20], 1.0, 0.0, Entropy),
		assertion(Entropy >= 0.0),
		Examples = [weighted_example(1, yes, [a-first], 1.0000000000000002), weighted_example(2, no, [a-second], 1.0e-20)],
		c45_classifier<<discrete_candidate(a, Examples, 1.0, _, Gain, Ratio),
		assertion(Gain >= 0.0),
		assertion(Ratio >= 0.0).

	test(c45_classifier_histogram_zero_information, deterministic) :-
		Context = split_context(2.0, 1.0, 1.0),
		c45_classifier<<histogram_threshold_score([yes-1.0], 1.0, [no-1.0], 1.0, Context, Gain, Ratio),
		assertion(Gain =~= 0.0),
		assertion(Ratio =~= 0.0).

	test(c45_classifier_export_skewed_clause_order, deterministic) :-
		Tree = tree(x, threshold(1.0), leaf(first), tree(x, threshold(2.0), leaf(second), tree(kind, [a-leaf(third), b-leaf(fourth)]))),
		c45_classifier<<tree_to_clauses(Tree, classify, [x, kind], [], Clauses),
		reference_export(Tree, [x, kind], [], Reference),
		assertion(lgtunit::variant(Clauses, Reference)).

	test(c45_classifier_prune_bottom_up_reference, deterministic) :-
		forall(list::member(Kind, [integer_numeric, missing, missing_categorical, unused_branch, continuous_xor]), pruning_reference_case(Kind)).

	test(c45_classifier_prune_raw_compatibility, deterministic) :-
		forall(list::member(Kind, [singleton, integer_numeric, unused_branch]), raw_pruning_reference_case(Kind)).

	quick_check(c45_classifier_generated_model_invariants, generated_model_invariants(+list(byte,12)), [n(100)]).

	quick_check(c45_classifier_generated_export_equivalence, generated_export_equivalence(+list(byte,12)), [n(100)]).

	test(c45_classifier_deep_pruning_export_reference, deterministic) :-
		Tree = tree(x, threshold(0.5), leaf(yes), tree(x, threshold(1.5), leaf(no), tree(x, threshold(2.5), leaf(yes), tree(x, threshold(3.5), leaf(no), tree(x, threshold(4.5), leaf(yes), tree(x, threshold(5.5), leaf(no), tree(x, threshold(6.5), leaf(yes), leaf(no)))))))),
		findall(Id-Class-AVs, c45_fixture(pruning_skew)::example(Id, Class, AVs), Examples),
		assertion(Examples \== []),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<prune_node(Tree, 0.6745, 1, Weighted, [], Pruned, Statistics, Error),
		reference_prune(Tree, 0.6745, 1, Weighted, Reference),
		assertion(Pruned == Reference),
		c45_classifier<<subtree_error(Pruned, 0.6745, Weighted, ExpectedError),
		assertion(Error =~= ExpectedError),
		c45_classifier<<tree_statistics(Pruned, Weighted, ExpectedStats),
		assertion(lgtunit::variant(Statistics, ExpectedStats)),
		c45_classifier<<tree_to_clauses(Tree, classify, [x], [], Clauses),
		reference_export(Tree, [x], [], ExpectedClauses),
		assertion(lgtunit::variant(Clauses, ExpectedClauses)).

	% auxiliary predicates

	pruning_reference_case(Kind) :-
		Dataset = c45_fixture(Kind),
		c45_classifier::learn(Dataset, c45_model(Tree, _)),
		findall(Id-Class-AVs, Dataset::example(Id, Class, AVs), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<prune_node(Tree, 0.6745, 2, Weighted, [], Pruned, Statistics, Error),
		reference_prune(Tree, 0.6745, 2, Weighted, Reference),
		assertion(Pruned == Reference),
		c45_classifier<<tree_statistics(Pruned, Weighted, ExpectedStats),
		assertion(lgtunit::variant(Statistics, ExpectedStats)),
		c45_classifier<<subtree_error(Pruned, 0.6745, Weighted, ReferenceError),
		assertion(Error =~= ReferenceError).

	raw_pruning_reference_case(Kind) :-
		Dataset = c45_fixture(Kind),
		c45_classifier::learn(Dataset, c45_model(Tree, _)),
		c45_classifier::prune(Dataset, Tree, Pruned),
		findall(Id-Class-AVs, Dataset::example(Id, Class, AVs), Examples),
		c45_classifier<<weight_examples(Examples, Weighted),
		c45_classifier<<confidence_to_zscore(0.25, ZScore),
		reference_prune(Tree, ZScore, 2, Weighted, Reference),
		assertion(Pruned == Reference),
		assertion(c45_classifier::valid_classifier(Pruned)).

	generated_model_invariants(Bytes) :-
		generated_rows(Bytes, 1, Rows),
		copy_term(Rows, Original),
		Dataset = c45_fixture(generated(Rows)),
		c45_classifier::learn(Dataset, Model),
		assertion(lgtunit::variant(Rows, Original)),
		assertion(c45_classifier::valid_classifier(Model)),
		Model = c45_model(_, Statistics),
		statistics_mass(Statistics, Mass),
		assertion(Mass =~= 12.0),
		c45_classifier::prune(Dataset, Model, 0.5, 1, Pruned),
		assertion(c45_classifier::valid_classifier(Pruned)),
		Pruned = c45_model(_, PrunedStats),
		statistics_mass(PrunedStats, PrunedMass),
		assertion(PrunedMass =~= Mass),
		c45_classifier::predict(Model, [], Class),
		assertion(list::memberchk(Class, [yes, no])).

	generated_export_equivalence(Bytes) :-
		generated_rows(Bytes, 1, Rows),
		Dataset = c45_fixture(generated(Rows)),
		c45_classifier::learn(Dataset, Model),
		c45_classifier::export_to_clauses(Dataset, Model, classify, Clauses),
		create_object(Object, [], [public(classify/3)], Clauses),
		catch(
			(generated_export_cases(Object, Model) -> Success = true; Success = false),
			Error,
			(abolish_object(Object), throw(Error))
		),
		abolish_object(Object),
		Success == true.

	generated_export_cases(Object, Model) :-
		forall(
			list::member(Instance, [[x-0, a-0], [x-1.0, a-1], [x-_, a-0], [x-0, a-_], [x-_, a-_], [x-0, a-2]]),
			(
				copy_term(Instance, Original),
				Instance = [x-X, a-A],
				c45_classifier::predict(Model, Instance, Expected),
				findall(Class, Object::classify(X, A, Class), Classes),
				assertion(Classes == [Expected]),
				assertion(lgtunit::variant(Instance, Original))
			)
		).

	generated_rows([], _, []).
	generated_rows([Byte| Bytes], Id, [example(Id, Class, [x-X, a-A])| Rows]) :-
		Parity is Byte mod 2,
		(	Parity =:= 0 ->
			Class = yes
		;	Class = no
		),
		Kind is Byte mod 5,
		(	Kind =:= 0 ->
			true
		;	X is Byte mod 7 - 3
		),
		(	Byte mod 4 =:= 0 ->
			true
		;	A = Parity
		),
		Next is Id + 1,
		generated_rows(Bytes, Next, Rows).

	statistics_mass(leaf_stats(Counts), Mass) :-
		count_mass(Counts, 0.0, Mass).
	statistics_mass(threshold_stats(Counts, LeftMass, Left, RightMass, Right), Mass) :-
		count_mass(Counts, 0.0, Mass),
		Combined is LeftMass + RightMass,
		assertion(Mass =~= Combined),
		branch_statistics_mass(LeftMass, Left),
		branch_statistics_mass(RightMass, Right).
	statistics_mass(discrete_stats(Counts, Branches), Mass) :-
		count_mass(Counts, 0.0, Mass),
		statistics_branch_mass(Branches, 0.0, Combined),
		assertion(Mass =~= Combined).

	statistics_branch_mass([], Mass, Mass).
	statistics_branch_mass([branch(_, Mass, Statistics)| Branches], Acc, Total) :-
		branch_statistics_mass(Mass, Statistics),
		Next is Acc + Mass,
		statistics_branch_mass(Branches, Next, Total).

	branch_statistics_mass(Mass, Statistics) :-
		(	Mass > 0.0 ->
			statistics_mass(Statistics, CountMass),
			assertion(Mass =~= CountMass)
		;	true
		).

	count_mass([], Mass, Mass).
	count_mass([_-Count| Counts], Acc, Mass) :-
		Next is Acc + Count,
		count_mass(Counts, Next, Mass).

	reference_prune(leaf(Class), _, _, _, leaf(Class)).
	reference_prune(tree(Attribute, threshold(Threshold), Left, Right), ZScore, Minimum, Examples, Pruned) :-
		(	Examples == [] ->
			Pruned = tree(Attribute, threshold(Threshold), Left, Right)
		;	c45_classifier<<partition_weighted_threshold(Examples, Attribute, Threshold, LeftExamples, RightExamples),
			reference_prune(Left, ZScore, Minimum, LeftExamples, PrunedLeft),
			reference_prune(Right, ZScore, Minimum, RightExamples, PrunedRight),
			reference_prune_decision(tree(Attribute, threshold(Threshold), PrunedLeft, PrunedRight), ZScore, Minimum, Examples, Pruned)
		).
	reference_prune(tree(Attribute, Subtrees), ZScore, Minimum, Examples, Pruned) :-
		(	Examples == [] ->
			Pruned = tree(Attribute, Subtrees)
		;	list::length(Subtrees, Count),
			Share is 1.0 / Count,
			reference_prune_branches(Subtrees, Attribute, ZScore, Minimum, Examples, Share, PrunedSubtrees),
			reference_prune_decision(tree(Attribute, PrunedSubtrees), ZScore, Minimum, Examples, Pruned)
		).

	reference_prune_branches([], _, _, _, _, _, []).
	reference_prune_branches([Value-Tree| Subtrees], Attribute, ZScore, Minimum, Examples, Share, [Value-Pruned| PrunedSubtrees]) :-
		c45_classifier<<partition_weighted_value(Examples, Attribute, Value, Share, Filtered),
		reference_prune(Tree, ZScore, Minimum, Filtered, Pruned),
		reference_prune_branches(Subtrees, Attribute, ZScore, Minimum, Examples, Share, PrunedSubtrees).

	reference_prune_decision(Tree, ZScore, Minimum, Examples, Pruned) :-
		c45_classifier<<subtree_error(Tree, ZScore, Examples, SubtreeError),
		c45_classifier<<leaf_error(Examples, ZScore, Minimum, LeafError, Class),
		c45_classifier<<example_mass(Examples, Total),
		(	(Total < Minimum; LeafError =< SubtreeError) ->
			Pruned = leaf(Class)
		;	Pruned = Tree
		).

	reference_export(leaf(Class), Attributes, Bindings, [Clause]) :-
		c45_classifier<<build_clause(classify, Attributes, Bindings, Class, Clause).
	reference_export(tree(Attribute, threshold(Threshold), Left, Right), Attributes, Bindings, Clauses) :-
		reference_export(Left, Attributes, [upper_bound(Attribute, Threshold)| Bindings], LeftClauses),
		reference_export(Right, Attributes, [lower_bound(Attribute, Threshold)| Bindings], RightClauses),
		list::append(LeftClauses, RightClauses, Clauses).
	reference_export(tree(Attribute, Subtrees), Attributes, Bindings, Clauses) :-
		reference_export_branches(Subtrees, Attribute, Attributes, Bindings, Clauses).

	reference_export_branches([], _, _, _, []).
	reference_export_branches([Value-Tree| Subtrees], Attribute, Attributes, Bindings, Clauses) :-
		reference_export(Tree, Attributes, [categorical(Attribute, Value)| Bindings], First),
		reference_export_branches(Subtrees, Attribute, Attributes, Bindings, Rest),
		list::append(First, Rest, Clauses).

	reference_best_threshold(Examples, Total, Gain, Ratio, Threshold) :-
		findall(Value, (member(weighted_example(_, _, Values, _), Examples), member(x-Value, Values), nonvar(Value)), AllValues),
		sort(AllValues, Sorted),
		c45_classifier<<candidate_thresholds(Sorted, Candidates),
		reference_thresholds(Candidates, Examples, Total, best_split(-1.0, 0.0, none), Best),
		(	Best = best_split(_, _, none) ->
			Gain = 0.0,
			Ratio = 0.0,
			Threshold = none
		;	Best = best_split(Gain, Ratio, Threshold)
		).

	reference_thresholds([], _, _, Best, Best).
	reference_thresholds([Threshold| Thresholds], Examples, Total, Best0, Best) :-
		c45_classifier<<known_examples(Examples, x, Known),
		c45_classifier<<example_mass(Known, KnownMass),
		c45_classifier<<filter_examples_threshold(Known, x, Threshold, Left, Right),
		c45_classifier<<example_mass(Left, LeftMass),
		c45_classifier<<example_mass(Right, RightMass),
		c45_classifier<<entropy(Known, KnownMass, BaseEntropy),
		c45_classifier<<entropy(Left, LeftMass, LeftEntropy),
		c45_classifier<<entropy(Right, RightMass, RightEntropy),
		Gain is (KnownMass / Total) * (BaseEntropy - (LeftMass / KnownMass) * LeftEntropy - (RightMass / KnownMass) * RightEntropy),
		c45_classifier<<threshold_gain_ratio(Threshold, x, Examples, Total, 0.0, Ratio),
		Best0 = best_split(PreviousGain, _, _),
		(	Gain > PreviousGain ->
			Best1 = best_split(Gain, Ratio, Threshold)
		;	Best1 = Best0
		),
		reference_thresholds(Thresholds, Examples, Total, Best1, Best).

	tree_node_count(c45_model(Tree, _), Count) :-
		tree_node_count(Tree, Count).
	tree_node_count(leaf(_), 1).
	tree_node_count(tree(_, threshold(_), Left, Right), Count) :-
		tree_node_count(Left, LeftCount),
		tree_node_count(Right, RightCount),
		Count is LeftCount + RightCount + 1.
	tree_node_count(tree(_, Subtrees), Count) :-
		subtree_node_count(Subtrees, 0, Children),
		Count is Children + 1.

	subtree_node_count([], Count, Count).
	subtree_node_count([_-Tree| Subtrees], Acc, Count) :-
		tree_node_count(Tree, Nodes),
		Next is Acc + Nodes,
		subtree_node_count(Subtrees, Next, Count).

:- end_object.

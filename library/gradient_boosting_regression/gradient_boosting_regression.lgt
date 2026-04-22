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


:- object(gradient_boosting_regression,
	imports([options, regressor_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Gradient boosting regression using regression trees as additive base learners fitted to successive residuals.',
		remarks is [
			'Algorithm' - 'Builds an additive ensemble of regression trees by repeatedly fitting the current residuals under squared-error loss.',
			'Initial model' - 'Starts from the arithmetic mean of the training targets and then adds a scaled tree prediction at each boosting stage.',
			'Shrinkage' - 'Uses a fixed learning rate to scale each stage contribution and reduce overfitting.',
			'Regressor representation' - 'The learned regressor is represented by default as ``gradient_boosting_regressor(InitialPrediction, WeightedTrees, Options)`` where ``WeightedTrees`` contains ``weighted_tree(LearningRate, Tree)`` terms.'
		],
		see_also is [linear_regression, knn_regression, regression_tree, random_forest_regression]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a regressor from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Regressor', 'Options']
	]).

	:- uses(regression_tree, [
		learn/3 as tree_learn/3, predict/3 as tree_predict/3
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	:- uses(population, [
		arithmetic_mean/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(_Target),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		mean_target(Examples, InitialPrediction),
		initialize_predictions(Examples, InitialPrediction, Predictions),
		^^option(number_of_estimators(NumberOfEstimators), Options),
		^^option(learning_rate(LearningRate), Options),
		build_tree_options(Options, TreeOptions),
		build_ensemble(Attributes, Examples, Predictions, NumberOfEstimators, LearningRate, TreeOptions, 1, [], WeightedTrees),
		Regressor = gradient_boosting_regressor(InitialPrediction, WeightedTrees, Options).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, InitialPrediction, WeightedTrees, _Options],
		predict_stages(WeightedTrees, Instance, 0.0, Increment),
		Target is InitialPrediction + Increment.

	mean_target(Examples, Mean) :-
		findall(Target, member(example(_Id, Target, _AttributeValues), Examples), Targets),
		arithmetic_mean(Targets, Mean).

	initialize_predictions([], _InitialPrediction, []).
	initialize_predictions([_| Examples], InitialPrediction, [InitialPrediction| Predictions]) :-
		initialize_predictions(Examples, InitialPrediction, Predictions).

	build_tree_options(Options, [maximum_depth(MaximumDepth), minimum_samples_leaf(MinimumSamplesLeaf), minimum_variance_reduction(MinimumVarianceReduction), feature_scaling(FeatureScaling)]) :-
		^^option(maximum_depth(MaximumDepth), Options),
		^^option(minimum_samples_leaf(MinimumSamplesLeaf), Options),
		^^option(minimum_variance_reduction(MinimumVarianceReduction), Options),
		^^option(feature_scaling(FeatureScaling), Options).

	build_ensemble(_Attributes, _Examples, _Predictions, NumberOfEstimators, _LearningRate, _TreeOptions, Round, WeightedTrees, WeightedTrees) :-
		Round > NumberOfEstimators,
		!.
	build_ensemble(Attributes, Examples, Predictions, NumberOfEstimators, LearningRate, TreeOptions, Round, WeightedTrees0, WeightedTrees) :-
		create_residual_examples(Examples, Predictions, ResidualExamples, 0.0, ResidualSSE),
		(   ResidualSSE =< 1.0e-12 ->
			WeightedTrees = WeightedTrees0
		;   create_residual_dataset(Attributes, ResidualExamples, ResidualDataset),
			tree_learn(ResidualDataset, Tree, TreeOptions),
			abolish_object(ResidualDataset),
			update_predictions(Examples, Predictions, LearningRate, Tree, UpdatedPredictions),
			append(WeightedTrees0, [weighted_tree(LearningRate, Tree)], WeightedTrees1),
			NextRound is Round + 1,
			build_ensemble(Attributes, Examples, UpdatedPredictions, NumberOfEstimators, LearningRate, TreeOptions, NextRound, WeightedTrees1, WeightedTrees)
		).

	create_residual_examples([], [], [], ResidualSSE, ResidualSSE).
	create_residual_examples([example(Id, Target, AttributeValues)| Examples], [Prediction| Predictions], [example(Id, Residual, AttributeValues)| ResidualExamples], ResidualSSE0, ResidualSSE) :-
		Residual is Target - Prediction,
		ResidualSSE1 is ResidualSSE0 + Residual * Residual,
		create_residual_examples(Examples, Predictions, ResidualExamples, ResidualSSE1, ResidualSSE).

	create_residual_dataset(Attributes, ResidualExamples, ResidualDataset) :-
		build_attribute_clauses(Attributes, AttributeClauses),
		build_example_clauses(ResidualExamples, ExampleClauses),
		append(AttributeClauses, ExampleClauses, DataClauses),
		AllClauses = [target(residual)| DataClauses],
		create_object(ResidualDataset, [implements(regression_dataset_protocol)], [], AllClauses).

	build_attribute_clauses([], []).
	build_attribute_clauses([Attribute-Values| Attributes], [attribute_values(Attribute, Values)| Clauses]) :-
		build_attribute_clauses(Attributes, Clauses).

	build_example_clauses([], []).
	build_example_clauses([example(Id, Target, AttributeValues)| Examples], [example(Id, Target, AttributeValues)| Clauses]) :-
		build_example_clauses(Examples, Clauses).

	update_predictions([], [], _LearningRate, _Tree, []).
	update_predictions([example(_Id, _Target, AttributeValues)| Examples], [Prediction0| Predictions0], LearningRate, Tree, [Prediction| Predictions]) :-
		tree_predict(Tree, AttributeValues, TreePrediction),
		Prediction is Prediction0 + LearningRate * TreePrediction,
		update_predictions(Examples, Predictions0, LearningRate, Tree, Predictions).

	predict_stages([], _Instance, Increment, Increment).
	predict_stages([weighted_tree(LearningRate, Tree)| WeightedTrees], Instance, Increment0, Increment) :-
		tree_predict(Tree, Instance, TreePrediction),
		Increment1 is Increment0 + LearningRate * TreePrediction,
		predict_stages(WeightedTrees, Instance, Increment1, Increment).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'InitialPrediction', 'WeightedTrees', 'Options'].

	regressor_term_template(gradient_boosting_regressor(_InitialPrediction, _WeightedTrees, _Options), gradient_boosting_regressor('InitialPrediction', 'WeightedTrees', 'Options')).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = gradient_boosting_regressor(InitialPrediction, WeightedTrees, Options),
		Clause =.. [Functor, InitialPrediction, WeightedTrees, Options].

	print_regressor(Regressor) :-
		Regressor = gradient_boosting_regressor(InitialPrediction, WeightedTrees, Options),
		format('Gradient Boosting Regression~n', []),
		format('============================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Options: ~w~n', [Options]),
		format('Initial prediction: ~4f~n', [InitialPrediction]),
		length(WeightedTrees, StageCount),
		format('Stages: ~w~n', [StageCount]),
		print_weighted_trees(WeightedTrees, 1).

	print_weighted_trees([], _).
	print_weighted_trees([weighted_tree(LearningRate, _Tree)| WeightedTrees], Index) :-
		format('  Stage ~w (learning rate ~4f)~n', [Index, LearningRate]),
		NextIndex is Index + 1,
		print_weighted_trees(WeightedTrees, NextIndex).

	default_option(number_of_estimators(50)).
	default_option(learning_rate(0.1)).
	default_option(maximum_depth(3)).
	default_option(minimum_samples_leaf(1)).
	default_option(minimum_variance_reduction(0.0)).
	default_option(feature_scaling(false)).

	valid_option(number_of_estimators(NumberOfEstimators)) :-
		valid(positive_integer, NumberOfEstimators).
	valid_option(learning_rate(LearningRate)) :-
		valid(positive_float, LearningRate).
	valid_option(maximum_depth(MaximumDepth)) :-
		valid(positive_integer, MaximumDepth).
	valid_option(minimum_samples_leaf(MinimumSamplesLeaf)) :-
		valid(positive_integer, MinimumSamplesLeaf).
	valid_option(minimum_variance_reduction(MinimumVarianceReduction)) :-
		valid(non_negative_float, MinimumVarianceReduction).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.

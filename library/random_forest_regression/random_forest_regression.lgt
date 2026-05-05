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


:- object(random_forest_regression,
	imports(regressor_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Random Forest regression using regression trees as base learners trained on bootstrap samples and per-split random feature subsets.',
		see_also is [linear_regression, knn_regression, regression_tree, gradient_boosting_regression]
	]).

	:- uses(regression_tree, [
		learn/3 as tree_learn/3, predict/3 as tree_predict/3
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3, get_seed/1 as get_random_seed/1,
		randomize/1 as randomize_seed/1, set_seed/1 as set_random_seed/1
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, nth1/3
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
		Dataset::target(Target),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		length(Attributes, NumFeatures),
		^^option(number_of_trees(NumTrees), Options),
		(   ^^option(maximum_features_per_split(MaxFeatures), Options) ->
			true
		;   MaxFeatures is max(1, floor(sqrt(NumFeatures)))
		),
		build_tree_options(Options, MaxFeatures, TreeOptions),
		get_random_seed(OriginalSeed),
		^^option(random_seed(RandomSeed), Options),
		randomize_seed(RandomSeed),
		build_forest(Dataset, NumTrees, Attributes, TreeOptions, Trees),
		set_random_seed(OriginalSeed),
		length(Examples, TrainingExampleCount),
		build_diagnostics(Target, NumFeatures, TrainingExampleCount, Trees, Options, Diagnostics),
		Regressor = rf_regressor(Trees, Diagnostics).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Trees, _Diagnostics],
		collect_predictions(Trees, Instance, Predictions),
		(   Predictions == [] ->
			domain_error(non_empty_predictions, Regressor)
		;   arithmetic_mean(Predictions, Target)
		).

	build_diagnostics(Target, AttributeCount, TrainingExampleCount, Trees, Options, Diagnostics) :-
		length(Trees, TreeCount),
		^^base_regressor_diagnostics(random_forest_regression, Target, TrainingExampleCount, Options, [attribute_count(AttributeCount), tree_count(TreeCount)], Diagnostics).

	build_tree_options(Options, MaximumFeaturesPerSplit, [maximum_depth(MaximumDepth), minimum_samples_leaf(MinimumSamplesLeaf), minimum_variance_reduction(MinimumVarianceReduction), maximum_features_per_split(MaximumFeaturesPerSplit), feature_scaling(FeatureScaling)]) :-
		^^option(maximum_depth(MaximumDepth), Options),
		^^option(minimum_samples_leaf(MinimumSamplesLeaf), Options),
		^^option(minimum_variance_reduction(MinimumVarianceReduction), Options),
		^^option(feature_scaling(FeatureScaling), Options).

	build_forest(Dataset, NumTrees, Attributes, TreeOptions, Trees) :-
		build_forest(Dataset, NumTrees, Attributes, TreeOptions, 1, [], Trees).

	build_forest(_Dataset, NumTrees, _Attributes, _TreeOptions, TreeID, Trees, Trees) :-
		TreeID > NumTrees,
		!.
	build_forest(Dataset, NumTrees, Attributes, TreeOptions, TreeID, Trees0, Trees) :-
		create_bootstrap_dataset(Dataset, Attributes, BootstrapDataset),
		tree_learn(BootstrapDataset, Tree, TreeOptions),
		abolish_object(BootstrapDataset),
		NextTreeID is TreeID + 1,
		build_forest(Dataset, NumTrees, Attributes, TreeOptions, NextTreeID, [tree(Tree)| Trees0], Trees).

	create_bootstrap_dataset(Dataset, Attributes, BootstrapDataset) :-
		findall(example(Id, Target, AttributeValues), Dataset::example(Id, Target, AttributeValues), Examples),
		length(Examples, Count),
		bootstrap_sample(Examples, Count, BootstrapExamples),
		Dataset::target(TargetName),
		create_bootstrap_object(BootstrapDataset, Attributes, TargetName, BootstrapExamples).

	bootstrap_sample(Examples, Count, BootstrapExamples) :-
		bootstrap_sample(Examples, Count, Count, BootstrapExamples).

	bootstrap_sample(_Examples, _Count, 0, []) :-
		!.
	bootstrap_sample(Examples, Count, Remaining, [example(NewId, Target, AttributeValues)| BootstrapExamples]) :-
		random_between(1, Count, Index),
		nth1(Index, Examples, example(_Id, Target, AttributeValues)),
		NewId is Count - Remaining + 1,
		NextRemaining is Remaining - 1,
		bootstrap_sample(Examples, Count, NextRemaining, BootstrapExamples).

	create_bootstrap_object(Name, Attributes, TargetName, Examples) :-
		build_attribute_clauses(Attributes, AttributeClauses),
		build_example_clauses(Examples, ExampleClauses),
		append(AttributeClauses, ExampleClauses, DataClauses),
		AllClauses = [target(TargetName)| DataClauses],
		create_object(Name, [implements(regression_dataset_protocol)], [], AllClauses).

	build_attribute_clauses([], []).
	build_attribute_clauses([Attribute-Values| Attributes], [attribute_values(Attribute, Values)| Clauses]) :-
		build_attribute_clauses(Attributes, Clauses).

	build_example_clauses([], []).
	build_example_clauses([example(Id, Target, AttributeValues)| Examples], [example(Id, Target, AttributeValues)| Clauses]) :-
		build_example_clauses(Examples, Clauses).

	collect_predictions([], _Instance, []).
	collect_predictions([tree(Tree)| Trees], Instance, Predictions) :-
		(   catch(once(tree_predict(Tree, Instance, Prediction)), error(instantiation_error, _), fail),
			number(Prediction) ->
			Predictions = [Prediction| RestPredictions]
		;   Predictions = RestPredictions
		),
		collect_predictions(Trees, Instance, RestPredictions).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Trees', 'Diagnostics'].

	regressor_term_template(rf_regressor(_Trees, _Diagnostics), rf_regressor('Trees', 'Diagnostics')).

	check_regressor(Regressor) :-
		(   Regressor = rf_regressor(Trees, Diagnostics),
			Trees \== [],
			^^valid_regressor_metadata(random_forest_regression, Diagnostics),
			^^regressor_options(Regressor, Options),
			memberchk(number_of_trees(ExpectedTreeCount), Options),
			valid_trees(Trees),
			length(Trees, ExpectedTreeCount),
			^^valid_diagnostic_count(tree_count, Diagnostics, ExpectedTreeCount) ->
			true
		;   domain_error(regressor, Regressor)
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor =.. [_, Trees, Diagnostics],
		Clause =.. [Functor, Trees, Diagnostics].

	print_regressor(Regressor) :-
		Regressor = rf_regressor(Trees, Diagnostics),
		format('Random Forest Regressor~n', []),
		format('=======================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n', [Diagnostics]),
		length(Trees, TreeCount),
		format('Trees: ~w~n', [TreeCount]),
		print_trees(Trees, 1).

	print_trees([], _).
	print_trees([tree(_Tree)| Trees], Index) :-
		format('  Tree ~w~n', [Index]),
		NextIndex is Index + 1,
		print_trees(Trees, NextIndex).

	valid_trees([]).
	valid_trees([tree(TreeRegressor)| Trees]) :-
		regression_tree::valid_regressor(TreeRegressor),
		valid_trees(Trees).

	default_option(number_of_trees(10)).
	default_option(maximum_depth(10)).
	default_option(minimum_samples_leaf(1)).
	default_option(minimum_variance_reduction(0.0)).
	default_option(feature_scaling(false)).
	default_option(random_seed(1357911)).

	valid_option(number_of_trees(NumberOfTrees)) :-
		valid(positive_integer, NumberOfTrees).
	valid_option(maximum_features_per_split(MaximumFeaturesPerSplit)) :-
		(   MaximumFeaturesPerSplit == all ->
			true
		;   valid(positive_integer, MaximumFeaturesPerSplit)
		).
	valid_option(maximum_depth(MaximumDepth)) :-
		valid(positive_integer, MaximumDepth).
	valid_option(minimum_samples_leaf(MinimumSamplesLeaf)) :-
		valid(positive_integer, MinimumSamplesLeaf).
	valid_option(minimum_variance_reduction(MinimumVarianceReduction)) :-
		valid(non_negative_float, MinimumVarianceReduction).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(random_seed(RandomSeed)) :-
		valid(positive_integer, RandomSeed).

:- end_object.

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
	imports([options, regressor_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Random Forest regression using regression trees as base learners trained on bootstrap samples and random feature subsets.',
		remarks is [
			'Algorithm' - 'Builds an ensemble of regression trees trained on bootstrap samples and random feature subsets and predicts using the arithmetic mean of the individual tree predictions.',
			'Bootstrap sampling' - 'Each tree is trained on a bootstrap sample drawn with replacement from the original training examples.',
			'Feature randomization' - 'Each tree uses a random subset of the available dataset attributes. The default number of features is the square root of the total number of features.',
			'Reproducibility' - 'Random subsets are generated using the portable ``fast_random(xoshiro128pp)`` pseudo-random generator and can be reproduced by setting the ``random_seed/1`` option.',
			'Regressor representation' - 'The learned regressor is represented by default as ``rf_regressor(Trees, Options)`` where ``Trees`` contains ``tree(TreeRegressor, AttributeNames)`` terms.'
		],
		see_also is [linear_regression, knn_regression, regression_tree, gradient_boosting_regression]
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

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3, get_seed/1 as get_random_seed/1, permutation/2 as random_permutation/2,
		randomize/1 as randomize_seed/1, set_seed/1 as set_random_seed/1
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, nth1/3, take/3
	]).

	:- uses(pairs, [
		keys/2, keys_values/3
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
		keys(Attributes, AttributeNames),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		length(AttributeNames, NumFeatures),
		^^option(number_of_trees(NumTrees), Options),
		(   ^^option(maximum_features_per_tree(MaxFeatures), Options) ->
			true
		;   MaxFeatures is max(1, floor(sqrt(NumFeatures)))
		),
		build_tree_options(Options, TreeOptions),
		get_random_seed(OriginalSeed),
		^^option(random_seed(RandomSeed), Options),
		randomize_seed(RandomSeed),
		build_forest(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, TreeOptions, Trees),
		set_random_seed(OriginalSeed),
		Regressor = rf_regressor(Trees, Options).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Trees, _Options],
		collect_predictions(Trees, Instance, Predictions),
		(   Predictions == [] ->
			domain_error(non_empty_predictions, Regressor)
		;   arithmetic_mean(Predictions, Target)
		).

	build_tree_options(Options, [maximum_depth(MaximumDepth), minimum_samples_leaf(MinimumSamplesLeaf), minimum_variance_reduction(MinimumVarianceReduction), feature_scaling(FeatureScaling)]) :-
		^^option(maximum_depth(MaximumDepth), Options),
		^^option(minimum_samples_leaf(MinimumSamplesLeaf), Options),
		^^option(minimum_variance_reduction(MinimumVarianceReduction), Options),
		^^option(feature_scaling(FeatureScaling), Options).

	build_forest(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, TreeOptions, Trees) :-
		build_forest(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, TreeOptions, 1, [], Trees).

	build_forest(_Dataset, NumTrees, _Attributes, _AttributeNames, _MaxFeatures, _TreeOptions, TreeID, Trees, Trees) :-
		TreeID > NumTrees,
		!.
	build_forest(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, TreeOptions, TreeID, Trees0, Trees) :-
		select_feature_subset(AttributeNames, Attributes, MaxFeatures, SelectedNames, SelectedAttributes),
		create_bootstrap_dataset(Dataset, SelectedNames, SelectedAttributes, BootstrapDataset),
		tree_learn(BootstrapDataset, Tree, TreeOptions),
		abolish_object(BootstrapDataset),
		NextTreeID is TreeID + 1,
		build_forest(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, TreeOptions, NextTreeID, [tree(Tree, SelectedNames)| Trees0], Trees).

	select_feature_subset(AttributeNames, Attributes, MaxFeatures, SelectedNames, SelectedAttributes) :-
		keys_values(Pairs, AttributeNames, Attributes),
		random_permutation(Pairs, ShuffledPairs),
		take(MaxFeatures, ShuffledPairs, SelectedPairs),
		keys_values(SelectedPairs, SelectedNames, SelectedAttributes).

	create_bootstrap_dataset(Dataset, SelectedNames, SelectedAttributes, BootstrapDataset) :-
		findall(example(Id, Target, AttributeValues), Dataset::example(Id, Target, AttributeValues), Examples),
		length(Examples, Count),
		bootstrap_sample(Examples, Count, BootstrapExamples),
		Dataset::target(TargetName),
		create_bootstrap_object(BootstrapDataset, SelectedNames, SelectedAttributes, TargetName, BootstrapExamples).

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

	create_bootstrap_object(Name, SelectedNames, SelectedAttributes, TargetName, Examples) :-
		build_attribute_clauses(SelectedNames, SelectedAttributes, AttributeClauses),
		build_example_clauses(Examples, SelectedNames, ExampleClauses),
		append(AttributeClauses, ExampleClauses, DataClauses),
		AllClauses = [target(TargetName)| DataClauses],
		create_object(Name, [implements(regression_dataset_protocol)], [], AllClauses).

	build_attribute_clauses([], [], []).
	build_attribute_clauses([Name| Names], [Name-Values| Attributes], [attribute_values(Name, Values)| Clauses]) :-
		build_attribute_clauses(Names, Attributes, Clauses).

	build_example_clauses([], _SelectedNames, []).
	build_example_clauses([example(Id, Target, AttributeValues)| Examples], SelectedNames, [example(Id, Target, FilteredAttributeValues)| Clauses]) :-
		filter_attribute_values(SelectedNames, AttributeValues, FilteredAttributeValues),
		build_example_clauses(Examples, SelectedNames, Clauses).

	filter_attribute_values([], _AttributeValues, []).
	filter_attribute_values([Name| Names], AttributeValues, [Name-Value| FilteredAttributeValues]) :-
		memberchk(Name-Value, AttributeValues),
		filter_attribute_values(Names, AttributeValues, FilteredAttributeValues).

	collect_predictions([], _Instance, []).
	collect_predictions([tree(Tree, AttributeNames)| Trees], Instance, Predictions) :-
		filter_instance(AttributeNames, Instance, FilteredInstance),
		(   catch(once(tree_predict(Tree, FilteredInstance, Prediction)), error(instantiation_error, _), fail),
			number(Prediction) ->
			Predictions = [Prediction| RestPredictions]
		;   Predictions = RestPredictions
		),
		collect_predictions(Trees, Instance, RestPredictions).

	filter_instance([], _Instance, []).
	filter_instance([Name| Names], Instance, FilteredInstance) :-
		(   member(Name-Value, Instance) ->
			FilteredInstance = [Name-Value| RestInstance]
		;   FilteredInstance = RestInstance
		),
		filter_instance(Names, Instance, RestInstance).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Trees', 'Options'].

	regressor_term_template(rf_regressor(_Trees, _Options), rf_regressor('Trees', 'Options')).

	check_regressor(Regressor) :-
		(   Regressor = rf_regressor(Trees, Options),
			Trees \== [],
			^^valid_regressor_options(Options),
			memberchk(number_of_trees(ExpectedTreeCount), Options),
			valid_trees(Trees),
			length(Trees, ExpectedTreeCount) ->
			true
		;   domain_error(valid_regressor, Regressor)
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor =.. [_, Trees, Options],
		Clause =.. [Functor, Trees, Options].

	print_regressor(Regressor) :-
		Regressor = rf_regressor(Trees, Options),
		format('Random Forest Regressor~n', []),
		format('=======================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Options: ~w~n', [Options]),
		length(Trees, TreeCount),
		format('Trees: ~w~n', [TreeCount]),
		print_trees(Trees, 1).

	print_trees([], _).
	print_trees([tree(_Tree, AttributeNames)| Trees], Index) :-
		format('  Tree ~w features: ~w~n', [Index, AttributeNames]),
		NextIndex is Index + 1,
		print_trees(Trees, NextIndex).

	valid_trees([]).
	valid_trees([tree(TreeRegressor, AttributeNames)| Trees]) :-
		^^valid_attribute_names(AttributeNames),
		AttributeNames \== [],
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
	valid_option(maximum_features_per_tree(MaximumFeaturesPerTree)) :-
		valid(positive_integer, MaximumFeaturesPerTree).
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

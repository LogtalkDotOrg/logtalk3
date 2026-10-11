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


:- object(adaptive_boosting_classifier,
	imports(probabilistic_classifier_common)).

	:- info([
		version is 3:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Adaptive Boosting classifier using C4.5 decision trees as base learners. Implements the SAMME (Stagewise Additive Modeling using a Multi-class Exponential loss function) variant, which supports multi-class classification. Builds an ensemble of weighted decision trees where each subsequent tree focuses on the examples misclassified by previous trees.',
		see_also is [
			dataset_protocol, c45_classifier, isolation_forest_anomaly_detector, knn_classifier,
			naive_bayes_classifier, nearest_centroid_classifier, random_forest_classifier
		]
	]).

	:- uses(c45_classifier, [
		learn/2 as c45_learn/2, predict/3 as c45_predict/3
	]).

	:- uses(fast_random(xoshiro128pp), [
		random/1 as random_float/1
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2
	]).

	:- uses(pairs, [
		keys/2, values/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(avltree, [
		as_dictionary/2, lookup/3, update/4
	]).

	:- private(check_ensemble/2).
	:- mode(check_ensemble(+object_identifier, +list), one_or_error).
	:- info(check_ensemble/2, [
		comment is 'Checks that learning accepted at least one estimator.',
		argnames is ['Dataset', 'WeightedTrees'],
		exceptions is [
			'No estimator was accepted' - domain_error(non_empty_ensemble, 'Dataset')
		]
	]).

	:- private(train_with_cleanup/1).
	:- mode(train_with_cleanup(+pair(object_identifier, term)), zero_or_one).
	:- info(train_with_cleanup/1, [
		comment is 'Learns a C4.5 tree using a dataset-tree pair and abolishes the temporary dataset on success, failure, or exception.',
		argnames is ['DatasetTree'],
		exceptions is [
			'C4.5 learning throws an exception' - 'Exception'
		]
	]).

	% learn/3 - learns a classifier with specified options
	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(number_of_estimators(NumEstimators), Options),
		% Get attribute information from dataset
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		% Get class values
		Dataset::class_values(ClassValues),
		length(ClassValues, NumClasses),
		% Get all examples
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples(Dataset, Examples),
		length(Examples, N),
		% Initialize uniform weights
		InitWeight is 1.0 / N,
		initialize_weights(Examples, InitWeight, WeightedExamples),
		% Build the ensemble
		(	ClassValues = [OnlyClass] ->
			WeightedTrees = [weighted_tree(1.0, leaf(OnlyClass), AttributeNames)]
		;	Dataset::class(ClassName),
			build_attribute_clauses(AttributeNames, Attributes, AttributeClauses),
			Metadata = [class(ClassName), class_values(ClassValues)| AttributeClauses],
			build_ensemble(
				NumEstimators, AttributeNames, NumClasses, WeightedExamples,
				N, Metadata, 1, [], WeightedTrees
			)
		),
		check_ensemble(Dataset, WeightedTrees),
		% Create classifier term
		Classifier = ab_classifier(WeightedTrees, ClassValues, Options).

	check_ensemble(Dataset, WeightedTrees) :-
		(	WeightedTrees == [] ->
			domain_error(non_empty_ensemble, Dataset)
		;	true
		).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	% initialize_weights/3 - assign initial weight to each example
	initialize_weights([], _, []).
	initialize_weights([Id-Class-AVs| Examples], Weight, [weighted_example(Id, Class, AVs, Weight)| WeightedExamples]) :-
		initialize_weights(Examples, Weight, WeightedExamples).

	build_ensemble(NumEstimators, AttributeNames, NumClasses, WeightedExamples, N, Metadata, Round, Previous, WeightedTrees) :-
		append(Previous, Tail, Ensemble),
		build_ensemble_rounds(NumEstimators, AttributeNames, NumClasses, WeightedExamples, N, Metadata, Round, Tail, Ensemble, WeightedTrees).

	build_ensemble_rounds(NumEstimators, _, _, _, _, _, Round, Tail, Ensemble, WeightedTrees) :-
		Round > NumEstimators,
		!,
		Tail = [],
		WeightedTrees = Ensemble.
	build_ensemble_rounds(
		NumEstimators, AttributeNames, NumClasses, WeightedExamples,
		N, Metadata, Round, Tail, Ensemble, WeightedTrees
	) :-
		% Create a weighted bootstrap dataset
		create_weighted_dataset(WeightedExamples, N, Metadata, WeightedDataset),
		% Train a C4.5 tree on the weighted sample
		train_with_cleanup(WeightedDataset-Tree),
		% Compute weighted error
		compute_weighted_error(WeightedExamples, Tree, WeightedError, Misclassified),
		% Check if this learner is useful
		RandomError is (NumClasses - 1) / NumClasses,
		(	WeightedError >= RandomError ->
			% Learner not better than random; stop early
			Tail = [],
			WeightedTrees = Ensemble
		;	WeightedError =< 0.0 ->
			Tail = [],
			WeightedTrees = [weighted_tree(1.0, Tree, AttributeNames)]
		;	% Compute learner weight (SAMME formula)
			learner_weight(WeightedError, NumClasses, Alpha),
			% Update example weights
			update_weights(WeightedExamples, Misclassified, WeightedError, NumClasses, UpdatedWeights0),
			% Normalize weights
			normalize_weights(UpdatedWeights0, UpdatedWeights),
			% Continue building
			NextRound is Round + 1,
			Tail = [weighted_tree(Alpha, Tree, AttributeNames)| NextTail],
			build_ensemble_rounds(
				NumEstimators, AttributeNames, NumClasses, UpdatedWeights,
				N, Metadata, NextRound, NextTail, Ensemble, WeightedTrees
			)
		).

	train_with_cleanup(Dataset-Tree) :-
		catch(
			(	c45_learn(Dataset, Tree) ->
				Result = success
			;	Result = failure
			),
			Error,
			(	abolish_object(Dataset),
				throw(Error)
			)
		),
		abolish_object(Dataset),
		Result == success.

	create_weighted_dataset(WeightedExamples, N, Metadata, WeightedDataset) :-
		% Build cumulative distribution from weights
		build_cumulative_distribution(WeightedExamples, CDF),
		% Sample N examples with replacement according to weights
		weighted_sample(CDF, N, SampledExamples),
		append(Metadata, SampledExamples, AllClauses),
		create_object(
			WeightedDataset,
			[implements(dataset_protocol)],
			[],
			AllClauses
		).

	% build_cumulative_distribution/2 - build CDF from weighted examples
	build_cumulative_distribution(WeightedExamples, CDF) :-
		build_cdf_acc(WeightedExamples, 0.0, CDF).

	build_cdf_acc([], _, []).
	build_cdf_acc([weighted_example(_Id, Class, AVs, Weight)| Rest], Acc, CDF) :-
		(	Weight > 0.0 ->
			CumWeight is Acc + Weight,
			CDF = [cdf_entry(CumWeight, Class, AVs)| CDFRest]
		;	CumWeight = Acc,
			CDF = CDFRest
		),
		build_cdf_acc(Rest, CumWeight, CDFRest).

	weighted_sample(CDF, Count, Examples) :-
		random_draws(Count, 1, Draws),
		keysort(Draws, SortedDraws),
		sample_sorted_draws(CDF, SortedDraws, Selections),
		keysort(Selections, OrderedSelections),
		values(OrderedSelections, Examples).

	random_draws(0, _, []) :-
		!.
	random_draws(Remaining, Id, [Random-Id| Draws]) :-
		random_float(Random),
		Remaining1 is Remaining - 1,
		NextId is Id + 1,
		random_draws(Remaining1, NextId, Draws).

	sample_sorted_draws(_, [], []) :-
		!.
	sample_sorted_draws(CDF, [Random-Id| Draws], Selections) :-
		CDF = [cdf_entry(CumWeight, Class, AVs)| Rest],
		(	(Random < CumWeight; Rest == []) ->
			Selections = [Id-example(Id, Class, AVs)| Selected],
			sample_sorted_draws(CDF, Draws, Selected)
		;	sample_sorted_draws(Rest, [Random-Id| Draws], Selections)
		).

	% build_attribute_clauses/3 - build attribute_values/2 clauses for the dynamic dataset
	build_attribute_clauses([], _, []).
	build_attribute_clauses([Name| Names], Attributes, [attribute_values(Name, Values)| Clauses]) :-
		memberchk(Name-Values, Attributes),
		build_attribute_clauses(Names, Attributes, Clauses).

	% compute_weighted_error/4 - sum weights of misclassified examples
	compute_weighted_error(WeightedExamples, Tree, WeightedError, Misclassified) :-
		compute_error_acc(WeightedExamples, Tree, 0.0, WeightedError, Misclassified).

	compute_error_acc([], _, Error, Error, []).
	compute_error_acc([weighted_example(_Id, Class, AVs, Weight)| Rest], Tree, Error0, Error, [Incorrect| Misclassified]) :-
		c45_predict(Tree, AVs, Predicted),
		(	Predicted == Class ->
			Incorrect = false,
			compute_error_acc(Rest, Tree, Error0, Error, Misclassified)
		;	Error1 is Error0 + Weight,
			Incorrect = true,
			compute_error_acc(Rest, Tree, Error1, Error, Misclassified)
		).

	learner_weight(Error, NumClasses, Alpha) :-
		Alpha is log(1.0 - Error) - log(Error) + log(NumClasses - 1).

	update_weights(WeightedExamples, Misclassified, Error, NumClasses, Updated) :-
		CorrectMass is 1.0 - Error,
		CorrectShare is 1.0 / NumClasses,
		IncorrectShare is (NumClasses - 1.0) / NumClasses,
		update_normalized_weights(WeightedExamples, Misclassified, Error, CorrectMass, CorrectShare, IncorrectShare, Updated).

	update_normalized_weights([], [], _, _, _, _, []).
	update_normalized_weights([weighted_example(Id, Class, AVs, Weight)| Rest], [Incorrect| Misclassified], Error, CorrectMass, CorrectShare, IncorrectShare, [weighted_example(Id, Class, AVs, NewWeight)| UpdatedRest]) :-
		(	Incorrect == false ->
			NewWeight is (Weight / CorrectMass) * CorrectShare
		;	NewWeight is (Weight / Error) * IncorrectShare
		),
		update_normalized_weights(Rest, Misclassified, Error, CorrectMass, CorrectShare, IncorrectShare, UpdatedRest).

	% normalize_weights/2 - normalize weights so they sum to 1.0
	normalize_weights(WeightedExamples, Normalized) :-
		sum_weights(WeightedExamples, 0.0, TotalWeight),
		(	TotalWeight > 0.0 ->
			divide_weights(WeightedExamples, TotalWeight, Normalized)
		;	% Fallback: uniform weights
			length(WeightedExamples, N),
			UniformWeight is 1.0 / N,
			set_uniform_weights(WeightedExamples, UniformWeight, Normalized)
		).

	sum_weights([], Sum, Sum).
	sum_weights([weighted_example(_, _, _, Weight)| Rest], Acc, Sum) :-
		Acc1 is Acc + Weight,
		sum_weights(Rest, Acc1, Sum).

	divide_weights([], _, []).
	divide_weights([weighted_example(Id, Class, AVs, Weight)| Rest], Total, [weighted_example(Id, Class, AVs, NormWeight)| NormRest]) :-
		NormWeight is Weight / Total,
		divide_weights(Rest, Total, NormRest).

	set_uniform_weights([], _, []).
	set_uniform_weights([weighted_example(Id, Class, AVs, _)| Rest], Weight, [weighted_example(Id, Class, AVs, Weight)| NormRest]) :-
		set_uniform_weights(Rest, Weight, NormRest).

	% filter_instance_attrs/3 - filter instance attributes to the given names
	filter_instance_attrs(_, [], []) :-
		!.
	filter_instance_attrs(AVs, [Name| Names], [Name-Value| Filtered]) :-
		memberchk(Name-Value, AVs),
		filter_instance_attrs(AVs, Names, Filtered).

	% predict/3 - predicts the class using weighted majority voting
	predict(Classifier, Instance, Class) :-
		classifier_data(Classifier, WeightedTrees, ClassValues, _Options),
		collect_weighted_predictions(WeightedTrees, Instance, WeightedPredictions),
		accumulate_weighted_votes(WeightedPredictions, ClassValues, Votes),
		^^max_probability(Votes, Class, _Weight).

	% predict_probabilities/3 - returns class probabilities based on weighted votes
	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier =.. [_, WeightedTrees, ClassValues, _Options],
		% Collect weighted predictions from all trees
		collect_weighted_predictions(WeightedTrees, Instance, WeightedPredictions),
		% Accumulate weighted votes for each class
		accumulate_weighted_votes(WeightedPredictions, ClassValues, Votes),
		% Normalize to probabilities
		sum_vote_weights(Votes, 0.0, TotalWeight),
		normalize_votes(Votes, TotalWeight, Probabilities).

	collect_weighted_predictions([], _, []) :-
		!.
	collect_weighted_predictions(WeightedTrees, Instance, Predictions) :-
		WeightedTrees = [weighted_tree(_, _, AttributeNames)| _],
		filter_instance_attrs(Instance, AttributeNames, FilteredInstance),
		collect_tree_predictions(WeightedTrees, Instance, AttributeNames, FilteredInstance, Predictions).

	collect_tree_predictions([], _, _, _, []).
	collect_tree_predictions([weighted_tree(Alpha, Tree, AttributeNames)| Rest], Instance, SharedNames, SharedInstance, [Alpha-Prediction| Predictions]) :-
		(	AttributeNames == SharedNames ->
			FilteredInstance = SharedInstance
		;	filter_instance_attrs(Instance, AttributeNames, FilteredInstance)
		),
		c45_predict(Tree, FilteredInstance, Prediction),
		collect_tree_predictions(Rest, Instance, SharedNames, SharedInstance, Predictions).

	accumulate_weighted_votes(WeightedPredictions, ClassValues, Votes) :-
		length(ClassValues, ClassCount),
		(	ClassCount >= 16,
			length(WeightedPredictions, EstimatorCount),
			EstimatorCount >= 64 ->
			indexed_votes(WeightedPredictions, ClassValues, Votes)
		;	accumulate_votes_for_classes(ClassValues, WeightedPredictions, Votes)
		).

	indexed_votes(WeightedPredictions, ClassValues, Votes) :-
		zero_votes(ClassValues, InitialVotes),
		as_dictionary(InitialVotes, InitialTotals),
		add_indexed_votes(WeightedPredictions, InitialTotals, Totals),
		ordered_votes(ClassValues, Totals, Votes).

	zero_votes([], []).
	zero_votes([Class| Classes], [Class-0.0| Votes]) :-
		zero_votes(Classes, Votes).

	add_indexed_votes([], Totals, Totals).
	add_indexed_votes([Alpha-Class| Predictions], Totals0, Totals) :-
		lookup(Class, Previous, Totals0),
		Weight is Previous + Alpha,
		update(Totals0, Class, Weight, Totals1),
		add_indexed_votes(Predictions, Totals1, Totals).

	ordered_votes([], _, []).
	ordered_votes([Class| Classes], Totals, [Class-Weight| Votes]) :-
		lookup(Class, Weight, Totals),
		ordered_votes(Classes, Totals, Votes).

	accumulate_votes_for_classes([], _, []).
	accumulate_votes_for_classes([Class| Classes], WeightedPredictions, [Class-TotalWeight| Votes]) :-
		sum_class_weight(WeightedPredictions, Class, 0.0, TotalWeight),
		accumulate_votes_for_classes(Classes, WeightedPredictions, Votes).

	sum_class_weight([], _, Total, Total).
	sum_class_weight([Alpha-Class| Rest], Class, Acc, Total) :-
		!,
		Acc1 is Acc + Alpha,
		sum_class_weight(Rest, Class, Acc1, Total).
	sum_class_weight([_| Rest], Class, Acc, Total) :-
		sum_class_weight(Rest, Class, Acc, Total).

	sum_vote_weights([], Sum, Sum).
	sum_vote_weights([_-Weight| Rest], Acc, Sum) :-
		Acc1 is Acc + Weight,
		sum_vote_weights(Rest, Acc1, Sum).

	normalize_votes([], _, []).
	normalize_votes([Class-Weight| Votes], Total, [Class-Probability| Probs]) :-
		(	Total > 0.0 ->
			Probability is float(Weight / Total)
		;	Probability is 0.0
		),
		normalize_votes(Votes, Total, Probs).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		classifier_data(Classifier, WeightedTrees, ClassValues, Options),
		length(WeightedTrees, EstimatorCount),
		Diagnostics = [
			model(adaptive_boosting_classifier),
			classes(ClassValues),
			estimators(EstimatorCount),
			options(Options)
		].

	check_classifier(Classifier) :-
		(	var(Classifier) ->
			instantiation_error
		;	classifier_data(Classifier, WeightedTrees, ClassValues, Options),
			valid(list(compound), WeightedTrees),
			WeightedTrees \== [],
			^^valid_class_values(ClassValues),
			catch(::check_options(Options), _Error, fail),
			memberchk(number_of_estimators(ExpectedEstimators), Options),
			length(WeightedTrees, EstimatorCount),
			EstimatorCount =< ExpectedEstimators,
			valid_weighted_trees(WeightedTrees, ClassValues) ->
			true
		;	domain_error(classifier, Classifier)
		).

	% export_to_clauses/4 - exports classifier as a clause
	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(ab_classifier(_WeightedTrees, _ClassValues, _Options), ab_classifier('WeightedTrees', 'ClassValues', 'Options')).

	valid_weighted_trees([], _).
	valid_weighted_trees([weighted_tree(Alpha, Tree, AttributeNames)| WeightedTrees], ClassValues) :-
		valid(positive_float, Alpha),
		^^valid_attribute_names(AttributeNames),
		c45_classifier::valid_classifier(Tree),
		valid_tree_metadata(Tree, AttributeNames, ClassValues),
		valid_weighted_trees(WeightedTrees, ClassValues).

	valid_tree_metadata(c45_model(Tree, Statistics), AttributeNames, ClassValues) :-
		c45_classifier::diagnostic(c45_model(Tree, Statistics), classes(ModelClasses)),
		forall(list::member(Class, ModelClasses), list::memberchk(Class, ClassValues)),
		valid_tree_metadata(Tree, AttributeNames, ClassValues).
	valid_tree_metadata(leaf(Class), _, ClassValues) :-
		memberchk(Class, ClassValues).
	valid_tree_metadata(tree(Attribute, threshold(_), Left, Right), AttributeNames, ClassValues) :-
		memberchk(Attribute, AttributeNames),
		valid_tree_metadata(Left, AttributeNames, ClassValues),
		valid_tree_metadata(Right, AttributeNames, ClassValues).
	valid_tree_metadata(tree(Attribute, Subtrees), AttributeNames, ClassValues) :-
		memberchk(Attribute, AttributeNames),
		valid_subtree_metadata(Subtrees, AttributeNames, ClassValues).

	valid_subtree_metadata([], _, _).
	valid_subtree_metadata([_-Tree| Subtrees], AttributeNames, ClassValues) :-
		valid_tree_metadata(Tree, AttributeNames, ClassValues),
		valid_subtree_metadata(Subtrees, AttributeNames, ClassValues).

	classifier_data(Classifier, WeightedTrees, ClassValues, Options) :-
		Classifier =.. [_Functor, WeightedTrees, ClassValues, Options].

	% print_classifier/1 - pretty prints the classifier
	print_classifier(Classifier) :-
		classifier_data(Classifier, WeightedTrees, ClassValues, Options),
		format('AdaBoost Classifier~n', []),
		format('===================~n~n', []),
		^^print_classifier_template(Classifier),
		format('Learning options: ~w~n~n', [Options]),
		format('Class values: ~w~n', [ClassValues]),
		length(WeightedTrees, NumTrees),
		format('Number of estimators: ~w~n~n', [NumTrees]),
		format('Weighted trees:~n', []),
		print_weighted_trees(WeightedTrees, 1).

	print_weighted_trees([], _).
	print_weighted_trees([weighted_tree(Alpha, Tree, AttributeNames)| Rest], N) :-
		format('  Estimator ~w (alpha=~4f, features: ~w):~n', [N, Alpha, AttributeNames]),
		print_tree_summary(Tree),
		N1 is N + 1,
		print_weighted_trees(Rest, N1).

	print_tree_summary(c45_model(Tree, _Statistics)) :-
		print_tree_summary(Tree).
	print_tree_summary(leaf(Class)) :-
		format('    -> leaf(~w)~n', [Class]).
	print_tree_summary(tree(Attr, _)) :-
		format('    -> tree rooted at ~w~n', [Attr]).
	print_tree_summary(tree(Attr, threshold(_), _, _)) :-
		format('    -> tree rooted at ~w (continuous)~n', [Attr]).

	% Default options
	default_option(number_of_estimators(10)).

	% Option validation
	valid_option(number_of_estimators(N)) :-
		valid(positive_integer, N).

:- end_object.

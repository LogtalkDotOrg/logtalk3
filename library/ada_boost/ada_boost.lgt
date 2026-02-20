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


:- object(ada_boost,
	implements(classifier_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'AdaBoost (Adaptive Boosting) classifier using C4.5 decision trees as base learners. Implements the SAMME (Stagewise Additive Modeling using a Multi-class Exponential loss function) variant, which supports multi-class classification. Builds an ensemble of weighted decision trees where each subsequent tree focuses on the examples misclassified by previous trees.',
		remarks is [
			'Algorithm' - 'AdaBoost iteratively trains weak learners (C4.5 decision trees) on weighted versions of the training data. After each iteration, the weights of misclassified examples are increased so that subsequent learners focus more on difficult cases.',
			'SAMME variant' - 'This implementation uses the SAMME algorithm (Zhu et al., 2009) which extends AdaBoost to the multi-class case by adjusting the weight update formula to account for the number of classes.',
			'Learner weights' - 'Each base learner receives a weight (alpha) proportional to its accuracy. More accurate learners have higher weights in the final ensemble vote.',
			'Classifier representation' - 'The learned classifier is represented as a ``ab_classifier(WeightedTrees, ClassValues, Options)`` term where ``WeightedTrees`` is a list of ``weighted_tree(Alpha, Tree, AttributeNames)`` elements.',
			'Early stopping' - 'Training stops early if a perfect classifier is found (zero weighted error) or if a base learner performs worse than random guessing.'
		],
		see_also is [dataset_protocol, c45, isolation_forest, knn, naive_bayes, nearest_centroid, random_forest]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a classifier from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Classifier', 'Options']
	]).

	:- public(predict_probabilities/3).
	:- mode(predict_probabilities(+compound, +list, -list), one).
	:- info(predict_probabilities/3, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier. Returns a list of ``Class-Probability`` pairs sorted by descending probability. Probabilities are derived from the weighted votes of all base learners.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

	:- uses(c45, [
		learn/2 as c45_learn/2,
		predict/3 as c45_predict/3
	]).

	:- uses(fast_random(xoshiro128pp), [
		random/1 as random_float/1
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	% learn/2 - learns a classifier with default options
	learn(Dataset, Classifier) :-
		learn(Dataset, Classifier, []).

	% learn/3 - learns a classifier with specified options
	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(number_of_estimators(NumEstimators), Options),
		% Get attribute information from dataset
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		% Get class values
		Dataset::class_values(ClassValues),
		length(ClassValues, NumClasses),
		% Get all examples
		findall(
			Id-Class-AVs,
			Dataset::example(Id, Class, AVs),
			Examples
		),
		length(Examples, N),
		% Initialize uniform weights
		InitWeight is 1.0 / N,
		initialize_weights(Examples, InitWeight, WeightedExamples),
		% Build the ensemble
		build_ensemble(
			Dataset, NumEstimators, Attributes, AttributeNames,
			NumClasses, WeightedExamples, N, 1, [], WeightedTrees
		),
		% Create classifier term
		Classifier = ab_classifier(WeightedTrees, ClassValues, Options).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	% initialize_weights/3 - assign initial weight to each example
	initialize_weights([], _, []).
	initialize_weights([Id-Class-AVs| Examples], Weight, [Id-Class-AVs-Weight| WeightedExamples]) :-
		initialize_weights(Examples, Weight, WeightedExamples).

	% build_ensemble/10 - iteratively build weighted decision trees
	build_ensemble(_, NumEstimators, _, _, _, _, _, Round, WeightedTrees0, WeightedTrees) :-
		Round > NumEstimators,
		!,
		WeightedTrees = WeightedTrees0.
	build_ensemble(
		Dataset, NumEstimators, Attributes, AttributeNames,
		NumClasses, WeightedExamples, N, Round, WeightedTrees0, WeightedTrees
	) :-
		% Create a weighted bootstrap dataset
		create_weighted_dataset(Dataset, WeightedExamples, AttributeNames, Attributes, WeightedDataset),
		% Train a C4.5 tree on the weighted sample
		c45_learn(WeightedDataset, Tree),
		% Clean up the temporary dataset
		abolish_object(WeightedDataset),
		% Compute weighted error
		compute_weighted_error(WeightedExamples, Tree, AttributeNames, WeightedError),
		% Check if this learner is useful
		RandomError is (NumClasses - 1) / NumClasses,
		(	WeightedError >= RandomError ->
			% Learner not better than random; stop early
			WeightedTrees = WeightedTrees0
		;	WeightedError =< 0.0 ->
			% Perfect classifier; add with high weight and stop
			Alpha is 10.0,
			append(WeightedTrees0, [weighted_tree(Alpha, Tree, AttributeNames)], WeightedTrees)
		;	% Compute learner weight (SAMME formula)
			Alpha is log((1.0 - WeightedError) / WeightedError) + log(NumClasses - 1),
			% Update example weights
			update_weights(WeightedExamples, Tree, AttributeNames, Alpha, UpdatedWeights0),
			% Normalize weights
			normalize_weights(UpdatedWeights0, UpdatedWeights),
			% Continue building
			NextRound is Round + 1,
			append(WeightedTrees0, [weighted_tree(Alpha, Tree, AttributeNames)], WeightedTrees1),
			build_ensemble(
				Dataset, NumEstimators, Attributes, AttributeNames,
				NumClasses, UpdatedWeights, N, NextRound, WeightedTrees1, WeightedTrees
			)
		).

	% create_weighted_dataset/5 - creates a weighted sample dataset
	% Uses weighted random sampling with replacement
	create_weighted_dataset(Dataset, WeightedExamples, AttributeNames, Attributes, WeightedDataset) :-
		length(WeightedExamples, N),
		% Build cumulative distribution from weights
		build_cumulative_distribution(WeightedExamples, CDF),
		% Sample N examples with replacement according to weights
		weighted_sample(CDF, WeightedExamples, N, N, SampledExamples),
		% Get class info from original dataset
		Dataset::class(ClassName),
		Dataset::class_values(ClassValues),
		% Build the temporary dataset object
		build_attribute_clauses(AttributeNames, Attributes, AttributeClauses),
		build_example_clauses(SampledExamples, ExampleClauses),
		append(AttributeClauses, ExampleClauses, DataClauses),
		AllClauses = [class(ClassName), class_values(ClassValues)| DataClauses],
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
	build_cdf_acc([Id-Class-AVs-Weight| Rest], Acc, [CumWeight-Id-Class-AVs| CDFRest]) :-
		CumWeight is Acc + Weight,
		build_cdf_acc(Rest, CumWeight, CDFRest).

	% weighted_sample/5 - sample with replacement using CDF
	weighted_sample(_, _, _, 0, []) :-
		!.
	weighted_sample(CDF, WeightedExamples, TotalN, Remaining, [NewId-Class-AVs| Rest]) :-
		random_float(Random),
		select_from_cdf(CDF, Random, _-Class-AVs),
		NewId is TotalN - Remaining + 1,
		Remaining1 is Remaining - 1,
		weighted_sample(CDF, WeightedExamples, TotalN, Remaining1, Rest).

	% select_from_cdf/3 - find the first entry in CDF whose cumulative weight >= R
	select_from_cdf([CumWeight-Id-Class-AVs| _], Random, Id-Class-AVs) :-
		CumWeight >= Random,
		!.
	% fallback: if R is very close to 1.0, pick last element
	select_from_cdf([_-Id-Class-AVs], _, Id-Class-AVs) :-
		!.
	select_from_cdf([_| Rest], Random, Selected) :-
		select_from_cdf(Rest, Random, Selected).

	% build_attribute_clauses/3 - build attribute_values/2 clauses for the dynamic dataset
	build_attribute_clauses([], _, []).
	build_attribute_clauses([Name| Names], Attributes, [attribute_values(Name, Values)| Clauses]) :-
		memberchk(Name-Values, Attributes),
		build_attribute_clauses(Names, Attributes, Clauses).

	% build_example_clauses/2 - build example/3 clauses
	build_example_clauses([], []).
	build_example_clauses([Id-Class-AVs| Examples], [example(Id, Class, AVs)| Clauses]) :-
		build_example_clauses(Examples, Clauses).

	% compute_weighted_error/4 - sum weights of misclassified examples
	compute_weighted_error(WeightedExamples, Tree, AttributeNames, WeightedError) :-
		compute_error_acc(WeightedExamples, Tree, AttributeNames, 0.0, WeightedError).

	compute_error_acc([], _, _, Error, Error).
	compute_error_acc([_Id-Class-AVs-Weight| Rest], Tree, AttributeNames, Error0, Error) :-
		filter_instance_attrs(AVs, AttributeNames, FilteredAVs),
		c45_predict(Tree, FilteredAVs, Predicted),
		(	Predicted == Class ->
			compute_error_acc(Rest, Tree, AttributeNames, Error0, Error)
		;	Error1 is Error0 + Weight,
			compute_error_acc(Rest, Tree, AttributeNames, Error1, Error)
		).

	% update_weights/5 - increase weights for misclassified examples
	update_weights([], _, _, _, []).
	update_weights([Id-Class-AVs-Weight| Rest], Tree, AttributeNames, Alpha, [Id-Class-AVs-NewWeight| UpdatedRest]) :-
		filter_instance_attrs(AVs, AttributeNames, FilteredAVs),
		c45_predict(Tree, FilteredAVs, Predicted),
		(	Predicted == Class ->
			NewWeight = Weight
		;	NewWeight is Weight * exp(Alpha)
		),
		update_weights(Rest, Tree, AttributeNames, Alpha, UpdatedRest).

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
	sum_weights([_-_-_-Weight| Rest], Acc, Sum) :-
		Acc1 is Acc + Weight,
		sum_weights(Rest, Acc1, Sum).

	divide_weights([], _, []).
	divide_weights([Id-Class-AVs-Weight| Rest], Total, [Id-Class-AVs-NormWeight| NormRest]) :-
		NormWeight is Weight / Total,
		divide_weights(Rest, Total, NormRest).

	set_uniform_weights([], _, []).
	set_uniform_weights([Id-Class-AVs-_| Rest], Weight, [Id-Class-AVs-Weight| NormRest]) :-
		set_uniform_weights(Rest, Weight, NormRest).

	% filter_instance_attrs/3 - filter instance attributes to the given names
	filter_instance_attrs(_, [], []) :-
		!.
	filter_instance_attrs(AVs, [Name| Names], [Name-Value| Filtered]) :-
		memberchk(Name-Value, AVs),
		filter_instance_attrs(AVs, Names, Filtered).

	% predict/3 - predicts the class using weighted majority voting
	predict(Classifier, Instance, Class) :-
		predict_probabilities(Classifier, Instance, Probabilities),
		max_probability(Probabilities, Class, _).

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

	collect_weighted_predictions([], _, []).
	collect_weighted_predictions([weighted_tree(Alpha, Tree, AttributeNames)| Rest], Instance, [Alpha-Prediction| Predictions]) :-
		filter_instance_attrs(Instance, AttributeNames, FilteredInstance),
		c45_predict(Tree, FilteredInstance, Prediction),
		collect_weighted_predictions(Rest, Instance, Predictions).

	accumulate_weighted_votes(WeightedPredictions, ClassValues, Votes) :-
		accumulate_votes_for_classes(ClassValues, WeightedPredictions, Votes).

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

	max_probability([Class-Probability], Class, Probability) :-
		!.
	max_probability([Class1-Prob1, Class2-Prob2| Rest], MaxClass, MaxProb) :-
		(	Prob1 >= Prob2 ->
			max_probability([Class1-Prob1| Rest], MaxClass, MaxProb)
		;	max_probability([Class2-Prob2| Rest], MaxClass, MaxProb)
		).

	% classifier_to_clauses/4 - exports classifier as a clause
	classifier_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Classifier =.. [_, WeightedTrees, ClassValues, Options],
		Clause =.. [Functor, WeightedTrees, ClassValues, Options].

	% classifier_to_file/4 - exports classifier to a file
	classifier_to_file(Dataset, Classifier, Functor, File) :-
		classifier_to_clauses(Dataset, Classifier, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Functor, Stream) :-
		format(Stream, '% ~q(WeightedTrees, ClassValues, Options)~n', [Functor]).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	% print_classifier/1 - pretty prints the classifier
	print_classifier(Classifier) :-
		Classifier =.. [_, WeightedTrees, ClassValues, Options],
		format('AdaBoost Classifier~n', []),
		format('===================~n~n', []),
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

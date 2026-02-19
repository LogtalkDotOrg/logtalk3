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


:- object(knn,
	implements(classifier_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'k-Nearest Neighbors classifier with multiple distance metrics and weighting options. Learns from a dataset object implementing the ``dataset_protocol`` protocol and returns a classifier term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'k-NN is a lazy learning algorithm that classifies instances based on the majority class among the k nearest training instances.',
			'Distance metrics' - 'Supports Euclidean, Manhattan, Chebyshev, and Minkowski distance metrics.',
			'Weighting schemes' - 'Supports uniform, distance-based, and Gaussian weighting of neighbors.',
			'Feature types' - 'Automatically handles numeric and categorical features.',
			'Classifier representation' - 'The learned classifier is represented (by default) as a ``knn_classifier(AttributeNames, FeatureTypes, Instances)`` where ``Instances`` contains the training data.'
		],
		see_also is [dataset_protocol, c45, isolation_forest, naive_bayes, nearest_centroid, random_forest]
	]).

	:- public(predict/4).
	:- mode(predict(+compound, +list, -atom, +list(compound)), one).
	:- info(predict/4, [
		comment is 'Predicts the class label for a new instance using the learned classifier and the given options. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Class', 'Options']
	]).

	:- public(predict_probabilities/3).
	:- mode(predict_probabilities(+compound, +list, -list), one).
	:- info(predict_probabilities/3, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier and default options. Returns a list of ``Class-Probability`` pairs. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

	:- public(predict_probabilities/4).
	:- mode(predict_probabilities(+compound, +list, -list, +list(compound)), one).
	:- info(predict_probabilities/4, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier and the given options. Returns a list of ``Class-Probability`` pairs. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities', 'Options']
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, take/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Classifier) :-
		% Get attribute information from dataset
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		% Get all examples from dataset
		findall(
			Id-Class-AttributeValues,
			Dataset::example(Id, Class, AttributeValues),
			Examples
		),
		% Extract instances and labels (Labels include instance features for kNN)
		examples_to_instances(Examples, AttributeNames, Instances),
		% Determine feature types from dataset attributes
		determine_feature_types_from_dataset(Attributes, FeatureTypes),
		% Build classifier term
		Classifier = knn_classifier(AttributeNames, FeatureTypes, Instances).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	% Convert examples to instances (list of values) and labels
	examples_to_instances([], _, []).
	examples_to_instances([_-Class-AttributeValues| Examples], AttributeNames, [Instance-Class| Instances]) :-
		extract_values(AttributeNames, AttributeValues, Instance),
		examples_to_instances(Examples, AttributeNames, Instances).

	extract_values([], _, []).
	extract_values([Attribute| Attributes], AttributeValues, [Value| Values]) :-
		memberchk(Attribute-Value, AttributeValues),
		extract_values(Attributes, AttributeValues, Values).

	% Determine feature types from dataset attribute definitions
	determine_feature_types_from_dataset([], []).
	determine_feature_types_from_dataset([_-Values| Pairs], [Type| Types]) :-
		(	Values == continuous ->
			Type = numeric
		;	Type = categorical
		),
		determine_feature_types_from_dataset(Pairs, Types).

	predict(Classifier, Instance, Class) :-
		predict(Classifier, Instance, Class, []).

	predict(Classifier, Instance, Class, UserOptions) :-
		predict_probabilities(Classifier, Instance, Probabilities, UserOptions),
		max_probability(Probabilities, Class, _).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		predict_probabilities(Classifier, Instance, Probabilities, []).

	predict_probabilities(Classifier, Instance, Probabilities, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Classifier =.. [_, AttributeNames, FeatureTypes, Instances],
		% Extract values from instance in correct order
		extract_values(AttributeNames, Instance, Values),
		% Get k value
		^^option(k(K), Options),
		% Find k nearest neighbors
		find_k_nearest(Values, Instances, FeatureTypes, K, Neighbors, Options),
		% Compute class weights
		compute_class_weights(Neighbors, Probabilities, Options).

	find_k_nearest(Instance, Instances, FeatureTypes, K, Neighbors, Options) :-
		findall(
			Distance-Class,
			(	member(Features-Class, Instances),
				compute_distance(Instance, Features, FeatureTypes, Distance, Options)
			),
			AllDistances
		),
		keysort(AllDistances, SortedDistances),
		take(K, SortedDistances, Neighbors).

	compute_distance(Instance1, Instance2, FeatureTypes, Distance, Options) :-
		^^option(distance_metric(Metric), Options),
		compute_distance_with_metric(Metric, FeatureTypes, Instance1, Instance2, Distance).

	compute_distance_with_metric(euclidean, Types, Instance1, Instance2, Distance) :-
		euclidean_distance(Types, Instance1, Instance2, Distance).
	compute_distance_with_metric(manhattan, Types, Instance1, Instance2, Distance) :-
		manhattan_distance(Types, Instance1, Instance2, Distance).
	compute_distance_with_metric(chebyshev, Types, Instance1, Instance2, Distance) :-
		chebyshev_distance(Types, Instance1, Instance2, Distance).
	compute_distance_with_metric(minkowski, Types, Instance1, Instance2, Distance) :-
		minkowski_distance(Types, Instance1, Instance2, 3, Distance).

	% Distance metrics
	euclidean_distance(Types, Instance1, Instance2, Distance) :-
		sum_squared_diffs(Types, Instance1, Instance2, 0, SumSq),
		Distance is sqrt(SumSq).

	manhattan_distance(Types, Instance1, Instance2, Distance) :-
		sum_abs_diffs(Types, Instance1, Instance2, 0, Distance).

	chebyshev_distance(Types, Instance1, Instance2, Distance) :-
		max_abs_diff(Types, Instance1, Instance2, 0, Distance).

	minkowski_distance(Types, Instance1, Instance2, P, Distance) :-
		sum_powered_diffs(Types, Instance1, Instance2, P, 0, Sum),
		Distance is Sum ** (1/P).

	sum_squared_diffs([], [], [], Sum, Sum).
	sum_squared_diffs([Type| Types], [Value1| Values1], [Value2| Values2], Sum0, Sum) :-
		feature_distance_squared(Type, Value1, Value2, DistanceSq),
		Sum1 is Sum0 + DistanceSq,
		sum_squared_diffs(Types, Values1, Values2, Sum1, Sum).

	sum_abs_diffs([], [], [], Sum, Sum).
	sum_abs_diffs([Type| Types], [Value1| Values1], [Value2| Values2], Sum0, Sum) :-
		feature_distance_abs(Type, Value1, Value2, Distance),
		Sum1 is Sum0 + Distance,
		sum_abs_diffs(Types, Values1, Values2, Sum1, Sum).

	max_abs_diff([], [], [], Max, Max).
	max_abs_diff([Type| Types], [Value1| Values1], [Value2| Values2], Max0, Max) :-
		feature_distance_abs(Type, Value1, Value2, Distance),
		Max1 is max(Max0, Distance),
		max_abs_diff(Types, Values1, Values2, Max1, Max).

	sum_powered_diffs([], [], [], _, Sum, Sum).
	sum_powered_diffs([Type| Types], [Value1| Values1], [Value2| Values2], P, Sum0, Sum) :-
		feature_distance_abs(Type, Value1, Value2, Distance),
		Powered is Distance ** P,
		Sum1 is Sum0 + Powered,
		sum_powered_diffs(Types, Values1, Values2, P, Sum1, Sum).

	% Feature-level distance
	feature_distance_squared(numeric, Value1, Value2, DistanceSquared) :-
		Difference is Value1 - Value2,
		DistanceSquared is Difference * Difference.
	feature_distance_squared(categorical, Value1, Value2, 0) :-
		Value1 == Value2,
		!.
	feature_distance_squared(categorical, _, _, 1).

	feature_distance_abs(numeric, Value1, Value2, Distance) :-
		Distance is abs(Value1 - Value2).
	feature_distance_abs(categorical, Value1, Value2, 0) :-
		Value1 == Value2,
		!.
	feature_distance_abs(categorical, _, _, 1).

	% Compute class weights from neighbors
	compute_class_weights(Neighbors, Probabilities, Options) :-
		^^option(weight_scheme(Scheme), Options),
		apply_weighting(Scheme, Neighbors, WeightedNeighbors),
		aggregate_votes(WeightedNeighbors, Probabilities).

	apply_weighting(uniform, Neighbors, WeightedNeighbors) :-
		uniform_weights(Neighbors, WeightedNeighbors).
	apply_weighting(distance, Neighbors, WeightedNeighbors) :-
		distance_weights(Neighbors, WeightedNeighbors).
	apply_weighting(gaussian, Neighbors, WeightedNeighbors) :-
		gaussian_weights(Neighbors, WeightedNeighbors).

	uniform_weights([], []).
	uniform_weights([_Distance-Label| DistanceLabels], [1.0-Label| WeightedLabels]) :-
		uniform_weights(DistanceLabels, WeightedLabels).

	distance_weights([], []).
	distance_weights([Distance-Label| DistanceLabels], [Weight-Label| WeightedLabels]) :-
		(	Distance =:= 0 ->
			% Very large weight for exact matches
			Weight = 1.0e10
		;	Weight is 1.0 / Distance
		),
		distance_weights(DistanceLabels, WeightedLabels).

	gaussian_weights([], []).
	gaussian_weights([Distance-Label| DistanceLabels], [Weight-Label| WeightedLabels]) :-
		Sigma = 1.0,
		Weight is exp(-(Distance * Distance) / (2 * Sigma * Sigma)),
		gaussian_weights(DistanceLabels, WeightedLabels).

	aggregate_votes(WeightedNeighbors, Probabilities) :-
		collect_class_weights(WeightedNeighbors, ClassWeights),
		compute_total_weight(ClassWeights, 0, TotalWeight),
		normalize_weights(ClassWeights, TotalWeight, Probabilities).

	collect_class_weights(Neighbors, ClassWeights) :-
		collect_weights_by_class(Neighbors, [], ClassWeights0),
		keysort(ClassWeights0, ClassWeights).

	collect_weights_by_class([], ClassWeights, ClassWeights).
	collect_weights_by_class([Weight-Label| Rest], ClassWeights0, ClassWeights) :-
		(	member_class_weight(Label, ClassWeights0, ExistingWeight) ->
			NewWeight is ExistingWeight + Weight,
			update_class_weight(Label, NewWeight, ClassWeights0, ClassWeights1)
		;	ClassWeights1 = [Label-Weight| ClassWeights0]
		),
		collect_weights_by_class(Rest, ClassWeights1, ClassWeights).

	member_class_weight(Label, [Label-Weight| _], Weight) :-
		!.
	member_class_weight(Label, [_| LabelWeights], Weight) :-
		member_class_weight(Label, LabelWeights, Weight).

	update_class_weight(Label, NewWeight, [Label-_| Tail], [Label-NewWeight| Tail]) :-
		!.
	update_class_weight(Label, NewWeight, [OtherLabel| Tail0], [OtherLabel| Tail]) :-
		update_class_weight(Label, NewWeight, Tail0, Tail).

	compute_total_weight([], Total, Total).
	compute_total_weight([_-Weight| Tail], Total0, Total) :-
		Total1 is Total0 + Weight,
		compute_total_weight(Tail, Total1, Total).

	normalize_weights([], _, []).
	normalize_weights([Class-Weight| ClassWeights], Total, [Class-Probability| ClassProbabilities]) :-
		Probability is float(Weight / Total),
		normalize_weights(ClassWeights, Total, ClassProbabilities).

	max_probability([Class-Probability], Class, Probability) :-
		!.
	max_probability([Class1-Probability1, Class2-Probability2| ClassProbabilities], MaxClass, MaxProbability) :-
		(	Probability1 >= Probability2 ->
			max_probability([Class1-Probability1| ClassProbabilities], MaxClass, MaxProbability)
		;	max_probability([Class2-Probability2| ClassProbabilities], MaxClass, MaxProbability)
		).

	classifier_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Classifier =.. [_, AttributeNames, FeatureTypes, Instances],
		Clause =.. [Functor, AttributeNames, FeatureTypes, Instances].

	classifier_to_file(Dataset, Classifier, Functor, File) :-
		classifier_to_clauses(Dataset, Classifier, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Functor, Stream) :-
		format(Stream, '% ~q(AttributeNames, FeatureTypes, Instances)~n', [Functor]).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	print_classifier(Classifier) :-
		Classifier =.. [_, AttributeNames, FeatureTypes, Instances],
		format('k-Nearest Neighbors Classifier~n', []),
		format('==============================~n~n', []),
		length(Instances, NumInstances),
		format('Training instances: ~w~n~n', [NumInstances]),
		format('Features:~n', []),
		print_features(AttributeNames, FeatureTypes).

	print_features([], []).
	print_features([Name| Names], [Type| Types]) :-
		format('  ~w (~w)~n', [Name, Type]),
		print_features(Names, Types).

	default_option(k(3)).
	default_option(distance_metric(euclidean)).
	default_option(weight_scheme(uniform)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(distance_metric(Metric)) :-
		valid(one_of(atom, [euclidean, manhattan, chebyshev, minkowski]), Metric).
	valid_option(weight_scheme(Scheme)) :-
		valid(one_of(atom, [uniform, distance, gaussian]), Scheme).

:- end_object.

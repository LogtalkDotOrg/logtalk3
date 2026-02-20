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


:- object(nearest_centroid,
	implements(classifier_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Nearest Centroid classifier with multiple distance metrics. Learns from a dataset object implementing the ``dataset_protocol`` protocol and returns a classifier term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Assign to an instance the the class of the training samples whose mean (centroid) is closest to the instance.',
			'Distance metrics' - 'Supports Euclidean, Manhattan, and cosine distance metrics.',
			'Feature types' - 'Automatically handles numeric and categorical features.',
			'Classifier representation' - 'The learned classifier is represented by default as a ``nc_classifier(AttributeNames, FeatureTypes, Centroids)`` term.'
		],
		see_also is [dataset_protocol, isolation_forest, c45, knn, naive_bayes, random_forest, ada_boost]
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
		length/2, member/2, memberchk/2, nth1/3, sort/4, take/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(pairs, [
		keys/2, values/2
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
		% Extract instances
		examples_to_instances(Examples, AttributeNames, Instances),
		% Determine feature types from dataset attributes
		determine_feature_types_from_dataset(Attributes, FeatureTypes),
        % Compute centroid for each class
		Dataset::class_values(Classes),
        compute_all_centroids(Classes, FeatureTypes, Instances, Centroids),
		% Build classifier term
		Classifier = nc_classifier(AttributeNames, FeatureTypes, Centroids).

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

    % Compute centroids for all classes
    compute_all_centroids([], _, _, []).
    compute_all_centroids([Class| Classes], FeatureTypes, Instances, [Class-Centroid| Centroids]) :-
        get_instances_for_class(Instances, Class, ClassInstances),
        compute_centroid(FeatureTypes, ClassInstances, Centroid),
        compute_all_centroids(Classes, FeatureTypes, Instances, Centroids).

    % Get all instances for a specific class
    get_instances_for_class([], _, []).
    get_instances_for_class([Instance-Class|RestInstances], Class, [Instance|FilteredRest]) :-
        !,
        get_instances_for_class(RestInstances, Class, FilteredRest).
    get_instances_for_class([_|RestInstances], Class, Filtered) :-
        get_instances_for_class(RestInstances, Class, Filtered).

    % Compute centroid (mean) of a set of instances
    compute_centroid(FeatureTypes, Instances, Centroid) :-
        transpose(Instances, Features),
        compute_centroid_features(Features, FeatureTypes, Centroid).

    % Transpose list of instances to list of features
    transpose([], []) :- !.
    transpose([[]|_], []) :- !.
    transpose(Instances, [Column|RestColumns]) :-
        extract_first_column(Instances, Column, Remaining),
        transpose(Remaining, RestColumns).

    extract_first_column([], [], []).
    extract_first_column([[H|T]|Rest], [H|Column], [T|Remaining]) :-
        extract_first_column(Rest, Column, Remaining).

    % Compute centroid for each feature
    compute_centroid_features([], [], []).
    compute_centroid_features([FeatureValues|RestFeatures], [Type|RestTypes], [CentroidValue|RestCentroid]) :-
        compute_feature_centroid(Type, FeatureValues, CentroidValue),
        compute_centroid_features(RestFeatures, RestTypes, RestCentroid).

    compute_feature_centroid(numeric, Values, Mean) :-
        compute_mean(Values, Mean).
    compute_feature_centroid(categorical, Values, Mode) :-
        compute_mode(Values, Mode).

    compute_mean(Values, Mean) :-
        length(Values, N),
        N > 0,
        sum(Values, Sum),
        Mean is Sum / N.

    compute_mode(Values, Mode) :-
        count_frequencies(Values, Frequencies),
        max_frequency(Frequencies, Mode, _).

    count_frequencies(Values, Frequencies) :-
        count_freq_acc(Values, [], Frequencies).

    count_freq_acc([], Acc, Frequencies) :-
        sort(2, @>=, Acc, Frequencies).
    count_freq_acc([Value|Rest], Acc, Frequencies) :-
        (   member_freq(Value, Acc, Count) ->
            NewCount is Count + 1,
            update_freq(Value, NewCount, Acc, NewAcc)
        ;   NewAcc = [Value-1|Acc]
        ),
        count_freq_acc(Rest, NewAcc, Frequencies).

    member_freq(Value, [Value-Count|_], Count) :-
		!.
    member_freq(Value, [_|Rest], Count) :-
        member_freq(Value, Rest, Count).

    update_freq(Value, NewCount, [Value-_|Rest], [Value-NewCount|Rest]) :- !.
    update_freq(Value, NewCount, [H|Rest], [H|NewRest]) :-
        update_freq(Value, NewCount, Rest, NewRest).

    max_frequency([Value-Count], Value, Count).
    max_frequency([Value1-Count1, Value2-Count2|Rest], MaxValue, MaxCount) :-
        (   Count1 >= Count2 ->
            max_frequency([Value1-Count1|Rest], MaxValue, MaxCount)
        ;   max_frequency([Value2-Count2|Rest], MaxValue, MaxCount)
        ).

	predict(Classifier, Instance, Class) :-
		predict(Classifier, Instance, Class, []).

	predict(Classifier, Instance, Class, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(distance_metric(DistanceMetric), Options),
		Classifier =.. [_, AttributeNames, FeatureTypes, Centroids],
		% Extract values from instance in correct order
		extract_values(AttributeNames, Instance, Values),
        findall(
			Distance-Class,
			(	member(Class-Centroid, Centroids),
            	compute_distance(DistanceMetric, FeatureTypes, Values, Centroid, Distance)
			),
			Distances
		),
        sort(Distances, [_MinDist-Class|_]).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		predict_probabilities(Classifier, Instance, Probabilities, []).

	predict_probabilities(Classifier, Instance, Probabilities, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(distance_metric(DistanceMetric), Options),
		Classifier =.. [_, AttributeNames, FeatureTypes, Centroids],
		% Extract values from instance in correct order
		extract_values(AttributeNames, Instance, Values),
        findall(
			Distance-Class,
			(	member(Class-Centroid, Centroids),
            	compute_distance(DistanceMetric, FeatureTypes, Values, Centroid, Distance)
    		),
			Distances
		),
        convert_distances_to_probabilities(Distances, Probabilities).

    % Convert distances to probabilities using inverse distance weighting
    convert_distances_to_probabilities(Distances, Probabilities) :-
        compute_inverse_weights(Distances, WeightedDistances),
        normalize_to_probabilities(WeightedDistances, Probabilities).

    compute_inverse_weights([], []).
    compute_inverse_weights([Distance-Class|Rest], [Weight-Class|WeightedRest]) :-
        (   Distance =:= 0 ->
            Weight = 1.0e10  % Very large weight for exact match
        ;   Weight is 1.0 / Distance
        ),
        compute_inverse_weights(Rest, WeightedRest).

    normalize_to_probabilities(WeightedDistances, Probabilities) :-
        sum_weights(WeightedDistances, TotalWeight),
        normalize_weights(WeightedDistances, TotalWeight, Probabilities).

    sum_weights([], 0).
    sum_weights([Weight-_|Rest], Total) :-
        sum_weights(Rest, RestTotal),
        Total is Weight + RestTotal.

    normalize_weights([], _, []).
    normalize_weights([Weight-Class|Rest], Total, [Class-Prob|NormRest]) :-
        Prob is Weight / Total,
        normalize_weights(Rest, Total, NormRest).

	compute_distance(euclidean, Types, Instance1, Instance2, Distance) :-
		euclidean_distance(Types, Instance1, Instance2, Distance).
	compute_distance(manhattan, Types, Instance1, Instance2, Distance) :-
		manhattan_distance(Types, Instance1, Instance2, Distance).
	compute_distance(cosine, Types, Instance1, Instance2, Distance) :-
		cosine_distance(Types, Instance1, Instance2, Distance).

	euclidean_distance(Types, Instance1, Instance2, Distance) :-
		sum_squared_diffs(Types, Instance1, Instance2, 0, SumSq),
		Distance is sqrt(SumSq).

	manhattan_distance(Types, Instance1, Instance2, Distance) :-
		sum_abs_diffs(Types, Instance1, Instance2, 0, Distance).

    cosine_distance(_Types, Instance1, Instance2, Distance) :-
        % Only works for numeric features
        dot_product(Instance1, Instance2, 0, Dot),
        vector_magnitude(Instance1, Mag1),
        vector_magnitude(Instance2, Mag2),
        (   (Mag1 =:= 0 ; Mag2 =:= 0) ->
            Distance = 1.0
        ;   Similarity is float(Dot / (Mag1 * Mag2)),
            Distance is float(1.0 - Similarity)
        ).

    dot_product([], [], Dot, Dot).
    dot_product([V1| V1s], [V2| V2s], Dot0, Dot) :-
        (   number(V1), number(V2) ->
            Dot1 is Dot0 + V1 * V2,
            dot_product(V1s, V2s, Dot1, Dot)
        ;   dot_product(V1s, V2s, Dot0, Dot)
        ).

    vector_magnitude(Vector, Magnitude) :-
        sum_of_squares(Vector, 0, Sum),
        Magnitude is sqrt(Sum).

    sum_of_squares([], Sum, Sum).
    sum_of_squares([V| Vs], Sum0, Sum) :-
        (   number(V) ->
            Sum1 is Sum0 + V * V,
            sum_of_squares(Vs, Sum1, Sum)
        ;   sum_of_squares(Vs, Sum0, Sum)
        ).

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

	classifier_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Classifier =.. [_, AttributeNames, FeatureTypes, Centroids],
		Clause =.. [Functor, AttributeNames, FeatureTypes, Centroids].

	classifier_to_file(Dataset, Classifier, Functor, File) :-
		classifier_to_clauses(Dataset, Classifier, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Functor, Stream) :-
		format(Stream, '% ~q(AttributeNames, FeatureTypes, Centroids)~n', [Functor]).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	print_classifier(Classifier) :-
		Classifier =.. [_, AttributeNames, FeatureTypes, Centroids],
		format('Nearest Centroid Classifier~n', []),
		format('==============================~n~n', []),
		length(Centroids, NumCentroids),
		format('Number of centroids: ~w~n~n', [NumCentroids]),
		nl,
		format('Attributes:~n', []),
		print_features(AttributeNames, FeatureTypes).

	print_features([], []).
	print_features([Name| Names], [Type| Types]) :-
		format('  ~w (~w)~n', [Name, Type]),
		print_features(Names, Types).

	default_option(distance_metric(euclidean)).

	valid_option(distance_metric(Metric)) :-
		valid(one_of(atom, [euclidean, manhattan, cosine]), Metric).

:- end_object.

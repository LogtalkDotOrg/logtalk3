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


:- object(nearest_centroid_classifier,
	imports(probabilistic_classifier_common)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Nearest Centroid classifier with multiple distance metrics. Learns from a dataset object implementing the ``dataset_protocol`` protocol and returns a classifier term that can be used for prediction and exported as predicate clauses.',
		see_also is [dataset_protocol, isolation_forest_anomaly_detector, c45_classifier, knn_classifier, naive_bayes_classifier, random_forest_classifier, adaptive_boosting_classifier]
	]).

	:- public(predict/4).
	:- mode(predict(+compound, +list, -atom, +list(compound)), one).
	:- info(predict/4, [
		comment is 'Predicts the class label for a new instance using the learned classifier and the given options. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Class', 'Options']
	]).

	:- public(predict_probabilities/4).
	:- mode(predict_probabilities(+compound, +list, -list, +list(compound)), one).
	:- info(predict_probabilities/4, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier and the given options. Returns a list of ``Class-Probability`` pairs. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, sort/4
	]).

	:- uses(linear_algebra, [
		transpose_matrix/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Classifier, []) :-
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
		transpose_matrix(Instances, Features),
        compute_centroid_features(Features, FeatureTypes, Centroid).

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
    compute_inverse_weights([Distance-Class| Rest], [Weight-Class| WeightedRest]) :-
        (   Distance =:= 0 ->
            Weight = 1.0e10  % Very large weight for exact match
        ;   Weight is 1.0 / Distance
        ),
        compute_inverse_weights(Rest, WeightedRest).

    normalize_to_probabilities(WeightedDistances, Probabilities) :-
        sum_weights(WeightedDistances, 0, TotalWeight),
        normalize_weights(WeightedDistances, TotalWeight, Probabilities).

    sum_weights([], Total, Total).
    sum_weights([Weight-_| Rest], Total0, Total) :-
        Total1 is Weight + Total0,
        sum_weights(Rest, Total1, Total).

    normalize_weights([], _, []).
    normalize_weights([Weight-Class| Rest], Total, [Class-Prob| NormRest]) :-
        Prob is Weight / Total,
        normalize_weights(Rest, Total, NormRest).

	compute_distance(euclidean, Types, Instance1, Instance2, Distance) :-
		^^mixed_feature_distance(euclidean, Types, Instance1, Instance2, Distance).
	compute_distance(manhattan, Types, Instance1, Instance2, Distance) :-
		^^mixed_feature_distance(manhattan, Types, Instance1, Instance2, Distance).
	compute_distance(cosine, Types, Instance1, Instance2, Distance) :-
		^^mixed_feature_distance(cosine, Types, Instance1, Instance2, Distance).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		classifier_data(Classifier, AttributeNames, FeatureTypes, Centroids),
		length(Centroids, CentroidCount),
		Diagnostics = [
			model(nearest_centroid_classifier),
			attributes(AttributeNames),
			feature_types(FeatureTypes),
			centroids(CentroidCount)
		].

	check_classifier(Classifier) :-
		(   classifier_data(Classifier, AttributeNames, FeatureTypes, Centroids),
			^^valid_attribute_names(AttributeNames),
			^^valid_feature_types(FeatureTypes, [numeric, categorical]),
			length(AttributeNames, FeatureCount),
			length(FeatureTypes, FeatureCount),
			valid_centroids(Centroids, FeatureTypes) ->
			true
		;   domain_error(classifier, Classifier)
		).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(nc_classifier(_AttributeNames, _FeatureTypes, _Centroids), nc_classifier('AttributeNames', 'FeatureTypes', 'Centroids')).

	valid_centroids(Centroids, FeatureTypes) :-
		valid(list(compound), Centroids),
		Centroids \== [],
		valid_centroids_(Centroids, FeatureTypes, []).

	valid_centroids_([], _FeatureTypes, _SeenClasses).
	valid_centroids_([Class-Centroid| Centroids], FeatureTypes, SeenClasses) :-
		atom(Class),
		\+ member(Class, SeenClasses),
		length(Centroid, FeatureCount),
		length(FeatureTypes, FeatureCount),
		valid_centroid_values(Centroid, FeatureTypes),
		valid_centroids_(Centroids, FeatureTypes, [Class| SeenClasses]).

	valid_centroid_values([], []).
	valid_centroid_values([Value| Values], [numeric| FeatureTypes]) :-
		valid(float, Value),
		valid_centroid_values(Values, FeatureTypes).
	valid_centroid_values([Value| Values], [categorical| FeatureTypes]) :-
		nonvar(Value),
		valid_centroid_values(Values, FeatureTypes).

	classifier_data(Classifier, AttributeNames, FeatureTypes, Centroids) :-
		Classifier =.. [_Functor, AttributeNames, FeatureTypes, Centroids].

	print_classifier(Classifier) :-
		classifier_data(Classifier, AttributeNames, FeatureTypes, Centroids),
		format('Nearest Centroid Classifier~n', []),
		format('==============================~n~n', []),
		length(Centroids, NumCentroids),
		format('Number of centroids: ~w~n~n', [NumCentroids]),
		^^print_classifier_template(Classifier),
		format('~nAttributes:~n', []),
		print_features(AttributeNames, FeatureTypes).

	print_features([], []).
	print_features([Name| Names], [Type| Types]) :-
		format('  ~w (~w)~n', [Name, Type]),
		print_features(Names, Types).

	default_option(distance_metric(euclidean)).

	valid_option(distance_metric(Metric)) :-
		valid(one_of(atom, [euclidean, manhattan, cosine]), Metric).

:- end_object.

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


:- object(knn_regression,
	imports([options, regressor_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'k-Nearest Neighbors regressor with multiple distance metrics, weighting schemes, optional feature scaling, and mixed-feature support.',
		remarks is [
			'Algorithm' - 'Learns lazily by storing encoded training rows and predicts targets as the weighted average of the k nearest neighbors.',
			'Distance metrics' - 'Supports Euclidean, Manhattan, Chebyshev, and Minkowski distance metrics over encoded numeric feature vectors.',
			'Weighting schemes' - 'Supports uniform, distance-based, and Gaussian weighting of neighbors.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are one-hot encoded from the declared dataset attribute values.',
			'Missing values' - 'Missing feature values represented using anonymous variables are encoded using explicit missing-value indicator features.',
			'Regressor representation' - 'The learned regressor is represented by default as ``knn_regressor(Encoders, Rows, Options)`` where ``Rows`` stores encoded feature vectors paired with numeric targets.'
		],
		see_also is [linear_regression, regression_tree, random_forest_regression, gradient_boosting_regression]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a regressor from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Regressor', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, take/3
	]).

	:- uses(numberlist, [
		chebyshev_distance/3, euclidean_distance/3, manhattan_distance/3
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
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
		build_encoders(Attributes, Examples, Options, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		Regressor = knn_regressor(Encoders, Rows, Options).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, Rows, Options],
		encode_instance(Encoders, Instance, Features),
		^^option(k(K), Options),
		find_k_nearest(Features, Rows, K, Neighbors, Options),
		predict_from_neighbors(Neighbors, Options, Target).

	build_encoders([], _, _, []).
	build_encoders([Attribute-Values| Rest], Examples, Options, [Encoder| Encoders]) :-
		(   Values == continuous ->
			continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;   Encoder = categorical(Attribute, Values)
		),
		build_encoders(Rest, Examples, Options, Encoders).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == true ->
			known_attribute_values(Examples, Attribute, Values),
			(   Values == [] ->
				Mean = 0.0,
				Scale = 1.0
			;   arithmetic_mean(Values, Mean),
				length(Values, Count),
				(   Count > 1 ->
					variance(Values, Variance)
				;   Variance = 0.0
				),
				(   Variance > 0.0 ->
					Scale is sqrt(Variance)
				;   Scale = 1.0
				)
			)
		;   Mean = 0.0,
			Scale = 1.0
		).

	known_attribute_values([], _, []).
	known_attribute_values([example(_Id, _Target, AttributeValues)| Examples], Attribute, Values) :-
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			Values = [Value| Rest]
		;   Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	examples_to_rows([], _, []).
	examples_to_rows([example(_Id, Target, AttributeValues)| Examples], Encoders, [Features-Target| Rows]) :-
		encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature, Missing| Features]) :-
		!,
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			normalize_continuous(Value, Mean, Scale, Feature),
			Missing = 0.0
		;   Feature = 0.0,
			Missing = 1.0
		),
		encode_instance(Encoders, AttributeValues, Features).
	encode_instance([categorical(Attribute, Values)| Encoders], AttributeValues, Features) :-
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			check_categorical_value(Attribute, Values, Value),
			one_hot_encode(Values, Value, Encoded)
		;   missing_one_hot_encode(Values, Encoded)
		),
		encode_instance(Encoders, AttributeValues, RestFeatures),
		append(Encoded, RestFeatures, Features).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale.

	check_categorical_value(Attribute, Values, Value) :-
		(   member(Value, Values) ->
			true
		;   domain_error(attribute_value(Attribute, Values), Value)
		).

	one_hot_encode(Values, Value, Encoded) :-
		one_hot_encode_(Values, Value, Encoded0),
		append(Encoded0, [0.0], Encoded).

	one_hot_encode_([], _, []).
	one_hot_encode_([Category| Categories], Value, [Feature| Features]) :-
		(   Value == Category ->
			Feature = 1.0
		;   Feature = 0.0
		),
		one_hot_encode_(Categories, Value, Features).

	missing_one_hot_encode(Values, Encoded) :-
		zero_vector_from_values(Values, Zeroes),
		append(Zeroes, [1.0], Encoded).

	zero_vector_from_values([], []).
	zero_vector_from_values([_| Values], [0.0| Zeroes]) :-
		zero_vector_from_values(Values, Zeroes).

	find_k_nearest(Features, Rows, K, Neighbors, Options) :-
		findall(Distance-Target, (member(Row, Rows), row_distance(Features, Row, Distance, Target, Options)), Distances),
		keysort(Distances, SortedDistances),
		take(K, SortedDistances, Neighbors).

	row_distance(Features, Row, Distance, Target, Options) :-
		Row = NeighborFeatures-Target,
		compute_distance(Features, NeighborFeatures, Distance, Options).

	compute_distance(Features1, Features2, Distance, Options) :-
		^^option(distance_metric(Metric), Options),
		(   Metric == euclidean ->
			euclidean_distance(Features1, Features2, Distance)
		;   Metric == manhattan ->
			manhattan_distance(Features1, Features2, Distance)
		;   Metric == chebyshev ->
			chebyshev_distance(Features1, Features2, Distance)
		;   ^^option(minkowski_power(Power), Options),
			minkowski_distance(Features1, Features2, Power, Distance)
		).

	minkowski_distance(Features1, Features2, Power, Distance) :-
		sum_powered_diffs(Features1, Features2, Power, 0.0, Sum),
		Distance is Sum ** (1.0 / Power).

	sum_powered_diffs([], [], _Power, Sum, Sum).
	sum_powered_diffs([Feature1| Features1], [Feature2| Features2], Power, Sum0, Sum) :-
		Difference is abs(Feature1 - Feature2),
		Sum1 is Sum0 + Difference ** Power,
		sum_powered_diffs(Features1, Features2, Power, Sum1, Sum).

	predict_from_neighbors(Neighbors, Options, Target) :-
		^^option(weight_scheme(Scheme), Options),
		apply_weighting(Scheme, Neighbors, WeightedNeighbors),
		weighted_average_target(WeightedNeighbors, 0.0, 0.0, Target).

	apply_weighting(uniform, Neighbors, WeightedNeighbors) :-
		uniform_weights(Neighbors, WeightedNeighbors).
	apply_weighting(distance, Neighbors, WeightedNeighbors) :-
		distance_weights(Neighbors, WeightedNeighbors).
	apply_weighting(gaussian, Neighbors, WeightedNeighbors) :-
		gaussian_weights(Neighbors, WeightedNeighbors).

	uniform_weights([], []).
	uniform_weights([_Distance-Target| Neighbors], [1.0-Target| WeightedNeighbors]) :-
		uniform_weights(Neighbors, WeightedNeighbors).

	distance_weights([], []).
	distance_weights([Distance-Target| Neighbors], [Weight-Target| WeightedNeighbors]) :-
		(   Distance =< 1.0e-12 ->
			Weight = 1.0e10
		;   Weight is 1.0 / Distance
		),
		distance_weights(Neighbors, WeightedNeighbors).

	gaussian_weights([], []).
	gaussian_weights([Distance-Target| Neighbors], [Weight-Target| WeightedNeighbors]) :-
		Sigma = 1.0,
		Weight is exp(-(Distance * Distance) / (2.0 * Sigma * Sigma)),
		gaussian_weights(Neighbors, WeightedNeighbors).

	weighted_average_target([], _WeightedSum, _TotalWeight, 0.0).
	weighted_average_target(WeightedNeighbors, WeightedSum0, TotalWeight0, Target) :-
		accumulate_weighted_targets(WeightedNeighbors, WeightedSum0, WeightedSum, TotalWeight0, TotalWeight),
		(   TotalWeight =< 1.0e-12 ->
			Target = 0.0
		;   Target is WeightedSum / TotalWeight
		).

	accumulate_weighted_targets([], WeightedSum, WeightedSum, TotalWeight, TotalWeight).
	accumulate_weighted_targets([Weight-Value| WeightedNeighbors], WeightedSum0, WeightedSum, TotalWeight0, TotalWeight) :-
		WeightedSum1 is WeightedSum0 + Weight * Value,
		TotalWeight1 is TotalWeight0 + Weight,
		accumulate_weighted_targets(WeightedNeighbors, WeightedSum1, WeightedSum, TotalWeight1, TotalWeight).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'Rows', 'Options'].

	regressor_term_template(knn_regressor(_Encoders, _Rows, _Options), knn_regressor('Encoders', 'Rows', 'Options')).

	check_regressor(Regressor) :-
		(   Regressor = knn_regressor(Encoders, Rows, Options),
			^^valid_regression_encoders(Encoders),
			^^valid_encoded_rows(Encoders, Rows),
			^^valid_regressor_options(Options) ->
			true
		;   domain_error(valid_regressor, Regressor)
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = knn_regressor(Encoders, Rows, Options),
		Clause =.. [Functor, Encoders, Rows, Options].

	print_regressor(Regressor) :-
		Regressor = knn_regressor(Encoders, Rows, Options),
		format('k-Nearest Neighbors Regressor~n', []),
		format('=============================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Options: ~w~n', [Options]),
		length(Rows, RowCount),
		format('Training rows: ~w~n~n', [RowCount]),
		format('Encoders: ~w~n', [Encoders]).

	default_option(k(3)).
	default_option(distance_metric(euclidean)).
	default_option(weight_scheme(uniform)).
	default_option(minkowski_power(3.0)).
	default_option(feature_scaling(true)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(distance_metric(Metric)) :-
		valid(one_of(atom, [euclidean, manhattan, chebyshev, minkowski]), Metric).
	valid_option(weight_scheme(Scheme)) :-
		valid(one_of(atom, [uniform, distance, gaussian]), Scheme).
	valid_option(minkowski_power(Power)) :-
		number(Power),
		Power >= 1.0.
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.

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


:- object(linear_regression,
	imports(regressor_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-02,
		comment is 'Linear regression regressor supporting continuous and mixed-feature datasets using batch gradient descent. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses batch gradient descent to minimize mean squared error. Supports optional L2 regularization and optional feature scaling for continuous attributes.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are one-hot encoded from the declared dataset attribute values.',
			'Missing values' - 'Missing feature values represented using anonymous variables are encoded using explicit missing-value indicator features.',
			'Unknown values' - 'Prediction requests containing categorical values that are not declared by the dataset raise a domain error.',
			'Regressor representation' - 'The learned regressor is represented by default as ``linear_regressor(Encoders, Bias, Weights, Diagnostics)`` where ``Encoders`` stores the feature encoding metadata, ``Bias`` stores the intercept, ``Weights`` stores one coefficient per encoded feature, and ``Diagnostics`` stores training metadata including the effective options.'
		],
		see_also is [ridge_regression, knn_regression, regression_tree, random_forest_regression, gradient_boosting_regression]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(Target),
		^^fit_linear_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, TrainingDiagnostics),
		build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics),
		Regressor = linear_regressor(Encoders, Bias, Weights, Diagnostics).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, Bias, Weights, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		dot_product(Weights, Features, Linear),
		Target is Bias + Linear.

	build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics) :-
		^^encoded_feature_count(Encoders, FeatureCount),
		append(TrainingDiagnostics, [encoded_feature_count(FeatureCount)], ExtraDiagnostics),
		^^base_regressor_diagnostics(linear_regression, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'Bias', 'Weights', 'Diagnostics'].

	regressor_term_template(linear_regressor(_Encoders, _Bias, _Weights, _Diagnostics), linear_regressor('Encoders', 'Bias', 'Weights', 'Diagnostics')).

	check_regressor(Regressor) :-
		(   Regressor = linear_regressor(Encoders, Bias, Weights, Diagnostics),
			^^valid_regression_encoders(Encoders),
			valid(float, Bias),
			^^encoded_feature_count(Encoders, FeatureCount),
			valid(list(float, FeatureCount), Weights),
			^^valid_regressor_metadata(linear_regression, Diagnostics),
			^^valid_linear_model_diagnostics(Diagnostics),
			^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount) ->
			true
		;   domain_error(regressor, Regressor)
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = linear_regressor(Encoders, Bias, Weights, Diagnostics),
		Clause =.. [Functor, Encoders, Bias, Weights, Diagnostics].

	print_regressor(Regressor) :-
		Regressor = linear_regressor(Encoders, Bias, Weights, Diagnostics),
		format('Linear Regression Regressor~n', []),
		format('===========================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Bias: ~4f~n', [Bias]),
		format('Weights: ~w coefficients~n~n', [Weights]),
		format('Encoders:~n', []),
		print_encoders(Encoders).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		!,
		format('  ~w (continuous, mean=~4f, scale=~4f, missing-indicator=yes)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).
	print_encoders([categorical(Attribute, Values)| Encoders]) :-
		format('  ~w (categorical, values=~w, missing-indicator=yes)~n', [Attribute, Values]),
		print_encoders(Encoders).

	default_option(learning_rate(0.05)).
	default_option(maximum_iterations(2000)).
	default_option(tolerance(1.0e-7)).
	default_option(l2_regularization(0.0)).
	default_option(feature_scaling(true)).

	valid_option(learning_rate(Rate)) :-
		valid(positive_float, Rate).
	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(tolerance(Tolerance)) :-
		valid(non_negative_float, Tolerance).
	valid_option(l2_regularization(Regularization)) :-
		valid(non_negative_float, Regularization).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.

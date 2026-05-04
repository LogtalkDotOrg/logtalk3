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
		date is 2026-05-04,
		comment is 'Linear regression regressor supporting continuous and mixed-feature datasets using a direct ordinary least-squares solve. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses the shared regression encoding core to build a row-oriented design matrix with an explicit intercept column, then delegates least-squares solving and rank estimation to the linear_algebra library. The intercept is always retained and encoded feature columns that are numerically dependent on the design matrix are assigned zero coefficients.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are encoded using reference-level dummy coding from the declared dataset attribute values, and encoded columns with no independent signal after accounting for the intercept and previously selected features are assigned zero coefficients.',
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
		append/3, memberchk/2
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
			valid_linear_regression_diagnostics(Diagnostics),
			^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount) ->
			true
		;   domain_error(regressor, Regressor)
		).

	valid_linear_regression_diagnostics(Diagnostics) :-
		memberchk(solver(modified_gram_schmidt_column_pivoting), Diagnostics),
		memberchk(residual_sum_of_squares(ResidualSumOfSquares), Diagnostics),
		valid(non_negative_float, ResidualSumOfSquares),
		memberchk(effective_rank(EffectiveRank), Diagnostics),
		integer(EffectiveRank),
		EffectiveRank >= 1,
		memberchk(active_feature_count(ActiveFeatureCount), Diagnostics),
		integer(ActiveFeatureCount),
		ActiveFeatureCount >= 0.

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

	default_option(feature_scaling(true)).

	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.

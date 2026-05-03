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


:- object(ridge_regression,
	imports(regressor_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-02,
		comment is 'Ridge regression regressor supporting continuous and mixed-feature datasets using a direct weighted linear-system solve with L2 regularization. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses the shared regression encoding core to solve the weighted ridge normal equations directly with partial pivoting. The intercept is left unpenalized while encoded feature columns are penalized using scale-aware weights equivalent to standardizing penalized columns before applying the L2 penalty.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are encoded using reference-level dummy coding from the declared dataset attribute values, and zero-variance encoded columns are dropped from the direct solve and assigned zero coefficients.',
			'Missing values' - 'Missing feature values represented using anonymous variables are encoded using explicit missing-value indicator features.',
			'Unknown values' - 'Prediction requests containing categorical values that are not declared by the dataset raise a domain error.',
			'Regressor representation' - 'The learned regressor is represented by default as ``ridge_regressor(Encoders, Bias, Weights, Diagnostics)`` where ``Encoders`` stores the feature encoding metadata, ``Bias`` stores the intercept, ``Weights`` stores one coefficient per encoded feature, and ``Diagnostics`` stores training metadata including the effective options.'
		],
		see_also is [linear_regression, knn_regression, regression_tree, random_forest_regression, gradient_boosting_regression]
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
		^^fit_ridge_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, TrainingDiagnostics),
		build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics),
		Regressor = ridge_regressor(Encoders, Bias, Weights, Diagnostics).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, Bias, Weights, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		dot_product(Weights, Features, Linear),
		Target is Bias + Linear.

	build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics) :-
		^^encoded_feature_count(Encoders, FeatureCount),
		append(TrainingDiagnostics, [encoded_feature_count(FeatureCount)], ExtraDiagnostics),
		^^base_regressor_diagnostics(ridge_regression, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'Bias', 'Weights', 'Diagnostics'].

	regressor_term_template(ridge_regressor(_Encoders, _Bias, _Weights, _Diagnostics), ridge_regressor('Encoders', 'Bias', 'Weights', 'Diagnostics')).

	check_regressor(Regressor) :-
		(   Regressor = ridge_regressor(Encoders, Bias, Weights, Diagnostics),
			^^valid_regression_encoders(Encoders),
			valid(float, Bias),
			^^encoded_feature_count(Encoders, FeatureCount),
			valid(list(float, FeatureCount), Weights),
			^^valid_regressor_metadata(ridge_regression, Diagnostics),
			valid_ridge_diagnostics(Diagnostics),
			^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount) ->
			true
		;   domain_error(regressor, Regressor)
		).

	valid_ridge_diagnostics(Diagnostics) :-
		memberchk(penalty_scaling(encoded_feature_standardization), Diagnostics),
		memberchk(active_feature_count(ActiveFeatureCount), Diagnostics),
		integer(ActiveFeatureCount),
		ActiveFeatureCount >= 0,
		(	memberchk(solver(pivoted_gaussian_elimination), Diagnostics),
			memberchk(linear_system_residual(Residual), Diagnostics),
			valid(non_negative_float, Residual)
		;	memberchk(solver(modified_gram_schmidt_column_pivoting), Diagnostics),
			memberchk(residual_sum_of_squares(ResidualSumOfSquares), Diagnostics),
			valid(non_negative_float, ResidualSumOfSquares),
			memberchk(effective_rank(EffectiveRank), Diagnostics),
			integer(EffectiveRank),
			EffectiveRank >= 1
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = ridge_regressor(Encoders, Bias, Weights, Diagnostics),
		Clause =.. [Functor, Encoders, Bias, Weights, Diagnostics].

	print_regressor(Regressor) :-
		Regressor = ridge_regressor(Encoders, Bias, Weights, Diagnostics),
		format('Ridge Regression Regressor~n', []),
		format('==========================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Bias: ~4f~n', [Bias]),
		format('Weights: ~w coefficients~n~n', [Weights]),
		format('Encoders: ~w~n', [Encoders]).

	default_option(regularization(0.01)).
	default_option(feature_scaling(true)).

	valid_option(regularization(Regularization)) :-
		valid(non_negative_float, Regularization).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.

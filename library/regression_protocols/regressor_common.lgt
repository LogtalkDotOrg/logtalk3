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


:- category(regressor_common,
	implements(regressor_protocol),
	extends(options)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-05-21,
		comment is 'Shared predicates for regressor learning defaults, diagnostics, validation, dataset validation, export, and pretty-print helpers.'
	]).

	:- protected(regressor_diagnostics_data/2).
	:- mode(regressor_diagnostics_data(+compound, -list(compound)), one).
	:- info(regressor_diagnostics_data/2, [
		comment is 'Default hook predicate for exposing diagnostics metadata from a regressor term. Importing implementations may override it when using a non-standard regressor representation.',
		argnames is ['Regressor', 'Diagnostics']
	]).

	:- protected(regressor_export_template/4).
	:- mode(regressor_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(regressor_export_template/4, [
		comment is 'Hook predicate that importing regressor implementations must define in order to expose the exported regressor template for a given functor.',
		argnames is ['Dataset', 'Regressor', 'Functor', 'Template']
	]).

	:- protected(regressor_term_template/2).
	:- mode(regressor_term_template(+compound, -callable), one).
	:- info(regressor_term_template/2, [
		comment is 'Hook predicate that importing regressor implementations must define in order to expose the learned regressor term template used by pretty-printing helpers.',
		argnames is ['Regressor', 'Template']
	]).

	:- protected(dataset_attributes/2).
	:- mode(dataset_attributes(+object_identifier, -list(pair)), one).
	:- info(dataset_attributes/2, [
		comment is 'Collects the dataset attribute declarations as `Attribute-Values` pairs.',
		argnames is ['Dataset', 'Attributes']
	]).

	:- protected(dataset_examples/2).
	:- mode(dataset_examples(+object_identifier, -list(compound)), one).
	:- info(dataset_examples/2, [
		comment is 'Collects the dataset examples as `example(Id, TargetValue, AttributeValues)` terms.',
		argnames is ['Dataset', 'Examples']
	]).

	:- private(check_attribute_declarations/1).
	:- mode(check_attribute_declarations(+list(pair)), one_or_error).
	:- info(check_attribute_declarations/1, [
		comment is 'Checks that dataset attribute declarations use distinct attribute names and valid value domains.',
		argnames is ['Attributes'],
		exceptions is [
			'An attribute is declared more than once or uses an invalid value domain' - domain_error(attribute_declarations, 'Attribute')
		]
	]).

	:- protected(check_examples/2).
	:- mode(check_examples(+object_identifier, +list), one_or_error).
	:- info(check_examples/2, [
		comment is 'Validates that the collected examples list is non-empty, only contains numeric targets, and only uses declared attributes without duplicate bindings.',
		argnames is ['Dataset', 'Examples'],
		exceptions is [
			'Examples is the empty list' - domain_error(non_empty_dataset, 'Dataset'),
			'An example target is not numeric' - type_error(number, 'Target'),
			'An example repeats a declared attribute binding' - domain_error(attribute_occurrences, 'Attribute'),
			'An example contains an undeclared attribute binding' - domain_error(declared_attribute, 'Attribute')
		]
	]).

	:- protected(print_regressor_template/1).
	:- mode(print_regressor_template(+compound), one).
	:- info(print_regressor_template/1, [
		comment is 'Pretty-printing helper predicate used by importing regressor implementations to show the learned regressor term template.',
		argnames is ['Regressor']
	]).

	:- protected(base_regressor_diagnostics/6).
	:- mode(base_regressor_diagnostics(+atom, +atom, +integer, +list(compound), +list(compound), -list(compound)), one).
	:- info(base_regressor_diagnostics/6, [
		comment is 'Builds common diagnostics metadata terms for a learned regressor and appends regressor-specific diagnostics terms.',
		argnames is ['Model', 'Target', 'TrainingExampleCount', 'Options', 'ExtraDiagnostics', 'Diagnostics']
	]).

	:- protected(valid_attribute_names/1).
	:- mode(valid_attribute_names(+list(atom)), zero_or_one).
	:- info(valid_attribute_names/1, [
		comment is 'True when a list of attribute names is a proper list of distinct atoms.',
		argnames is ['AttributeNames']
	]).

	:- protected(valid_attribute_declarations/1).
	:- mode(valid_attribute_declarations(+list(pair)), zero_or_one).
	:- info(valid_attribute_declarations/1, [
		comment is 'True when a list of attribute declarations is a proper list of distinct ``Attribute-Values`` pairs where values are either ``continuous`` or a valid discrete value list.',
		argnames is ['Attributes']
	]).

	:- protected(valid_discrete_values/1).
	:- mode(valid_discrete_values(+list), zero_or_one).
	:- info(valid_discrete_values/1, [
		comment is 'True when a list of categorical values is non-empty, contains only nonvar terms, and has no duplicates.',
		argnames is ['Values']
	]).

	:- protected(valid_regression_encoders/1).
	:- mode(valid_regression_encoders(+list(compound)), zero_or_one).
	:- info(valid_regression_encoders/1, [
		comment is 'True when a list of encoders only contains valid ``continuous/3`` or ``categorical/2`` encoder terms with distinct attributes.',
		argnames is ['Encoders']
	]).

	:- protected(valid_regressor_options/1).
	:- mode(valid_regressor_options(+list(compound)), zero_or_one).
	:- info(valid_regressor_options/1, [
		comment is 'True when a list of options is structurally valid for the receiving regressor implementation.',
		argnames is ['Options']
	]).

	:- protected(valid_regressor_metadata/2).
	:- mode(valid_regressor_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_regressor_metadata/2, [
		comment is 'True when diagnostics metadata contains the expected model term and records a structurally valid effective options list.',
		argnames is ['Model', 'Diagnostics']
	]).

	:- protected(valid_diagnostic_count/3).
	:- mode(valid_diagnostic_count(+atom, +list(compound), +integer), zero_or_one).
	:- info(valid_diagnostic_count/3, [
		comment is 'True when diagnostics contains a count term with the given functor and integer value.',
		argnames is ['Functor', 'Diagnostics', 'Count']
	]).

	:- protected(valid_linear_model_diagnostics/1).
	:- mode(valid_linear_model_diagnostics(+list(compound)), zero_or_one).
	:- info(valid_linear_model_diagnostics/1, [
		comment is 'True when diagnostics contains structurally valid linear-model optimization metadata terms for convergence, completed iterations, and final parameter delta.',
		argnames is ['Diagnostics']
	]).

	:- protected(valid_encoded_rows/2).
	:- mode(valid_encoded_rows(+list(compound), +list), zero_or_one).
	:- info(valid_encoded_rows/2, [
		comment is 'True when encoded training rows match the feature count induced by the encoders and carry numeric targets.',
		argnames is ['Encoders', 'Rows']
	]).

	:- protected(encoded_feature_count/2).
	:- mode(encoded_feature_count(+list(compound), -integer), one).
	:- info(encoded_feature_count/2, [
		comment is 'Counts the number of numeric features induced by a list of continuous and categorical encoders, including missing-value indicator features.',
		argnames is ['Encoders', 'FeatureCount']
	]).

	:- protected(continuous_stats/5).
	:- mode(continuous_stats(+atom, +list(compound), +list(compound), -float, -positive_float), one).
	:- info(continuous_stats/5, [
		comment is 'Computes the mean and scaling factor used to encode a continuous attribute from the training examples according to the effective feature scaling option.',
		argnames is ['Attribute', 'Examples', 'Options', 'Mean', 'Scale']
	]).

	:- private(known_attribute_values/3).
	:- mode(known_attribute_values(+list(compound), +atom, -list(number)), one).
	:- info(known_attribute_values/3, [
		comment is 'Collects the known numeric values for a continuous attribute from the training examples, skipping omitted and variable values.',
		argnames is ['Examples', 'Attribute', 'Values']
	]).

	:- protected(examples_to_rows/3).
	:- mode(examples_to_rows(+list(compound), +list(compound), -list(pair)), one).
	:- info(examples_to_rows/3, [
		comment is 'Encodes dataset examples as numeric feature-vector and target pairs using a list of encoders.',
		argnames is ['Examples', 'Encoders', 'Rows']
	]).

	:- protected(encode_instance/3).
	:- mode(encode_instance(+list(compound), +list(pair), -list(float)), one).
	:- info(encode_instance/3, [
		comment is 'Validates and encodes an attribute-value list as a numeric feature vector using a list of continuous and categorical encoders.',
		argnames is ['Encoders', 'AttributeValues', 'Features']
	]).

	:- private(check_attribute_bindings/2).
	:- mode(check_attribute_bindings(+list(atom), +list(pair)), one).
	:- info(check_attribute_bindings/2, [
		comment is 'Checks that an attribute-value list contains only declared attributes and does not repeat any declared attribute; omitted declared attributes are allowed and treated as missing values.',
		argnames is ['AttributeNames', 'AttributeValues']
	]).

	:- private(check_declared_attribute_bindings/2).
	:- mode(check_declared_attribute_bindings(+list(atom), +list(pair)), one).
	:- info(check_declared_attribute_bindings/2, [
		comment is 'Checks that no declared attribute appears more than once in an attribute-value list.',
		argnames is ['AttributeNames', 'AttributeValues']
	]).

	:- private(check_undeclared_attribute_bindings/2).
	:- mode(check_undeclared_attribute_bindings(+list(pair), +list(atom)), one).
	:- info(check_undeclared_attribute_bindings/2, [
		comment is 'Checks that every attribute occurring in an attribute-value list is declared.',
		argnames is ['AttributeValues', 'AttributeNames']
	]).

	:- private(attribute_occurrences/4).
	:- mode(attribute_occurrences(+list(pair), +atom, +integer, -integer), one).
	:- info(attribute_occurrences/4, [
		comment is 'Counts the number of times an attribute occurs in an attribute-value list.',
		argnames is ['AttributeValues', 'Attribute', 'Count0', 'Count']
	]).

	:- private(declared_attribute_names/2).
	:- mode(declared_attribute_names(+list(pair), -list(atom)), one).
	:- info(declared_attribute_names/2, [
		comment is 'Collects the declared attribute names from dataset attribute declarations.',
		argnames is ['Attributes', 'AttributeNames']
	]).

	:- private(encoder_attribute_names/2).
	:- mode(encoder_attribute_names(+list(compound), -list(atom)), one).
	:- info(encoder_attribute_names/2, [
		comment is 'Collects the declared attribute names from a list of regression encoders.',
		argnames is ['Encoders', 'AttributeNames']
	]).

	:- private(encode_instance_checked/3).
	:- mode(encode_instance_checked(+list(compound), +list(pair), -list(float)), one).
	:- info(encode_instance_checked/3, [
		comment is 'Encodes an already validated attribute-value list as a numeric feature vector using a list of continuous and categorical encoders.',
		argnames is ['Encoders', 'AttributeValues', 'Features']
	]).

	:- protected(fit_linear_model/7).
	:- mode(fit_linear_model(+object_identifier, +list(compound), -list(compound), -integer, -float, -list(float), -list(compound)), one).
	:- info(fit_linear_model/7, [
		comment is 'Builds linear-model encoders from the training dataset, encodes the examples, fits an ordinary least-squares bias plus weight vector using the linear_algebra least-squares solver, and returns solver diagnostics terms.',
		argnames is ['Dataset', 'Options', 'Encoders', 'TrainingExampleCount', 'Bias', 'Weights', 'TrainingDiagnostics']
	]).

	:- protected(fit_ridge_model/7).
	:- mode(fit_ridge_model(+object_identifier, +list(compound), -list(compound), -integer, -float, -list(float), -list(compound)), one).
	:- info(fit_ridge_model/7, [
		comment is 'Builds linear-model encoders from the training dataset, encodes the examples, fits a bias plus weight vector by solving the ridge normal equations with partial pivoting, and returns ridge-specific diagnostics terms.',
		argnames is ['Dataset', 'Options', 'Encoders', 'TrainingExampleCount', 'Bias', 'Weights', 'TrainingDiagnostics']
	]).

	:- private(valid_linear_model_convergence/1).
	:- mode(valid_linear_model_convergence(+atom), zero_or_one).
	:- info(valid_linear_model_convergence/1, [
		comment is 'True when a linear-model optimization stop reason is recognized.',
		argnames is ['Convergence']
	]).

	:- protected(valid_feature_labels/1).
	:- mode(valid_feature_labels(+list(compound)), zero_or_one).
	:- info(valid_feature_labels/1, [
		comment is 'True when a list of regression-tree feature labels only contains valid ``feature/2`` terms.',
		argnames is ['FeatureLabels']
	]).

	:- protected(valid_regression_tree/2).
	:- mode(valid_regression_tree(+compound, +positive_integer), zero_or_one).
	:- info(valid_regression_tree/2, [
		comment is 'True when a regression tree only contains valid ``leaf/1`` and ``node/5`` terms using feature indexes within bounds.',
		argnames is ['Tree', 'FeatureCount']
	]).

	:- private(normalize_continuous/4).
	:- mode(normalize_continuous(+number, +float, +positive_float, -float), one).
	:- info(normalize_continuous/4, [
		comment is 'Normalizes a continuous value using a stored mean and scale.',
		argnames is ['Value', 'Mean', 'Scale', 'Feature']
	]).

	:- private(check_categorical_value/3).
	:- mode(check_categorical_value(+atom, +list, +nonvar), one).
	:- info(check_categorical_value/3, [
		comment is 'Validates that a categorical value is declared for an attribute.',
		argnames is ['Attribute', 'Values', 'Value']
	]).

	:- private(one_hot_encode/4).
	:- mode(one_hot_encode(+list, +nonvar, -list(float), +list(float)), one).
	:- info(one_hot_encode/4, [
		comment is 'Encodes a declared categorical value using reference-level dummy coding plus a trailing missing-value indicator feature set to zero.',
		argnames is ['Values', 'Value', 'Encoded', 'Tail']
	]).

	:- private(missing_one_hot_encode/3).
	:- mode(missing_one_hot_encode(+list, -list(float), +list(float)), one).
	:- info(missing_one_hot_encode/3, [
		comment is 'Encodes a missing categorical value as reference-level dummy zeroes plus a trailing missing-value indicator feature set to one.',
		argnames is ['Values', 'Encoded', 'Tail']
	]).

	:- private(zero_vector_from_values/3).
	:- mode(zero_vector_from_values(+list, -list(float), +list(float)), one).
	:- info(zero_vector_from_values/3, [
		comment is 'Creates a zero vector with one element per supplied categorical value slot.',
		argnames is ['Values', 'Zeroes', 'Tail']
	]).

	:- private(build_linear_encoders/4).
	:- mode(build_linear_encoders(+list(pair), +list(compound), +list(compound), -list(compound)), one).
	:- info(build_linear_encoders/4, [
		comment is 'Builds the encoder list used by linear models from dataset attribute declarations, training examples, and the effective feature scaling option.',
		argnames is ['Attributes', 'Examples', 'Options', 'Encoders']
	]).

	:- private(train_linear_model/6).
	:- mode(train_linear_model(+list(pair), +integer, +list(compound), -float, -list(float), -list(compound)), one).
	:- info(train_linear_model/6, [
		comment is 'Fits an ordinary least-squares linear model bias and weight vector from encoded rows by delegating the solve and rank estimation to the linear_algebra library and returns solver diagnostics terms.',
		argnames is ['Rows', 'FeatureCount', 'Options', 'Bias', 'Weights', 'TrainingDiagnostics']
	]).

	:- private(rows_to_design_matrix/3).
	:- mode(rows_to_design_matrix(+list(pair), -list(list(float)), -list(float)), one).
	:- info(rows_to_design_matrix/3, [
		comment is 'Transforms encoded training rows into a row-oriented design matrix with a leading intercept column and a target vector.',
		argnames is ['Rows', 'DesignMatrix', 'Targets']
	]).

	:- private(residual_sum_of_squares/4).
	:- mode(residual_sum_of_squares(+list(pair), +float, +list(float), -float), one).
	:- info(residual_sum_of_squares/4, [
		comment is 'Computes the residual sum of squares for encoded training rows and a learned intercept plus weight vector.',
		argnames is ['Rows', 'Bias', 'Weights', 'ResidualSumOfSquares']
	]).

	:- private(train_ridge_model/6).
	:- mode(train_ridge_model(+list(pair), +integer, +list(compound), -float, -list(float), -list(compound)), one).
	:- info(train_ridge_model/6, [
		comment is 'Fits a ridge model bias and weight vector from encoded rows by solving a direct weighted linear system and returns diagnostics terms describing the solve.',
		argnames is ['Rows', 'FeatureCount', 'Options', 'Bias', 'Weights', 'TrainingDiagnostics']
	]).

	:- private(ridge_feature_statistics/3).
	:- mode(ridge_feature_statistics(+list(pair), -list(float), -list(atom)), one).
	:- info(ridge_feature_statistics/3, [
		comment is 'Computes per-feature ridge penalty weights and active-feature flags from encoded training rows, dropping zero-variance columns from the direct solve.',
		argnames is ['Rows', 'PenaltyWeights', 'ActiveFlags']
	]).

	:- private(accumulate_feature_statistics/5).
	:- mode(accumulate_feature_statistics(+list(pair), +list(float), +list(float), -list(float), -list(float)), one).
	:- info(accumulate_feature_statistics/5, [
		comment is 'Accumulates per-feature sums and squared sums over encoded training rows.',
		argnames is ['Rows', 'Sums0', 'SumSquares0', 'Sums', 'SumSquares']
	]).

	:- private(add_squared_vector/3).
	:- mode(add_squared_vector(+list(float), +list(float), -list(float)), one).
	:- info(add_squared_vector/3, [
		comment is 'Adds the element-wise squares of a feature vector to an accumulated vector.',
		argnames is ['Features', 'AccumulatedSquares0', 'AccumulatedSquares']
	]).

	:- private(feature_penalty_profiles/5).
	:- mode(feature_penalty_profiles(+list(float), +list(float), +integer, -list(float), -list(atom)), one).
	:- info(feature_penalty_profiles/5, [
		comment is 'Builds ridge penalty weights and active-feature flags from per-feature sums and squared sums.',
		argnames is ['Sums', 'SumSquares', 'Count', 'PenaltyWeights', 'ActiveFlags']
	]).

	:- private(active_feature_count/2).
	:- mode(active_feature_count(+list(atom), -integer), one).
	:- info(active_feature_count/2, [
		comment is 'Counts the number of active encoded features retained for the direct ridge solve.',
		argnames is ['ActiveFlags', 'Count']
	]).

	:- private(compress_rows/3).
	:- mode(compress_rows(+list(pair), +list(atom), -list(pair)), one).
	:- info(compress_rows/3, [
		comment is 'Filters encoded training rows down to the active feature subset selected for the direct ridge solve.',
		argnames is ['Rows', 'ActiveFlags', 'CompressedRows']
	]).

	:- private(compress_features/3).
	:- mode(compress_features(+list(float), +list(atom), -list(float)), one).
	:- info(compress_features/3, [
		comment is 'Filters a feature vector down to the active feature subset selected for the direct ridge solve.',
		argnames is ['Features', 'ActiveFlags', 'CompressedFeatures']
	]).

	:- private(compress_vector/3).
	:- mode(compress_vector(+list(float), +list(atom), -list(float)), one).
	:- info(compress_vector/3, [
		comment is 'Filters a numeric vector down to the active positions selected for the direct ridge solve.',
		argnames is ['Vector', 'ActiveFlags', 'CompressedVector']
	]).

	:- private(build_ridge_system/5).
	:- mode(build_ridge_system(+list(pair), +list(float), +float, -list(list(float)), -list(float)), one).
	:- info(build_ridge_system/5, [
		comment is 'Builds the regularized linear system for the intercept plus active ridge coefficients.',
		argnames is ['Rows', 'PenaltyWeights', 'Regularization', 'Matrix', 'Vector']
	]).

	:- private(accumulate_ridge_system/5).
	:- mode(accumulate_ridge_system(+list(pair), +list(list(float)), +list(float), -list(list(float)), -list(float)), one).
	:- info(accumulate_ridge_system/5, [
		comment is 'Accumulates the unregularized normal-equation matrix and target vector for a set of active encoded rows.',
		argnames is ['Rows', 'Matrix0', 'Vector0', 'Matrix', 'Vector']
	]).

	:- private(add_outer_product/4).
	:- mode(add_outer_product(+list(float), +list(float), +list(list(float)), -list(list(float))), one).
	:- info(add_outer_product/4, [
		comment is 'Adds the outer product of two vectors to an accumulated matrix.',
		argnames is ['Vector1', 'Vector2', 'Matrix0', 'Matrix']
	]).

	:- private(regularize_ridge_matrix/4).
	:- mode(regularize_ridge_matrix(+list(list(float)), +list(float), +float, -list(list(float))), one).
	:- info(regularize_ridge_matrix/4, [
		comment is 'Adds the ridge penalty weights to the feature-feature diagonal block of a linear system matrix.',
		argnames is ['Matrix0', 'PenaltyWeights', 'Regularization', 'Matrix']
	]).

	:- private(solve_linear_system/4).
	:- mode(solve_linear_system(+list(list(float)), +list(float), -list(float), -atom), one_or_error).
	:- info(solve_linear_system/4, [
		comment is 'Solves a square linear system using partial pivoting Gaussian elimination and returns the solver name.',
		argnames is ['Matrix', 'Vector', 'Solution', 'Solver']
	]).

	:- private(augment_rows/3).
	:- mode(augment_rows(+list(list(float)), +list(float), -list(compound)), one).
	:- info(augment_rows/3, [
		comment is 'Pairs each matrix row with its corresponding right-hand-side value for elimination.',
		argnames is ['Matrix', 'Vector', 'Rows']
	]).

	:- private(triangularize/2).
	:- mode(triangularize(+list(compound), -list(compound)), one_or_error).
	:- info(triangularize/2, [
		comment is 'Transforms an augmented matrix into upper-triangular form using partial pivoting.',
		argnames is ['Rows0', 'UpperRows']
	]).

	:- private(select_pivot_row/3).
	:- mode(select_pivot_row(+list(compound), -compound, -list(compound)), one).
	:- info(select_pivot_row/3, [
		comment is 'Selects the pivot row with the largest leading magnitude and returns the remaining rows.',
		argnames is ['Rows0', 'PivotRow', 'RemainingRows']
	]).

	:- private(leading_magnitude/2).
	:- mode(leading_magnitude(+compound, -float), one).
	:- info(leading_magnitude/2, [
		comment is 'Returns the absolute leading coefficient magnitude of an augmented row.',
		argnames is ['Row', 'Magnitude']
	]).

	:- private(ensure_non_zero/1).
	:- mode(ensure_non_zero(+float), one_or_error).
	:- info(ensure_non_zero/1, [
		comment is 'Checks that a pivot coefficient is numerically non-zero.',
		argnames is ['Value'],
		exceptions is [
			'A pivot coefficient is numerically zero' - evaluation_error(zero_divisor)
		]
	]).

	:- private(eliminate_rows/5).
	:- mode(eliminate_rows(+float, +list(float), +float, +list(compound), -list(compound)), one).
	:- info(eliminate_rows/5, [
		comment is 'Eliminates the leading coefficient from remaining augmented rows using a pivot row.',
		argnames is ['Pivot', 'PivotTail', 'PivotValue', 'Rows0', 'Rows']
	]).

	:- private(scaled_row_difference/4).
	:- mode(scaled_row_difference(+list(float), +list(float), +float, -list(float)), one).
	:- info(scaled_row_difference/4, [
		comment is 'Subtracts a scaled pivot tail from another row tail.',
		argnames is ['PivotTail', 'RowTail', 'Factor', 'Difference']
	]).

	:- private(back_substitution/2).
	:- mode(back_substitution(+list(compound), -list(float)), one_or_error).
	:- info(back_substitution/2, [
		comment is 'Performs back-substitution on an upper-triangular augmented matrix.',
		argnames is ['UpperRows', 'Solution']
	]).

	:- private(maximum_linear_system_residual/4).
	:- mode(maximum_linear_system_residual(+list(list(float)), +list(float), +list(float), -float), one).
	:- info(maximum_linear_system_residual/4, [
		comment is 'Computes the maximum absolute residual of a solved linear system.',
		argnames is ['Matrix', 'Vector', 'Solution', 'MaximumResidual']
	]).

	:- private(expand_weights/3).
	:- mode(expand_weights(+list(atom), +list(float), -list(float)), one).
	:- info(expand_weights/3, [
		comment is 'Expands active ridge coefficients back to the full encoded feature vector, inserting zeroes for dropped zero-variance features.',
		argnames is ['ActiveFlags', 'ActiveWeights', 'Weights']
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		append/3, length/2, last/2, member/2, memberchk/2
	]).

	:- uses(linear_algebra, [
		add_scaled_vector/4, least_squares/3, matrix_rank/2, new_matrix/4, new_vector/3, new_vector_like/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor) :-
		::learn(Dataset, Regressor, []).

	check_regressor(Regressor) :-
		(	var(Regressor) ->
			instantiation_error
		;	::regressor_term_template(Regressor, _Template),
			::regressor_diagnostics_data(Regressor, _Diagnostics) ->
			true
		;	domain_error(regressor, Regressor)
		).

	valid_regressor(Regressor) :-
		catch(::check_regressor(Regressor), _Error, fail).

	diagnostics(Regressor, Diagnostics) :-
		::regressor_diagnostics_data(Regressor, Diagnostics).

	diagnostic(Regressor, Diagnostic) :-
		::regressor_diagnostics_data(Regressor, Diagnostics),
		member(Diagnostic, Diagnostics).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes0
		),
		check_attribute_declarations(Attributes0),
		Attributes = Attributes0.

	dataset_examples(Dataset, Examples) :-
		findall(
			example(Id, TargetValue, AttributeValues),
			Dataset::example(Id, TargetValue, AttributeValues),
			Examples
		).

	check_examples(Dataset, Examples) :-
		(	Examples == [] ->
			domain_error(non_empty_dataset, Dataset)
		;	true
		),
		::dataset_attributes(Dataset, Attributes),
		declared_attribute_names(Attributes, AttributeNames),
		check_targets(Examples, AttributeNames).

	check_targets([], _AttributeNames).
	check_targets([example(_Id, Target, AttributeValues)| Examples], AttributeNames) :-
		(	number(Target) ->
			true
		;	type_error(number, Target)
		),
		check_attribute_bindings(AttributeNames, AttributeValues),
		check_targets(Examples, AttributeNames).

	print_regressor_template(Regressor) :-
		::regressor_term_template(Regressor, Template),
		format('Template: ~w~n', [Template]).

	regressor_diagnostics_data(Regressor, Diagnostics) :-
		Regressor =.. [_| Arguments],
		last(Arguments, Diagnostics).

	base_regressor_diagnostics(Model, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics) :-
		Diagnostics = [
			model(Model),
			target(Target),
			training_example_count(TrainingExampleCount),
			options(Options)
		| ExtraDiagnostics
		].

	valid_attribute_names(AttributeNames) :-
		valid(list(atom), AttributeNames),
		valid_distinct_terms(AttributeNames).

	valid_attribute_declarations(Attributes) :-
		valid(list(pair), Attributes),
		valid_attribute_declarations_(Attributes, []).

	valid_discrete_values(Values) :-
		valid(list(nonvar), Values),
		Values \== [],
		valid_distinct_terms(Values).

	valid_regression_encoders(Encoders) :-
		valid(list(compound), Encoders),
		valid_regression_encoders_(Encoders, []).

	valid_regressor_options(Options) :-
		valid(list(compound), Options),
		catch(::check_options(Options), _Error, fail).

	valid_regressor_metadata(Model, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail).

	valid_diagnostic_count(Functor, Diagnostics, Count) :-
		integer(Count),
		Diagnostic =.. [Functor, Value],
		memberchk(Diagnostic, Diagnostics),
		integer(Value),
		Value =:= Count.

	valid_linear_model_diagnostics(Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		valid_linear_model_convergence(Convergence),
		memberchk(iterations(Iterations), Diagnostics),
		integer(Iterations),
		Iterations >= 1,
		memberchk(final_delta(FinalDelta), Diagnostics),
		valid(non_negative_float, FinalDelta).

	valid_linear_model_convergence(tolerance).
	valid_linear_model_convergence(maximum_iterations_exhausted).

	valid_encoded_rows(Encoders, Rows) :-
		valid(list(compound), Rows),
		Rows \== [],
		encoded_feature_count(Encoders, FeatureCount),
		valid_encoded_rows_(Rows, FeatureCount).

	valid_feature_labels(FeatureLabels) :-
		valid(list(compound), FeatureLabels),
		valid_feature_labels_(FeatureLabels).

	valid_regression_tree(Tree, FeatureCount) :-
		integer(FeatureCount),
		FeatureCount > 0,
		valid_regression_tree_(Tree, FeatureCount).

	regressor_options(Regressor, Options) :-
		diagnostics(Regressor, Diagnostics),
		memberchk(options(Options), Diagnostics).

	export_to_file(Dataset, Regressor, Functor, File) :-
		::export_to_clauses(Dataset, Regressor, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, Functor, Regressor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Dataset, Functor, Regressor, Stream) :-
		::regressor_export_template(Dataset, Regressor, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported regressor predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		Dataset::target(Target),
		format(Stream, '% target: ~q~n', [Target]),
		::dataset_attributes(Dataset, Attributes),
		format(Stream, '% attributes: ~q~n', [Attributes]),
		(	::diagnostics(Regressor, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;	true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	valid_distinct_terms([]).
	valid_distinct_terms([Term| Terms]) :-
		\+ member(Term, Terms),
		valid_distinct_terms(Terms).

	check_attribute_declarations([]).
	check_attribute_declarations([Attribute-Values| Attributes]) :-
		(	atom(Attribute),
			\+ member(Attribute-_, Attributes),
			(	Values == continuous
			;	valid_discrete_values(Values)
			) ->
			true
		;	domain_error(attribute_declarations, Attribute)
		),
		check_attribute_declarations(Attributes).

	valid_attribute_declarations_([], _SeenAttributes).
	valid_attribute_declarations_([Attribute-Values| Attributes], SeenAttributes) :-
		atom(Attribute),
		\+ member(Attribute, SeenAttributes),
		(	Values == continuous
		;	valid_discrete_values(Values)
		),
		valid_attribute_declarations_(Attributes, [Attribute| SeenAttributes]).

	valid_regression_encoders_([], _SeenAttributes).
	valid_regression_encoders_([continuous(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		!,
		atom(Attribute),
		valid(float, Mean),
		valid(positive_float, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_regression_encoders_(Encoders, [Attribute| SeenAttributes]).
	valid_regression_encoders_([categorical(Attribute, Values)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid_discrete_values(Values),
		\+ member(Attribute, SeenAttributes),
		valid_regression_encoders_(Encoders, [Attribute| SeenAttributes]).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		::option(feature_scaling(FeatureScaling), Options),
		(	FeatureScaling == true ->
			::known_attribute_values(Examples, Attribute, Values),
			(	Values == [] ->
				Mean = 0.0,
				Scale = 1.0
			;	arithmetic_mean(Values, Mean),
				length(Values, Count),
				(	Count > 1 ->
					variance(Values, Variance)
				;	Variance = 0.0
				),
				(	Variance > 0.0 ->
					Scale is sqrt(Variance)
				;	Scale = 1.0
				)
			)
		;	Mean = 0.0,
			Scale = 1.0
		).

	known_attribute_values([], _, []).
	known_attribute_values([example(_Id, _Target, AttributeValues)| Examples], Attribute, Values) :-
		(	memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	examples_to_rows([], _, []).
	examples_to_rows([example(_Id, Target, AttributeValues)| Examples], Encoders, [Features-Target| Rows]) :-
		encode_instance_checked(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance(Encoders, AttributeValues, Features) :-
		encoder_attribute_names(Encoders, AttributeNames),
		check_attribute_bindings(AttributeNames, AttributeValues),
		encode_instance_checked(Encoders, AttributeValues, Features).

	encode_instance_checked(Encoders, AttributeValues, Features) :-
		encode_instance_checked_(Encoders, AttributeValues, Features, []).

	encode_instance_checked_([], _, Features, Features).
	encode_instance_checked_([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature, Missing| Features], Tail) :-
		!,
		(	memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			normalize_continuous(Value, Mean, Scale, Feature),
			Missing = 0.0
		;	Feature = 0.0,
			Missing = 1.0
		),
		encode_instance_checked_(Encoders, AttributeValues, Features, Tail).
	encode_instance_checked_([categorical(Attribute, Values)| Encoders], AttributeValues, Features, Tail) :-
		(	memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			check_categorical_value(Attribute, Values, Value),
			one_hot_encode(Values, Value, Features, RestFeatures)
		;	missing_one_hot_encode(Values, Features, RestFeatures)
		),
		encode_instance_checked_(Encoders, AttributeValues, RestFeatures, Tail).

	check_attribute_bindings(AttributeNames, AttributeValues) :-
		check_declared_attribute_bindings(AttributeNames, AttributeValues),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_declared_attribute_bindings([], _AttributeValues).
	check_declared_attribute_bindings([Attribute| Attributes], AttributeValues) :-
		attribute_occurrences(AttributeValues, Attribute, 0, Count),
		(	Count =< 1 ->
			true
		;	domain_error(attribute_occurrences, Attribute)
		),
		check_declared_attribute_bindings(Attributes, AttributeValues).

	check_undeclared_attribute_bindings([], _AttributeNames).
	check_undeclared_attribute_bindings([Attribute-_Value| AttributeValues], AttributeNames) :-
		(	member(Attribute, AttributeNames) ->
			true
		;	domain_error(declared_attribute, Attribute)
		),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	attribute_occurrences([], _Attribute, Count, Count).
	attribute_occurrences([Attribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		attribute_occurrences(AttributeValues, Attribute, Count1, Count).
	attribute_occurrences([_OtherAttribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		attribute_occurrences(AttributeValues, Attribute, Count0, Count).

	declared_attribute_names([], []).
	declared_attribute_names([Attribute-_Values| Attributes], [Attribute| AttributeNames]) :-
		declared_attribute_names(Attributes, AttributeNames).

	encoder_attribute_names([], []).
	encoder_attribute_names([continuous(Attribute, _Mean, _Scale)| Encoders], [Attribute| AttributeNames]) :-
		!,
		encoder_attribute_names(Encoders, AttributeNames).
	encoder_attribute_names([categorical(Attribute, _Values)| Encoders], [Attribute| AttributeNames]) :-
		encoder_attribute_names(Encoders, AttributeNames).

	fit_linear_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, TrainingDiagnostics) :-
		::dataset_attributes(Dataset, Attributes),
		::dataset_examples(Dataset, Examples),
		::check_examples(Dataset, Examples),
		build_linear_encoders(Attributes, Examples, Options, Encoders),
		::examples_to_rows(Examples, Encoders, Rows),
		::encoded_feature_count(Encoders, FeatureCount),
		train_linear_model(Rows, FeatureCount, Options, Bias, Weights, TrainingDiagnostics),
		length(Examples, TrainingExampleCount).

	fit_ridge_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, TrainingDiagnostics) :-
		::dataset_attributes(Dataset, Attributes),
		::dataset_examples(Dataset, Examples),
		::check_examples(Dataset, Examples),
		build_linear_encoders(Attributes, Examples, Options, Encoders),
		::examples_to_rows(Examples, Encoders, Rows),
		::encoded_feature_count(Encoders, FeatureCount),
		train_ridge_model(Rows, FeatureCount, Options, Bias, Weights, TrainingDiagnostics),
		length(Examples, TrainingExampleCount).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(	number(Value) ->
			true
		;	type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale.

	check_categorical_value(Attribute, Values, Value) :-
		(	member(Value, Values) ->
			true
		;	domain_error(attribute_value(Attribute, Values), Value)
		).

	one_hot_encode([Baseline| Values], Value, Encoded, Tail) :-
		(	Value == Baseline ->
			zero_vector_from_values(Values, Encoded, [0.0| Tail])
		;	one_hot_encode_(Values, Value, Encoded, [0.0| Tail])
		).

	one_hot_encode_([], _Value, Tail, Tail).
	one_hot_encode_([Category| Categories], Value, [Feature| Features], Tail) :-
		(	Value == Category ->
			Feature = 1.0,
			zero_vector_from_values(Categories, Features, Tail)
		;	Feature = 0.0,
			one_hot_encode_(Categories, Value, Features, Tail)
		).

	missing_one_hot_encode([_Baseline| Values], Encoded, Tail) :-
		zero_vector_from_values(Values, Encoded, [1.0| Tail]).

	zero_vector_from_values([], Tail, Tail).
	zero_vector_from_values([_| Values], [0.0| Zeroes], Tail) :-
		zero_vector_from_values(Values, Zeroes, Tail).

	encoded_feature_count([], 0).
	encoded_feature_count([continuous(_, _, _)| Encoders], Count) :-
		!,
		encoded_feature_count(Encoders, RestCount),
		Count is RestCount + 2.
	encoded_feature_count([categorical(_, Values)| Encoders], Count) :-
		length(Values, ValueCount),
		encoded_feature_count(Encoders, RestCount),
		Count is RestCount + ValueCount.

	build_linear_encoders([], _Examples, _Options, []).
	build_linear_encoders([Attribute-Values| Rest], Examples, Options, [Encoder| Encoders]) :-
		(	Values == continuous ->
			::continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;	Encoder = categorical(Attribute, Values)
		),
		build_linear_encoders(Rest, Examples, Options, Encoders).

	train_linear_model(Rows, _FeatureCount, _Options, Bias, Weights, TrainingDiagnostics) :-
		rows_to_design_matrix(Rows, DesignMatrix, Targets),
		least_squares(DesignMatrix, Targets, [Bias| Weights]),
		residual_sum_of_squares(Rows, Bias, Weights, ResidualSumOfSquares),
		matrix_rank(DesignMatrix, EffectiveRank),
		ActiveFeatureCount is EffectiveRank - 1,
		TrainingDiagnostics = [solver(modified_gram_schmidt_column_pivoting), residual_sum_of_squares(ResidualSumOfSquares), effective_rank(EffectiveRank), active_feature_count(ActiveFeatureCount)].

	rows_to_design_matrix([], [], []).
	rows_to_design_matrix([Features-Target| Rows], [[1.0| Features]| DesignMatrix], [Target| Targets]) :-
		rows_to_design_matrix(Rows, DesignMatrix, Targets).

	residual_sum_of_squares(Rows, Bias, Weights, ResidualSumOfSquares) :-
		residual_sum_of_squares(Rows, Bias, Weights, 0.0, ResidualSumOfSquares).

	residual_sum_of_squares([], _Bias, _Weights, ResidualSumOfSquares, ResidualSumOfSquares).
	residual_sum_of_squares([Features-Target| Rows], Bias, Weights, ResidualSumOfSquares0, ResidualSumOfSquares) :-
		dot_product(Weights, Features, Linear),
		Residual is Bias + Linear - Target,
		ResidualSumOfSquares1 is ResidualSumOfSquares0 + Residual * Residual,
		residual_sum_of_squares(Rows, Bias, Weights, ResidualSumOfSquares1, ResidualSumOfSquares).

	train_ridge_model(Rows, FeatureCount, Options, Bias, Weights, TrainingDiagnostics) :-
		::option(regularization(Regularization), Options),
		(	Regularization =< 1.0e-12 ->
			train_linear_model(Rows, FeatureCount, Options, Bias, Weights, LinearDiagnostics),
			append(LinearDiagnostics, [penalty_scaling(encoded_feature_standardization)], TrainingDiagnostics)
		;	ridge_feature_statistics(Rows, PenaltyWeights, ActiveFlags),
			active_feature_count(ActiveFlags, ActiveFeatureCount),
			compress_rows(Rows, ActiveFlags, ActiveRows),
			compress_vector(PenaltyWeights, ActiveFlags, ActivePenaltyWeights),
			build_ridge_system(ActiveRows, ActivePenaltyWeights, Regularization, Matrix, Vector),
			solve_linear_system(Matrix, Vector, [Bias| ActiveWeights], Solver),
			expand_weights(ActiveFlags, ActiveWeights, Weights0),
			(	FeatureCount =:= 0 ->
				Weights = []
			;	Weights = Weights0
			),
			maximum_linear_system_residual(Matrix, Vector, [Bias| ActiveWeights], Residual),
			TrainingDiagnostics = [solver(Solver), linear_system_residual(Residual), active_feature_count(ActiveFeatureCount), penalty_scaling(encoded_feature_standardization)]
		).

	ridge_feature_statistics([Features-_Target| Rows], PenaltyWeights, ActiveFlags) :-
		new_vector_like(Features, Zeroes),
		accumulate_feature_statistics([Features-_Target| Rows], Zeroes, Zeroes, Sums, SumSquares),
		length([Features-_Target| Rows], Count),
		feature_penalty_profiles(Sums, SumSquares, Count, PenaltyWeights, ActiveFlags).

	accumulate_feature_statistics([], Sums, SumSquares, Sums, SumSquares).
	accumulate_feature_statistics([Features-_Target| Rows], Sums0, SumSquares0, Sums, SumSquares) :-
		add_scaled_vector(Features, 1.0, Sums0, Sums1),
		add_squared_vector(Features, SumSquares0, SumSquares1),
		accumulate_feature_statistics(Rows, Sums1, SumSquares1, Sums, SumSquares).

	add_squared_vector([], [], []).
	add_squared_vector([Feature| Features], [Square0| Squares0], [Square| Squares]) :-
		Square is Square0 + Feature * Feature,
		add_squared_vector(Features, Squares0, Squares).

	feature_penalty_profiles([], [], _Count, [], []).
	feature_penalty_profiles([Sum| Sums], [SumSquares| Squares], Count, [PenaltyWeight| PenaltyWeights], [Active| ActiveFlags]) :-
		Mean is Sum / Count,
		Variance0 is SumSquares / Count - Mean * Mean,
		(	Variance0 > 1.0e-12 ->
			PenaltyWeight = Variance0,
			Active = active
		;	PenaltyWeight = 1.0,
			Active = inactive
		),
		feature_penalty_profiles(Sums, Squares, Count, PenaltyWeights, ActiveFlags).

	active_feature_count([], 0).
	active_feature_count([active| ActiveFlags], Count) :-
		!,
		active_feature_count(ActiveFlags, RestCount),
		Count is RestCount + 1.
	active_feature_count([inactive| ActiveFlags], Count) :-
		active_feature_count(ActiveFlags, Count).

	compress_rows([], _ActiveFlags, []).
	compress_rows([Features-Target| Rows], ActiveFlags, [CompressedFeatures-Target| CompressedRows]) :-
		compress_features(Features, ActiveFlags, CompressedFeatures),
		compress_rows(Rows, ActiveFlags, CompressedRows).

	compress_features([], [], []).
	compress_features([Feature| Features], [active| ActiveFlags], [Feature| Compressed]) :-
		!,
		compress_features(Features, ActiveFlags, Compressed).
	compress_features([_Feature| Features], [inactive| ActiveFlags], Compressed) :-
		compress_features(Features, ActiveFlags, Compressed).

	compress_vector([], [], []).
	compress_vector([Value| Values], [active| ActiveFlags], [Value| Compressed]) :-
		!,
		compress_vector(Values, ActiveFlags, Compressed).
	compress_vector([_Value| Values], [inactive| ActiveFlags], Compressed) :-
		compress_vector(Values, ActiveFlags, Compressed).

	build_ridge_system(Rows, PenaltyWeights, Regularization, Matrix, Vector) :-
		length(PenaltyWeights, ActiveFeatureCount),
		Size is ActiveFeatureCount + 1,
		new_matrix(Size, Size, 0.0, Matrix0),
		new_vector(Size, 0.0, Vector0),
		accumulate_ridge_system(Rows, Matrix0, Vector0, Matrix1, Vector),
		regularize_ridge_matrix(Matrix1, PenaltyWeights, Regularization, Matrix).

	accumulate_ridge_system([], Matrix, Vector, Matrix, Vector).
	accumulate_ridge_system([Features-Target| Rows], Matrix0, Vector0, Matrix, Vector) :-
		add_outer_product([1.0| Features], [1.0| Features], Matrix0, Matrix1),
		add_scaled_vector([1.0| Features], Target, Vector0, Vector1),
		accumulate_ridge_system(Rows, Matrix1, Vector1, Matrix, Vector).

	add_outer_product([], _Vector2, [], []).
	add_outer_product([Value| Values], Vector2, [Row0| Rows0], [Row| Rows]) :-
		add_scaled_vector(Vector2, Value, Row0, Row),
		add_outer_product(Values, Vector2, Rows0, Rows).

	regularize_ridge_matrix(Matrix0, PenaltyWeights, Regularization, Matrix) :-
		regularize_ridge_matrix(Matrix0, PenaltyWeights, Regularization, 1, Matrix).

	regularize_ridge_matrix([], _PenaltyWeights, _Regularization, _RowIndex, []).
	regularize_ridge_matrix([Row0| Rows0], PenaltyWeights, Regularization, RowIndex, [Row| Rows]) :-
		regularize_ridge_row(Row0, PenaltyWeights, Regularization, RowIndex, 1, Row),
		NextRowIndex is RowIndex + 1,
		regularize_ridge_matrix(Rows0, PenaltyWeights, Regularization, NextRowIndex, Rows).

	regularize_ridge_row([], _PenaltyWeights, _Regularization, _RowIndex, _ColumnIndex, []).
	regularize_ridge_row([Value| Values], PenaltyWeights, Regularization, RowIndex, ColumnIndex, [RegularizedValue| RegularizedValues]) :-
		(	RowIndex > 1,
			ColumnIndex =:= RowIndex ->
			PenaltyIndex is RowIndex - 1,
			penalty_weight(PenaltyWeights, PenaltyIndex, PenaltyWeight),
			RegularizedValue is Value + Regularization * PenaltyWeight
		;	RegularizedValue = Value
		),
		NextColumnIndex is ColumnIndex + 1,
		regularize_ridge_row(Values, PenaltyWeights, Regularization, RowIndex, NextColumnIndex, RegularizedValues).

	penalty_weight([PenaltyWeight| _PenaltyWeights], 1, PenaltyWeight) :-
		!.
	penalty_weight([_PenaltyWeight| PenaltyWeights], Index, PenaltyWeight) :-
		NextIndex is Index - 1,
		penalty_weight(PenaltyWeights, NextIndex, PenaltyWeight).

	solve_linear_system(Matrix, Vector, Solution, pivoted_gaussian_elimination) :-
		augment_rows(Matrix, Vector, Rows),
		triangularize(Rows, UpperRows),
		back_substitution(UpperRows, Solution).

	augment_rows([], [], []).
	augment_rows([Row| Matrix], [Value| Vector], [row(Row, Value)| Rows]) :-
		augment_rows(Matrix, Vector, Rows).

	triangularize([], []).
	triangularize([Row0| Rows0], [PivotRow| UpperRows]) :-
		select_pivot_row([Row0| Rows0], PivotRow, Rows),
		PivotRow = row([Pivot| PivotTail], PivotValue),
		ensure_non_zero(Pivot),
		eliminate_rows(Pivot, PivotTail, PivotValue, Rows, ReducedRows),
		triangularize(ReducedRows, UpperRows).

	select_pivot_row([Row| Rows], PivotRow, RemainingRows) :-
		select_pivot_row(Rows, Row, [], PivotRow, RemainingRows).

	select_pivot_row([], PivotRow, RemainingRows, PivotRow, RemainingRows).
	select_pivot_row([Row| Rows], Candidate0, RemainingRows0, PivotRow, RemainingRows) :-
		leading_magnitude(Row, Magnitude),
		leading_magnitude(Candidate0, CandidateMagnitude),
		(	Magnitude > CandidateMagnitude ->
			Candidate = Row,
			RemainingRows1 = [Candidate0| RemainingRows0]
		;	Candidate = Candidate0,
			RemainingRows1 = [Row| RemainingRows0]
		),
		select_pivot_row(Rows, Candidate, RemainingRows1, PivotRow, RemainingRows).

	leading_magnitude(row([Leading| _Tail], _Value), Magnitude) :-
		Magnitude is abs(Leading).

	ensure_non_zero(Value) :-
		(	abs(Value) > 1.0e-12 ->
			true
		;	evaluation_error(zero_divisor)
		).

	eliminate_rows(_Pivot, _PivotTail, _PivotValue, [], []) :-
		!.
	eliminate_rows(Pivot, PivotTail, PivotValue, [row([Leading| Tail], Value)| Rows], [row(NewTail, NewValue)| ReducedRows]) :-
		Factor is Leading / Pivot,
		scaled_row_difference(PivotTail, Tail, Factor, NewTail),
		NewValue is Value - Factor * PivotValue,
		eliminate_rows(Pivot, PivotTail, PivotValue, Rows, ReducedRows).

	scaled_row_difference([], [], _Factor, []).
	scaled_row_difference([PivotCoefficient| PivotTail], [Coefficient| Tail], Factor, [NewCoefficient| NewTail]) :-
		NewCoefficient is Coefficient - Factor * PivotCoefficient,
		scaled_row_difference(PivotTail, Tail, Factor, NewTail).

	back_substitution([], []).
	back_substitution([row([Diagonal], Value)], [Solution]) :-
		!,
		ensure_non_zero(Diagonal),
		Solution is Value / Diagonal.
	back_substitution([row([Diagonal| Tail], Value)| Rows], [Solution| KnownSolutions]) :-
		back_substitution(Rows, KnownSolutions),
		ensure_non_zero(Diagonal),
		dot_product(Tail, KnownSolutions, Correction),
		Solution is (Value - Correction) / Diagonal.

	maximum_linear_system_residual(Matrix, Vector, Solution, MaximumResidual) :-
		maximum_linear_system_residual(Matrix, Vector, Solution, 0.0, MaximumResidual).

	maximum_linear_system_residual([], [], _Solution, MaximumResidual, MaximumResidual).
	maximum_linear_system_residual([Row| Matrix], [Value| Vector], Solution, MaximumResidual0, MaximumResidual) :-
		dot_product(Row, Solution, RowValue),
		Residual is abs(RowValue - Value),
		(	Residual > MaximumResidual0 ->
			MaximumResidual1 = Residual
		;	MaximumResidual1 = MaximumResidual0
		),
		maximum_linear_system_residual(Matrix, Vector, Solution, MaximumResidual1, MaximumResidual).

	expand_weights([], [], []).
	expand_weights([active| ActiveFlags], [Weight| ActiveWeights], [Weight| Weights]) :-
		!,
		expand_weights(ActiveFlags, ActiveWeights, Weights).
	expand_weights([inactive| ActiveFlags], ActiveWeights, [0.0| Weights]) :-
		expand_weights(ActiveFlags, ActiveWeights, Weights).

	valid_encoded_rows_([], _FeatureCount).
	valid_encoded_rows_([Features-Target| Rows], FeatureCount) :-
		valid(list(float, FeatureCount), Features),
		number(Target),
		valid_encoded_rows_(Rows, FeatureCount).

	valid_feature_labels_([]).
	valid_feature_labels_([feature(Attribute, value)| FeatureLabels]) :-
		!,
		atom(Attribute),
		valid_feature_labels_(FeatureLabels).
	valid_feature_labels_([feature(Attribute, missing)| FeatureLabels]) :-
		!,
		atom(Attribute),
		valid_feature_labels_(FeatureLabels).
	valid_feature_labels_([feature(Attribute, category(Value))| FeatureLabels]) :-
		atom(Attribute),
		nonvar(Value),
		valid_feature_labels_(FeatureLabels).

	valid_regression_tree_(leaf(Target), _FeatureCount) :-
		number(Target).
	valid_regression_tree_(node(Index, Threshold, Fallback, LeftTree, RightTree), FeatureCount) :-
		integer(Index),
		Index >= 1,
		Index =< FeatureCount,
		number(Threshold),
		number(Fallback),
		valid_regression_tree_(LeftTree, FeatureCount),
		valid_regression_tree_(RightTree, FeatureCount).

:- end_category.

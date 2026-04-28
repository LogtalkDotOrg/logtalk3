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


:- object(kernel_pca,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Kernel Principal Component Analysis reducer for continuous datasets using a portable power-iteration eigensolver over centered kernel Gram matrices.',
		remarks is [
			'Algorithm' - 'Centers the training data, optionally standardizes continuous attributes, builds a centered kernel Gram matrix, and extracts deterministic principal directions in sample space using portable power iteration with deflation.',
			'Feature handling' - 'Supports continuous attributes only. Missing or nonnumeric values are rejected.',
			'Shortfall handling' - 'Supports a `shortfall_policy/1` option for choosing whether a component-extraction shortfall raises an error or returns a truncated reducer with explicit diagnostics.',
			'Kernels' - 'Supports linear, polynomial with non-negative offset, and radial basis function kernels through the `kernel/1` option.',
			'Dimension reducer representation' - 'The learned reducer is represented by default as ``kernel_pca_reducer(Encoders, TrainingRows, RowMeans, TotalMean, Components, ExplainedVariances, Diagnostics)`` where ``Encoders`` stores attribute centering/scaling metadata, ``TrainingRows`` stores the encoded training rows, ``RowMeans`` and ``TotalMean`` store the kernel-centering statistics, ``Components`` stores the normalized dual projection vectors, and ``Diagnostics`` records the learned model metadata, effective options, and any truncate-mode shortfall details.'
		],
		see_also is [pca, random_projection]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		arithmetic_mean/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, DimensionReducer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		length(Rows, SampleCount),
		^^option(n_components(RequestedComponents), Options),
		MaxComponentCount is SampleCount - 1,
		^^check_component_count(RequestedComponents, MaxComponentCount, ComponentCount),
		^^option(kernel(Kernel), Options),
		build_kernel_matrix(Rows, Kernel, KernelMatrix),
		center_kernel_matrix(KernelMatrix, CenteredKernelMatrix, RowMeans, TotalMean),
		^^extract_components(CenteredKernelMatrix, ComponentCount, Options, DualEigenvectors, ExplainedVariances),
		handle_component_shortfall(ComponentCount, Options, CenteredKernelMatrix, DualEigenvectors, ExplainedVariances, ExtractionDiagnostics),
		normalize_dual_components(DualEigenvectors, ExplainedVariances, Components),
		build_diagnostics(AttributeNames, SampleCount, Components, ExplainedVariances, Options, ExtractionDiagnostics, Diagnostics),
		DimensionReducer = kernel_pca_reducer(Encoders, Rows, RowMeans, TotalMean, Components, ExplainedVariances, Diagnostics).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::check_dimension_reducer(DimensionReducer),
		DimensionReducer = kernel_pca_reducer(Encoders, TrainingRows, RowMeans, TotalMean, Components, _ExplainedVariances, _Diagnostics),
		::dimension_reducer_options(DimensionReducer, Options),
		^^option(kernel(Kernel), Options),
		^^encode_instance(Encoders, Instance, Features),
		kernel_vector(TrainingRows, Features, Kernel, KernelVector),
		center_kernel_vector(KernelVector, RowMeans, TotalMean, CenteredKernelVector),
		^^project_components(Components, CenteredKernelVector, 1, ReducedInstance).

	check_dimension_reducer(DimensionReducer) :-
		(   DimensionReducer = kernel_pca_reducer(Encoders, TrainingRows, RowMeans, TotalMean, Components, ExplainedVariances, Diagnostics),
			^^valid_linear_encoders(Encoders),
			valid_training_rows(Encoders, TrainingRows),
			length(TrainingRows, SampleCount),
			valid(list(number, SampleCount), RowMeans),
			number(TotalMean),
			valid_dual_components(Components, SampleCount),
			valid_kernel_pca_diagnostics(Components, ExplainedVariances, Diagnostics) ->
			true
		;   domain_error(valid_dimension_reducer, DimensionReducer)
		).

	print_dimension_reducer_properties(kernel_pca_reducer(Encoders, _TrainingRows, _RowMeans, _TotalMean, Components, ExplainedVariances, Diagnostics)) :-
		format('Kernel PCA Dimension Reducer~n', []),
		format('============================~n~n', []),
		^^print_dimension_reducer_details(Diagnostics, Encoders, Components),
		format('Explained variances: ~w~n', [ExplainedVariances]).

	check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		check_minimum_examples(Examples),
		^^check_example_values(Examples, AttributeNames).

	check_minimum_examples(Examples) :-
		length(Examples, Count),
		(   Count >= 2 ->
			true
		;   domain_error(minimum_number_of_examples, Count)
		).

	example_attribute_values(_-AttributeValues, AttributeValues).

	handle_component_shortfall(RequestedComponentCount, _Options, _Matrix, Components, _ExplainedVariances, complete) :-
		length(Components, RequestedComponentCount),
		!.
	handle_component_shortfall(RequestedComponentCount, Options, Matrix, Components, ExplainedVariances, ExtractionDiagnostics) :-
		length(Components, LearnedComponentCount),
		^^option(shortfall_policy(Policy), Options),
		(	Policy == error ->
			domain_error(component_count, RequestedComponentCount-LearnedComponentCount)
		;	residual_eigenvalue(Matrix, Components, ExplainedVariances, Options, ResidualEigenvalue),
			^^option(tolerance(Tolerance), Options),
			ExtractionDiagnostics = truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance)
		).

	residual_eigenvalue(Matrix, Components, ExplainedVariances, Options, ResidualEigenvalue) :-
		deflate_extracted_components(Components, ExplainedVariances, Matrix, ResidualMatrix),
		^^extract_components(ResidualMatrix, 1, Options, _ResidualComponents, ResidualEigenvalues),
		(	ResidualEigenvalues = [ResidualEigenvalue0| _] ->
			ResidualEigenvalue = ResidualEigenvalue0
		;	ResidualEigenvalue = 0.0
		).

	deflate_extracted_components([], [], Matrix, Matrix).
	deflate_extracted_components([Component| Components], [ExplainedVariance| ExplainedVariances], Matrix, ResidualMatrix) :-
		deflate_component_matrix(Component, ExplainedVariance, Matrix, DeflatedMatrix),
		deflate_extracted_components(Components, ExplainedVariances, DeflatedMatrix, ResidualMatrix).

	deflate_component_matrix(Component, ExplainedVariance, Matrix, DeflatedMatrix) :-
		^^outer_product(Component, Component, OuterProduct),
		^^scale_matrix(OuterProduct, ExplainedVariance, ScaledOuterProduct),
		^^subtract_matrices(Matrix, ScaledOuterProduct, DeflatedMatrix).

	build_diagnostics(AttributeNames, SampleCount, Components, ExplainedVariances, Options, ExtractionDiagnostics, Diagnostics) :-
		^^preprocessing_diagnostics(true, Options, Preprocessing),
		extraction_diagnostics(ExtractionDiagnostics, ExtraExtractionDiagnostics),
		^^base_dimension_reducer_diagnostics(kernel_pca, AttributeNames, Components, Options, [
			sample_count(SampleCount),
			explained_variances(ExplainedVariances),
			preprocessing(Preprocessing)
			| ExtraExtractionDiagnostics
		], Diagnostics).

	extraction_diagnostics(complete, []).
	extraction_diagnostics(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance), [
		shortfall(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance))
	]).

	build_kernel_matrix(Rows, Kernel, KernelMatrix) :-
		build_kernel_rows(Rows, Rows, Kernel, KernelMatrix).

	build_kernel_rows([], _AllRows, _Kernel, []).
	build_kernel_rows([Row| Rows], AllRows, Kernel, [KernelRow| KernelMatrix]) :-
		build_kernel_row(AllRows, Row, Kernel, KernelRow),
		build_kernel_rows(Rows, AllRows, Kernel, KernelMatrix).

	build_kernel_row([], _Row, _Kernel, []).
	build_kernel_row([OtherRow| OtherRows], Row, Kernel, [KernelValue| KernelValues]) :-
		kernel_value(Kernel, Row, OtherRow, KernelValue),
		build_kernel_row(OtherRows, Row, Kernel, KernelValues).

	kernel_value(linear, Row1, Row2, KernelValue) :-
		dot_product(Row1, Row2, KernelValue).
	kernel_value(polynomial(Degree, Gamma, Coef0), Row1, Row2, KernelValue) :-
		dot_product(Row1, Row2, DotProduct),
		Base is Gamma * DotProduct + Coef0,
		KernelValue is Base ** Degree.
	kernel_value(rbf(Gamma), Row1, Row2, KernelValue) :-
		^^subtract_vectors(Row1, Row2, Difference),
		dot_product(Difference, Difference, SquaredDistance),
		KernelValue is exp(-Gamma * SquaredDistance).

	center_kernel_matrix(KernelMatrix, CenteredKernelMatrix, RowMeans, TotalMean) :-
		kernel_row_means(KernelMatrix, RowMeans),
		arithmetic_mean(RowMeans, TotalMean),
		center_kernel_rows(KernelMatrix, RowMeans, RowMeans, TotalMean, CenteredKernelMatrix).

	kernel_row_means([], []).
	kernel_row_means([KernelRow| KernelMatrix], [RowMean| RowMeans]) :-
		arithmetic_mean(KernelRow, RowMean),
		kernel_row_means(KernelMatrix, RowMeans).

	center_kernel_rows([], [], _AllRowMeans, _TotalMean, []).
	center_kernel_rows([KernelRow| KernelMatrix], [RowMean| RowMeans], AllRowMeans, TotalMean, [CenteredKernelRow| CenteredKernelMatrix]) :-
		center_kernel_row(KernelRow, RowMean, AllRowMeans, TotalMean, CenteredKernelRow),
		center_kernel_rows(KernelMatrix, RowMeans, AllRowMeans, TotalMean, CenteredKernelMatrix).

	center_kernel_row([], _RowMean, [], _TotalMean, []).
	center_kernel_row([KernelValue| KernelValues], RowMean, [ColumnMean| ColumnMeans], TotalMean, [CenteredKernelValue| CenteredKernelValues]) :-
		CenteredKernelValue is KernelValue - RowMean - ColumnMean + TotalMean,
		center_kernel_row(KernelValues, RowMean, ColumnMeans, TotalMean, CenteredKernelValues).

	center_kernel_vector(KernelVector, RowMeans, TotalMean, CenteredKernelVector) :-
		arithmetic_mean(KernelVector, KernelMean),
		center_kernel_vector(KernelVector, RowMeans, KernelMean, TotalMean, CenteredKernelVector).

	center_kernel_vector([], [], _KernelMean, _TotalMean, []).
	center_kernel_vector([KernelValue| KernelValues], [RowMean| RowMeans], KernelMean, TotalMean, [CenteredKernelValue| CenteredKernelValues]) :-
		CenteredKernelValue is KernelValue - RowMean - KernelMean + TotalMean,
		center_kernel_vector(KernelValues, RowMeans, KernelMean, TotalMean, CenteredKernelValues).

	kernel_vector([], _Features, _Kernel, []).
	kernel_vector([TrainingRow| TrainingRows], Features, Kernel, [KernelValue| KernelValues]) :-
		kernel_value(Kernel, TrainingRow, Features, KernelValue),
		kernel_vector(TrainingRows, Features, Kernel, KernelValues).

	normalize_dual_components([], [], []).
	normalize_dual_components([DualEigenvector| DualEigenvectors], [ExplainedVariance| ExplainedVariances], [Component| Components]) :-
		Scale is 1.0 / sqrt(ExplainedVariance),
		^^scale_vector(DualEigenvector, Scale, Component),
		normalize_dual_components(DualEigenvectors, ExplainedVariances, Components).

	valid_training_rows(Encoders, TrainingRows) :-
		valid(list, TrainingRows),
		length(Encoders, FeatureCount),
		valid_training_rows(TrainingRows, FeatureCount).

	valid_training_rows([], _FeatureCount).
	valid_training_rows([TrainingRow| TrainingRows], FeatureCount) :-
		valid(list(number, FeatureCount), TrainingRow),
		valid_training_rows(TrainingRows, FeatureCount).

	valid_dual_components(Components, SampleCount) :-
		valid(list, Components),
		valid_dual_components(Components, SampleCount, 0).

	valid_dual_components([], _SampleCount, _Count).
	valid_dual_components([Component| Components], SampleCount, Count0) :-
		valid(list(number, SampleCount), Component),
		Count1 is Count0 + 1,
		valid_dual_components(Components, SampleCount, Count1).

	valid_kernel_pca_diagnostics(Components, ExplainedVariances, Diagnostics) :-
		^^valid_dimension_reducer_metadata(Diagnostics),
		memberchk(model(kernel_pca), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		valid(positive_integer, SampleCount),
		memberchk(explained_variances(ExplainedVariances), Diagnostics),
		valid(list(number), ExplainedVariances),
		valid(list(positive_number), ExplainedVariances),
		length(Components, ComponentCount),
		length(ExplainedVariances, ComponentCount),
		memberchk(preprocessing(Preprocessing), Diagnostics),
		valid_preprocessing(Preprocessing),
		valid_shortfall_diagnostics(Components, Diagnostics).

	valid_preprocessing([center(true), feature_scaling(FeatureScaling)]) :-
		valid(boolean, FeatureScaling).

	valid_shortfall_diagnostics(Components, Diagnostics) :-
		(	memberchk(shortfall(Shortfall), Diagnostics) ->
			valid_shortfall_diagnostic(Shortfall),
			length(Components, LearnedComponentCount),
			Shortfall = truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, _Tolerance),
			RequestedComponentCount > LearnedComponentCount,
			ResidualEigenvalue >= 0.0
		;	true
		).

	valid_shortfall_diagnostic(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance)) :-
		valid(positive_integer, RequestedComponentCount),
		valid(non_negative_integer, LearnedComponentCount),
		valid(number, ResidualEigenvalue),
		valid(positive_number, Tolerance).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(shortfall_policy(truncate)).
	default_option(kernel(linear)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-8)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(shortfall_policy(Policy)) :-
		once((Policy == error; Policy == truncate)).
	valid_option(kernel(Kernel)) :-
		nonvar(Kernel),
		valid_kernel(Kernel).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).

	valid_kernel(linear).
	valid_kernel(polynomial(Degree, Gamma, Coef0)) :-
		valid(positive_integer, Degree),
		valid(positive_number, Gamma),
		valid(non_negative_number, Coef0).
	valid_kernel(rbf(Gamma)) :-
		valid(positive_number, Gamma).

:- end_object.

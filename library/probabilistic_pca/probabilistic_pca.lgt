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


:- object(probabilistic_pca,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'Probabilistic Principal Component Analysis reducer for continuous datasets using a portable covariance eigensolver.',
		remarks is [
			'Algorithm' - 'Centers the training data, optionally standardizes continuous attributes, estimates the sample covariance matrix, extracts deterministic leading eigenvectors using portable power iteration with deflation, and then converts them into the closed-form maximum-likelihood probabilistic PCA loading matrix and posterior latent projection.',
			'Feature handling' - 'Supports continuous attributes only. Missing or nonnumeric values are rejected.',
			'Shortfall handling' - 'Supports a ``shortfall_policy/1`` option for choosing whether a numerically rank-deficient covariance matrix raises an error or returns a truncated reducer with explicit diagnostics.',
			'Dimension reducer representation' - 'The learned reducer is represented by default as ``probabilistic_pca_reducer(Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics)`` where ``Encoders`` stores preprocessing metadata, ``Components`` stores posterior latent projection vectors, ``Loadings`` stores the maximum-likelihood loading vectors, and ``Diagnostics`` records the learned model metadata, effective options, and any truncate-mode shortfall details.'
		],
		see_also is [kernel_pca, pca, truncated_svd]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(numberlist, [
		rescale/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(linear_algebra, [
		covariance_matrix/2, matrix_trace/2, new_vector/3, outer_product/3, scale_matrix/3,
		subtract_matrices/3
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
		length(AttributeNames, FeatureCount),
		length(Examples, SampleCount),
		^^option(n_components(RequestedComponents), Options),
		MaxComponentCount is min(FeatureCount, SampleCount - 1),
		^^check_component_count(RequestedComponents, MaxComponentCount, ComponentCount),
		covariance_matrix(Rows, CovarianceMatrix),
		matrix_trace(CovarianceMatrix, CovarianceTrace),
		^^extract_components(CovarianceMatrix, ComponentCount, Options, Eigenvectors, ExplainedVariances),
		handle_component_shortfall(ComponentCount, Options, CovarianceMatrix, Eigenvectors, ExplainedVariances, ExtractionDiagnostics),
		estimate_noise_variance(CovarianceTrace, FeatureCount, ExplainedVariances, NoiseVariance),
		^^option(tolerance(Tolerance), Options),
		build_loadings(Eigenvectors, ExplainedVariances, NoiseVariance, Tolerance, Loadings),
		build_posterior_components(Eigenvectors, ExplainedVariances, NoiseVariance, Tolerance, Components),
		build_diagnostics(AttributeNames, SampleCount, Components, ExplainedVariances, NoiseVariance, Options, ExtractionDiagnostics, Diagnostics),
		DimensionReducer = probabilistic_pca_reducer(Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics).

	check_dimension_reducer(DimensionReducer) :-
		(   DimensionReducer = probabilistic_pca_reducer(Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics),
			^^valid_linear_encoders(Encoders),
			^^valid_projection_components(Encoders, Components),
			^^valid_projection_components(Encoders, Loadings),
			valid(non_negative_number, NoiseVariance),
			valid_probabilistic_pca_diagnostics(Components, Loadings, ExplainedVariances, Diagnostics) ->
			true
		;   domain_error(dimension_reducer, DimensionReducer)
		).

	print_dimension_reducer_properties(probabilistic_pca_reducer(Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics)) :-
		format('Probabilistic PCA Dimension Reducer~n', []),
		format('===================================~n~n', []),
		^^print_dimension_reducer_details(Diagnostics, Encoders, Components),
		format('Noise variance: ~w~n', [NoiseVariance]),
		format('Explained variances: ~w~n', [ExplainedVariances]),
		format('Loadings: ~w~n', [Loadings]).

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

	handle_component_shortfall(RequestedComponentCount, _Options, _CovarianceMatrix, Components, _ExplainedVariances, complete) :-
		length(Components, RequestedComponentCount),
		!.
	handle_component_shortfall(RequestedComponentCount, Options, CovarianceMatrix, Components, ExplainedVariances, ExtractionDiagnostics) :-
		length(Components, LearnedComponentCount),
		^^option(shortfall_policy(Policy), Options),
		(   Policy == error ->
			domain_error(component_count, RequestedComponentCount-LearnedComponentCount)
		;   residual_eigenvalue(CovarianceMatrix, Components, ExplainedVariances, Options, ResidualEigenvalue),
			^^option(tolerance(Tolerance), Options),
			ExtractionDiagnostics = truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance)
		).

	residual_eigenvalue(CovarianceMatrix, Components, ExplainedVariances, Options, ResidualEigenvalue) :-
		deflate_extracted_components(Components, ExplainedVariances, CovarianceMatrix, ResidualCovarianceMatrix),
		^^extract_components(ResidualCovarianceMatrix, 1, Options, _ResidualComponents, ResidualEigenvalues),
		(   ResidualEigenvalues = [ResidualEigenvalue0| _] ->
			ResidualEigenvalue = ResidualEigenvalue0
		;   ResidualEigenvalue = 0.0
		).

	deflate_extracted_components([], [], CovarianceMatrix, CovarianceMatrix).
	deflate_extracted_components([Component| Components], [ExplainedVariance| ExplainedVariances], CovarianceMatrix, ResidualCovarianceMatrix) :-
		deflate_component_matrix(Component, ExplainedVariance, CovarianceMatrix, DeflatedCovarianceMatrix),
		deflate_extracted_components(Components, ExplainedVariances, DeflatedCovarianceMatrix, ResidualCovarianceMatrix).

	deflate_component_matrix(Component, ExplainedVariance, CovarianceMatrix, DeflatedCovarianceMatrix) :-
		outer_product(Component, Component, OuterProduct),
		scale_matrix(OuterProduct, ExplainedVariance, ScaledOuterProduct),
		subtract_matrices(CovarianceMatrix, ScaledOuterProduct, DeflatedCovarianceMatrix).

	estimate_noise_variance(CovarianceTrace, FeatureCount, ExplainedVariances, NoiseVariance) :-
		length(ExplainedVariances, LearnedComponentCount),
		ResidualDimensions is FeatureCount - LearnedComponentCount,
		(   ResidualDimensions =< 0 ->
			NoiseVariance = 0.0
		;   sum_numbers(ExplainedVariances, 0.0, RetainedVariance),
			ResidualVariance0 is CovarianceTrace - RetainedVariance,
			(   ResidualVariance0 > 0.0 ->
				ResidualVariance = ResidualVariance0
			;   ResidualVariance = 0.0
			),
			NoiseVariance is ResidualVariance / ResidualDimensions
		).

	sum_numbers([], Sum, Sum).
	sum_numbers([Number| Numbers], Sum0, Sum) :-
		Sum1 is Sum0 + Number,
		sum_numbers(Numbers, Sum1, Sum).

	build_loadings([], [], _NoiseVariance, _Tolerance, []).
	build_loadings([Eigenvector| Eigenvectors], [ExplainedVariance| ExplainedVariances], NoiseVariance, Tolerance, [Loading| Loadings]) :-
		latent_variance(ExplainedVariance, NoiseVariance, Tolerance, LoadingVariance),
		(   LoadingVariance =< Tolerance ->
			length(Eigenvector, FeatureCount),
			new_vector(FeatureCount, 0.0, Loading)
		;   LoadingScale is sqrt(LoadingVariance),
			rescale(Eigenvector, LoadingScale, Loading)
		),
		build_loadings(Eigenvectors, ExplainedVariances, NoiseVariance, Tolerance, Loadings).

	build_posterior_components([], [], _NoiseVariance, _Tolerance, []).
	build_posterior_components([Eigenvector| Eigenvectors], [ExplainedVariance| ExplainedVariances], NoiseVariance, Tolerance, [Component| Components]) :-
		latent_variance(ExplainedVariance, NoiseVariance, Tolerance, PosteriorVariance),
		(   ExplainedVariance =< Tolerance ->
			length(Eigenvector, FeatureCount),
			new_vector(FeatureCount, 0.0, Component)
		;   PosteriorVariance =< Tolerance ->
			length(Eigenvector, FeatureCount),
			new_vector(FeatureCount, 0.0, Component)
		;   Scale is sqrt(PosteriorVariance) / ExplainedVariance,
			rescale(Eigenvector, Scale, Component)
		),
		build_posterior_components(Eigenvectors, ExplainedVariances, NoiseVariance, Tolerance, Components).

	latent_variance(ExplainedVariance, NoiseVariance, Tolerance, LatentVariance) :-
		Variance0 is ExplainedVariance - NoiseVariance,
		(   Variance0 =< Tolerance ->
			LatentVariance = 0.0
		;   LatentVariance = Variance0
		).

	valid_probabilistic_pca_diagnostics(Components, Loadings, ExplainedVariances, Diagnostics) :-
		^^valid_dimension_reducer_metadata(Diagnostics),
		memberchk(model(probabilistic_pca), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		valid(positive_integer, SampleCount),
		memberchk(explained_variances(ExplainedVariances), Diagnostics),
		valid(list(positive_number), ExplainedVariances),
		memberchk(noise_variance(NoiseVariance), Diagnostics),
		valid(non_negative_number, NoiseVariance),
		length(Components, ComponentCount),
		length(Loadings, ComponentCount),
		length(ExplainedVariances, ComponentCount),
		memberchk(preprocessing(Preprocessing), Diagnostics),
		valid_preprocessing(Preprocessing),
		valid_shortfall_diagnostics(Components, Diagnostics).

	valid_preprocessing([center(true), feature_scaling(FeatureScaling)]) :-
		valid(boolean, FeatureScaling).

	valid_shortfall_diagnostics(Components, Diagnostics) :-
		(   memberchk(shortfall(Shortfall), Diagnostics) ->
			valid_shortfall_diagnostic(Shortfall),
			length(Components, LearnedComponentCount),
			Shortfall = truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, _Tolerance),
			RequestedComponentCount > LearnedComponentCount,
			ResidualEigenvalue >= 0.0
		;   true
		).

	valid_shortfall_diagnostic(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance)) :-
		valid(positive_integer, RequestedComponentCount),
		valid(non_negative_integer, LearnedComponentCount),
		valid(number, ResidualEigenvalue),
		valid(positive_number, Tolerance).

	build_diagnostics(AttributeNames, SampleCount, Components, ExplainedVariances, NoiseVariance, Options, ExtractionDiagnostics, Diagnostics) :-
		^^preprocessing_diagnostics(true, Options, Preprocessing),
		extraction_diagnostics(ExtractionDiagnostics, ExtraExtractionDiagnostics),
		^^base_dimension_reducer_diagnostics(probabilistic_pca, AttributeNames, Components, Options, [
			sample_count(SampleCount),
			explained_variances(ExplainedVariances),
			noise_variance(NoiseVariance),
			preprocessing(Preprocessing)
			| ExtraExtractionDiagnostics
		], Diagnostics).

	extraction_diagnostics(complete, []).
	extraction_diagnostics(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance), [
		shortfall(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance))
	]).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(shortfall_policy(truncate)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-8)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(shortfall_policy(Policy)) :-
		once((Policy == error; Policy == truncate)).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).

:- end_object.

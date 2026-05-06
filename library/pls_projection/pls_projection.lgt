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


:- object(pls_projection,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Partial Least Squares projection for target-valued continuous datasets using deterministic PLS1 deflation.',
		see_also is [lda_projection, pca_projection]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, memberchk/2, reverse/2
	]).

	:- uses(numberlist, [
		rescale/3, scalar_product/3 as dot_product/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(linear_algebra, [
		matrix_vector_product/3, normalize_vector/2, stabilize_vector_sign/3, subtract_vectors/3,
		transpose_matrix/2
	]).

	learn(Dataset, DimensionReducer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		Dataset::target(TargetAttribute),
		check_target_attribute(TargetAttribute, AttributeNames),
		findall(Id-Target-AttributeValues, Dataset::example(Id, Target, AttributeValues), Examples),
		check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		target_values(Examples, Targets0),
		center_targets(TargetAttribute, Targets0, TargetMean, Targets),
		length(AttributeNames, FeatureCount),
		length(Rows, SampleCount),
		^^option(n_components(RequestedComponents), Options),
		MaxComponentCount is min(FeatureCount, SampleCount - 1),
		^^check_component_count(RequestedComponents, MaxComponentCount, ComponentCount),
		extract_pls_components(Rows, Targets, ComponentCount, Options, Components, Loadings, TargetLoadings, ExtractionDiagnostics),
		build_rotations(Components, Loadings, Rotations),
		build_diagnostics(AttributeNames, SampleCount, Rotations, Options, TargetAttribute, TargetMean, TargetLoadings, ExtractionDiagnostics, Diagnostics),
		DimensionReducer = pls_projection_reducer(Encoders, Rotations, Diagnostics).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::check_dimension_reducer(DimensionReducer),
		DimensionReducer = pls_projection_reducer(Encoders, Rotations, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		^^project_components(Rotations, Features, 1, ReducedInstance).

	check_dimension_reducer(DimensionReducer) :-
		(   DimensionReducer = pls_projection_reducer(Encoders, Rotations, Diagnostics),
			^^valid_linear_encoders(Encoders),
			^^valid_projection_components(Encoders, Rotations),
			valid_pls_diagnostics(Rotations, Diagnostics) ->
			true
		;   domain_error(dimension_reducer, DimensionReducer)
		).

	print_dimension_reducer_properties(pls_projection_reducer(Encoders, Rotations, Diagnostics)) :-
		format('PLS Projection Dimension Reducer~n', []),
		format('================================~n~n', []),
		^^print_dimension_reducer_details(Diagnostics, Encoders, Rotations),
		format('Rotations: ~w~n', [Rotations]).

	example_attribute_values(_-_-AttributeValues, AttributeValues).

	check_target_attribute(TargetAttribute, AttributeNames) :-
		(   memberchk(TargetAttribute, AttributeNames) ->
			domain_error(target_attribute, TargetAttribute)
		;   true
		).

	check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		length(Examples, SampleCount),
		(   SampleCount >= 2 ->
			true
		;   domain_error(minimum_number_of_examples, Dataset)
		),
		check_example_values(Examples, AttributeNames).

	check_example_values([], _AttributeNames).
	check_example_values([_-Target-AttributeValues| Examples], AttributeNames) :-
		(   nonvar(Target) ->
			true
		;   instantiation_error
		),
		(   number(Target) ->
			true
		;   type_error(number, Target)
		),
		^^check_example_attributes(AttributeNames, AttributeValues),
		check_example_values(Examples, AttributeNames).

	target_values([], []).
	target_values([_-Target-_| Examples], [Target| Targets]) :-
		target_values(Examples, Targets).

	center_targets(TargetAttribute, Targets0, TargetMean, Targets) :-
		arithmetic_mean(Targets0, TargetMean),
		variance(Targets0, TargetVariance),
		(   TargetVariance =< 1.0e-12 ->
			domain_error(target_variance, TargetAttribute)
		;   subtract_mean(Targets0, TargetMean, Targets)
		).

	subtract_mean([], _Mean, []).
	subtract_mean([Value| Values], Mean, [Centered| CenteredValues]) :-
		Centered is Value - Mean,
		subtract_mean(Values, Mean, CenteredValues).

	extract_pls_components(_Rows, _Targets, 0, _Options, [], [], [], complete) :-
		!.
	extract_pls_components(Rows, Targets, Requested, Options, Components, Loadings, TargetLoadings, ExtractionDiagnostics) :-
		extract_pls_components(Rows, Targets, Requested, Requested, Options, [], [], [], Components, Loadings, TargetLoadings, ExtractionDiagnostics).

	extract_pls_components(_Rows, _Targets, 0, _Requested, _Options, ComponentsAcc, LoadingsAcc, TargetLoadingsAcc, Components, Loadings, TargetLoadings, complete) :-
		!,
		reverse(ComponentsAcc, Components),
		reverse(LoadingsAcc, Loadings),
		reverse(TargetLoadingsAcc, TargetLoadings).
	extract_pls_components(Rows, Targets, Requested, TotalRequested, Options, ComponentsAcc, LoadingsAcc, TargetLoadingsAcc, Components, Loadings, TargetLoadings, ExtractionDiagnostics) :-
		covariance_vector(Rows, Targets, RawComponent),
		normalize_vector(RawComponent, NormalizedComponent),
		score_vector(Rows, NormalizedComponent, Scores0),
		dot_product(Scores0, Scores0, Denominator),
		^^option(tolerance(Tolerance), Options),
		(   Denominator =< Tolerance ->
			handle_component_shortfall(TotalRequested, Options, ComponentsAcc, LoadingsAcc, TargetLoadingsAcc, Denominator, Tolerance, Components, Loadings, TargetLoadings, ExtractionDiagnostics)
		;   component_loading(Rows, Scores0, Denominator, Loading0),
			dot_product(Targets, Scores0, TargetLoadingNumerator),
			TargetLoading0 is TargetLoadingNumerator / Denominator,
			stabilize_component_sign(NormalizedComponent, Scores0, Loading0, TargetLoading0, Tolerance, Component, Scores, Loading, TargetLoading),
			deflate_rows(Rows, Scores, Loading, DeflatedRows),
			deflate_targets(Targets, Scores, TargetLoading, DeflatedTargets),
			NextRequested is Requested - 1,
			extract_pls_components(DeflatedRows, DeflatedTargets, NextRequested, TotalRequested, Options, [Component| ComponentsAcc], [Loading| LoadingsAcc], [TargetLoading| TargetLoadingsAcc], Components, Loadings, TargetLoadings, ExtractionDiagnostics)
		).

	handle_component_shortfall(TotalRequested, Options, ComponentsAcc, LoadingsAcc, TargetLoadingsAcc, ScoreEnergy, Tolerance, Components, Loadings, TargetLoadings, ExtractionDiagnostics) :-
		length(ComponentsAcc, ExtractedCount),
		^^option(shortfall_policy(Policy), Options),
		(	Policy == error ->
			domain_error(component_count, TotalRequested-ExtractedCount)
		;	reverse(ComponentsAcc, Components),
			reverse(LoadingsAcc, Loadings),
			reverse(TargetLoadingsAcc, TargetLoadings),
			ExtractionDiagnostics = truncated(TotalRequested, ExtractedCount, ScoreEnergy, Tolerance)
		).

	covariance_vector(Rows, Targets, CovarianceVector) :-
		transpose_matrix(Rows, Columns),
		column_covariances(Columns, Targets, CovarianceVector).

	column_covariances([], _Targets, []).
	column_covariances([Column| Columns], Targets, [Covariance| Covariances]) :-
		dot_product(Column, Targets, Covariance),
		column_covariances(Columns, Targets, Covariances).

	score_vector(Rows, Component, Scores) :-
		matrix_vector_product(Rows, Component, Scores).

	component_loading(Rows, Scores, Denominator, Loading) :-
		transpose_matrix(Rows, Columns),
		column_loadings(Columns, Scores, Denominator, Loading).

	column_loadings([], _Scores, _Denominator, []).
	column_loadings([Column| Columns], Scores, Denominator, [Loading| Loadings]) :-
		dot_product(Column, Scores, Numerator),
		Loading is Numerator / Denominator,
		column_loadings(Columns, Scores, Denominator, Loadings).

	stabilize_component_sign(Component0, Scores0, Loading0, TargetLoading0, Tolerance, Component, Scores, Loading, TargetLoading) :-
		stabilize_vector_sign(Component0, Tolerance, Component),
		(   Component == Component0 ->
			Scores = Scores0,
			Loading = Loading0,
			TargetLoading = TargetLoading0
		;
			rescale(Scores0, -1.0, Scores),
			rescale(Loading0, -1.0, Loading),
			TargetLoading is -TargetLoading0
		).

	deflate_rows([], [], _Loading, []).
	deflate_rows([Row| Rows], [Score| Scores], Loading, [DeflatedRow| DeflatedRows]) :-
		rescale(Loading, Score, Adjustment),
		subtract_vectors(Row, Adjustment, DeflatedRow),
		deflate_rows(Rows, Scores, Loading, DeflatedRows).

	deflate_targets([], [], _TargetLoading, []).
	deflate_targets([Target| Targets], [Score| Scores], TargetLoading, [DeflatedTarget| DeflatedTargets]) :-
		DeflatedTarget is Target - TargetLoading * Score,
		deflate_targets(Targets, Scores, TargetLoading, DeflatedTargets).

	build_rotations(Components, Loadings, Rotations) :-
		build_rotations(Components, Loadings, [], Rotations).

	build_rotations([], [], _PreviousPairsReversed, []).
	build_rotations([Component| Components], [Loading| Loadings], PreviousPairsReversed, [Rotation| Rotations]) :-
		build_rotation(PreviousPairsReversed, Component, Rotation),
		build_rotations(Components, Loadings, [Loading-Rotation| PreviousPairsReversed], Rotations).

	build_rotation(PreviousPairs, Component, Rotation) :-
		build_rotation(PreviousPairs, Component, Component, Rotation).

	build_rotation([], _Component, Rotation, Rotation).
	build_rotation([Loading-PreviousRotation| PreviousPairs], Component, Rotation0, Rotation) :-
		dot_product(Loading, Component, Projection),
		rescale(PreviousRotation, Projection, Adjustment),
		subtract_vectors(Rotation0, Adjustment, Rotation1),
		build_rotation(PreviousPairs, Component, Rotation1, Rotation).

	valid_pls_diagnostics(Rotations, Diagnostics) :-
		^^valid_dimension_reducer_metadata(Diagnostics),
		memberchk(model(pls_projection), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		valid(positive_integer, SampleCount),
		memberchk(target(TargetAttribute), Diagnostics),
		atom(TargetAttribute),
		memberchk(target_mean(TargetMean), Diagnostics),
		number(TargetMean),
		memberchk(target_loadings(TargetLoadings), Diagnostics),
		valid(list(number), TargetLoadings),
		length(Rotations, ComponentCount),
		length(TargetLoadings, ComponentCount),
		memberchk(preprocessing(Preprocessing), Diagnostics),
		valid_preprocessing(Preprocessing),
		valid_shortfall_diagnostics(Rotations, Diagnostics).

	valid_preprocessing([center(true), feature_scaling(FeatureScaling)]) :-
		valid(boolean, FeatureScaling).

	valid_shortfall_diagnostics(Rotations, Diagnostics) :-
		(	memberchk(shortfall(Shortfall), Diagnostics) ->
			valid_shortfall_diagnostic(Shortfall),
			length(Rotations, LearnedComponentCount),
			Shortfall = truncated(RequestedComponentCount, LearnedComponentCount, ScoreEnergy, _Tolerance),
			RequestedComponentCount > LearnedComponentCount,
			ScoreEnergy >= 0.0
		;	true
		).

	valid_shortfall_diagnostic(truncated(RequestedComponentCount, LearnedComponentCount, ScoreEnergy, Tolerance)) :-
		valid(positive_integer, RequestedComponentCount),
		valid(non_negative_integer, LearnedComponentCount),
		number(ScoreEnergy),
		valid(positive_number, Tolerance).

	build_diagnostics(AttributeNames, SampleCount, Rotations, Options, TargetAttribute, TargetMean, TargetLoadings, ExtractionDiagnostics, Diagnostics) :-
		^^preprocessing_diagnostics(true, Options, Preprocessing),
		extraction_diagnostics(ExtractionDiagnostics, ExtraExtractionDiagnostics),
		ExtraDiagnostics = [
			sample_count(SampleCount),
			target(TargetAttribute),
			target_mean(TargetMean),
			target_loadings(TargetLoadings),
			preprocessing(Preprocessing)
			| ExtraExtractionDiagnostics
		],
		^^base_dimension_reducer_diagnostics(pls_projection, AttributeNames, Rotations, Options, ExtraDiagnostics, Diagnostics).

	extraction_diagnostics(complete, []).
	extraction_diagnostics(truncated(RequestedComponentCount, LearnedComponentCount, ScoreEnergy, Tolerance), [
		shortfall(truncated(RequestedComponentCount, LearnedComponentCount, ScoreEnergy, Tolerance))
	]).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(shortfall_policy(truncate)).
	default_option(tolerance(1.0e-8)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(shortfall_policy(Policy)) :-
		once((Policy == error; Policy == truncate)).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_number, Tolerance).

:- end_object.

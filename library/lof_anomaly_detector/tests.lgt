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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Unit tests for the "lof_anomaly_detector" library.'
	]).

	:- uses(list, [
		last/2, length/2, member/2, memberchk/2, msort/2, take/3
	]).

	:- uses(lgtunit, [
		assertion/1, op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(lof_anomaly_detector, [
		anomaly_detector_options/2, check_anomaly_detector/1, diagnostic/2, diagnostics/2, export_to_clauses/4,
		export_to_file/4, learn/2, learn/3, predict/3, predict/4, print_anomaly_detector/1, score/3,
		score_all/3, valid_anomaly_detector/1
	]).

	:- set_logtalk_flag(arithmetic_expressions, silent).

	cover(lof_anomaly_detector).

	cleanup :-
		^^clean_file('test_output.pl').

	learn_filtered(Dataset, Detector) :-
		learn(Dataset, Detector, [baseline_selection_policy(filter)]).

	learn_filtered(Dataset, ExtraOptions, Detector) :-
		learn(Dataset, Detector, [baseline_selection_policy(filter)| ExtraOptions]).

	test(lof_learn_2_gaussian_error, error(domain_error(baseline_only_training_data, gaussian_anomalies))) :-
		learn(gaussian_anomalies, _Detector).

	test(lof_learn_3_gaussian_filter, true(ground(Detector))) :-
		learn_filtered(gaussian_anomalies, Detector).

	test(lof_learn_2_mixed, true(ground(Detector))) :-
		learn_filtered(mixed_anomalies, Detector).

	test(lof_score_3_gaussian_anomaly_higher_than_normal, true(AnomalyScore > NormalScore)) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore).

	test(lof_score_3_sensor_missing_value, true((Score >= 0.0, Score =< 1.0))) :-
		learn_filtered(sensor_anomalies, [k(3)], Detector),
		score(Detector, [temperature-101.0, pressure-_, vibration-2.10], Score).

	test(lof_score_3_mixed_distance_ordering, true((NumericScore > BaseScore, CategoricalScore > BaseScore, CombinedScore >= NumericScore, CombinedScore >= CategoricalScore, MissingCategoricalScore > BaseScore, MissingNumericScore > BaseScore))) :-
		learn_filtered(mixed_distance_behaviors, [k(3)], Detector),
		score(Detector, [size-10.05, weight-100.05, color-red,  shape-round], BaseScore),
		score(Detector, [size-12.0,  weight-104.0,  color-red,  shape-round], NumericScore),
		score(Detector, [size-10.05, weight-100.05, color-blue, shape-square], CategoricalScore),
		score(Detector, [size-12.0,  weight-104.0,  color-blue, shape-square], CombinedScore),
		score(Detector, [size-12.0,  weight-104.0,  color-_,    shape-_], MissingCategoricalScore),
		score(Detector, [size-_,     weight-_,      color-blue, shape-square], MissingNumericScore).

	test(lof_score_3_mixed_distance_manhattan, true(CombinedScore > BaseScore)) :-
		learn_filtered(mixed_distance_behaviors, [k(3), distance_metric(manhattan)], Detector),
		score(Detector, [size-10.05, weight-100.05, color-red,  shape-round], BaseScore),
		score(Detector, [size-12.0,  weight-104.0,  color-blue, shape-square], CombinedScore).

	test(lof_score_3_fresh_identical_query_is_not_leave_one_out, true((FreshScore =:= 0.0, TrainingScore > FreshScore))) :-
		learn_filtered(lof_identical_query_fixture, [k(1)], Detector),
		score(Detector, [x-0.00], FreshScore),
		score_all(lof_identical_query_fixture, Detector, Scores),
		member(1-normal-TrainingScore, Scores).

	test(lof_learn_3_empty_dataset_error, error(domain_error(non_empty_dataset, lof_empty_anomalies))) :-
		learn(lof_empty_anomalies, _Detector, [k(1)]).

	test(lof_score_3_singleton_dataset_bounded_and_consistent, true((FreshScore =:= TrainingScore, FreshScore >= 0.0, FreshScore =< 1.0))) :-
		learn(lof_singleton_anomalies, Detector, [k(3)]),
		score(Detector, [x-1.00], FreshScore),
		score_all(lof_singleton_anomalies, Detector, [1-normal-TrainingScore]).

	test(lof_valid_anomaly_detector_1, deterministic(valid_anomaly_detector(Detector))) :-
		learn(lof_singleton_anomalies, Detector, [k(3)]).

	test(lof_invalid_anomaly_detector_1, error(domain_error(anomaly_detector, lof_detector(lof_singleton_anomalies, [x], [numeric], [1.0], [1-normal-[1.0]], [], [model(lof_anomaly_detector), training_dataset(lof_singleton_anomalies), attribute_names([x]), feature_types([numeric]), example_count(1), reference_score_count(0), options([k(1)])])))) :-
		check_anomaly_detector(lof_detector(lof_singleton_anomalies, [x], [numeric], [1.0], [1-normal-[1.0]], [], [model(lof_anomaly_detector), training_dataset(lof_singleton_anomalies), attribute_names([x]), feature_types([numeric]), example_count(1), reference_score_count(0), options([k(1)])])).

	test(lof_diagnostics_2, deterministic([Model, TrainingDataset, ExampleCount, ReferenceScoreCount] == [lof_anomaly_detector, gaussian_anomalies, 40, 40])) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		diagnostics(Detector, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(training_dataset(TrainingDataset), Diagnostics),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(reference_score_count(ReferenceScoreCount), Diagnostics).

	test(lof_anomaly_detector_options_2, deterministic([K, BaselineClassValues, BaselineSelectionPolicy] == [5, [normal], filter])) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		anomaly_detector_options(Detector, Options),
		memberchk(k(K), Options),
		memberchk(baseline_class_values(BaselineClassValues), Options),
		memberchk(baseline_selection_policy(BaselineSelectionPolicy), Options).

	test(lof_diagnostic_2, deterministic(Diagnostics == Enumerated)) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		diagnostics(Detector, Diagnostics),
		findall(Diagnostic, diagnostic(Detector, Diagnostic), Enumerated).

	test(lof_predict_3_gaussian_normal, true(Prediction == normal)) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		predict(Detector, [x-0.12, y-0.34], Prediction).

	test(lof_predict_3_gaussian_anomaly, true(Prediction == anomaly)) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction).

	test(lof_predict_4_threshold_override, true(Prediction == anomaly)) :-
		learn_filtered(gaussian_anomalies, [k(5), anomaly_threshold(0.99)], Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.4)]).

	test(lof_score_all_3_gaussian_count, true(length(Scores, 48))) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		score_all(gaussian_anomalies, Detector, Scores),
		length(Scores, 48).

	test(lof_score_all_3_gaussian_sorted, true(FirstScore >= SecondScore)) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		score_all(gaussian_anomalies, Detector, [_-_-FirstScore, _-_-SecondScore| _]).

	test(lof_score_all_3_gaussian_top_anomalies, true(AnomalyCount >= 5)) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		score_all(gaussian_anomalies, Detector, Scores),
		take(8, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(lof_score_all_3_mixed_top_anomalies, true(AnomalyCount >= 2)) :-
		learn_filtered(mixed_anomalies, [k(3)], Detector),
		score_all(mixed_anomalies, Detector, Scores),
		take(4, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(lof_score_all_3_mixed_distance_top_anomalies, true(AnomalyCount == 2)) :-
		learn_filtered(mixed_distance_behaviors, [k(3)], Detector),
		score_all(mixed_distance_behaviors, Detector, Scores),
		take(2, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(lof_export_to_clauses_4, true(ground(Clauses))) :-
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		export_to_clauses(gaussian_anomalies, Detector, detect, Clauses).

	test(lof_export_to_file_4, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		export_to_file(gaussian_anomalies, Detector, detect, File).

	test(lof_export_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		export_to_file(gaussian_anomalies, Detector, detector, File),
		logtalk_load(File),
		{detector(LoadedDetector)},
		predict(LoadedDetector, [x-4.50, y-4.20], Prediction).

	test(lof_print_anomaly_detector_1, deterministic) :-
		^^suppress_text_output,
		learn_filtered(gaussian_anomalies, [k(5)], Detector),
		print_anomaly_detector(Detector).

	test(lof_anomaly_detector_missing_training_complete, deterministic) :-
		learn(lof_missing_anomalies, Detector, [k(3)]),
		assertion(lof_anomaly_detector::valid_anomaly_detector(Detector)),
		Detector = lof_detector(_, _, _, _, Instances, References, _, _),
		length(Instances, 4),
		length(References, 4),
		score_all(lof_missing_anomalies, Detector, Scores),
		length(Scores, 4).

	test(lof_anomaly_detector_missing_query, deterministic) :-
		learn(lof_missing_anomalies, Detector, [k(3)]),
		score(Detector, [x-_, y-_], Score),
		assertion(Score >= 0.0),
		assertion(Score =< 1.0).

	test(lof_anomaly_detector_short_neighborhood, deterministic) :-
		learn(lof_missing_anomalies, Detector, [k(20)]),
		score(Detector, [x-0.5, y-_], Score),
		assertion(Score >= 0.0),
		assertion(Score =< 1.0).

	test(lof_anomaly_detector_missing_attribute, error(existence_error(attribute, y))) :-
		learn(lof_missing_anomalies, Detector, [k(3)]),
		score(Detector, [x-0.5], _).

	test(lof_anomaly_detector_unbound_model, error(instantiation_error)) :-
		check_anomaly_detector(_).

	test(lof_anomaly_detector_partial_model, error(instantiation_error)) :-
		check_anomaly_detector(lof_detector(_, [x], [numeric], [1.0], [], [], [])).

	test(lof_anomaly_detector_numeric_model_rejected, deterministic) :-
		learn(lof_singleton_anomalies, Detector),
		Detector = lof_detector(Dataset, Names, Types, Scales, _, References, Cache, Diagnostics),
		Malformed = lof_detector(Dataset, Names, Types, Scales, [1-normal-[oops]], References, Cache, Diagnostics),
		assertion(\+ lof_anomaly_detector::valid_anomaly_detector(Malformed)).

	test(lof_anomaly_detector_numeric_query_rejected, error(type_error(number, oops))) :-
		learn(lof_singleton_anomalies, Detector),
		score(Detector, [x-oops], _).

	test(lof_anomaly_detector_partial_query_rejected, error(instantiation_error)) :-
		learn(lof_singleton_anomalies, Detector),
		score(Detector, [x-value(_)], _).

	test(lof_anomaly_detector_unbound_model_invalid, false) :-
		valid_anomaly_detector(_).

	test(lof_anomaly_detector_standard_training_scores, deterministic) :-
		learn(lof_line_anomalies, Detector, [k(1)]),
		Detector = lof_detector(_, _, _, _, _, [First, Second, Third], _, _),
		assertion(First =~= 1.0),
		assertion(Second =~= 1.0),
		assertion(Third =~= 2.0).

	test(lof_anomaly_detector_standard_fresh_density, deterministic(Raw =~= 1.0)) :-
		learn(lof_line_anomalies, Detector, [k(1)]),
		Detector = lof_detector(_, _, Types, Scales, Instances, _, _, _),
		lof_anomaly_detector << raw_lof([1.2], Types, Scales, Instances, [k(1), distance_metric(euclidean)], Raw).

	test(lof_anomaly_detector_tied_boundary_and_id_invariance, all(check_tied_metric(Metric))) :-
		member(Metric, [euclidean, manhattan, chebyshev, minkowski]).

	test(lof_anomaly_detector_normalization_branches, deterministic) :-
		lof_anomaly_detector << normalize_against_maximum(0.5, 2.0, Below),
		lof_anomaly_detector << normalize_against_maximum(1.5, 1.0, Saturated),
		lof_anomaly_detector << normalize_against_maximum(1.5, 2.0, Middle),
		lof_anomaly_detector << normalize_against_maximum(3.0, 2.0, Clamped),
		assertion(Below =~= 0.0),
		assertion(Saturated =~= 1.0),
		assertion(Middle =~= 0.5),
		assertion(Clamped =~= 1.0).

	test(lof_anomaly_detector_attribute_order, deterministic(First =~= Second)) :-
		learn(lof_missing_anomalies, Detector, [k(1)]),
		score(Detector, [x-0.5, y-2.0], First),
		score(Detector, [y-2.0, x-0.5], Second).

	test(lof_anomaly_detector_cache_matches_uncached, all(check_cached_dataset(Dataset))) :-
		member(Dataset, [lof_line_anomalies, lof_tied_anomalies(false), lof_missing_anomalies, lof_singleton_anomalies, lof_identical_query_fixture]).

	test(lof_anomaly_detector_large_heap_and_scan_equivalence, deterministic) :-
		learn(lof_scaling_anomalies(130), Detector, [k(3)]),
		Detector = lof_detector(_, _, Types, Scales, Instances, _, _, _),
		lof_anomaly_detector << compact_neighbors([0.0, 2.0], Types, Scales, Instances, euclidean, 3, Compact),
		lof_anomaly_detector << k_nearest_neighbors([0.0, 2.0], Types, Scales, Instances, [distance_metric(euclidean)], 3, Full),
		findall(Distance-Id, member(Distance-Id-_-_, Full), Expected),
		assertion(Compact == Expected).

	test(lof_anomaly_detector_rooted_boundary_ties, deterministic) :-
		First is sqrt(1.0),
		Second is sqrt(1.0000000000000002),
		assertion(First == Second),
		lof_anomaly_detector << select_compact_distances([First-1, Second-2], 1, Neighbors),
		assertion(Neighbors == [First-1, Second-2]).

	test(lof_anomaly_detector_independent_oracle_matrix, all(check_independent_oracle(Dataset, K, Metric))) :-
		member(Dataset, [lof_line_anomalies, lof_tied_anomalies(false), lof_identical_query_fixture, lof_duplicate_points]),
		member(K, [1, 2, 5]),
		member(Metric, [euclidean, manhattan, chebyshev, minkowski]).

	test(lof_anomaly_detector_external_dataset_all_scores, all(Score =~= Expected)) :-
		learn(lof_line_anomalies, Detector, [k(1)]),
		score_all(lof_tied_anomalies(false), Detector, Scores),
		length(Scores, 3),
		member(Id-Class-Score, Scores),
		lof_tied_anomalies(false)::example(Id, Class, Values),
		score(Detector, Values, Expected).

	test(lof_anomaly_detector_invalid_training_value, error(type_error(number, oops))) :-
		learn(lof_invalid_numeric, _).

	test(lof_anomaly_detector_duplicate_id_error, error(domain_error(unique_example_id, 1))) :-
		learn(lof_duplicate_ids, _).

	test(lof_anomaly_detector_categorical_query_error, error(type_error(atom, 42))) :-
		learn_filtered(mixed_distance_behaviors, Detector),
		score(Detector, [size-10.0, weight-100.0, color-42, shape-round], _).

	test(lof_anomaly_detector_malformed_cache_rejected, deterministic(\+ lof_anomaly_detector::valid_anomaly_detector(Malformed))) :-
		learn(lof_line_anomalies, Detector),
		Detector = lof_detector(Dataset, Names, Types, Scales, Instances, References, lof_cache(K, Metric, Graph, Index, _), Diagnostics),
		BadCache = lof_cache(K, Metric, Graph, Index, -1.0),
		Malformed = lof_detector(Dataset, Names, Types, Scales, Instances, References, BadCache, Diagnostics).

	test(lof_anomaly_detector_distinct_detectors_independent, deterministic) :-
		learn(lof_line_anomalies, First, [k(1)]),
		score(First, [x-2.5], Before),
		learn(lof_duplicate_points, Second, [k(2)]),
		score(Second, [x-1.0], Duplicate),
		score(First, [x-2.5], After),
		assertion(Before =~= After),
		assertion(Duplicate =~= 0.0).

	test(lof_anomaly_detector_large_training_fallback, deterministic) :-
		learn(lof_scaling_anomalies(257), Detector, [k(3)]),
		assertion(lof_anomaly_detector::valid_anomaly_detector(Detector)),
		score(Detector, [x-0.0, y-2.0], Score),
		assertion(Score >= 0.0),
		assertion(Score =< 1.0).

	test(lof_anomaly_detector_empty_removal, deterministic(Remaining == [])) :-
		lof_anomaly_detector << remove_instance([], 1, Remaining).

	% auxiliary predicates

	check_independent_oracle(Dataset, K, Metric) :-
		findall(Id-X, Dataset::example(Id, _, [x-X]), Points),
		learn(Dataset, Detector, [k(K), distance_metric(Metric)]),
		findall(
			Raw,
			(	member(Id-X, Points),
				oracle_lof(X, Id, Points, K, Raw)
			),
			Expected
		),
		Detector = lof_detector(_, _, Types, Scales, Instances, References, Cache, OptionsData),
		assertion(References =~= Expected),
		memberchk(options(Options), OptionsData),
		forall(
			member(Query, [0.0, 1.0, 1.2, 2.5, 5.0]),
			(	oracle_lof(Query, query, Points, K, Oracle),
				lof_anomaly_detector << cached_raw_score([Query], Types, Scales, Instances, Options, Cache, Actual),
				assertion(Actual =~= Oracle)
			)
		).

	oracle_lof(Query, Exclude, Points, K, Raw) :-
		oracle_neighbors(Query, Exclude, Points, K, Neighbors),
		(	Neighbors == [] ->
			Raw = 1.0
		;	oracle_density(Neighbors, Points, K, Density),
			findall(
				Ratio,
				(	member(_-Id, Neighbors),
					memberchk(Id-Value, Points),
					oracle_neighbors(Value, Id, Points, K, OtherNeighbors),
					oracle_density(OtherNeighbors, Points, K, OtherDensity),
					Ratio is OtherDensity / Density
				),
				Ratios
			),
			oracle_sum(Ratios, 0.0, Sum),
			length(Neighbors, Count),
			Raw is Sum / Count
		).

	oracle_neighbors(Query, Exclude, Points, K, Neighbors) :-
		findall(
			Distance-Id,
			(	member(Id-Value, Points),
				Id \== Exclude,
				Distance is abs(Query - Value)
			),
			Distances
		),
		msort(Distances, Sorted),
		length(Sorted, Count),
		Taken is min(K, Count),
		(	Taken =:= 0 ->
			Neighbors = []
		;	take(Taken, Sorted, Prefix),
			last(Prefix, Boundary-_),
			findall(
				Distance-Id,
				(	member(Distance-Id, Sorted),
					Distance =< Boundary
				),
				Neighbors
			)
		).

	oracle_density([], _Points, _K, 1.0) :-
		!.
	oracle_density(Neighbors, Points, K, Density) :-
		findall(
			Reachability,
			(	member(Distance-Id, Neighbors),
				memberchk(Id-Value, Points),
				oracle_neighbors(Value, Id, Points, K, OtherNeighbors),
				(	OtherNeighbors == [] ->
					Boundary = 0.0
				;	last(OtherNeighbors, Boundary-_)
				),
				Reachability is max(Distance, Boundary)
			),
			Distances
		),
		oracle_sum(Distances, 0.0, Sum),
		length(Neighbors, Count),
		(	Sum =< 0.0 ->
			Density = 1.0e12
		;	Density is Count / Sum
		).

	oracle_sum([], Sum, Sum).
	oracle_sum([Number| Numbers], Sum0, Sum) :-
		Sum1 is Sum0 + Number,
		oracle_sum(Numbers, Sum1, Sum).

	check_cached_dataset(Dataset) :-
		learn(Dataset, Detector, [k(1)]),
		Detector = lof_detector(_, Names, Types, Scales, Instances, References, Cache, Diagnostics),
		memberchk(options(Options), Diagnostics),
		lof_anomaly_detector << training_raw_scores(Types, Scales, Instances, Options, Expected),
		assertion(References =~= Expected),
		forall(
			Dataset::example(_, _, AttributeValues),
			check_cached_query(AttributeValues, Names, Types, Scales, Instances, Options, Cache)
		).

	check_cached_query(AttributeValues, Names, Types, Scales, Instances, Options, Cache) :-
		lof_anomaly_detector << extract_values(Names, AttributeValues, RawValues),
		lof_anomaly_detector << sanitize_input_values(RawValues, Values),
		lof_anomaly_detector << raw_lof(Values, Types, Scales, Instances, Options, Expected),
		lof_anomaly_detector << cached_raw_score(Values, Types, Scales, Instances, Options, Cache, Actual),
		assertion(Actual =~= Expected).

	check_tied_metric(Metric) :-
		learn(lof_tied_anomalies(false), First, [k(1), distance_metric(Metric)]),
		learn(lof_tied_anomalies(true), Second, [k(1), distance_metric(Metric)]),
		score(First, [x-1.0], FirstScore),
		score(Second, [x-1.0], SecondScore),
		assertion(FirstScore =~= 0.125),
		assertion(SecondScore =~= FirstScore).

	count_class([], _Class, 0).
	count_class([_-Class-_| Scores], Class, Count) :-
		!,
		count_class(Scores, Class, Rest),
		Count is Rest + 1.
	count_class([_| Scores], Class, Count) :-
		count_class(Scores, Class, Count).

:- end_object.

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
		comment is 'Unit tests for the "one_class_svm_anomaly_detector" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- private(cached_detector_/2).
	:- dynamic(cached_detector_/2).

	:- uses(one_class_svm_anomaly_detector, [
		anomaly_detector_options/2, check_anomaly_detector/1, diagnostics/2, export_to_clauses/4,
		export_to_file/4, learn/2, learn/3, predict/3, predict/4, print_anomaly_detector/1, score/3,
		score_all/3, valid_anomaly_detector/1
	]).

	cover(one_class_svm_anomaly_detector).

	setup :-
		retractall(cached_detector_(_, _)),
		learn(gaussian_anomalies, Gaussian, [baseline_selection_policy(filter)]),
		assertz(cached_detector_(gaussian, Gaussian)),
		learn(mixed_anomalies, Mixed, [baseline_selection_policy(filter)]),
		assertz(cached_detector_(mixed, Mixed)).

	cleanup :-
		retractall(cached_detector_(_, _)),
		^^clean_file('test_output.pl'),
		^^clean_file('svm_export_linear.pl'),
		^^clean_file('svm_export_polynomial.pl'),
		^^clean_file('svm_export_rbf.pl'),
		^^clean_file('svm_export_pruned.pl').

	test(one_class_svm_learn_2_rejects_labeled_anomalies, error(domain_error(baseline_only_training_data, gaussian_anomalies))) :-
		learn(gaussian_anomalies, _Detector).

	test(one_class_svm_learn_3_gaussian, deterministic(ground(Detector))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter)]).

	test(one_class_svm_cached_mixed_valid, deterministic(valid_anomaly_detector(Detector))) :-
		cached_detector_(mixed, Detector).

	test(one_class_svm_unscaled_features, deterministic(valid_anomaly_detector(Detector))) :-
		learn(svm_small, Detector, [feature_scaling(false)]).

	test(one_class_svm_fixture_cache, deterministic(Names == [gaussian, mixed])) :-
		findall(Name, cached_detector_(Name, _), Names).

	test(one_class_svm_valid_anomaly_detector_1, deterministic(valid_anomaly_detector(Detector))) :-
		learn_gaussian(Detector).

	test(one_class_svm_prunes_zero_coefficients, deterministic((SupportVectorCount < ExampleCount, ReferenceScoreCount == ExampleCount))) :-
		learn_gaussian(one_class_svm_detector(_Encoders, _Kernel, SupportVectors, Coefficients, ReferenceScores, _Cache, Diagnostics)),
		length(SupportVectors, SupportVectorCount),
		length(Coefficients, SupportVectorCount),
		length(ReferenceScores, ReferenceScoreCount),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(support_vectors(SupportVectorCount), Diagnostics).

	test(one_class_svm_support_vector_tolerance, deterministic((PrunedCount < DefaultCount, AnomalyScore > NormalScore))) :-
		learn_gaussian(one_class_svm_detector(_Encoders0, _Kernel0, DefaultSupportVectors, _Coefficients0, _ReferenceScores0, _Cache0, _Diagnostics0)),
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), support_vector_tolerance(0.01)]),
		Detector = one_class_svm_detector(_Encoders, _Kernel, PrunedSupportVectors, _Coefficients, _ReferenceScores, _Cache, _Diagnostics),
		length(DefaultSupportVectors, DefaultCount),
		length(PrunedSupportVectors, PrunedCount),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore).

	test(one_class_svm_support_vector_tolerance_retains_largest, deterministic((SupportVectors = [_], Coefficients = [_], Score >= 0.0, Score =< 1.0))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), support_vector_tolerance(1.0)]),
		Detector = one_class_svm_detector(_Encoders, _Kernel, SupportVectors, Coefficients, _ReferenceScores, _Cache, _Diagnostics),
		valid_anomaly_detector(Detector),
		score(Detector, [x-4.50, y-4.20], Score).

	test(one_class_svm_check_anomaly_detector_1_invalid, error(domain_error(anomaly_detector, one_class_svm_detector([], linear, [], [], [], [])))) :-
		check_anomaly_detector(one_class_svm_detector([], linear, [], [], [], [])).

	test(one_class_svm_wrong_detector_functor, false) :-
		learn_gaussian(Detector),
		Detector =.. [_| Arguments],
		Wrong =.. [wrong_detector| Arguments],
		valid_anomaly_detector(Wrong).

	test(one_class_svm_negative_coefficient, false) :-
		model_with_coefficients([-0.5], Detector),
		valid_anomaly_detector(Detector).

	test(one_class_svm_zero_coefficient_mass, false) :-
		model_with_coefficients([0.0], Detector),
		valid_anomaly_detector(Detector).

	test(one_class_svm_excess_coefficient_mass, false) :-
		model_with_coefficients([2.0], Detector),
		valid_anomaly_detector(Detector).

	test(one_class_svm_coefficient_upper_bound, false) :-
		model_with_coefficients([0.5], Detector),
		valid_anomaly_detector(Detector).

	test(one_class_svm_model_variable, error(instantiation_error)) :-
		check_anomaly_detector(_Detector).

	test(one_class_svm_model_nonground, error(instantiation_error)) :-
		check_anomaly_detector(one_class_svm_detector([], linear, [], [], _, [])).

	test(one_class_svm_unspecified_kernel, error(domain_error(option, kernel(_)))) :-
		learn(svm_small, _Detector, [kernel(_Kernel)]).

	test(one_class_svm_partial_kernel, error(domain_error(option, kernel(rbf(_))))) :-
		learn(svm_small, _Detector, [kernel(rbf(_Gamma))]).

	test(one_class_svm_duplicate_categories, false) :-
		cached_detector_(mixed, Detector),
		Detector = one_class_svm_detector([_| Encoders], Kernel, Rows, Coefficients, References, Cache, Diagnostics),
		Bad = one_class_svm_detector([categorical(age, [yes, yes])| Encoders], Kernel, Rows, Coefficients, References, Cache, Diagnostics),
		valid_anomaly_detector(Bad).

	test(one_class_svm_score_3_gaussian_ordering, deterministic((AnomalyScore > NormalScore, NormalScore >= 0.0, AnomalyScore =< 1.0))) :-
		learn_gaussian(Detector),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore).

	test(one_class_svm_projection_analytical_cases, deterministic) :-
		forall(
			member(Values-Cap-Expected, [
				[-1.0e20, -1.0e20]-1.0-[0.5, 0.5],
				[0.0, 0.0, -1.0e20]-1.0-[0.5, 0.5, 0.0],
				[0.0, -1.0e20, -1.0e20]-0.6-[0.6, 0.2, 0.2],
				[0.0, 0.0]-1.0e100-[0.5, 0.5],
				[0.0, -1.0e20]-0.6-[0.6, 0.4],
				[-1.0e20, 0.0]-0.6-[0.4, 0.6],
				[2.0, -2.0]-1.0-[1.0, 0.0],
				[0.0, 0.0]-0.5-[0.5, 0.5],
				[0.0]-1.0-[1.0]
			]),
			(
				one_class_svm_anomaly_detector<<project_capped_simplex(Values, Cap, Actual),
				check_float_vectors(Actual, Expected),
				one_class_svm_anomaly_detector<<project_capped_simplex(Actual, Cap, Again),
				check_float_vectors(Again, Actual)
			)
		).

	test(one_class_svm_large_value_training_feasible, deterministic(valid_anomaly_detector(Detector))) :-
		learn(svm_large, Detector, [kernel(linear), feature_scaling(false), maximum_iterations(1)]),
		Detector = one_class_svm_detector(_, _, _, Coefficients, _, _, _),
		check_float_vectors(Coefficients, [0.5, 0.5]).

	test(one_class_svm_score_3_mixed, deterministic((AnomalyScore > NormalScore, AnomalyScore =< 1.0))) :-
		cached_detector_(mixed, Detector),
		score(Detector, [age-35, income-60000, student-yes, credit_rating-excellent], NormalScore),
		score(Detector, [age-19, income-150000, student-no, credit_rating-excellent], AnomalyScore).

	test(one_class_svm_score_3_unseen_categorical_value, deterministic((Score >= 0.0, Score =< 1.0))) :-
		cached_detector_(mixed, Detector),
		score(Detector, [age-35, income-60000, student-unknown, credit_rating-excellent], Score).

	test(one_class_svm_linear_kernel, deterministic(ground(Detector))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), kernel(linear)]).

	test(one_class_svm_diagonal_optimum, deterministic(Iterations < 100)) :-
		optimizer_options([], Options),
		one_class_svm_anomaly_detector<<train_model([[100.0, 0.0], [0.0, 400.0]], Options, Coefficients, Iterations, _Delta, Diagnostics),
		check_float_vectors(Coefficients, [0.8, 0.2]),
		Coefficients = [First, Second],
		Objective is (100.0 * First * First + 400.0 * Second * Second) / 2.0,
		assertion(Objective =~= 40.0),
		assertion(list::memberchk(converged(true), Diagnostics)).

	test(one_class_svm_tiny_step_not_converged, deterministic) :-
		optimizer_options([learning_rate(1.0e-30), maximum_iterations(2)], Options),
		one_class_svm_anomaly_detector<<train_model([[100.0, 0.0], [0.0, 400.0]], Options, _Coefficients, 2, _Delta, Diagnostics),
		assertion(list::memberchk(converged(false), Diagnostics)),
		assertion(list::memberchk(stopping_reason(iteration_limit), Diagnostics)).

	test(one_class_svm_zero_matrix, deterministic) :-
		optimizer_options([], Options),
		one_class_svm_anomaly_detector<<train_model([[0.0, 0.0], [0.0, 0.0]], Options, Coefficients, 1, _Delta, Diagnostics),
		check_float_vectors(Coefficients, [0.5, 0.5]),
		assertion(list::memberchk(converged(true), Diagnostics)).

	test(one_class_svm_large_step_and_duplicate_options, deterministic) :-
		optimizer_options([learning_rate(1000.0), learning_rate(1.0e-30)], Options),
		one_class_svm_anomaly_detector<<train_model([[100.0, 0.0], [0.0, 400.0]], Options, Coefficients, _Iterations, _Delta, _Diagnostics),
		check_float_vectors(Coefficients, [0.8, 0.2]).

	test(one_class_svm_nu_one, deterministic(valid_anomaly_detector(Detector))) :-
		learn(svm_small, Detector, [nu(1.0)]),
		Detector = one_class_svm_detector(_, _, _, Coefficients, _, _, Diagnostics),
		check_float_vectors(Coefficients, [0.5, 0.5]),
		assertion(list::memberchk(converged(true), Diagnostics)).

	test(one_class_svm_centered_linear_degenerate, deterministic) :-
		learn(svm_centered, Detector, [kernel(linear)]),
		score(Detector, [x-0.0], Center),
		score(Detector, [x-100.0], Distant),
		assertion(Center =~= 0.0),
		assertion(Distant =~= 0.0),
		diagnostics(Detector, Diagnostics),
		assertion(list::memberchk(decision_geometry(zero_linear), Diagnostics)).

	test(one_class_svm_uncentered_linear_ordering, deterministic(Anomaly > Normal)) :-
		learn(svm_small, Detector, [kernel(linear), feature_scaling(false)]),
		score(Detector, [x-2.0], Normal),
		score(Detector, [x-0.0], Anomaly).

	test(one_class_svm_invalid_optimality_tolerance, error(domain_error(option, optimality_tolerance(0.0)))) :-
		learn(svm_small, _Detector, [optimality_tolerance(0.0)]).

	test(one_class_svm_polynomial_kernel, deterministic(ground(Detector))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), kernel(polynomial(2, 0.5, 1.0))]).

	test(one_class_svm_fused_kernel_oracle, all(Actual =~= Expected)) :-
		member(Kernel, [linear, polynomial(2, 0.5, 1.0), rbf(0.5)]),
		member(Features, [[0.0, 0.0], [1.0, 0.0], [0.5, -0.25], [-1.0, 1.0]]),
		Rows = [[1.0, 0.0], [-1.0, 1.0]],
		Coefficients = [0.25, 0.75],
		oracle_kernel_vector(Rows, Features, Kernel, Values),
		linear_algebra::dot_product(Coefficients, Values, Expected),
		one_class_svm_anomaly_detector<<weighted_kernel_score(Rows, Coefficients, Features, Kernel, 0.0, Actual).

	test(one_class_svm_missing_features_and_numeric_type, deterministic) :-
		cached_detector_(mixed, Detector),
		score(Detector, [age-_, income-60000, student-_, credit_rating-unknown], Missing),
		score(Detector, [], Absent),
		assertion(Missing >= 0.0),
		assertion(Absent =< 1.0).

	test(one_class_svm_numeric_type_error, error(type_error(number, invalid))) :-
		learn_gaussian(Detector),
		score(Detector, [x-invalid], _Score).

	test(one_class_svm_symmetric_matrix_oracle, all(check_float_matrices(Actual, Expected))) :-
		member(Kernel, [linear, polynomial(2, 0.5, 1.0), rbf(0.5)]),
		member(Rows, [[], [[1.0]], [[1.0, 0.0], [-1.0, 1.0], [0.5, -0.25]]]),
		one_class_svm_anomaly_detector<<build_kernel_matrix(Rows, Kernel, Actual),
		oracle_matrix(Rows, Rows, Kernel, Expected).

	test(one_class_svm_pruned_reference_oracle, all(check_float_vectors(References, Expected))) :-
		member(Tolerance, [0.0, 0.01, 1.0]),
		learn(svm_small, Detector, [support_vector_tolerance(Tolerance)]),
		Detector = one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, _, _),
		findall(
			Raw,
			(	svm_small::example(_, _, Instance),
				one_class_svm_anomaly_detector<<encode_instance(Encoders, Instance, Features),
				oracle_kernel_vector(Rows, Features, Kernel, Values),
				linear_algebra::dot_product(Coefficients, Values, Raw)
			),
			Expected
		).

	test(one_class_svm_largest_support_vector_exact, deterministic) :-
		Rows = [[1.0], [2.0], [3.0]],
		one_class_svm_anomaly_detector<<select_support_vectors(Rows, [0.1, 0.7, 0.2], 1.0, Selected, Coefficients),
		assertion(Selected == [[2.0]]),
		check_float_vectors(Coefficients, [0.7]),
		one_class_svm_anomaly_detector<<effective_coefficient_vector([0.1, 0.7, 0.2], 1.0, Effective),
		check_float_vectors(Effective, [0.0, 0.7, 0.0]),
		one_class_svm_anomaly_detector<<effective_coefficient_vector([0.5, 0.5], 1.0, Tied),
		check_float_vectors(Tied, [0.5, 0.0]).

	test(one_class_svm_inverse_scaling_learning_schedule, deterministic((AnomalyScore > NormalScore, memberchk(learning_schedule(inverse_scaling(0.5)), Options)))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), learning_schedule(inverse_scaling(0.5))]),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore),
		anomaly_detector_options(Detector, Options).

	test(one_class_svm_invalid_nu, error(domain_error(option, nu(0.0)))) :-
		learn(gaussian_anomalies, _Detector, [nu(0.0)]).

	test(one_class_svm_invalid_learning_schedule, error(domain_error(option, learning_schedule(inverse_scaling(0.0))))) :-
		learn(gaussian_anomalies, _Detector, [learning_schedule(inverse_scaling(0.0))]).

	test(one_class_svm_invalid_support_vector_tolerance, error(domain_error(option, support_vector_tolerance(-0.01)))) :-
		learn(gaussian_anomalies, _Detector, [support_vector_tolerance(-0.01)]).

	test(one_class_svm_predict_3_gaussian_normal, deterministic(Prediction == normal)) :-
		learn_gaussian(Detector),
		predict(Detector, [x-0.12, y-0.34], Prediction).

	test(one_class_svm_predict_3_gaussian_anomaly, deterministic(Prediction == anomaly)) :-
		learn_gaussian(Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction).

	test(one_class_svm_predict_4_threshold_override, deterministic(Prediction == anomaly)) :-
		learn_gaussian(Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.5)]).

	test(one_class_svm_score_all_3_sorted, deterministic((length(Scores, 48), FirstScore >= SecondScore))) :-
		learn_gaussian(Detector),
		score_all(gaussian_anomalies, Detector, Scores),
		Scores = [_-_-FirstScore, _-_-SecondScore| _],
		check_scored_dataset(gaussian_anomalies, Detector, Scores).

	test(one_class_svm_score_all_external_ties, deterministic) :-
		learn_gaussian(Detector),
		score_all(svm_small, Detector, Scores),
		check_scored_dataset(svm_small, Detector, Scores).

	test(one_class_svm_diagnostics_2, deterministic((memberchk(model(one_class_svm_anomaly_detector), Diagnostics), memberchk(kernel(rbf(0.5)), Diagnostics), memberchk(example_count(40), Diagnostics)))) :-
		learn_gaussian(Detector),
		diagnostics(Detector, Diagnostics).

	test(one_class_svm_anomaly_detector_options_2, deterministic((memberchk(nu(0.2), Options), memberchk(baseline_selection_policy(filter), Options)))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), nu(0.2)]),
		anomaly_detector_options(Detector, Options).

	test(one_class_svm_export_to_clauses_4, deterministic(ExportedDetector == Detector)) :-
		learn_gaussian(Detector),
		export_to_clauses(gaussian_anomalies, Detector, detector, [detector(ExportedDetector)]).

	test(one_class_svm_export_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn_gaussian(Detector),
		export_to_file(gaussian_anomalies, Detector, detector, File),
		logtalk_load(File),
		{detector(LoadedDetector)},
		predict(LoadedDetector, [x-4.50, y-4.20], Prediction).

	test(one_class_svm_print_anomaly_detector_1, deterministic) :-
		^^suppress_text_output,
		learn_gaussian(Detector),
		print_anomaly_detector(Detector).

	test(one_class_svm_linear_gradient_oracle, deterministic) :-
		Rows = [[1.0, 0.0], [-1.0, 1.0], [0.5, -0.25]],
		Coefficients = [0.2, 0.3, 0.5],
		oracle_matrix(Rows, Rows, linear, Matrix),
		one_class_svm_anomaly_detector<<problem_gradient(dense(Matrix), Coefficients, Expected),
		one_class_svm_anomaly_detector<<problem_gradient(linear(Rows), Coefficients, Actual),
		check_float_vectors(Actual, Expected).

	test(one_class_svm_wrong_cache, false) :-
		learn_gaussian(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, _, Diagnostics)),
		valid_anomaly_detector(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, svm_cache(linear_weights([]), 40, reference_list(References)), Diagnostics)).

	test(one_class_svm_legacy_model_rejected, false) :-
		learn_gaussian(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, _, Diagnostics)),
		valid_anomaly_detector(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, Diagnostics)).

	test(one_class_svm_cached_export_roundtrips, all(check_export_roundtrip(Name, Options))) :-
		member(Name-Options, [
			svm_export_linear-[kernel(linear), feature_scaling(false)],
			svm_export_polynomial-[kernel(polynomial(2, 0.5, 1.0))],
			svm_export_rbf-[kernel(rbf(0.5))],
			svm_export_pruned-[support_vector_tolerance(1.0)]
		]).

	test(one_class_svm_rank_index_exact_oracle, all(Actual == Expected)) :-
		member(References, [[0.0], [0.5, 0.5], [0.0, 0.5, 1.0], [1.0, 0.5, 0.5, 0.0], [-0.0, 0.0]]),
		member(Query, [-1.0, 0.0, 0.25, 0.5, 0.75, 1.0, 2.0]),
		one_class_svm_anomaly_detector<<build_rank_index(References, Index),
		one_class_svm_anomaly_detector<<rank_greater(Index, Query, 0, Actual),
		oracle_greater(References, Query, 0, Expected).

	test(one_class_svm_rank_index_balanced, deterministic(Depth == 8)) :-
		range_references(255, References),
		one_class_svm_anomaly_detector<<build_rank_index(References, Index),
		rank_depth(Index, Depth),
		one_class_svm_anomaly_detector<<rank_greater(Index, 128.0, 0, Count),
		assertion(Count == 127).

	test(one_class_svm_corrupt_rank_cache, all(\+ one_class_svm_anomaly_detector::valid_anomaly_detector(Bad))) :-
		learn_gaussian(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, svm_cache(Scorer, Count, Index), Diagnostics)),
		Index = rank_node(Key, Multiplicity, Total, Left, Right),
		WrongCount is Count + 1,
		WrongTotal is Total + 1,
		member(Cache, [
			svm_cache(Scorer, WrongCount, Index),
			svm_cache(Scorer, Count, rank_empty),
			svm_cache(Scorer, Count, rank_node(Key, -1, Total, Left, Right)),
			svm_cache(Scorer, Count, rank_node(Key, Multiplicity, WrongTotal, Left, Right)),
			svm_cache(Scorer, Count, rank_node(Key, Multiplicity, Total, Right, Left))
		]),
		Bad = one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, Cache, Diagnostics).

	test(one_class_svm_projection_residual_correction, deterministic) :-
		one_class_svm_anomaly_detector<<repair_projection([0.0, -10.0], 0.6, -10.0, [0.6, 0.0], 0.4, Added),
		check_float_vectors(Added, [0.6, 0.4]),
		one_class_svm_anomaly_detector<<repair_projection([0.0, -10.0], 1.0, 0.0, [0.6, 0.6], -0.2, Removed),
		check_float_vectors(Removed, [0.4, 0.6]).

	test(one_class_svm_optimizer_objective_nonincrease, deterministic) :-
		check_objective_iterations(1, 10, 62.5).

	test(one_class_svm_malformed_rows_and_diagnostics, all(\+ one_class_svm_anomaly_detector::valid_anomaly_detector(Bad))) :-
		learn_gaussian(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, Cache, Diagnostics)),
		member(Bad, [
			one_class_svm_detector(Encoders, Kernel, [[]], Coefficients, References, Cache, Diagnostics),
			one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, [], Cache, Diagnostics),
			one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, Cache, [])
		]).

	test(one_class_svm_linear_cache_weights_checked, false) :-
		learn(svm_small, one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, svm_cache(_, Count, Index), Diagnostics), [kernel(linear), feature_scaling(false)]),
		valid_anomaly_detector(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, svm_cache(linear_weights([100.0, 0.0]), Count, Index), Diagnostics)).

	test(one_class_svm_invalid_optimization_diagnostics, all(\+ one_class_svm_anomaly_detector::valid_anomaly_detector(Bad))) :-
		learn_gaussian(one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, Cache, Diagnostics)),
		member(Replacement, [iterations(0), final_delta(-1.0), optimality_residual(-1.0), step_limit(0.0), converged(invalid), stopping_reason(invalid), decision_geometry(zero_linear)]),
		replace_diagnostic(Replacement, Diagnostics, BadDiagnostics),
		Bad = one_class_svm_detector(Encoders, Kernel, Rows, Coefficients, References, Cache, BadDiagnostics).

	% auxiliary predicates

	learn_gaussian(Detector) :-
		cached_detector_(gaussian, Detector).

	check_objective_iterations(Iteration, Maximum, _Previous) :-
		Iteration > Maximum,
		!.
	check_objective_iterations(Iteration, Maximum, Previous) :-
		optimizer_options([maximum_iterations(Iteration)], Options),
		one_class_svm_anomaly_detector<<train_model([[100.0, 0.0], [0.0, 400.0]], Options, [First, Second], _, _, _),
		Objective is (100.0 * First * First + 400.0 * Second * Second) / 2.0,
		assertion(Objective =< Previous + 1.0e-10),
		Mass is First + Second,
		assertion(Mass =~= 1.0),
		Next is Iteration + 1,
		check_objective_iterations(Next, Maximum, Objective).

	oracle_greater([], _Query, Count, Count).
	oracle_greater([Value| Values], Query, Count0, Count) :-
		(	Value > Query ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		oracle_greater(Values, Query, Count1, Count).

	range_references(0, []) :-
		!.
	range_references(Count, [Value| Values]) :-
		Value is float(Count),
		Next is Count - 1,
		range_references(Next, Values).

	rank_depth(rank_empty, 0).
	rank_depth(rank_node(_, _, _, Left, Right), Depth) :-
		rank_depth(Left, LeftDepth),
		rank_depth(Right, RightDepth),
		Depth is 1 + max(LeftDepth, RightDepth).

	oracle_matrix([], _AllRows, _Kernel, []).
	oracle_matrix([Row| Rows], AllRows, Kernel, [Values| Matrix]) :-
		oracle_kernel_vector(AllRows, Row, Kernel, Values),
		oracle_matrix(Rows, AllRows, Kernel, Matrix).

	check_float_matrices([], []).
	check_float_matrices([Actual| Actuals], [Expected| Expecteds]) :-
		check_float_vectors(Actual, Expected),
		check_float_matrices(Actuals, Expecteds).

	oracle_kernel_vector([], _Features, _Kernel, []).
	oracle_kernel_vector([Row| Rows], Features, Kernel, [Value| Values]) :-
		oracle_kernel_value(Kernel, Row, Features, Value),
		oracle_kernel_vector(Rows, Features, Kernel, Values).

	oracle_kernel_value(linear, Row, Features, Value) :-
		linear_algebra::dot_product(Row, Features, Value).
	oracle_kernel_value(polynomial(Degree, Gamma, Offset), Row, Features, Value) :-
		linear_algebra::dot_product(Row, Features, Dot),
		Value is (Gamma * Dot + Offset) ** Degree.
	oracle_kernel_value(rbf(Gamma), Row, Features, Value) :-
		linear_algebra::subtract_vectors(Row, Features, Difference),
		linear_algebra::dot_product(Difference, Difference, Squared),
		Value is exp(-Gamma * Squared).

	optimizer_options(Updates, Options) :-
		learn_gaussian(Detector),
		anomaly_detector_options(Detector, Defaults),
		apply_option_updates(Updates, Defaults, Options).

	apply_option_updates([], Options, Options).
	apply_option_updates([Update| Updates], Options0, [Update| Options]) :-
		apply_option_updates(Updates, Options0, Options).

	check_float_vectors([], []).
	check_float_vectors([Actual| Actuals], [Expected| Expecteds]) :-
		assertion(Actual =~= Expected),
		check_float_vectors(Actuals, Expecteds).

	model_with_coefficients(Coefficients, Detector) :-
		learn_gaussian(one_class_svm_detector(Encoders, Kernel, [Row| _], _Coefficients, References, Cache, Diagnostics0)),
		replace_diagnostic(support_vectors(1), Diagnostics0, Diagnostics),
		Detector = one_class_svm_detector(Encoders, Kernel, [Row], Coefficients, References, Cache, Diagnostics).

	check_export_roundtrip(Name, Options) :-
		learn(svm_small, Detector, Options),
		atom_concat(Name, '.pl', Basename),
		^^file_path(Basename, File),
		export_to_file(svm_small, Detector, Name, File),
		logtalk_load(File),
		Goal =.. [Name, Restored],
		{Goal},
		assertion(Restored == Detector),
		valid_anomaly_detector(Restored),
		score(Detector, [x-0.0], Expected),
		score(Restored, [x-0.0], Actual),
		assertion(Actual == Expected).

	replace_diagnostic(Replacement, [Diagnostic| Diagnostics], [Replacement| Diagnostics]) :-
		functor(Replacement, Name, Arity),
		functor(Diagnostic, Name, Arity),
		!.
	replace_diagnostic(Replacement, [Diagnostic| Diagnostics0], [Diagnostic| Diagnostics]) :-
		replace_diagnostic(Replacement, Diagnostics0, Diagnostics).

	check_scored_dataset(Dataset, Detector, Scores) :-
		findall(Id-Class, Dataset::example(Id, Class, _), Expected),
		findall(Id-Class, member(Id-Class-_, Scores), Actual),
		msort(Expected, SortedExpected),
		msort(Actual, SortedActual),
		assertion(SortedExpected == SortedActual),
		check_score_order(Scores),
		forall(
			member(Id-Class-Score, Scores),
			(
				Dataset::example(Id, Class, Instance),
				score(Detector, Instance, ExpectedScore),
				assertion(Score =~= ExpectedScore)
			)
		).

	check_score_order([]).
	check_score_order([_]) :-
		!.
	check_score_order([_-_-First, Id-Class-Second| Scores]) :-
		assertion(First >= Second),
		check_score_order([Id-Class-Second| Scores]).

:- end_object.

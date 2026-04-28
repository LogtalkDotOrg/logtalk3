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


:- object(invalid_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score_feature, continuous).

	target(score).

	example(1, [channel-online, score_feature-1.0]).
	example(2, [channel-retail, score_feature-2.0]).

	example(1, 1.0, [channel-online, score_feature-1.0]).
	example(2, 2.0, [channel-retail, score_feature-2.0]).

:- end_object.


:- object(duplicate_attribute_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f1-1.1, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

	example(1, 1.0, [f1-1.0, f1-1.1, f2-2.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).

:- end_object.


:- object(undeclared_attribute_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0, junk-9.0]).
	example(2, [f1-2.0, f2-4.0]).

	example(1, 1.0, [f1-1.0, f2-2.0, junk-9.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

	example(1, 1.0, [f1-1.0, f2-2.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).

:- end_object.


:- object(constant_target_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).
	example(3, [f1-3.0, f2-6.0]).

	example(1, 7.0, [f1-1.0, f2-2.0]).
	example(2, 7.0, [f1-2.0, f2-4.0]).
	example(3, 7.0, [f1-3.0, f2-6.0]).

:- end_object.


:- object(target_leakage_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(score, continuous).
	attribute_values(f1, continuous).

	target(score).

	example(1, [score-1.0, f1-1.0]).
	example(2, [score-2.0, f1-2.0]).

	example(1, 1.0, [score-1.0, f1-1.0]).
	example(2, 2.0, [score-2.0, f1-2.0]).

:- end_object.


:- object(shortfall_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).
	example(3, [f1-3.0, f2-6.0]).

	example(1, 1.0, [f1-1.0, f2-2.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).
	example(3, 3.0, [f1-3.0, f2-6.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Unit tests for the "pls_projection" library.'
	]).

	:- uses(lgtunit, [
		assertion/1, tolerance_equal/4
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(pls_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	test(pls_projection_learn_2_target_latent_measurements, deterministic(ground(DimensionReducer))) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer).

	test(pls_projection_learn_2_structure, deterministic(functor(DimensionReducer, pls_projection_reducer, 3))) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer).

	test(pls_projection_learn_3_custom_options, deterministic) :-
		pls_projection::learn(target_latent_measurements, pls_projection_reducer(_Encoders, Rotations, Diagnostics), [n_components(1), feature_scaling(false), tolerance(1.0e-10)]),
		assertion(ground(Rotations)),
		assertion(length(Rotations, 1)),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(n_components(1), Options)),
		assertion(memberchk(feature_scaling(false), Options)),
		assertion(memberchk(tolerance(1.0e-10), Options)),
		assertion(memberchk(model(pls_projection), Diagnostics)).

	test(pls_projection_learn_3_shortfall_policy_truncate, deterministic) :-
		pls_projection::learn(shortfall_pls_dataset, pls_projection_reducer(_Encoders, Rotations, Diagnostics), [n_components(2), feature_scaling(false), shortfall_policy(truncate)]),
		assertion(length(Rotations, 1)),
		assertion(memberchk(component_count(1), Diagnostics)),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(shortfall_policy(truncate), Options)),
		memberchk(shortfall(truncated(RequestedComponentCount, LearnedComponentCount, ScoreEnergy, Tolerance)), Diagnostics),
		assertion(RequestedComponentCount == 2),
		assertion(LearnedComponentCount == 1),
		assertion(ScoreEnergy >= 0.0),
		assertion(Tolerance =:= 1.0e-8).

	test(pls_projection_learn_3_shortfall_policy_error, error(domain_error(component_count, 2-1))) :-
		pls_projection::learn(shortfall_pls_dataset, _DimensionReducer, [n_components(2), feature_scaling(false), shortfall_policy(error)]).

	test(pls_projection_learn_3_shortfall_policy_unbound_value, error(domain_error(option, shortfall_policy(_)))) :-
		pls_projection::learn(target_latent_measurements, _DimensionReducer, [shortfall_policy(_Policy)]).

	test(pls_projection_learn_2_component_count_exceeds_shape, error(domain_error(component_count, 5-4))) :-
		pls_projection::learn(target_latent_measurements, _DimensionReducer, [n_components(5)]).

	test(pls_projection_check_dimension_reducer_1, deterministic) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
		pls_projection::check_dimension_reducer(DimensionReducer).

	test(pls_projection_diagnostics_2, deterministic) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
		pls_projection::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(model(pls_projection), Diagnostics)),
		assertion(memberchk(sample_count(6), Diagnostics)),
		assertion(memberchk(target(score), Diagnostics)),
		assertion(memberchk(component_count(2), Diagnostics)),
		assertion(memberchk(preprocessing([center(true), feature_scaling(true)]), Diagnostics)),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(shortfall_policy(truncate), Options)),
		memberchk(target_loadings(TargetLoadings), Diagnostics),
		assertion(length(TargetLoadings, 2)),
		assertion(\+ memberchk(shortfall(_), Diagnostics)).

	test(pls_projection_transform_3_component_names, deterministic) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer),
		pls_projection::transform(DimensionReducer, [f1-4.0, f2-8.0, f3-2.0, f4-(-2.0)], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		assertion(memberchk(component_1-_, ReducedInstance)),
		assertion(memberchk(component_2-_, ReducedInstance)).

	test(pls_projection_transform_3_target_ordering, deterministic(HighScore > LowScore)) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(1), feature_scaling(false)]),
		pls_projection::transform(DimensionReducer, [f1-1.0, f2-2.0, f3-0.0, f4-0.0], [component_1-LowScore]),
		pls_projection::transform(DimensionReducer, [f1-6.0, f2-12.0, f3-1.0, f4-(-1.0)], [component_1-HighScore]).

	test(pls_projection_transform_3_second_component_sensitivity, deterministic(SecondA =\= SecondB)) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2), feature_scaling(false)]),
		pls_projection::transform(DimensionReducer, [f1-3.0, f2-6.0, f3-(-1.0), f4-1.0], [component_1-_, component_2-SecondA]),
		pls_projection::transform(DimensionReducer, [f1-3.0, f2-6.0, f3-2.0, f4-(-2.0)], [component_1-_, component_2-SecondB]).

	test(pls_projection_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
		pls_projection::export_to_clauses(target_latent_measurements, DimensionReducer, reduced, [Clause]).

	test(pls_projection_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
		pls_projection::export_to_file(target_latent_measurements, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		Reducer = pls_projection_reducer(_Encoders, Rotations, _Diagnostics),
		assertion(ground(Rotations)),
		assertion(length(Rotations, 2)).

	test(pls_projection_transform_3_exported_functor, deterministic) :-
		^^file_path('test_output.pl', File),
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2), feature_scaling(false)]),
		pls_projection::transform(DimensionReducer, [f1-4.0, f2-8.0, f3-2.0, f4-(-2.0)], [component_1-Score1, component_2-Score2]),
		pls_projection::export_to_file(target_latent_measurements, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		pls_projection::transform(Reducer, [f1-4.0, f2-8.0, f3-2.0, f4-(-2.0)], [component_1-ExportedScore1, component_2-ExportedScore2]),
		assertion(tolerance_equal(Score1, ExportedScore1, 1.0e-12, 1.0e-12)),
		assertion(tolerance_equal(Score2, ExportedScore2, 1.0e-12, 1.0e-12)).

	test(pls_projection_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		pls_projection::learn(target_latent_measurements, DimensionReducer),
		pls_projection::print_dimension_reducer(DimensionReducer).

	test(pls_projection_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, f1))) :-
		pls_projection::learn(duplicate_attribute_pls_dataset, _DimensionReducer).

	test(pls_projection_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		pls_projection::learn(undeclared_attribute_pls_dataset, _DimensionReducer).

	test(pls_projection_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, f1))) :-
		pls_projection::learn(duplicate_attribute_declaration_pls_dataset, _DimensionReducer).

	test(pls_projection_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, f1))) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
		pls_projection::transform(DimensionReducer, [f1-3.0, f1-3.1, f2-6.0, f3-(-1.0), f4-1.0], _ReducedInstance).

	test(pls_projection_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
		pls_projection::transform(DimensionReducer, [f1-3.0, f2-6.0, f3-(-1.0), f4-1.0, junk-9.0], _ReducedInstance).

	test(pls_projection_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		pls_projection::learn(invalid_pls_dataset, _DimensionReducer).

	test(pls_projection_learn_2_target_leakage, error(domain_error(target_attribute, score))) :-
		pls_projection::learn(target_leakage_pls_dataset, _DimensionReducer).

	test(pls_projection_learn_2_constant_target, error(domain_error(target_variance, score))) :-
		pls_projection::learn(constant_target_pls_dataset, _DimensionReducer).

:- end_object.

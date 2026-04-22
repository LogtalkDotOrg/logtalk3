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


:- object(invalid_lda_dataset,
	implements(supervised_dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	class(label).

	class_values([positive, negative]).

	example(1, positive, [channel-online, score-1.0]).
	example(2, negative, [channel-retail, score-2.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Unit tests for the "lda_projection" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(lda_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	test(lda_projection_learn_2_labeled_measurements, deterministic(ground(DimensionReducer))) :-
		lda_projection::learn(labeled_measurements, DimensionReducer).

	test(lda_projection_learn_2_structure, deterministic(functor(DimensionReducer, lda_projection_reducer, 4))) :-
		lda_projection::learn(labeled_measurements, DimensionReducer).

	test(lda_projection_learn_3_custom_options, deterministic((length(Components, 2), ClassValues == [alpha, beta, gamma], memberchk(n_components(4), Options), memberchk(feature_scaling(false), Options), memberchk(regularization(1.0e-5), Options)))) :-
		lda_projection::learn(labeled_measurements, lda_projection_reducer(_Encoders, Components, ClassValues, Options), [n_components(4), feature_scaling(false), maximum_iterations(250), tolerance(1.0e-7), regularization(1.0e-5)]).

	test(lda_projection_transform_3_component_names, deterministic((length(ReducedInstance, 2), memberchk(component_1-_, ReducedInstance), memberchk(component_2-_, ReducedInstance)))) :-
		lda_projection::learn(labeled_measurements, DimensionReducer),
		lda_projection::transform(DimensionReducer, [length-5.1, width-3.5, height-1.4, weight-0.2], ReducedInstance).

	test(lda_projection_transform_3_class_separation, deterministic(AlphaScore =\= GammaScore)) :-
		lda_projection::learn(labeled_measurements, DimensionReducer),
		lda_projection::transform(DimensionReducer, [length-5.1, width-3.5, height-1.4, weight-0.2], [component_1-AlphaScore| _]),
		lda_projection::transform(DimensionReducer, [length-6.2, width-3.4, height-5.4, weight-2.3], [component_1-GammaScore| _]).

	test(lda_projection_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::export_to_clauses(labeled_measurements, DimensionReducer, reduced, [Clause]).

	test(lda_projection_export_to_file_4, deterministic((Reducer = lda_projection_reducer(_Encoders, Components, ClassValues, _Options), length(Components, 1), ClassValues == [alpha, beta, gamma]))) :-
		^^file_path('test_output.pl', File),
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::export_to_file(labeled_measurements, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)}.

	test(lda_projection_transform_3_exported_functor, deterministic(memberchk(component_1-_, ReducedInstance))) :-
		^^file_path('test_output.pl', File),
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::export_to_file(labeled_measurements, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		lda_projection::transform(Reducer, [length-5.1, width-3.5, height-1.4, weight-0.2], ReducedInstance).

	test(lda_projection_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		lda_projection::learn(labeled_measurements, DimensionReducer),
		lda_projection::print_dimension_reducer(DimensionReducer).

	test(lda_projection_learn_2_invalid_dataset, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		lda_projection::learn(invalid_lda_dataset, _DimensionReducer).

:- end_object.

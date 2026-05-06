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


:- object(duplicate_attribute_lda_dataset,
	implements(supervised_dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	class(label).
	class_values([left, right]).

	example(1, left, [x-1.0, x-1.1, y-2.0]).
	example(2, right, [x-(-1.0), y-(-2.0)]).

:- end_object.


:- object(undeclared_attribute_lda_dataset,
	implements(supervised_dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	class(label).
	class_values([left, right]).

	example(1, left, [x-1.0, y-2.0, junk-9.0]).
	example(2, right, [x-(-1.0), y-(-2.0)]).

:- end_object.


:- object(duplicate_attribute_declaration_lda_dataset,
	implements(supervised_dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(x, continuous).
	attribute_values(y, continuous).

	class(label).
	class_values([left, right]).

	example(1, left, [x-1.0, y-2.0]).
	example(2, right, [x-(-1.0), y-(-2.0)]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "lda_projection" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
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

	test(lda_projection_learn_3_custom_options, deterministic) :-
		lda_projection::learn(labeled_measurements, lda_projection_reducer(_Encoders, Components, ClassValues, Diagnostics), [n_components(2), feature_scaling(false), maximum_iterations(250), tolerance(1.0e-7), regularization(1.0e-5)]),
		assertion(ground(Components)),
		assertion(length(Components, 2)),
		assertion(ClassValues == [alpha, beta, gamma]),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(n_components(2), Options)),
		assertion(memberchk(feature_scaling(false), Options)),
		assertion(memberchk(maximum_iterations(250), Options)),
		assertion(memberchk(tolerance(1.0e-7), Options)),
		assertion(memberchk(regularization(1.0e-5), Options)),
		assertion(memberchk(model(lda_projection), Diagnostics)).

	test(lda_projection_learn_2_component_count_exceeds_shape, error(domain_error(component_count, 4-2))) :-
		lda_projection::learn(labeled_measurements, _DimensionReducer, [n_components(4), feature_scaling(false), maximum_iterations(250), tolerance(1.0e-7), regularization(1.0e-5)]).

	test(lda_projection_check_dimension_reducer_1, deterministic) :-
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::check_dimension_reducer(DimensionReducer).

	test(lda_projection_diagnostics_2, deterministic) :-
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(model(lda_projection), Diagnostics)),
		assertion(memberchk(class_values([alpha, beta, gamma]), Diagnostics)),
		assertion(memberchk(component_count(1), Diagnostics)).

	test(lda_projection_transform_3_component_names, deterministic) :-
		lda_projection::learn(labeled_measurements, DimensionReducer),
		lda_projection::transform(DimensionReducer, [length-5.1, width-3.5, height-1.4, weight-0.2], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		assertion(memberchk(component_1-_, ReducedInstance)),
		assertion(memberchk(component_2-_, ReducedInstance)).

	test(lda_projection_transform_3_class_separation, deterministic(AlphaScore =\= GammaScore)) :-
		lda_projection::learn(labeled_measurements, DimensionReducer),
		lda_projection::transform(DimensionReducer, [length-5.1, width-3.5, height-1.4, weight-0.2], [component_1-AlphaScore| _]),
		lda_projection::transform(DimensionReducer, [length-6.2, width-3.4, height-5.4, weight-2.3], [component_1-GammaScore| _]).

	test(lda_projection_learn_2_anti_diagonal_singletons_regression, deterministic) :-
		lda_projection::learn(anti_diagonal_singletons, DimensionReducer, [n_components(1), feature_scaling(false), regularization(1.0e-6)]),
		lda_projection::transform(DimensionReducer, [x-(-1.0), y-1.0], [component_1-Score1]),
		lda_projection::transform(DimensionReducer, [x-1.0, y-(-1.0)], [component_1-Score2]),
		assertion(Score1 =\= 0.0),
		assertion(Score2 =\= 0.0),
		assertion(Score1 * Score2 < 0.0).

	test(lda_projection_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::export_to_clauses(labeled_measurements, DimensionReducer, reduced, [Clause]).

	test(lda_projection_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::export_to_file(labeled_measurements, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		Reducer = lda_projection_reducer(_Encoders, Components, ClassValues, _Diagnostics),
		assertion(ground(Components)),
		assertion(length(Components, 1)),
		assertion(ClassValues == [alpha, beta, gamma]).

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

	test(lda_projection_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, x))) :-
		lda_projection::learn(duplicate_attribute_lda_dataset, _DimensionReducer).

	test(lda_projection_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		lda_projection::learn(undeclared_attribute_lda_dataset, _DimensionReducer).

	test(lda_projection_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		lda_projection::learn(duplicate_attribute_declaration_lda_dataset, _DimensionReducer).

	test(lda_projection_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, length))) :-
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::transform(DimensionReducer, [length-5.1, length-5.2, width-3.5, height-1.4, weight-0.2], _ReducedInstance).

	test(lda_projection_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
		lda_projection::transform(DimensionReducer, [length-5.1, width-3.5, height-1.4, weight-0.2, junk-9.0], _ReducedInstance).

	test(lda_projection_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		lda_projection::learn(invalid_lda_dataset, _DimensionReducer).

:- end_object.

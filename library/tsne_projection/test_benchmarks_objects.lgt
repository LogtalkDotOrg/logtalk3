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


:- object(tsne_projection_benchmarks,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Focused portable benchmarks for the "tsne_projection" library.'
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Prints median CPU and wall times from five measurements after one warm-up.'
	]).

	:- private(measure/2).
	:- meta_predicate(measure(*, 0)).

	:- private(repeat_goal/2).
	:- meta_predicate(repeat_goal(*, 0)).

	:- private(samples/4).
	:- meta_predicate(samples(*, 0, *, *)).

	:- uses(os, [
		cpu_time/1, wall_time/1
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		nth1/3, msort/2, member/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	run :-
		rows(100, 2, Rows),
		tsne_projection << squared_distance_matrix(Rows, Distances),
		tsne_projection << conditional_probability_matrix(Distances, 5.0, Probabilities, _Summary),
		measure(symmetrization_100, repeat_goal(5, symmetrize(Probabilities))),
		measure(affinity_100, repeat_goal(2, affinities(Distances))),
		measure(weights_100, repeat_goal(20, weights(Rows))),
		measure(gradient_100, repeat_goal(5, gradients(Probabilities, Rows))),
		measure(centering_100, repeat_goal(200, center(Rows))),
		rows(100, 50, WideRows),
		attributes(50, Attributes),
		examples(WideRows, Attributes, 1, Examples),
		measure(preprocessing_100x50, repeat_goal(5, preprocess(Attributes, Examples))),
		findall(Id, member(Id-_, Examples), Ids),
		measure(identifier_validation_100, repeat_goal(100, validate_ids(Ids))),
		rows(1000, 20, LargeRows),
		measure(centering_wide_reference, repeat_goal(10, reference_center(LargeRows))),
		measure(centering_wide, repeat_goal(10, center(LargeRows))),
		ids(1000, LargeIds),
		measure(identifier_validation_1000_reference, repeat_goal(20, reference_ids(LargeIds, []))),
		measure(identifier_validation_1000, repeat_goal(20, validate_ids(LargeIds))),
		tsne_projection::default_options(Options),
		lookup_configuration(Options, Configuration),
		measure(optimizer_option_dispatch_reference, repeat_goal(20000, reference_phase(Options))),
		measure(optimizer_option_dispatch, repeat_goal(20000, configured_phase(Configuration))),
		measure(training_8, train),
		tsne_projection::learn(correlated_plane, Reducer, [perplexity(2.0), maximum_iterations(100), early_exaggeration_iterations(20), learning_rate(20.0)]),
		measure(transform_8, repeat_goal(20, transform(Reducer))).

	measure(Name, Goal) :-
		call(Goal),
		samples(5, Goal, CPU, Wall),
		msort(CPU, SortedCPU),
		msort(Wall, SortedWall),
		nth1(3, SortedCPU, MedianCPU),
		nth1(3, SortedWall, MedianWall),
		format('~w: median CPU ~6f s, wall ~6f s~n', [Name, MedianCPU, MedianWall]).

	samples(0, _Goal, [], []) :-
		!.
	samples(Count, Goal, [CPU| CPUs], [Wall| Walls]) :-
		cpu_time(CPU0),
		wall_time(Wall0),
		call(Goal),
		cpu_time(CPU1),
		wall_time(Wall1),
		CPU is CPU1 - CPU0,
		Wall is Wall1 - Wall0,
		Next is Count - 1,
		samples(Next, Goal, CPUs, Walls).

	repeat_goal(0, _Goal) :-
		!.
	repeat_goal(Count, Goal) :-
		call(Goal),
		Next is Count - 1,
		repeat_goal(Next, Goal).

	symmetrize(Matrix) :-
		tsne_projection << joint_probability_matrix(Matrix, _Joint).

	affinities(Distances) :-
		tsne_projection << conditional_probability_matrix(Distances, 5.0, _Probabilities, _Summary).

	weights(Rows) :-
		tsne_projection << low_dimensional_weights(Rows, _Weights, _WeightSum).

	gradients(Probabilities, Rows) :-
		tsne_projection << low_dimensional_weights(Rows, Weights, WeightSum),
		tsne_projection << gradient_matrix(Probabilities, Weights, WeightSum, Rows, 1.0, _Gradients).

	center(Rows) :-
		tsne_projection << center_embedding(Rows, _Centered).

	preprocess(Attributes, Examples) :-
		tsne_projection << check_examples(benchmark, Attributes, Examples, Aligned),
		pairs::values(Aligned, ValueRows),
		linear_algebra::transpose_matrix(ValueRows, Columns),
		tsne_projection << build_encoders(Attributes, Columns, [feature_scaling(true)], Encoders),
		tsne_projection << examples_to_rows(Aligned, Encoders, _Rows).

	validate_ids(Ids) :-
		tsne_projection << valid_example_ids(Ids).

	reference_ids([], _Seen).
	reference_ids([Id| Ids], Seen) :-
		ground(Id),
		\+ member(Id, Seen),
		reference_ids(Ids, [Id| Seen]).

	reference_center(Rows) :-
		linear_algebra::transpose_matrix(Rows, Columns),
		reference_center_columns(Columns, CenteredColumns),
		linear_algebra::transpose_matrix(CenteredColumns, _Centered).

	reference_center_columns([], []).
	reference_center_columns([Column| Columns], [Centered| CenteredColumns]) :-
		population::arithmetic_mean(Column, Mean),
		reference_subtract(Column, Mean, Centered),
		reference_center_columns(Columns, CenteredColumns).

	reference_subtract([], _Mean, []).
	reference_subtract([Value| Values], Mean, [Centered| CenteredValues]) :-
		Centered is Value - Mean,
		reference_subtract(Values, Mean, CenteredValues).

	lookup_configuration(Options, training_config(Rate, Scale, Early, Maximum, Tolerance)) :-
		^^option(learning_rate(Rate), Options),
		^^option(early_exaggeration(Scale), Options),
		^^option(early_exaggeration_iterations(Early), Options),
		^^option(maximum_iterations(Maximum), Options),
		^^option(tolerance(Tolerance), Options).

	reference_phase(Options) :-
		lookup_configuration(Options, Configuration),
		configured_phase(Configuration).

	configured_phase(training_config(_Rate, Scale, Early, _Maximum, _Tolerance)) :-
		tsne_projection << optimization_phase(1, Early, Scale, _ProbabilityScale, _Momentum).

	train :-
		tsne_projection::learn(correlated_plane, _Reducer, [perplexity(2.0), maximum_iterations(100), early_exaggeration_iterations(20), learning_rate(20.0)]).

	transform(Reducer) :-
		tsne_projection::transform(Reducer, [x-2.0, y-4.0, z-6.0], _Reduced).

	rows(0, _Width, []) :-
		!.
	rows(Count, Width, [Row| Rows]) :-
		row(Width, Count, Row),
		Next is Count - 1,
		rows(Next, Width, Rows).

	row(0, _Index, []) :-
		!.
	row(Count, Index, [Value| Values]) :-
		Value is ((Index * Count) mod 97) / 97.0,
		Next is Count - 1,
		row(Next, Index, Values).

	attributes(0, []) :-
		!.
	attributes(Count, [Name| Names]) :-
		atomic_concat(feature_, Count, Name)x,
		Next is Count - 1,
		attributes(Next, Names).

	examples([], _Attributes, _Id, []).
	examples([Row| Rows], Attributes, Id, [Id-Bindings| Examples]) :-
		bindings(Attributes, Row, Bindings),
		Next is Id + 1,
		examples(Rows, Attributes, Next, Examples).

	bindings([], [], []).
	bindings([Attribute| Attributes], [Value| Values], [Attribute-Value| Bindings]) :-
		bindings(Attributes, Values, Bindings).

	ids(0, []) :-
		!.
	ids(Count, [Count| Ids]) :-
		Next is Count - 1,
		ids(Next, Ids).

:- end_object.

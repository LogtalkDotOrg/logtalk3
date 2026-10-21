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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-21,
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
		length/2, nth1/3, msort/2, member/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	:- uses(linear_algebra, [
		transpose_matrix/2
	]).

	:- uses(pairs, [
		values/2
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
		measure(transform_8, repeat_goal(20, transform(Reducer))),
		matrix_workload(100, 2),
		matrix_workload(100, 5),
		matrix_workload(300, 2),
		matrix_workload(300, 5),
		measure(training_100, train_large),
		large_model(tsne_reducer(_Encoders, _Ids, _Training, _Embedding, Diagnostics)),
		member(iterations(Iterations), Diagnostics),
		!,
		format('training_100 iterations: ~w~n', [Iterations]).

	matrix_workload(Count, Width) :-
		rows(Count, Width, Rows),
		reference_matrix(Rows, weight, Weights),
		reference_matrix_sum(Weights, 0.0, Sum),
		tsne_projection << joint_probability_matrix(Weights, Probabilities),
		tsne_projection << zero_matrix_like(Rows, Updates),
		tsne_projection << constant_matrix_like(Rows, 1.0, Gains),
		format('~nMatrix workload: ~w examples, ~w coordinates~n', [Count, Width]),
		measure(matrix_reference, repeat_goal(5, reference_matrix(Rows, weight, _))),
		measure(matrix_candidate, repeat_goal(5, tsne_projection << symmetric_matrix(Rows, weight, _))),
		measure(weights_reference, repeat_goal(5, reference_weights(Rows))),
		measure(weights_candidate, repeat_goal(5, weights(Rows))),
		measure(step_reference, repeat_goal(5, reference_step(Probabilities, Weights, Sum, Rows, Updates, Gains))),
		measure(step_candidate, repeat_goal(5, candidate_step(Probabilities, Weights, Sum, Rows, Updates, Gains))).

	reference_weights(Rows) :-
		reference_matrix(Rows, weight, Weights),
		reference_matrix_sum(Weights, 0.0, _Sum).

	reference_matrix(Rows, Kind, Matrix) :-
		length(Rows, Count),
		reference_cells(Count, Count, Matrix),
		transpose_matrix(Matrix, Transposed),
		reference_pair_rows(Rows, Rows, Matrix, Transposed, 1, Kind).

	reference_cells(0, _Width, []) :-
		!.
	reference_cells(Count, Width, [Row| Rows]) :-
		length(Row, Width),
		Next is Count - 1,
		reference_cells(Next, Width, Rows).

	reference_pair_rows([], _AllRows, [], [], _Index, _Kind).
	reference_pair_rows([Row| Rows], AllRows, [Cells| Matrix], [Reverse| Transposed], Index, Kind) :-
		reference_pair_row(AllRows, Row, Cells, Reverse, Index, 1, Kind),
		Next is Index + 1,
		reference_pair_rows(Rows, AllRows, Matrix, Transposed, Next, Kind).

	reference_pair_row([], _Row, [], [], _RowIndex, _ColumnIndex, _Kind).
	reference_pair_row([Other| Rows], Row, [Cell| Cells], [Reverse| Reverses], RowIndex, ColumnIndex, Kind) :-
		(	RowIndex =:= ColumnIndex ->
			Cell = 0.0
		;	RowIndex < ColumnIndex ->
			reference_distance(Row, Other, 0.0, Distance),
			(	Kind == distance ->
				Cell = Distance
			;	Cell is 1.0 / (1.0 + Distance)
			),
			Reverse = Cell
		;	true
		),
		Next is ColumnIndex + 1,
		reference_pair_row(Rows, Row, Cells, Reverses, RowIndex, Next, Kind).

	reference_distance([], [], Distance, Distance).
	reference_distance([Left| Lefts], [Right| Rights], Distance0, Distance) :-
		Difference is Left - Right,
		Distance1 is Distance0 + Difference * Difference,
		reference_distance(Lefts, Rights, Distance1, Distance).

	reference_matrix_sum([], Sum, Sum).
	reference_matrix_sum([Row| Rows], Sum0, Sum) :-
		reference_row_sum(Row, Sum0, Sum1),
		reference_matrix_sum(Rows, Sum1, Sum).

	reference_row_sum([], Sum, Sum).
	reference_row_sum([Value| Values], Sum0, Sum) :-
		Sum1 is Sum0 + Value,
		reference_row_sum(Values, Sum1, Sum).

	reference_step(Probabilities, Weights, Sum, Rows, Updates, Gains) :-
		tsne_projection << gradient_matrix(Probabilities, Weights, Sum, Rows, 1.0, Gradients),
		reference_update(Rows, Gradients, Updates, Gains, _Updated, _NextUpdates, _NextGains, 0.0, _Delta).

	reference_update([], [], [], [], [], [], [], Delta, Delta).
	reference_update([Row| Rows], [Gradient| Gradients], [Previous| PreviousRows], [Gain| Gains0], [Updated| UpdatedRows], [Update| Updates], [UpdatedGain| Gains], Delta0, Delta) :-
		tsne_projection << update_row(Row, Gradient, Previous, Gain, 0.1, 0.8, Updated, Update, UpdatedGain, 0.0, RowDelta),
		Delta1 is max(RowDelta, Delta0),
		reference_update(Rows, Gradients, PreviousRows, Gains0, UpdatedRows, Updates, Gains, Delta1, Delta).

	candidate_step(Probabilities, Weights, Sum, Rows, Updates, Gains) :-
		tsne_projection << training_step(Probabilities, Weights, Sum, Rows, 1.0, Updates, Gains, 0.1, 0.8, _Updated, _NextUpdates, _NextGains, _Delta).

	train_large :-
		large_model(_Reducer).

	large_model(Reducer) :-
		tsne_projection::learn(tsne_projection_benchmark_dataset, Reducer, [perplexity(5.0), maximum_iterations(100), early_exaggeration_iterations(20), learning_rate(20.0), tolerance(1.0e-20)]).

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
		values(Aligned, ValueRows),
		transpose_matrix(ValueRows, Columns),
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
		transpose_matrix(Rows, Columns),
		reference_center_columns(Columns, CenteredColumns),
		transpose_matrix(CenteredColumns, _Centered).

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
		Value is sin(Index * Count * 0.13) + Index / (Count + 100.0),
		Next is Count - 1,
		row(Next, Index, Values).

	attributes(0, []) :-
		!.
	attributes(Count, [Name| Names]) :-
		atomic_concat(feature_, Count, Name),
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


:- object(tsne_projection_benchmark_dataset,
	implements(dimension_reduction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-21,
		comment is 'Deterministic 100-example continuous dataset for t-SNE benchmarks.'
	]).

	attribute_values(x, continuous).
	attribute_values(y, continuous).
	attribute_values(z, continuous).

	example(Id, [x-XValue, y-YValue, z-ZValue]) :-
		identifier(1, Id),
		XValue is Id / 100.0,
		YValue is sin(Id * 0.13),
		ZValue is cos(Id * 0.07).

	identifier(Id, Id) :-
		Id =< 100.
	identifier(Index, Id) :-
		Index < 100,
		Next is Index + 1,
		identifier(Next, Id).

:- end_object.

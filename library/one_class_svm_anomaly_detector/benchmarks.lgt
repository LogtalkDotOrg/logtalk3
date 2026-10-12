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


:- object(svm_benchmark_dataset(_Count_, _Dimensions_),
	implements(anomaly_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Deterministic generated data for opt-in one-class SVM benchmarks.'
	]).

	:- public(instance/2).
	:- mode(instance(+positive_integer, -list(pair)), one).
	:- info(instance/2, [
		comment is 'Constructs a deterministic numeric instance for a training or fresh query identity.',
		argnames is ['Id', 'Instance']
	]).

	attribute_values(Attribute, continuous) :-
		index(1, _Dimensions_, Dimension),
		attribute_name(Dimension, Attribute).

	class_values([normal]).

	example(Id, normal, Instance) :-
		index(1, _Count_, Id),
		instance(Id, Instance).

	instance(Id, Instance) :-
		instance_values(1, _Dimensions_, Id, Instance).

	instance_values(Dimension, Maximum, _Id, []) :-
		Dimension > Maximum,
		!.
	instance_values(Dimension, Maximum, Id, [Attribute-Value| Values]) :-
		attribute_name(Dimension, Attribute),
		Value is 2.0 + sin(Id * Dimension * 0.73) + cos(Id * 0.19) + (Id mod 7) / 10.0,
		Next is Dimension + 1,
		instance_values(Next, Maximum, Id, Values).

	attribute_name(Dimension, Attribute) :-
		number_codes(Dimension, Codes),
		atom_codes(Suffix, Codes),
		atom_concat(x, Suffix, Attribute).

	index(Low, High, Value) :-
		Low =< High,
		(	Value = Low
		;	Next is Low + 1,
			index(Next, High, Value)
		).

:- end_object.


:- object(svm_benchmarks).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Opt-in one-class SVM production benchmarks with warmed five-run CPU medians.'
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Prints training, query, and original-versus-optimized primitive CPU medians in seconds.'
	]).

	:- uses(list, [
		length/2, member/2, msort/2
	]).

	:- uses(numberlist, [
		max/2, min/2, sum/2
	]).

	:- uses(format, [
		format/2
	]).

	run :-
		format('Five-run warmed CPU medians; 30 learning iterations; 1000 fresh queries~n', []),
		forall(
			(
				member(Count, [40, 100, 250]),
				member(Dimensions, [2, 10]),
				member(Kernel, [linear, polynomial(2, 0.5, 1.0), rbf(0.5)])
			),
			benchmark_model(Count, Dimensions, Kernel)
		),
		benchmark_primitives.

	benchmark_model(Count, Dimensions, Kernel) :-
		Dataset = svm_benchmark_dataset(Count, Dimensions),
		Options = [kernel(Kernel), feature_scaling(false), maximum_iterations(30), tolerance(0.0), optimality_tolerance(1.0e-12)],
		one_class_svm_anomaly_detector::learn(Dataset, Detector, Options),
		query_batch(1000, Dataset, Count, Detector),
		median(one_class_svm_anomaly_detector::learn(Dataset, _Model, Options), Learn),
		median(query_batch(1000, Dataset, Count, Detector), Queries),
		one_class_svm_anomaly_detector::diagnostics(Detector, Diagnostics),
		list::memberchk(support_vectors(Supports), Diagnostics),
		list::memberchk(iterations(Iterations), Diagnostics),
		format('model(~w,~w,~w): learn=~6f queries=~6f supports=~w iterations=~w~n', [Count, Dimensions, Kernel, Learn, Queries, Supports, Iterations]).

	query_batch(0, _Dataset, _Offset, _Detector) :-
		!.
	query_batch(Count, Dataset, Offset, Detector) :-
		Id is Offset + Count,
		Dataset::instance(Id, Instance),
		one_class_svm_anomaly_detector::score(Detector, Instance, _Score),
		Next is Count - 1,
		query_batch(Next, Dataset, Offset, Detector).

	benchmark_primitives :-
		Dataset = svm_benchmark_dataset(100, 10),
		findall(Row, (Dataset::example(_, _, Instance), numeric_values(Instance, Row)), Rows),
		Rows = [Features| _],
		one_class_svm_anomaly_detector<<constant_vector(100, 0.01, Coefficients),
		median(repeat_goal(10, original_matrix(Rows, Rows, rbf(0.5), _)), OriginalGram),
		median(repeat_goal(10, one_class_svm_anomaly_detector<<build_kernel_matrix(Rows, rbf(0.5), _)), SymmetricGram),
		median(repeat_goal(1000, original_raw_score(Rows, Coefficients, Features, rbf(0.5), _)), OriginalScore),
		median(repeat_goal(1000, one_class_svm_anomaly_detector<<weighted_kernel_score(Rows, Coefficients, Features, rbf(0.5), 0.0, _)), FusedScore),
		range_values(100, ProjectionValues),
		median(repeat_goal(1000, original_projection(ProjectionValues, 0.1, _)), OriginalProjection),
		median(repeat_goal(1000, one_class_svm_anomaly_detector<<project_capped_simplex(ProjectionValues, 0.1, _)), NewProjection),
		range_values(10000, References),
		one_class_svm_anomaly_detector<<build_rank_index(References, Index),
		median(repeat_goal(1000, original_rank(References, 5000.0, 0, _)), OriginalRank),
		median(repeat_goal(1000, one_class_svm_anomaly_detector<<rank_greater(Index, 5000.0, 0, _)), IndexedRank),
		format('primitive(gram,10): original=~6f optimized=~6f~n', [OriginalGram, SymmetricGram]),
		format('primitive(score,1000): original=~6f optimized=~6f~n', [OriginalScore, FusedScore]),
		format('primitive(projection,1000): original=~6f optimized=~6f~n', [OriginalProjection, NewProjection]),
		format('primitive(rank,1000): original=~6f optimized=~6f~n', [OriginalRank, IndexedRank]).

	:- meta_predicate(median(0, *)).

	median(Goal, Median) :-
		call(Goal),
		samples(5, Goal, Times),
		msort(Times, [_, _, Median| _]).

	:- meta_predicate(samples(*, 0, *)).

	samples(0, _Goal, []) :-
		!.
	samples(Count, Goal, [Time| Times]) :-
		measure(Goal, Time),
		Next is Count - 1,
		samples(Next, Goal, Times).

	:- meta_predicate(measure(0, *)).

	measure(Goal, Seconds) :-
		os::cpu_time(Start),
		call(Goal),
		os::cpu_time(End),
		Seconds is End - Start.

	:- meta_predicate(repeat_goal(*, 0)).

	repeat_goal(0, _Goal) :-
		!.
	repeat_goal(Count, Goal) :-
		call(Goal),
		Next is Count - 1,
		repeat_goal(Next, Goal).

	numeric_values([], []).
	numeric_values([_-Value| Values], [Value| Rest]) :-
		numeric_values(Values, Rest).

	range_values(0, []) :-
		!.
	range_values(Count, [Value| Values]) :-
		Value is float(Count),
		Next is Count - 1,
		range_values(Next, Values).

	original_matrix([], _AllRows, _Kernel, []).
	original_matrix([Row| Rows], AllRows, Kernel, [Values| Matrix]) :-
		original_kernel_vector(AllRows, Row, Kernel, Values),
		original_matrix(Rows, AllRows, Kernel, Matrix).

	original_kernel_vector([], _Features, _Kernel, []).
	original_kernel_vector([Row| Rows], Features, Kernel, [Value| Values]) :-
		original_kernel(Kernel, Row, Features, Value),
		original_kernel_vector(Rows, Features, Kernel, Values).

	original_kernel(rbf(Gamma), Row, Features, Value) :-
		linear_algebra::subtract_vectors(Row, Features, Difference),
		linear_algebra::dot_product(Difference, Difference, Squared),
		Value is exp(-Gamma * Squared).

	original_raw_score(Rows, Coefficients, Features, Kernel, Score) :-
		original_kernel_vector(Rows, Features, Kernel, Values),
		linear_algebra::dot_product(Coefficients, Values, Score).

	original_projection(Values, Cap, Projection) :-
		min(Values, Minimum),
		max(Values, Maximum),
		Low is Minimum - Cap,
		original_bisection(50, Values, Cap, Low, Maximum, Projection).

	original_bisection(0, Values, Cap, Low, High, Projection) :-
		!,
		Lambda is (Low + High) / 2.0,
		one_class_svm_anomaly_detector<<clamp_projection(Values, Lambda, Cap, Projection).
	original_bisection(Count, Values, Cap, Low, High, Projection) :-
		Lambda is (Low + High) / 2.0,
		one_class_svm_anomaly_detector<<clamp_projection(Values, Lambda, Cap, Candidate),
		sum(Candidate, Total),
		Next is Count - 1,
		(	Total > 1.0 ->
			original_bisection(Next, Values, Cap, Lambda, High, Projection)
		;	original_bisection(Next, Values, Cap, Low, Lambda, Projection)
		).

	original_rank([], _Query, Count, Count).
	original_rank([Value| Values], Query, Count0, Count) :-
		(	Value > Query ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		original_rank(Values, Query, Count1, Count).

:- end_object.

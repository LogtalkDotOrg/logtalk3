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


:- object(tests_performance).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-21,
		comment is 'Repeatable kernel and end-to-end benchmarks for the "spectral_clusterer" library.'
	]).

	:- public(run/0).
	:- mode(run, one_or_error).
	:- info(run/0, [
		comment is 'Prints seven CPU-time samples per workload, excluding fixture preparation. Propagates learning and prediction errors.',
		exceptions is ['A learning or prediction workload raises an error' - error('Formal', 'Context')]
	]).

	:- uses(lgtunit, [benchmark/3]).
	:- uses(list, [member/2, msort/2]).
	:- uses(format, [format/2]).

	run :-
		forall(
			member(Size, [16, 32, 64]),
			(	make_rows(Size, Size, Matrix),
				diagonal_matrix(Matrix, 1, Diagonal),
				forall(
					member(Count, [1, 2, 4]),
					(	eigen_repetitions(Size, Count, Repetitions),
						report(eigen(Size, Count), eigen(Diagonal, Count), Repetitions)
					)
				)
			)
		),
		forall(
			member(Size, [128, 512, 2048]),
			forall(
				member(Count, [2, 8, 16]),
				(	make_rows(Size, Count, Rows),
					spread_repetitions(Size, Count, Repetitions),
					report(spread(Size, Count), spread(Rows, Count), Repetitions)
				)
			)
		),
		forall(
			member(Dataset-Options, [two_blobs-[], concentric_rings-[sigma(0.4), feature_scaling(off), eigensolver_tolerance(1.0e-6)]]),
			(	spectral_clusterer::learn(Dataset, Model, Options),
				findall(Instance, Dataset::example(_, Instance), Instances),
				(	Dataset == two_blobs ->
					Repetitions = 200
				;	Repetitions = 1
				),
				report(learn(Dataset), learn(Dataset, Options), Repetitions),
				report(predict(Dataset), predict(Model, Instances), 1000)
			)
		),
		forall(
			member(Size, [32, 128, 512]),
			(	Dataset = spectral_performance_dataset(Size),
				spectral_clusterer::learn(Dataset, Model, [k(1), sigma(1.0), feature_scaling(off)]),
				prediction_repetitions(Size, Repetitions),
				report(predict(Size), predict(Model, [[x-0.25, y-0.5]]), Repetitions)
			)
		).

	eigen_repetitions(16, 1, 150).
	eigen_repetitions(16, 2, 50).
	eigen_repetitions(16, 4, 20).
	eigen_repetitions(32, 1, 30).
	eigen_repetitions(32, 2, 15).
	eigen_repetitions(32, 4, 6).
	eigen_repetitions(64, 1, 6).
	eigen_repetitions(64, 2, 3).
	eigen_repetitions(64, 4, 2).

	spread_repetitions(128, 2, 5000).
	spread_repetitions(128, 8, 100).
	spread_repetitions(128, 16, 10).
	spread_repetitions(512, 2, 1500).
	spread_repetitions(512, 8, 20).
	spread_repetitions(512, 16, 3).
	spread_repetitions(2048, 2, 350).
	spread_repetitions(2048, 8, 5).
	spread_repetitions(2048, 16, 1).

	prediction_repetitions(32, 4000).
	prediction_repetitions(128, 1000).
	prediction_repetitions(512, 300).

	report(Label, Workload, Repetitions) :-
		workload(Workload),
		samples(7, Workload, Repetitions, Samples),
		msort(Samples, [Minimum, _, _, Median, _, _, Maximum]),
		PerCall is Median / Repetitions,
		format('~q repetitions=~d median=~9f min=~9f max=~9f per_call=~9f samples=~q~n', [Label, Repetitions, Median, Minimum, Maximum, PerCall, Samples]).

	samples(0, _Workload, _Repetitions, []) :-
		!.
	samples(Count, Workload, Repetitions, [Time| Times]) :-
		benchmark(workload(Workload), Repetitions, Time),
		Remaining is Count - 1,
		samples(Remaining, Workload, Repetitions, Times).

	workload(eigen(Matrix, Count)) :-
		linear_algebra::symmetric_eigen_partial(Matrix, Count, 1.0e-8, 100, _, _, _).
	workload(spread(Rows, Count)) :-
		spectral_clusterer<<initialize_centroids(spread, Count, Rows, _).
	workload(learn(Dataset, Options)) :-
		spectral_clusterer::learn(Dataset, _, Options).
	workload(predict(Model, Instances)) :-
		predict_instances(Instances, Model).

	predict_instances([], _Model).
	predict_instances([Instance| Instances], Model) :-
		spectral_clusterer::cluster(Model, Instance, _),
		predict_instances(Instances, Model).

	make_rows(Count, Dimension, Rows) :-
		make_rows(Count, Dimension, 1, Rows).

	make_rows(0, _Dimension, _Index, []) :-
		!.
	make_rows(Count, Dimension, Index, [Row| Rows]) :-
		make_row(Dimension, Index, 1, Row),
		Remaining is Count - 1,
		Next is Index + 1,
		make_rows(Remaining, Dimension, Next, Rows).

	make_row(0, _Index, _Column, []) :-
		!.
	make_row(Count, Index, Column, [Value| Values]) :-
		Value is ((Index * 37 + Column * 17 + Index * Column) mod 997) / 997.0,
		Remaining is Count - 1,
		Next is Column + 1,
		make_row(Remaining, Index, Next, Values).

	diagonal_matrix([], _Index, []).
	diagonal_matrix([Row| Rows], Index, [Diagonal| Matrix]) :-
		diagonal_row(Row, Index, 1, Diagonal),
		Next is Index + 1,
		diagonal_matrix(Rows, Next, Matrix).

	diagonal_row([], _Index, _Column, []).
	diagonal_row([_| Values], Index, Column, [Value| Diagonal]) :-
		(	Index =:= Column ->
			Value is 1.0 / Index
		;	Value = 0.0
		),
		Next is Column + 1,
		diagonal_row(Values, Index, Next, Diagonal).

:- end_object.


:- object(spectral_performance_dataset(_Count_),
	implements(clustering_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-21,
		comment is 'Deterministic continuous dataset for spectral prediction benchmarks.'
	]).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(Index, [x-X, y-Y]) :-
		index(1, _Count_, Index),
		X is (Index mod 37) / 37.0,
		Y is (Index mod 53) / 53.0.

	index(Lower, Upper, Index) :-
		Lower =< Upper,
		(	Index = Lower
		;	Next is Lower + 1,
			index(Next, Upper, Index)
		).

:- end_object.
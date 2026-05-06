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


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Performance and heap-path benchmarks for the "hierarchical_clustering" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(hierarchical_clustering, [
		cluster/3, diagnostics/2, learn/3
	]).

	test(large_two_blobs_heap_path, true, [note(metrics(train_seconds-TrainTime, merge_count-MergeCount, max_heap_size-MaximumHeapSize))]) :-
		benchmark(learn(large_two_blobs, _Clusterer, [k(2), feature_scaling(off)]), TrainTime),
		learn(large_two_blobs, Clusterer, [k(2), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(merge_count(MergeCount), Diagnostics),
		memberchk(heap_rebuild_count(HeapRebuildCount), Diagnostics),
		memberchk(scan_fallback_count(0), Diagnostics),
		memberchk(maximum_heap_size(MaximumHeapSize), Diagnostics),
		HeapRebuildCount >= 0,
		MaximumHeapSize > 0,
		cluster(Clusterer, [x-0.2, y-0.1], LeftCluster),
		cluster(Clusterer, [x-5.7, y-5.4], RightCluster),
		LeftCluster \== RightCluster.

	test(iris_unlabeled_heap_path, true, [note(metrics(train_seconds-TrainTime, max_heap_size-MaximumHeapSize))]) :-
		benchmark(learn(iris_unlabeled, _Clusterer, [k(3)]), TrainTime),
		learn(iris_unlabeled, Clusterer, [k(3)]),
		dataset_example_count(iris_unlabeled, ExampleCount),
		ExpectedMergeCount is ExampleCount - 1,
		diagnostics(Clusterer, Diagnostics),
		memberchk(training_example_count(ExampleCount), Diagnostics),
		memberchk(merge_count(ExpectedMergeCount), Diagnostics),
		memberchk(scan_fallback_count(0), Diagnostics),
		memberchk(maximum_heap_size(MaximumHeapSize), Diagnostics),
		MaximumHeapSize > 0.

	dataset_example_count(Dataset, Count) :-
		findall(AttributeValues, Dataset::example(_Id, AttributeValues), Examples),
		length(Examples, Count).

:- end_object.

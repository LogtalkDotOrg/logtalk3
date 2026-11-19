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


:- object(tests(_Random_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-19,
		comment is 'Unit tests for the "emeans_clusterer" library.',
		parameters is [
			'Random' - 'Random generator object and algorithm used by the main test fixture.'
		]
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(emeans_clusterer(_Random_), [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	:- uses(_Random_, [
		get_seed/1, randomize/1
	]).

	cover(emeans_clusterer(_)).

	cleanup :-
		^^clean_file('test_output.pl').

	test(emeans_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(emeans_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(emeans_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, emeans_clusterer(Encoders, Centroids, Options, _Diagnostics)),
		valid_clusterer(emeans_clusterer(Encoders, Centroids, Options, [model(emeans_clusterer), centroid_count(99), options(Options)])).

	test(emeans_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		randomize(1),
		get_seed(Seed),
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off), epochs(5), seed(Seed)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(emeans_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		randomize(1),
		get_seed(Seed),
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off), epochs(5), seed(Seed)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(emeans_learn_3_custom_options, deterministic([K, Epochs, Tolerance, Initialization, FeatureScaling, Seed] == [3, 4, 1.0e-5, first_k, off, Seed0])) :-
		randomize(1),
		get_seed(Seed0),
		learn(iris_unlabeled, emeans_clusterer(_Encoders, _Centroids, Options, _Diagnostics), [k(3), epochs(4), tolerance(1.0e-5), initialization(first_k), feature_scaling(off), seed(Seed0)]),
		memberchk(k(K), Options),
		memberchk(epochs(Epochs), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(initialization(Initialization), Options),
		memberchk(feature_scaling(FeatureScaling), Options),
		memberchk(seed(Seed), Options).

	test(emeans_diagnostics_2_rich_metadata, deterministic((Epochs >= 1, FinalShift >= 0.0, [Model, CentroidCount, TrainingExampleCount, FeatureScaling] == [emeans_clusterer, 2, 8, off]))) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off), epochs(5)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(centroid_count(CentroidCount), Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		assertion(nonvar(Convergence)),
		memberchk(epochs(Epochs), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(emeans_learn_3_maximum_epochs_termination, deterministic((FinalShift >= 0.0, Convergence == maximum_epochs, Epochs == 1))) :-
		learn(two_blobs, Clusterer, [k(2), epochs(1), tolerance(0.0), initialization(first_k), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(epochs(Epochs), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics).

	test(emeans_learn_3_tolerance_termination, deterministic((FinalShift >= 0.0, member(Convergence, [tolerance, maximum_epochs]), Epochs >= 1))) :-
		learn(two_blobs, Clusterer, [algorithm(ema_lloyd), k(2), epochs(40), tolerance(1000.0), initialization(first_k), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(epochs(Epochs), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics).

	test(emeans_cluster_3_iris_extremes, deterministic(Cluster1 \== Cluster2)) :-
		learn(iris_unlabeled, Clusterer, [k(3), epochs(5)]),
		cluster(Clusterer, [sepal_length-5.0, sepal_width-3.4, petal_length-1.5, petal_width-0.2], Cluster1),
		cluster(Clusterer, [sepal_length-6.8, sepal_width-3.1, petal_length-5.8, petal_width-2.2], Cluster2).

	test(emeans_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off), epochs(5)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(emeans_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(emeans_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off), epochs(5)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(emeans_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer),
		print_clusterer(Clusterer).

	test(emeans_cluster_3_duplicate_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(two_blobs, Clusterer),
		cluster(Clusterer, [x-1.0, x-1.1, y-1.0], _Cluster).

	test(emeans_cluster_3_undeclared_attribute, error(domain_error(declared_attribute(_), z))) :-
		learn(two_blobs, Clusterer),
		cluster(Clusterer, [x-1.0, y-1.0, z-2.0], _Cluster).

	test(emeans_learn_3_duplicate_training_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(invalid_two_blobs, _Clusterer).

	test(emeans_learn_3_duplicate_attribute_declaration, error(permission_error(repeat, attribute_declaration, x))) :-
		learn(invalid_two_blobs_declarations, _Clusterer, [feature_scaling(off)]).

	test(emeans_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	test(emeans_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 8), 9))) :-
		learn(two_blobs, _Clusterer, [k(9)]).

	test(emeans_shuffle_index_2_seeded_order, deterministic(Shuffled-Seed == [a,c,b,e,f,d,g,a]-seed(xoshiro128pp,s(3000033134,328064027,2388441145,2287643389)))) :-
		randomize(1),
		emeans_clusterer(_Random_)<<row_index([a,b,c,a,d,e,f,g], Index),
		emeans_clusterer(_Random_)<<shuffle_index(Index, Shuffled),
		get_seed(Seed).

	test(emeans_shuffle_index_2_short_lists, deterministic((Empty == [], Singleton == [row], Seed == InitialSeed))) :-
		get_seed(InitialSeed),
		emeans_clusterer(_Random_)<<row_index([], EmptyIndex),
		emeans_clusterer(_Random_)<<row_index([row], SingletonIndex),
		emeans_clusterer(_Random_)<<shuffle_index(EmptyIndex, Empty),
		emeans_clusterer(_Random_)<<shuffle_index(SingletonIndex, Singleton),
		get_seed(Seed).

	test(emeans_learn_3_integer_and_state_seed, deterministic((Left == Right, AfterInteger == AfterState, AfterState \== Seed))) :-
		randomize(42),
		get_seed(Seed),
		learn(two_blobs, emeans_clusterer(_, Left, _, _), [seed(42)]),
		get_seed(AfterInteger),
		learn(two_blobs, emeans_clusterer(_, Right, _, _), [seed(Seed)]),
		get_seed(AfterState).

	test(emeans_learn_3_alternative_generator, deterministic((First == Second, Before \== After, OtherBefore == OtherAfter))) :-
		get_seed(OtherBefore),
		fast_random(as183)::randomize(42),
		fast_random(as183)::get_seed(Before),
		emeans_clusterer(fast_random(as183))::learn(two_blobs, emeans_clusterer(_, First, _, _), [seed(42)]),
		emeans_clusterer(fast_random(as183))::learn(two_blobs, emeans_clusterer(_, Second, _, _), [seed(Before)]),
		fast_random(as183)::get_seed(After),
		get_seed(OtherAfter).

	test(emeans_learn_3_non_ground_seed, error(domain_error(option, seed(seed(xoshiro128pp,_))))) :-
		learn(two_blobs, _, [seed(seed(xoshiro128pp,_))]).

	test(emeans_annealed_alpha_4_partial_and_single_batch, deterministic((Batches == 3, abs(Single - 1.0) < 1.0e-12, Penultimate > Last, abs(Last - 0.1) < 1.0e-12))) :-
		emeans_clusterer(_Random_)<<batch_count(5, 2, Batches),
		emeans_clusterer(_Random_)<<annealed_alpha(1.0, 0, 1, Single),
		Updates is Batches * 2,
		emeans_clusterer(_Random_)<<annealed_alpha(1.0, 4, Updates, Penultimate),
		emeans_clusterer(_Random_)<<annealed_alpha(1.0, 5, Updates, Last).

	test(emeans_revive_dead_clusters_8_distinct_reseed, deterministic((Updated == [[0.0],[10.0]], Reseeded == true))) :-
		emeans_clusterer(_Random_)<<revive_dead_clusters([a-[0.0],b-[4.0],c-[10.0]], [[0.0]-1.0,[0.0]-0.0], [[0.0],[0.0]], 0.1, 0.5, _, Updated, Reseeded).

	test(emeans_initialize_centroids_4_spread_ties, deterministic(Centroids == [[0.0],[-3.0],[3.0],[-3.0]])) :-
		emeans_clusterer(_Random_)<<initialize_centroids(spread, 4, [a-[0.0],b-[-3.0],c-[3.0],d-[-3.0]], Centroids).

	test(emeans_emeans_epochs_11_reseed_defers_tolerance, deterministic((Capped == maximum_epochs, CapEpoch == 2, CapShift > 9.0, Reason == tolerance, Epoch == 3))) :-
		Rows = [a-[0.0],b-[10.0]],
		Initial = [[0.0],[0.0]],
		State = [[0.0]-1.0,[0.0]-0.0],
		emeans_clusterer(_Random_)<<row_index(Rows, Index),
		emeans_clusterer(_Random_)<<emeans_epochs(Rows, Index, settings(2,0.0,2,1.0,0.1,2,100.0), 1, 1, State, Initial, _, Capped, CapEpoch, CapShift),
		emeans_clusterer(_Random_)<<emeans_epochs(Rows, Index, settings(2,0.0,3,1.0,0.1,3,100.0), 1, 1, State, Initial, _, Reason, Epoch, _).

	test(emeans_diagnostics_2_occupancy_matches_training_assignments, deterministic((Blobs == [1,2], Repeated == [1]))) :-
		training_clusters(two_blobs, Blobs),
		training_clusters(repeated_rows, Repeated).

	test(emeans_learn_3_published_defaults, deterministic) :-
		learn(two_blobs, emeans_clusterer(_, _, Options, Diagnostics), [epochs(5), tolerance(1000.0)]),
		memberchk(algorithm(published), Options),
		memberchk(initialization(random), Options),
		memberchk(feature_scaling(off), Options),
		memberchk(convergence(maximum_epochs), Diagnostics),
		memberchk(epochs(5), Diagnostics),
		memberchk(finalization(last_epoch_mean), Diagnostics).

	test(emeans_published_batches_10_complete_batches_and_last_epoch_means, deterministic((Centroids == [[1.0]], abs(Shift) > 0.1))) :-
		Rows = [a-[0.0],b-[2.0],c-[100.0]],
		emeans_clusterer(_Random_)<<published_batches(1, Rows, schedule(2,1,1,0.5,1), 0, [[0.0]-2.0], [[0.0]], mean([[]-0.0]), _, Trained, Readout),
		emeans_clusterer(_Random_)<<finish_published(Readout, Trained, Centroids, last_epoch_mean(1,Shift)).

	test(emeans_published_scaling_laws_5_large_dataset_cutover, deterministic((AtCutover == 131072, AboveCutover == 16384, AbovePeak < AtPeak))) :-
		emeans_clusterer(_Random_)<<published_scaling_laws(2000000, 16384, 3, AtCutover, AtPeak),
		emeans_clusterer(_Random_)<<published_scaling_laws(2000001, 16384, 3, AboveCutover, AbovePeak).

	test(emeans_published_batches_10_split_every_fifth_batch, deterministic((Fourth == [[2.0,4.0],[0.0,0.0]], Child == [2.015625,3.96875], Donor == [1.984375,4.03125], After \== Seed))) :-
		get_seed(Seed),
		Rows = [a-[2.0,4.0]],
		Initial = [[2.0,4.0],[0.0,0.0]],
		State = [[4.0,8.0]-2.0,[0.0,0.0]-0.0],
		Schedule = schedule(1,1,5,0.0,2),
		emeans_clusterer(_Random_)<<published_batches(1, Rows, Schedule, 3, State, Initial, mean(none), _, Fourth, _),
		get_seed(Seed),
		emeans_clusterer(_Random_)<<published_batches(1, Rows, Schedule, 4, State, Initial, mean(none), _, [Donor,Child], _),
		get_seed(After).

	test(emeans_learn_3_slow_ema_finalization, deterministic((Mean > Slow, Slow > 1.0))) :-
		Options = [k(1), epochs(1), initialization(first_k)],
		learn(two_blobs, emeans_clusterer(_, [[Mean,_]], _, _), Options),
		learn(two_blobs, emeans_clusterer(_, [[Slow,_]], _, Diagnostics), [finalization(slow_ema(1))| Options]),
		memberchk(finalization(slow_ema), Diagnostics).

	test(emeans_refine_5_duplicate_rows, deterministic((Centroids == [[1.0],[1.0]], Result == refinement(stable,1,1,0.0)))) :-
		emeans_clusterer(_Random_)<<refine([a-[1.0],b-[1.0]], [[1.0],[1.0]], 5, Centroids, Result).

	test(emeans_refine_5_stability_and_cap, deterministic((Capped == iteration_limit, Stable == stable, Count == 2, Occupied == 2))) :-
		Rows = [a-[0.0],b-[1.0],c-[1.0],d-[2.0],e-[10.0]],
		emeans_clusterer(_Random_)<<refine(Rows, [[0.0],[2.0]], 1, _, refinement(Capped,1,2,_)),
		emeans_clusterer(_Random_)<<refine(Rows, [[0.0],[2.0]], 5, Centroids, refinement(Stable,Count,Occupied,_)),
		cluster(emeans_clusterer([continuous(x,0.0,1.0)], Centroids, [], []), [x-10.0], 2).

	test(emeans_valid_clusterer_1_legacy_seed_and_diagnostics, deterministic) :-
		randomize(1),
		get_seed(Seed),
		Options = [k(1), seed(Seed)],
		valid_clusterer(emeans_clusterer([continuous(x,0.0,1.0)], [[1.0]], Options, [model(emeans_clusterer), centroid_count(1), options(Options)])).

	% auxiliary predicates

	training_clusters(Dataset, Occupied) :-
		learn(Dataset, Clusterer, [algorithm(ema_lloyd), k(2)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(occupied_cluster_count(Count), Diagnostics),
		findall(Cluster, (Dataset::example(_, Values), cluster(Clusterer, Values, Cluster)), Clusters),
		findall(Id, Dataset::example(Id, _), Ids),
		length(Ids, N),
		length(Clusters, N),
		sort(Clusters, Occupied),
		length(Occupied, Count).

:- end_object.

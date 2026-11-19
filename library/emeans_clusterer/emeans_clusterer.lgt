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


:- object(emeans_clusterer(_Random_),
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-19,
		comment is 'e-Means clusterer for continuous datasets. Defaults to the published EMA training algorithm with count-weighted donor splitting and last-epoch-mean finalization. Also supports slow-EMA finalization and a hybrid of EMA training and bounded Lloyd refinement. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol.',
		parameters is [
			'Random' - 'Random number generator object providing ``random/1``, ``randomize/1``, ``get_seed/1``, and ``set_seed/1``.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, nearest_centroid_classifier, kmeans_clusterer]
	]).

	:- uses(avltree, [
		as_dictionary/2, insert/4, lookup/3, update/4, update/5, values/2
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		rescale/3
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(_Random_, [
		get_seed/1, set_seed/1, random/1, randomize/1
	]).

	learn(Dataset, Clusterer, UserOptions) :-
		^^check_options(UserOptions),
		training_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		findall(
			Id-AttributeValues,
			Dataset::example(Id, AttributeValues),
			Examples
		),
		^^prepare_training_rows(Dataset, Attributes, Examples, Options, Encoders, Rows),
		length(Rows, Count),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		^^option(algorithm(Algorithm), Options),
		^^option(seed(Seed), Options),
		initialize_seed(Seed),
		train(Algorithm, Rows, Options, Count, K, Centroids, Convergence, Epochs, EpochShift, Finalization),
		build_diagnostics(Count, Centroids, Options, Convergence, Epochs, EpochShift, Finalization, Diagnostics),
		Clusterer = emeans_clusterer(Encoders, Centroids, Options, Diagnostics).

	training_options(UserOptions, Options) :-
		(	memberchk(algorithm(Algorithm), UserOptions),
			Algorithm == ema_lloyd ->
			add_default_option(initialization(spread), UserOptions, Partial),
			add_default_option(feature_scaling(on), Partial, Effective)
		;	Effective = UserOptions
		),
		^^merge_options(Effective, Options).

	add_default_option(Default, Options, Effective) :-
		functor(Default, Name, 1),
		functor(Template, Name, 1),
		(	member(Template, Options) ->
			Effective = Options
		;	Effective = [Default| Options]
		).

	train(ema_lloyd, Rows, Options, Count, K, Centroids, Convergence, Epochs, EpochShift, Refinement) :-
		^^option(initialization(Initialization), Options),
		initialize_training(Initialization, K, Rows, Initial),
		init_state(Initial, State),
		optimize_ema_lloyd(Rows, Options, Count, K, Initial, State, Trained, Convergence, Epochs, EpochShift),
		^^option(refinement_iterations(Limit), Options),
		refine(Rows, Trained, Limit, Centroids, Refinement).
	train(published, Rows, Options, Count, K, Centroids, maximum_epochs, Epochs, EpochShift, Finalization) :-
		^^option(epochs(Epochs), Options),
		^^option(initialization(Initialization), Options),
		^^option(finalization(Readout), Options),
		row_index(Rows, Index),
		initialize_training(Initialization, K, Rows, Index, Initial),
		init_state(Initial, State),
		published_scaling_laws(Count, K, Epochs, BatchSize, Alpha),
		PerEpoch is Count // BatchSize,
		Updates is Epochs * PerEpoch,
		published_readout(Readout, Initial, State, Readout0),
		published_epochs(Epochs, Index, schedule(BatchSize, PerEpoch, Updates, Alpha, K), 0, State, Initial, Readout0, Trained, ReadoutState, EpochShift),
		finish_published(ReadoutState, Trained, Centroids, Finalization).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Centroids, _Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		nearest_centroid_classifier(Centroids, Features, Cluster, _DistanceSquared).

	clusterer_data(Clusterer, Encoders, Centroids, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, Centroids, Options, Diagnostics].

	build_diagnostics(TrainingExampleCount, Centroids, Options, Convergence, Epochs, EpochShift, Finalization, Diagnostics) :-
		length(Centroids, CentroidCount),
		Diagnostics = [
			model(emeans_clusterer),
			centroid_count(CentroidCount),
			training_example_count(TrainingExampleCount),
			convergence(Convergence),
			epochs(Epochs),
			epoch_shift(EpochShift),
			options(Options)
			| FinalDiagnostics
		],
		finalization_diagnostics(Finalization, FinalDiagnostics).

	finalization_diagnostics(refinement(Status, Updates, Occupied, FinalShift), [
			final_shift(FinalShift),
			refinement_status(Status),
			refinement_updates(Updates),
			occupied_cluster_count(Occupied)
	]).
	finalization_diagnostics(last_epoch_mean(Occupied, Shift), [
			final_shift(Shift),
			finalization(last_epoch_mean),
			last_epoch_occupied_cluster_count(Occupied)
	]).
	finalization_diagnostics(slow_ema(Shift), [
			final_shift(Shift),
			finalization(slow_ema)
	]).

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Centroids, _Options, Diagnostics).

	check_clusterer(Clusterer) :-
		(	clusterer_data(Clusterer, Encoders, Centroids, Options, Diagnostics),
			length(Encoders, FeatureCount),
			^^valid_continuous_encoders(Encoders),
			valid(list(list(float, FeatureCount)), Centroids),
			^^valid_clusterer_metadata(emeans_clusterer, Options, Diagnostics),
			length(Centroids, CentroidCount),
			^^valid_diagnostic_count(centroid_count, Diagnostics, CentroidCount) ->
			true
		;	domain_error(clusterer, Clusterer)
		).

	% initialization

	initialize_training(Strategy, K, Rows, Centroids) :-
		(	Strategy == random ->
			row_index(Rows, Index)
		;	Index = unused
		),
		initialize_training(Strategy, K, Rows, Index, Centroids).

	initialize_training(Strategy, K, Rows, Index, Centroids) :-
		(	Strategy == random ->
			shuffle_index(Index, Shuffled),
			^^take_first_k(K, Shuffled, Centroids)
		;	initialize_centroids(Strategy, K, Rows, Centroids)
		).

	initialize_centroids(first_k, K, Rows, Centroids) :-
		^^take_first_k(K, Rows, Centroids).
	initialize_centroids(spread, K, [_-First| Rows], [First| Centroids]) :-
		Remaining is K - 1,
		(	Remaining =:= 0 ->
			Centroids = []
		;	spread_distances(Rows, First, Candidates),
			select_spread_centroids(Remaining, Candidates, Centroids)
		).

	spread_distances([], _, []).
	spread_distances([_-Vector| Rows], Centroid, [Distance-Vector| Candidates]) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, Distance),
		spread_distances(Rows, Centroid, Candidates).

	select_spread_centroids(Count, [First| Candidates], [Vector| Centroids]) :-
		farthest_candidate(Candidates, First, Best),
		Best = _-Vector,
		NextCount is Count - 1,
		(	NextCount =:= 0 ->
			Centroids = []
		;	remove_spread_candidate([First| Candidates], Best, Rest),
			refresh_spread_distances(Rest, Vector, Updated),
			select_spread_centroids(NextCount, Updated, Centroids)
		).

	farthest_candidate([], Best, Best).
	farthest_candidate([Distance-Vector| Candidates], Distance0-Vector0, Best) :-
		(	Distance > Distance0 ->
			Next = Distance-Vector
		;	Next = Distance0-Vector0
		),
		farthest_candidate(Candidates, Next, Best).

	remove_spread_candidate([Candidate| Candidates], Best, Rest) :-
		(	Candidate == Best ->
			Rest = Candidates
		;	Rest = [Candidate| Remaining],
			remove_spread_candidate(Candidates, Best, Remaining)
		).

	refresh_spread_distances([], _, []).
	refresh_spread_distances([Distance0-Vector| Candidates], Centroid, [Distance-Vector| Updated]) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, NewDistance),
		Distance is min(Distance0, NewDistance),
		refresh_spread_distances(Candidates, Centroid, Updated).

	% e-means core

	% state is a list of sum-vector / count pairs, one per cluster
	init_state([], []).
	init_state([Centroid| Centroids], [Sum-Count| State]) :-
		% tiny initial mass so the seed is returned by S/N while having
		% almost no influence once real data arrives
		Count = 1.0e-10,
		rescale(Centroid, Count, Sum),
		init_state(Centroids, State).

	state_to_centroids([], [], []).
	state_to_centroids([Sum-Count| State], [Previous| PreviousCentroids], [Centroid| Centroids]) :-
		(	Count =< 0.0 ->
			Centroid = Previous
		;	Factor is 1.0 / Count,
			rescale(Sum, Factor, Centroid)
		),
		state_to_centroids(State, PreviousCentroids, Centroids).

	% automatic hyper-parameter selection (default scaling law from the paper)
	% B = 0.035 * k^0.13 * n^0.88 * T^0.81
	% alpha0 = min(0.824 * k^(-0.46) * B^0.90 * n^(-0.45) * T^(-0.11), 1)
	scaling_laws(N, K, T, BatchSize, Alpha0) :-
		batch_law(N, K, T, B0),
		BatchSize0 is max(1, round(B0)),
		BatchSize is min(BatchSize0, N),
		step_law(N, K, T, BatchSize, Alpha0).

	published_scaling_laws(N, K, T, BatchSize, Alpha0) :-
		batch_law(N, K, T, B0),
		(	N > 2000000 ->
			Raw is B0 / 8.0
		;	Raw = B0
		),
		Exponent is max(0, round(log(Raw) / log(2.0))),
		BatchSize is min(1 << Exponent, N),
		step_law(N, K, T, BatchSize, Alpha0).

	batch_law(N, K, T, BatchSize) :-
		BatchSize is 0.035 * (K ** 0.13) * (N ** 0.88) * (T ** 0.81).

	step_law(N, K, T, BatchSize, Alpha0) :-
		A0 is 0.824 * (K ** (-0.46)) * (BatchSize ** 0.90) * (N ** (-0.45)) * (T ** (-0.11)),
		Alpha0 is min(A0, 1.0).

	published_readout(last_epoch_mean, _, _, mean(none)).
	published_readout(slow_ema(HalfLife), Initial, State, slow(Alpha, State, Initial)) :-
		Alpha is 1.0 - 0.5 ** (1.0 / HalfLife).

	published_epochs(Left, Index, Schedule, T0, State0, Centroids0, Readout0, Centroids, Readout, Shift) :-
		Schedule = schedule(_, PerEpoch, _, _, K),
		(	Left =:= 1, Readout0 = mean(none) ->
			zero_stats(K, EmptyStats),
			BatchReadout = mean(EmptyStats)
		;	BatchReadout = Readout0
		),
		shuffle_index(Index, Shuffled),
		published_batches(PerEpoch, Shuffled, Schedule, T0, State0, Centroids0, BatchReadout, State1, Centroids1, Readout1),
		(	Left =:= 1 ->
			Centroids = Centroids1,
			Readout = Readout1,
			max_centroid_shift(Centroids0, Centroids1, 0.0, Shift)
		;	Remaining is Left - 1,
			T is T0 + PerEpoch,
			published_epochs(Remaining, Index, Schedule, T, State1, Centroids1, Readout1, Centroids, Readout, Shift)
		).

	published_batches(0, _, _, _, State, Centroids, Readout, State, Centroids, Readout) :-
		!.
	published_batches(Left, Rows, Schedule, T0, State0, Centroids0, Readout0, State, Centroids, Readout) :-
		Schedule = schedule(BatchSize, _, Total, Peak, K),
		take_batch(BatchSize, Rows, Batch, Rest),
		Alpha is Peak * (0.1 + 0.45 * (1.0 + cos(3.141592653589793 * T0 / Total))),
		batch_statistics(Batch, Centroids0, Stats),
		ema_update(State0, Stats, Alpha, Updated),
		update_readout(Readout0, Stats, Readout1),
		T is T0 + 1,
		(	T mod 5 =:= 0 ->
			revive_published(K, Updated, State1, Splits),
			split_readout(Splits, Readout1, Readout2)
		;	State1 = Updated,
			Readout2 = Readout1
		),
		state_to_centroids(State1, Centroids0, Centroids1),
		Remaining is Left - 1,
		published_batches(Remaining, Rest, Schedule, T, State1, Centroids1, Readout2, State, Centroids, Readout).

	update_readout(mean(none), _, mean(none)) :-
		!.
	update_readout(mean(Stats0), Batch, mean(Stats)) :-
		merge_batch_stats(Stats0, Batch, Stats).
	update_readout(slow(Alpha, State0, Previous), Batch, slow(Alpha, State, Centroids)) :-
		ema_update(State0, Batch, Alpha, State),
		state_to_centroids(State, Previous, Centroids).

	merge_batch_stats([], [], []).
	merge_batch_stats([Sum0-N0| Stats0], [BatchSum-BatchN| Batch], [Sum-N| Stats]) :-
		(	BatchN =< 0.0 ->
			Sum = Sum0,
			N = N0
		;	N0 =< 0.0 ->
			Sum = BatchSum,
			N = BatchN
		;	add_vectors(Sum0, BatchSum, Sum),
			N is N0 + BatchN
		),
		merge_batch_stats(Stats0, Batch, Stats).

	finish_published(mean(Stats), Trained, Centroids, last_epoch_mean(Occupied, Shift)) :-
		state_to_centroids(Stats, Trained, Centroids),
		occupied_stats(Stats, 0, Occupied),
		max_centroid_shift(Trained, Centroids, 0.0, Shift).
	finish_published(slow(_, _, Centroids), Trained, Centroids, slow_ema(Shift)) :-
		max_centroid_shift(Trained, Centroids, 0.0, Shift).

	revive_published(K, State0, State, Splits) :-
		total_mass(State0, 0.0, Total),
		Threshold is 0.1 * Total / K,
		(	has_low_mass(State0, Threshold) ->
			split_candidates(State0, Threshold, 1, Dead, Donors),
			rank_donors(Donors, Pairs),
			keysort(Pairs, Ranked),
			select_splits(Dead, Ranked, Splits),
			apply_splits(Splits, State0, State)
		;	State = State0,
			Splits = []
		).

	total_mass([], Total, Total).
	total_mass([_-Count| State], Total0, Total) :-
		Total1 is Total0 + Count,
		total_mass(State, Total1, Total).

	split_candidates([], _, _, [], []).
	split_candidates([_-Count| State], Threshold, I, Dead, Donors) :-
		(	Count < Threshold ->
			Dead = [I| Remaining],
			Donors = Live
		;	Remaining = Dead,
			Donors = [I-Count| Live]
		),
		Next is I + 1,
		split_candidates(State, Threshold, Next, Remaining, Live).

	% exponential race keys give count-proportional sampling without replacement
	rank_donors([], []).
	rank_donors([I-Count| Donors], [Key-I| Pairs]) :-
		random(R),
		Key is -log(1.0 - R) / Count,
		rank_donors(Donors, Pairs).

	select_splits([], _, []) :-
		!.
	select_splits(_, [], []) :-
		!.
	select_splits([Target| Dead], [_-Source| Donors], [Source-Target| Splits]) :-
		select_splits(Dead, Donors, Splits).

	split_readout([], Readout, Readout) :-
		!.
	split_readout([Split| Splits], slow(Alpha, State0, Previous), slow(Alpha, State, Centroids)) :-
		!,
		apply_splits([Split| Splits], State0, State),
		state_to_centroids(State, Previous, Centroids).
	split_readout(_, mean(Stats), mean(Stats)).

	apply_splits([], State, State) :-
		!.
	apply_splits([Split| Splits], State0, State) :-
		keysort([Split| Splits], Sources),
		split_updates(Sources, State0, 1, Changes0),
		keysort(Changes0, Changes),
		replace_split_states(Changes, State0, 1, State).

	split_updates([], _, _, []) :-
		!.
	split_updates([Source-Target| Splits], [Sum-Count| State], I, Changes) :-
		(	I =:= Source ->
			Half is 0.5 * Count,
			split_sum(Sum, 1.0 / 128.0, DonorSum, ChildSum),
			Changes = [Source-(DonorSum-Half),Target-(ChildSum-Half)| Rest],
			Remaining = Splits
		;	Changes = Rest,
			Remaining = [Source-Target| Splits]
		),
		Next is I + 1,
		split_updates(Remaining, State, Next, Rest).

	split_sum([], _, [], []).
	split_sum([Value| Sum], Delta, [Donor| Donors], [Child| Children]) :-
		Donor is 0.5 * Value * (1.0 - Delta),
		Child is 0.5 * Value * (1.0 + Delta),
		Next is -Delta,
		split_sum(Sum, Next, Donors, Children).

	replace_split_states([], State, _, State) :-
		!.
	replace_split_states([J-New| Changes], [Old| Rest], I, [Entry| State]) :-
		(	J =:= I ->
			Entry = New,
			Remaining = Changes
		;	Entry = Old,
			Remaining = [J-New| Changes]
		),
		Next is I + 1,
		replace_split_states(Remaining, Rest, Next, State).

	% cosine annealing of the step size
	annealed_alpha(Alpha0, T, M, Alpha) :-
		M > 1,
		!,
		Pi is 3.141592653589793,
		Cos is cos(Pi * T / (M - 1)),
		Alpha is Alpha0 * (0.1 + 0.9 * 0.5 * (1.0 + Cos)).
	annealed_alpha(Alpha0, _, _, Alpha0).

	optimize_ema_lloyd(Rows, Options, N, K, InitialCentroids, State0, Centroids, Convergence, Epochs, EpochShift) :-
		^^option(epochs(MaxEpochs), Options),
		^^option(tolerance(Tolerance), Options),
		scaling_laws(N, K, MaxEpochs, BatchSize, Alpha0),
		batch_count(N, BatchSize, Batches),
		M is MaxEpochs * Batches,
		Mass is BatchSize / K,
		Threshold is 0.1 * Mass,
		row_index(Rows, Index),
		state_to_centroids(State0, InitialCentroids, Centroids0),
		Settings = settings(BatchSize, Alpha0, M, Mass, Threshold, MaxEpochs, Tolerance),
		emeans_epochs(Rows, Index, Settings, 0, 0, State0, Centroids0, Centroids, Convergence, Epochs, EpochShift).

	batch_count(N, BatchSize, Batches) :-
		Batches is 1 + (N - 1) // BatchSize.

	emeans_epochs(Rows, Index, Settings, Epoch, T0, State0, Centroids0, Centroids, Convergence, Epochs, EpochShift) :-
		Settings = settings(BatchSize, Alpha0, M, Mass, Threshold, MaxEpochs, Tolerance),
		shuffle_index(Index, Shuffled),
		process_batches(Shuffled, BatchSize, Alpha0, M, T0, T, State0, Centroids0, State1, Centroids1),
		revive_dead_clusters(Rows, State1, Centroids1, Threshold, Mass, State2, Centroids2, Reseeded),
		max_centroid_shift(Centroids0, Centroids2, 0.0, Shift),
		NextEpoch is Epoch + 1,
		(	Reseeded == false, Shift =< Tolerance, NextEpoch > 1 ->
			Centroids = Centroids2,
			Convergence = tolerance,
			Epochs = NextEpoch,
			EpochShift = Shift
		;	NextEpoch >= MaxEpochs ->
			Centroids = Centroids2,
			Convergence = maximum_epochs,
			Epochs = NextEpoch,
			EpochShift = Shift
		;	emeans_epochs(Rows, Index, Settings, NextEpoch, T, State2, Centroids2, Centroids, Convergence, Epochs, EpochShift)
		).

	process_batches([], _, _, _, T, T, State, Centroids, State, Centroids) :-
		!.
	process_batches(Rows, BatchSize, Alpha0, M, T0, T, State0, Centroids0, State, Centroids) :-
		take_batch(BatchSize, Rows, Batch, Rest),
		annealed_alpha(Alpha0, T0, M, Alpha),
		batch_statistics(Batch, Centroids0, Stats),
		ema_update(State0, Stats, Alpha, State1),
		state_to_centroids(State1, Centroids0, NewCentroids),
		NextT is T0 + 1,
		process_batches(Rest, BatchSize, Alpha0, M, NextT, T, State1, NewCentroids, State, Centroids).

	take_batch(0, Rows, [], Rows) :-
		!.
	take_batch(_, [], [], []) :-
		!.
	take_batch(N, [Row| Rows], [Row| Batch], Rest) :-
		N > 0,
		N1 is N - 1,
		take_batch(N1, Rows, Batch, Rest).

	% stats is a list of (sum-vector, count) for the current batch, aligned with clusters
	batch_statistics(Batch, Centroids, Stats) :-
		length(Centroids, K),
		zero_stats(K, ZeroStats),
		accumulate_batch(Batch, Centroids, ZeroStats, Stats).

	zero_stats(0, []) :-
		!.
	zero_stats(K, [ZeroSum-0.0| Stats]) :-
		K > 0,
		% we do not know dimension yet; will be filled on first real point
		ZeroSum = [],
		K1 is K - 1,
		zero_stats(K1, Stats).

	accumulate_batch([], _Centroids, Stats, Stats).
	accumulate_batch([_-Vector| Batch], Centroids, Stats0, Stats) :-
		nearest_centroid_classifier(Centroids, Vector, Cluster, _),
		add_to_stat(Cluster, Vector, Stats0, Stats1),
		accumulate_batch(Batch, Centroids, Stats1, Stats).

	add_to_stat(1, Vector, [Sum0-Count0| Stats], [Sum-Count| Stats]) :-
		!,
		(	Sum0 == [] ->
			Sum = Vector,
			Count is Count0 + 1.0
		;	add_vectors(Sum0, Vector, Sum),
			Count is Count0 + 1.0
		).
	add_to_stat(Cluster, Vector, [Stat| Stats0], [Stat| Stats]) :-
		Cluster > 1,
		Next is Cluster - 1,
		add_to_stat(Next, Vector, Stats0, Stats).

	ema_update([], [], _, []).
	ema_update([Sum0-Count0| State0], [BatchSum-BatchCount| Stats], Alpha, [Sum-Count| State]) :-
		Gamma is 1.0 - Alpha,
		(	BatchCount =< 0.0 ->
			% no points assigned; just decay
			Count is Gamma * Count0,
			(	Sum0 == [] ->
				Sum = []
			;	rescale(Sum0, Gamma, Sum)
			)
		;	(	Sum0 == [] ->
				% first real assignment
				Sum = BatchSum,
				Count is BatchCount
			;	% normal EMA
				rescale(Sum0, Gamma, DecayedSum),
				rescale(BatchSum, Alpha, WeightedBatch),
				add_vectors(DecayedSum, WeightedBatch, Sum),
				Count is Gamma * Count0 + Alpha * BatchCount
			)
		),
		ema_update(State0, Stats, Alpha, State).

	revive_dead_clusters(Rows, State0, Centroids0, Threshold, Mass, State, Centroids, Reseeded) :-
		(	has_low_mass(State0, Threshold) ->
			residual_candidates(Rows, Centroids0, Pairs),
			keysort(Pairs, Candidates),
			as_dictionary([], Used),
			reseed_clusters(State0, Centroids0, Threshold, Mass, Candidates, Used, false, State, Centroids, Reseeded)
		;	State = State0,
			Centroids = Centroids0,
			Reseeded = false
		).

	has_low_mass([_-Count| State], Threshold) :-
		(	Count < Threshold ->
			true
		;	has_low_mass(State, Threshold)
		).

	residual_candidates([], _, []).
	residual_candidates([_-Vector| Rows], Centroids, Candidates) :-
		nearest_centroid_classifier(Centroids, Vector, _, Distance),
		(	Distance > 0.0 ->
			Key is -Distance,
			Candidates = [Key-Vector| Rest]
		;	Candidates = Rest
		),
		residual_candidates(Rows, Centroids, Rest).

	reseed_clusters([], [], _, _, _, _, Reseeded, [], [], Reseeded).
	reseed_clusters([Sum0-Count0| State0], [Centroid0| Centroids0], Threshold, Mass, Candidates0, Used0, Reseeded0, [Sum-Count| State], [Centroid| Centroids], Reseeded) :-
		(	Count0 < Threshold ->
			unused_candidates(Candidates0, Used0, Available)
		;	Available = Candidates0
		),
		(	Count0 < Threshold, Available = [_-Centroid| Candidates] ->
			rescale(Centroid, Mass, Sum),
			Count = Mass,
			insert(Used0, Centroid, true, Used),
			NextReseeded = true
		;	Sum = Sum0,
			Count = Count0,
			Centroid = Centroid0,
			Used = Used0,
			NextReseeded = Reseeded0,
			Candidates = Available
		),
		reseed_clusters(State0, Centroids0, Threshold, Mass, Candidates, Used, NextReseeded, State, Centroids, Reseeded).

	unused_candidates([], _, []).
	unused_candidates([Key-Vector| Candidates], Used, Rest) :-
		(	lookup(Vector, true, Used) ->
			unused_candidates(Candidates, Used, Rest)
		;	Rest = [Key-Vector| Candidates]
		).

	refine(Rows, Centroids0, Limit, Centroids, Refinement) :-
		zero_indexed_stats(Centroids0, 1, Pairs),
		as_dictionary(Pairs, Empty),
		refinement_assignments(Rows, Centroids0, Empty, Labels, Stats),
		refine(Rows, Empty, Centroids0, Labels, Stats, Limit, 0, 0.0, Centroids, Refinement).

	refine(Rows, Empty, Centroids0, Labels0, Stats0, Limit, Updates0, Shift0, Centroids, Refinement) :-
		state_to_centroids(Stats0, Centroids0, Means),
		revive_dead_clusters(Rows, Stats0, Means, 1.0, 1.0, _, Updated, Reseeded),
		max_centroid_shift(Centroids0, Updated, Shift0, Shift),
		Updates is Updates0 + 1,
		(	Updates >= Limit ->
			assignment_labels(Rows, Updated, Labels),
			(	Reseeded == false, Labels == Labels0 ->
				Status = stable
			;	Status = iteration_limit
			),
			occupied_labels(Labels, Occupied),
			Centroids = Updated,
			Refinement = refinement(Status, Updates, Occupied, Shift)
		;	refinement_assignments(Rows, Updated, Empty, Labels, Stats),
			(	Reseeded == false, Labels == Labels0 ->
				occupied_stats(Stats, 0, Occupied),
				Centroids = Updated,
				Refinement = refinement(stable, Updates, Occupied, Shift)
			;	refine(Rows, Empty, Updated, Labels, Stats, Limit, Updates, Shift, Centroids, Refinement)
			)
		).

	zero_indexed_stats([], _, []).
	zero_indexed_stats([_| Centroids], I, [I-([]-0.0)| Pairs]) :-
		Next is I + 1,
		zero_indexed_stats(Centroids, Next, Pairs).

	refinement_assignments(Rows, Centroids, Empty, Labels, Stats) :-
		accumulate_assignments(Rows, Centroids, Empty, Dictionary, Labels),
		values(Dictionary, Stats).

	accumulate_assignments([], _, Dictionary, Dictionary, []).
	accumulate_assignments([_-Vector| Rows], Centroids, Dictionary0, Dictionary, [Cluster| Labels]) :-
		nearest_centroid_classifier(Centroids, Vector, Cluster, _),
		lookup(Cluster, Sum0-Count0, Dictionary0),
		(	Count0 =< 0.0 ->
			Sum = Vector
		;	add_vectors(Sum0, Vector, Sum)
		),
		Count is Count0 + 1.0,
		update(Dictionary0, Cluster, Sum-Count, Dictionary1),
		accumulate_assignments(Rows, Centroids, Dictionary1, Dictionary, Labels).

	assignment_labels([], _, []).
	assignment_labels([_-Vector| Rows], Centroids, [Cluster| Labels]) :-
		nearest_centroid_classifier(Centroids, Vector, Cluster, _),
		assignment_labels(Rows, Centroids, Labels).

	occupied_labels(Labels, Occupied) :-
		sort(Labels, Unique),
		length(Unique, Occupied).

	occupied_stats([], Occupied, Occupied).
	occupied_stats([_-Count| Stats], Occupied0, Occupied) :-
		(	Count > 0.0 ->
			Occupied1 is Occupied0 + 1
		;	Occupied1 = Occupied0
		),
		occupied_stats(Stats, Occupied1, Occupied).

	% shared auxiliary predicates (identical to k-means where possible)

	nearest_centroid_classifier([Centroid| Centroids], Vector, Cluster, DistanceSquared) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, InitialDistance),
		nearest_centroid_classifier(Centroids, Vector, 2, 1, InitialDistance, Cluster, DistanceSquared).

	nearest_centroid_classifier([], _Vector, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_centroid_classifier([Centroid| Centroids], Vector, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, Distance),
		(	Distance < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance
		;	BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_centroid_classifier(Centroids, Vector, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	squared_euclidean_distance([], [], DistanceSquared, DistanceSquared).
	squared_euclidean_distance([Value| Values], [Centroid| Centroids], DistanceSquared0, DistanceSquared) :-
		Delta is Value - Centroid,
		DistanceSquared1 is DistanceSquared0 + Delta * Delta,
		squared_euclidean_distance(Values, Centroids, DistanceSquared1, DistanceSquared).

	add_vectors([], [], []).
	add_vectors([Left| Lefts], [Right| Rights], [Sum| Sums]) :-
		Sum is Left + Right,
		add_vectors(Lefts, Rights, Sums).

	max_centroid_shift([], [], MaxShift, MaxShift).
	max_centroid_shift([Centroid0| Centroids0], [Centroid1| Centroids1], MaxShift0, MaxShift) :-
		squared_euclidean_distance(Centroid0, Centroid1, 0.0, DistanceSquared),
		Shift is sqrt(DistanceSquared),
		MaxShift1 is max(MaxShift0, Shift),
		max_centroid_shift(Centroids0, Centroids1, MaxShift1, MaxShift).

	row_index(Rows, Index) :-
		length(Rows, N),
		(	N =< 1 ->
			Index = short(Rows)
		;	index_rows(Rows, 1, Pairs),
			as_dictionary(Pairs, Dictionary0),
			Index = indexed(N, Dictionary0)
		).

	shuffle_index(short(Rows), Rows).
	shuffle_index(indexed(N, Dictionary0), Shuffled) :-
		fisher_yates(N, Dictionary0, Dictionary),
		values(Dictionary, Shuffled).

	index_rows([], _, []).
	index_rows([Row| Rows], I, [I-Row| Pairs]) :-
		I1 is I + 1,
		index_rows(Rows, I1, Pairs).

	fisher_yates(1, Dictionary, Dictionary) :-
		!.
	fisher_yates(I, Dictionary0, Dictionary) :-
		random(R),
		J is 1 + floor(R * I),
		(	I =:= J ->
			Dictionary1 = Dictionary0
		;	lookup(I, RowI, Dictionary0),
			update(Dictionary0, J, RowJ, RowI, Intermediate),
			update(Intermediate, I, RowJ, Dictionary1)
		),
		I1 is I - 1,
		fisher_yates(I1, Dictionary1, Dictionary).

	% printing and options

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, Centroids, Options, Diagnostics),
		format('e-Means Clusterer~n', []),
		format('=================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nCentroids:~n', []),
		print_centroids(Centroids, 1).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_centroids([], _).
	print_centroids([Centroid| Centroids], Cluster) :-
		format('  cluster ~d: ~w~n', [Cluster, Centroid]),
		NextCluster is Cluster + 1,
		print_centroids(Centroids, NextCluster).

	default_option(k(2)).
	default_option(epochs(5)).					% paper default
	default_option(tolerance(1.0e-6)).
	default_option(algorithm(published)).
	default_option(initialization(random)).
	default_option(feature_scaling(off)).
	default_option(finalization(last_epoch_mean)).
	default_option(seed(1)).
	default_option(refinement_iterations(5)).

	valid_option(k(K)) :-
		integer(K),
		K > 0.
	valid_option(epochs(Epochs)) :-
		integer(Epochs),
		Epochs > 0.
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(initialization(Initialization)) :-
		once((Initialization == random; Initialization == first_k; Initialization == spread)).
	valid_option(algorithm(Algorithm)) :-
		once((Algorithm == published; Algorithm == ema_lloyd)).
	valid_option(finalization(Method)) :-
		(	Method == last_epoch_mean ->
			true
		;	nonvar(Method),
			Method = slow_ema(HalfLife),
			number(HalfLife),
			HalfLife > 0
		).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).
	valid_option(seed(Seed)) :-
		ground(Seed).
	valid_option(refinement_iterations(Limit)) :-
		integer(Limit),
		Limit > 0.

	initialize_seed(Value) :-
		(	integer(Value) ->
			randomize(Value),
			get_seed(Seed)
		;	Seed = Value
		),
		set_seed(Seed).

:- end_object.


:- object(emeans_clusterer,
	extends(emeans_clusterer(fast_random(xoshiro128pp)))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-19,
		comment is 'Convenience object for ``emeans_clusterer/1`` using the ``fast_random(xoshiro128pp)`` random number generator.',
		see_also is [emeans_clusterer(_)]
	]).

:- end_object.

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


:- object(hodge_rank,
	imports([ranking_dataset_common, score_ranker_model_common, pairwise_strength_ranker_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'HodgeRank pairwise measurement ranker. Learns one deterministic zero-sum score per item from a dataset object implementing the ``pairwise_measurement_dataset_protocol`` protocol by solving the weighted graph-Laplacian least-squares system induced by signed edge measurements and returns a self-describing ranker term with diagnostics and residuals.',
		see_also is [pairwise_measurement_dataset_protocol, ranker_protocol, massey]
	]).

	:- public(residuals/2).
	:- mode(residuals(+compound, -list(compound)), one).
	:- info(residuals/2, [
		comment is 'Returns the learned weighted edge residuals as ``residual(Item1,Item2,Residual,Weight)`` terms preserving measurement enumeration order.',
		argnames is ['Ranker', 'Residuals']
	]).

	:- uses(avltree, [
		as_dictionary/2, as_list/2 as dictionary_as_list/2, insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3, new/1 as dictionary_new/1
	]).

	:- uses(list, [
		length/2, memberchk/2, nth1/3, reverse/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_measurement_dataset(Dataset, DatasetSummary),
		^^pairwise_measurement_dataset_items(Dataset, Items),
		(   Items = [Item] ->
			singleton_ranker(Item, Options, DatasetSummary, Ranker)
		;   ^^pairwise_measurement_dataset_measurements(Dataset, Measurements),
			length(Items, Count),
			^^index_items(Items, 1, IndexPairs),
			as_dictionary(IndexPairs, IndexDictionary),
			measurement_system(Measurements, IndexDictionary, Count, Matrix, Vector),
			solve_linear_system(Matrix, Vector, ScoreValues0),
			stabilize_scores(ScoreValues0, ScoreValues),
			^^strength_pairs(Items, ScoreValues, Scores),
			measurement_residuals(Measurements, IndexDictionary, ScoreValues, Residuals),
			residual_norm(Residuals, ResidualNorm),
			build_ranker(Items, Scores, Residuals, ResidualNorm, Options, DatasetSummary, Ranker)
		).

	rank(Ranker, Candidates, Ranking) :-
		hodge_rank_data(Ranker, _Items, Scores, _Diagnostics, _Residuals),
		^^rank_by_scores(Scores, Candidates, Ranking).

	residuals(Ranker, Residuals) :-
		hodge_rank_data(Ranker, _Items, _Scores, _Diagnostics, Residuals).

	singleton_ranker(Item, Options, DatasetSummary, hodge_rank_ranker([Item], [Item-0.0], [
		model(hodge_rank),
		options(Options),
		residuals([]),
		residual_norm(0.0),
		dataset_summary(DatasetSummary)
	])).

	build_ranker(Items, Scores, Residuals, ResidualNorm, Options, DatasetSummary, hodge_rank_ranker(Items, Scores, [
		model(hodge_rank),
		options(Options),
		residuals(Residuals),
		residual_norm(ResidualNorm),
		dataset_summary(DatasetSummary)
	])).

	hodge_rank_data(Ranker, Items, Scores, Diagnostics, Residuals) :-
		::score_ranker_data(Ranker, Items, Scores, Diagnostics),
		(   memberchk(residuals(Residuals), Diagnostics) ->
			true
		;   domain_error(hodge_rank_ranker, Ranker)
		).

	valid_score_ranker_diagnostics(Items, _Scores, Diagnostics) :-
		^^valid_ranker_metadata(hodge_rank, Diagnostics),
		memberchk(residuals(Residuals), Diagnostics),
		valid_residuals(Items, Residuals),
		memberchk(residual_norm(ResidualNorm), Diagnostics),
		number(ResidualNorm),
		ResidualNorm >= 0.0.

	valid_residuals(_Items, []).
	valid_residuals(Items, [residual(Item1, Item2, Residual, Weight)| Residuals]) :-
		memberchk(Item1, Items),
		memberchk(Item2, Items),
		Item1 \== Item2,
		number(Residual),
		number(Weight),
		Weight > 0,
		valid_residuals(Items, Residuals).

	measurement_system(Measurements, IndexDictionary, Count, Matrix, Vector) :-
		dictionary_new(PairAdjacency0),
		dictionary_new(Deltas0),
		aggregate_measurements(Measurements, IndexDictionary, PairAdjacency0, PairAdjacency, Deltas0, Deltas),
		build_measurement_model(Count, PairAdjacency, Deltas, PairWeights, DeltaValues),
		hodge_system(PairWeights, DeltaValues, Matrix, Vector).

	aggregate_measurements([], _IndexDictionary, PairAdjacency, PairAdjacency, Deltas, Deltas).
	aggregate_measurements([m(Item1, Item2, Value, Weight)| Measurements], IndexDictionary, PairAdjacency0, PairAdjacency, Deltas0, Deltas) :-
		dictionary_lookup(Item1, Index1, IndexDictionary),
		dictionary_lookup(Item2, Index2, IndexDictionary),
		update_pair_adjacency_dictionary(PairAdjacency0, Index1, Index2, Weight, PairAdjacency1),
		ScaledValue is Weight * Value,
		update_weight_dictionary(Deltas0, Index1, ScaledValue, Deltas1),
		NegativeScaledValue is -ScaledValue,
		update_weight_dictionary(Deltas1, Index2, NegativeScaledValue, Deltas2),
		aggregate_measurements(Measurements, IndexDictionary, PairAdjacency1, PairAdjacency, Deltas2, Deltas).

	update_pair_adjacency_dictionary(Dictionary0, Left, Right, Delta, Dictionary) :-
		update_neighbor_weight_dictionary(Dictionary0, Left, Right, Delta, Dictionary1),
		update_neighbor_weight_dictionary(Dictionary1, Right, Left, Delta, Dictionary).

	update_neighbor_weight_dictionary(Dictionary0, Key, Neighbor, Delta, Dictionary) :-
		(   dictionary_lookup(Key, NeighborWeights0, Dictionary0) ->
			true
		;   dictionary_new(NeighborWeights0)
		),
		update_weight_dictionary(NeighborWeights0, Neighbor, Delta, NeighborWeights),
		dictionary_insert(Dictionary0, Key, NeighborWeights, Dictionary).

	update_weight_dictionary(Dictionary0, Key, Delta, Dictionary) :-
		(   dictionary_lookup(Key, Weight0, Dictionary0) ->
			Weight is Weight0 + Delta
		;   Weight = Delta
		),
		dictionary_insert(Dictionary0, Key, Weight, Dictionary).

	build_measurement_model(Count, PairAdjacency0, Deltas0, PairWeights, Deltas) :-
		dictionary_as_list(Deltas0, DeltaEntries),
		fill_weight_vector(1, Count, DeltaEntries, Deltas),
		dictionary_as_list(PairAdjacency0, PairAdjacencyEntries),
		fill_weighted_adjacency(1, Count, PairAdjacencyEntries, PairWeights).

	fill_weight_vector(Index, Count, _Entries, []) :-
		Index > Count,
		!.
	fill_weight_vector(Index, Count, [Index-Weight| Entries], [Weight| Weights]) :-
		!,
		NextIndex is Index + 1,
		fill_weight_vector(NextIndex, Count, Entries, Weights).
	fill_weight_vector(Index, Count, Entries, [0.0| Weights]) :-
		NextIndex is Index + 1,
		fill_weight_vector(NextIndex, Count, Entries, Weights).

	fill_weighted_adjacency(Index, Count, _Entries, []) :-
		Index > Count,
		!.
	fill_weighted_adjacency(Index, Count, [Index-NeighborWeights| Entries], [Neighbors| PairWeights]) :-
		!,
		dictionary_as_list(NeighborWeights, Neighbors),
		NextIndex is Index + 1,
		fill_weighted_adjacency(NextIndex, Count, Entries, PairWeights).
	fill_weighted_adjacency(Index, Count, Entries, [[]| PairWeights]) :-
		NextIndex is Index + 1,
		fill_weighted_adjacency(NextIndex, Count, Entries, PairWeights).

	hodge_system(PairWeights, Deltas, Matrix, Vector) :-
		length(PairWeights, Count),
		hodge_system(1, Count, PairWeights, Deltas, Matrix, Vector).

	hodge_system(Index, Count, _PairWeights, _Deltas, [], []) :-
		Index > Count,
		!.
	hodge_system(Index, Count, PairWeights, Deltas, [Row| Matrix], [Value| Vector]) :-
		(   Index =:= Count ->
			ones_row(Count, Row),
			Value = 0.0
		;   nth1(Index, PairWeights, Neighbors),
			nth1(Index, Deltas, Value),
			row_weight(Neighbors, 0.0, TotalWeight),
			row_coefficients(1, Count, Index, Neighbors, TotalWeight, Row)
		),
		NextIndex is Index + 1,
		hodge_system(NextIndex, Count, PairWeights, Deltas, Matrix, Vector).

	ones_row(0, []) :-
		!.
	ones_row(Count, [1.0| Row]) :-
		Count > 0,
		NextCount is Count - 1,
		ones_row(NextCount, Row).

	row_weight([], TotalWeight, TotalWeight).
	row_weight([_Neighbor-Weight| Neighbors], TotalWeight0, TotalWeight) :-
		TotalWeight1 is TotalWeight0 + Weight,
		row_weight(Neighbors, TotalWeight1, TotalWeight).

	row_coefficients(Column, Count, _Index, _Neighbors, _TotalWeight, []) :-
		Column > Count,
		!.
	row_coefficients(Column, Count, Index, Neighbors, TotalWeight, [Coefficient| Coefficients]) :-
		(   Column =:= Index ->
			Coefficient = TotalWeight
		;   neighbor_weight(Column, Neighbors, Weight) ->
			Coefficient is -Weight
		;   Coefficient = 0.0
		),
		NextColumn is Column + 1,
		row_coefficients(NextColumn, Count, Index, Neighbors, TotalWeight, Coefficients).

	neighbor_weight(Column, [Neighbor-Weight| _Neighbors], Weight) :-
		Column =:= Neighbor,
		!.
	neighbor_weight(Column, [_Neighbor-_Weight| Neighbors], Weight) :-
		neighbor_weight(Column, Neighbors, Weight).

	solve_linear_system(Matrix, Vector, Solution) :-
		linear_system_scale(Matrix, Vector, Scale),
		augment_rows(Matrix, Vector, Rows),
		triangularize(Rows, Scale, UpperRows),
		back_substitution(UpperRows, Scale, Solution),
		validate_solution(Matrix, Vector, Solution, Scale).

	linear_system_scale(Matrix, Vector, Scale) :-
		matrix_max_abs(Matrix, 0.0, MatrixScale),
		vector_max_abs(Vector, 0.0, VectorScale),
		Maximum is max(MatrixScale, VectorScale),
		Scale is max(1.0, Maximum).

	matrix_max_abs([], Maximum, Maximum).
	matrix_max_abs([Row| Matrix], Maximum0, Maximum) :-
		row_max_abs(Row, 0.0, RowMaximum),
		Maximum1 is max(Maximum0, RowMaximum),
		matrix_max_abs(Matrix, Maximum1, Maximum).

	row_max_abs([], Maximum, Maximum).
	row_max_abs([Value| Row], Maximum0, Maximum) :-
		Magnitude is abs(Value),
		Maximum1 is max(Maximum0, Magnitude),
		row_max_abs(Row, Maximum1, Maximum).

	vector_max_abs([], Maximum, Maximum).
	vector_max_abs([Value| Vector], Maximum0, Maximum) :-
		Magnitude is abs(Value),
		Maximum1 is max(Maximum0, Magnitude),
		vector_max_abs(Vector, Maximum1, Maximum).

	augment_rows([], [], []).
	augment_rows([Row| Matrix], [Value| Vector], [row(Row, Value)| Rows]) :-
		augment_rows(Matrix, Vector, Rows).

	triangularize([], _Scale, []).
	triangularize([Row0| Rows0], Scale, [PivotRow| UpperRows]) :-
		select_pivot_row([Row0| Rows0], PivotRow, Rows),
		PivotRow = row([Pivot| PivotTail], PivotValue),
		ensure_non_zero(Pivot, Scale),
		eliminate_rows(Pivot, PivotTail, PivotValue, Rows, ReducedRows),
		triangularize(ReducedRows, Scale, UpperRows).

	select_pivot_row([Row| Rows], PivotRow, RemainingRows) :-
		select_pivot_row(Rows, Row, [], PivotRow, RemainingRows).

	select_pivot_row([], PivotRow, RemainingRows, PivotRow, RemainingRows).
	select_pivot_row([Row| Rows], Candidate0, RemainingRows0, PivotRow, RemainingRows) :-
		leading_magnitude(Row, Magnitude),
		leading_magnitude(Candidate0, CandidateMagnitude),
		(   Magnitude > CandidateMagnitude ->
			Candidate = Row,
			RemainingRows1 = [Candidate0| RemainingRows0]
		;   Candidate = Candidate0,
			RemainingRows1 = [Row| RemainingRows0]
		),
		select_pivot_row(Rows, Candidate, RemainingRows1, PivotRow, RemainingRows).

	leading_magnitude(row([Leading| _Tail], _Value), Magnitude) :-
		Magnitude is abs(Leading).

	ensure_non_zero(Value, Scale) :-
		Threshold is Scale * 1.0e-12,
		(   abs(Value) > Threshold ->
			true
		;   evaluation_error(zero_divisor)
		).

	eliminate_rows(_Pivot, _PivotTail, _PivotValue, [], []) :-
		!.
	eliminate_rows(Pivot, PivotTail, PivotValue, [row([Leading| Tail], Value)| Rows], [row(NewTail, NewValue)| ReducedRows]) :-
		Factor is Leading / Pivot,
		scaled_row_difference(PivotTail, Tail, Factor, NewTail),
		NewValue is Value - Factor * PivotValue,
		eliminate_rows(Pivot, PivotTail, PivotValue, Rows, ReducedRows).

	scaled_row_difference([], [], _Factor, []).
	scaled_row_difference([PivotCoefficient| PivotTail], [Coefficient| Tail], Factor, [NewCoefficient| NewTail]) :-
		NewCoefficient is Coefficient - Factor * PivotCoefficient,
		scaled_row_difference(PivotTail, Tail, Factor, NewTail).

	back_substitution(UpperRows, Scale, Solution) :-
		reverse(UpperRows, ReversedRows),
		back_substitution(ReversedRows, Scale, [], Solution).

	back_substitution([], _Scale, Solution, Solution).
	back_substitution([row([Diagonal], Value)| Rows], Scale, KnownSolutions0, KnownSolutions) :-
		!,
		ensure_non_zero(Diagonal, Scale),
		Solution is Value / Diagonal,
		back_substitution(Rows, Scale, [Solution| KnownSolutions0], KnownSolutions).
	back_substitution([row([Diagonal| Tail], Value)| Rows], Scale, KnownSolutions0, KnownSolutions) :-
		ensure_non_zero(Diagonal, Scale),
		dot_product(Tail, KnownSolutions0, Correction),
		Solution is (Value - Correction) / Diagonal,
		back_substitution(Rows, Scale, [Solution| KnownSolutions0], KnownSolutions).

	validate_solution(Matrix, Vector, Solution, Scale) :-
		maximum_residual(Matrix, Vector, Solution, 0.0, MaximumResidual),
		Tolerance is Scale * 1.0e-8,
		(   MaximumResidual =< Tolerance ->
			true
		;   domain_error(hodge_rank_linear_system_residual, MaximumResidual)
		).

	maximum_residual([], [], _Solution, MaximumResidual, MaximumResidual).
	maximum_residual([Row| Matrix], [Value| Vector], Solution, MaximumResidual0, MaximumResidual) :-
		dot_product(Row, Solution, RowValue),
		Residual is abs(RowValue - Value),
		(   Residual > MaximumResidual0 ->
			MaximumResidual1 = Residual
		;   MaximumResidual1 = MaximumResidual0
		),
		maximum_residual(Matrix, Vector, Solution, MaximumResidual1, MaximumResidual).

	stabilize_scores([], []).
	stabilize_scores([Score0| Scores0], [Score| Scores]) :-
		stabilize_score(Score0, Score),
		stabilize_scores(Scores0, Scores).

	stabilize_score(Score0, 0.0) :-
		abs(Score0) =< 1.0e-12,
		!.
	stabilize_score(Score, Score).

	measurement_residuals([], _IndexDictionary, _ScoreValues, []).
	measurement_residuals([m(Item1, Item2, Value, Weight)| Measurements], IndexDictionary, ScoreValues, [residual(Item1, Item2, Residual, Weight)| Residuals]) :-
		dictionary_lookup(Item1, Index1, IndexDictionary),
		dictionary_lookup(Item2, Index2, IndexDictionary),
		nth1(Index1, ScoreValues, Score1),
		nth1(Index2, ScoreValues, Score2),
		FittedValue is Score1 - Score2,
		Residual0 is Value - FittedValue,
		stabilize_score(Residual0, Residual),
		measurement_residuals(Measurements, IndexDictionary, ScoreValues, Residuals).

	residual_norm(Residuals, ResidualNorm) :-
		residual_norm(Residuals, 0.0, SumSquares),
		ResidualNorm0 is sqrt(SumSquares),
		stabilize_score(ResidualNorm0, ResidualNorm).

	residual_norm([], SumSquares, SumSquares).
	residual_norm([residual(_Item1, _Item2, Residual, Weight)| Residuals], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Weight * Residual * Residual,
		residual_norm(Residuals, SumSquares1, SumSquares).

	score_ranker_model(hodge_rank).

	score_ranker_label('HodgeRank').

	score_ranker_term(Items, Scores, Diagnostics, hodge_rank_ranker(Items, Scores, Diagnostics)).

	valid_score(Score) :-
		number(Score).

	valid_option(_Option) :-
		fail.

	default_option(_Option) :-
		fail.

	fix_option(_Option, _FixedOption) :-
		fail.

:- end_object.

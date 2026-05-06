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


:- object(massey_ranker,
	imports([ranking_dataset_common, score_ranker_model_common, pairwise_strength_ranker_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Massey pairwise preference ranker. Learns one deterministic rating per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by solving the Massey linear system built from aggregated pairwise outcomes and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, colley_ranker, elo_ranker]
	]).

	:- uses(avltree, [
		as_dictionary/2
	]).

	:- uses(list, [
		length/2, nth1/3, reverse/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		(	Items = [Item] ->
			singleton_ranker(Item, Options, DatasetSummary, Ranker)
		;	^^pairwise_dataset_matchups(Dataset, Matchups),
			length(Items, Count),
			^^index_items(Items, 1, IndexPairs),
			as_dictionary(IndexPairs, IndexDictionary),
			^^preprocess_matchups(Matchups, IndexDictionary, _DirectedAdjacency, _ReverseAdjacency, WinWeights, PairAdjacency),
			^^build_dataset_model(Count, WinWeights, PairAdjacency, PairWeights, Wins),
			massey_system(PairWeights, Wins, Matrix, Vector),
			solve_linear_system(Matrix, Vector, RatingValues0),
			stabilize_ratings(RatingValues0, RatingValues),
			^^strength_pairs(Items, RatingValues, Ratings),
			^^build_score_ranker(Items, Ratings, Options, DatasetSummary, Ranker)
		).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Ratings, _Diagnostics),
		^^rank_by_scores(Ratings, Candidates, Ranking).

	singleton_ranker(Item, Options, DatasetSummary, Ranker) :-
		^^build_score_ranker([Item], [Item-0.0], Options, DatasetSummary, Ranker).

	massey_system(PairWeights, Wins, Matrix, Vector) :-
		length(PairWeights, Count),
		massey_system(1, Count, PairWeights, Wins, Matrix, Vector).

	massey_system(Index, Count, _PairWeights, _Wins, [], []) :-
		Index > Count,
		!.
	massey_system(Index, Count, PairWeights, Wins, [Row| Matrix], [Value| Vector]) :-
		(	Index =:= Count ->
			ones_row(Count, Row),
			Value = 0.0
		;	nth1(Index, PairWeights, Neighbors),
			nth1(Index, Wins, WinCount),
			row_games(Neighbors, 0.0, Games),
			row_value(WinCount, Games, Value),
			row_coefficients(1, Count, Index, Neighbors, Games, Row)
		),
		NextIndex is Index + 1,
		massey_system(NextIndex, Count, PairWeights, Wins, Matrix, Vector).

	ones_row(0, []) :-
		!.
	ones_row(Count, [1.0| Row]) :-
		Count > 0,
		NextCount is Count - 1,
		ones_row(NextCount, Row).

	row_games([], Games, Games).
	row_games([_Neighbor-Weight| Neighbors], Games0, Games) :-
		Games1 is Games0 + Weight,
		row_games(Neighbors, Games1, Games).

	row_value(Wins, Games, Value) :-
		Value is 2.0 * Wins - Games.

	row_coefficients(Column, Count, _Index, _Neighbors, _Games, []) :-
		Column > Count,
		!.
	row_coefficients(Column, Count, Index, Neighbors, Games, [Coefficient| Coefficients]) :-
		(	Column =:= Index ->
			Coefficient = Games
		;	neighbor_weight(Column, Neighbors, Weight) ->
			Coefficient is -Weight
		;	Coefficient = 0.0
		),
		NextColumn is Column + 1,
		row_coefficients(NextColumn, Count, Index, Neighbors, Games, Coefficients).

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
		(	Magnitude > CandidateMagnitude ->
			Candidate = Row,
			RemainingRows1 = [Candidate0| RemainingRows0]
		;	Candidate = Candidate0,
			RemainingRows1 = [Row| RemainingRows0]
		),
		select_pivot_row(Rows, Candidate, RemainingRows1, PivotRow, RemainingRows).

	leading_magnitude(row([Leading| _Tail], _Value), Magnitude) :-
		Magnitude is abs(Leading).

	ensure_non_zero(Value, Scale) :-
		Threshold is Scale * 1.0e-12,
		(	abs(Value) > Threshold ->
			true
		;	evaluation_error(zero_divisor)
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
		(	MaximumResidual =< Tolerance ->
			true
		;	domain_error(massey_linear_system_residual, MaximumResidual)
		).

	maximum_residual([], [], _Solution, MaximumResidual, MaximumResidual).
	maximum_residual([Row| Matrix], [Value| Vector], Solution, MaximumResidual0, MaximumResidual) :-
		dot_product(Row, Solution, RowValue),
		Residual is abs(RowValue - Value),
		(	Residual > MaximumResidual0 ->
			MaximumResidual1 = Residual
		;	MaximumResidual1 = MaximumResidual0
		),
		maximum_residual(Matrix, Vector, Solution, MaximumResidual1, MaximumResidual).

	stabilize_ratings([], []).
	stabilize_ratings([Rating0| Ratings0], [Rating| Ratings]) :-
		stabilize_rating(Rating0, Rating),
		stabilize_ratings(Ratings0, Ratings).

	stabilize_rating(Rating0, 0.0) :-
		abs(Rating0) =< 1.0e-12,
		!.
	stabilize_rating(Rating, Rating).

	score_ranker_model(massey_ranker).

	score_ranker_label('Massey').

	score_ranker_term(Items, Ratings, Diagnostics, massey_ranker(Items, Ratings, Diagnostics)).

	valid_score(Score) :-
		number(Score).

	valid_option(_Option) :-
		fail.

	default_option(_Option) :-
		fail.

	fix_option(_Option, _FixedOption) :-
		fail.

:- end_object.

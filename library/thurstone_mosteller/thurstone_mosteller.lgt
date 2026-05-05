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


:- object(thurstone_mosteller,
	imports([ranking_dataset_common, score_ranker_model_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Thurstone-Mosteller Case V pairwise preference ranker. Learns one real-valued latent utility per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by fitting continuity-corrected empirical paired-comparison probabilities with a deterministic weighted least-squares normal model, and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, bradley_terry, regularized_bradley_terry]
	]).

	:- uses(avltree, [
		as_dictionary/2, lookup/3 as dictionary_lookup/3
	]).

	:- uses(list, [
		length/2, memberchk/2, reverse/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3, sum/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		(   Items = [Item] ->
			singleton_ranker(Item, Options, DatasetSummary, Ranker)
		;   ^^pairwise_dataset_matchups(Dataset, Matchups),
			length(Items, Count),
			index_items(Items, 1, IndexPairs),
			as_dictionary(IndexPairs, IndexDictionary),
			build_system(Matchups, IndexDictionary, Count, Matrix, Vector),
			solve_linear_system(Matrix, Vector, ReducedUtilities),
			append_anchor_utility(ReducedUtilities, RawUtilities0),
			center_utilities(RawUtilities0, Utilities),
			score_pairs(Items, Utilities, Scores),
			Ranker = thurstone_mosteller_ranker(Items, Scores, [
				model(thurstone_mosteller),
				options(Options),
				fit(weighted_least_squares),
				continuity_correction(0.5),
				dataset_summary(DatasetSummary)
			])
		).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Scores, _Diagnostics),
		^^rank_by_scores(Scores, Candidates, Ranking).

	singleton_ranker(Item, Options, DatasetSummary, thurstone_mosteller_ranker([Item], [Item-0.0], [
		model(thurstone_mosteller),
		options(Options),
		fit(weighted_least_squares),
		continuity_correction(0.5),
		dataset_summary(DatasetSummary)
	])).

	valid_score_ranker_diagnostics(_Items, _Scores, Diagnostics) :-
		^^valid_ranker_metadata(thurstone_mosteller, Diagnostics),
		memberchk(fit(weighted_least_squares), Diagnostics),
		memberchk(continuity_correction(ContinuityCorrection), Diagnostics),
		number(ContinuityCorrection),
		ContinuityCorrection > 0.0.

	build_system(Matchups, IndexDictionary, Count, Matrix, Vector) :-
		VariableCount is Count - 1,
		zero_matrix(VariableCount, Matrix0),
		zero_vector(VariableCount, Vector0),
		build_system(Matchups, IndexDictionary, Count, Matrix0, Matrix, Vector0, Vector).

	build_system([], _IndexDictionary, _Count, Matrix, Matrix, Vector, Vector).
	build_system([matchup(Item1, Item2, Item1Wins, Item2Wins)| Matchups], IndexDictionary, Count, Matrix0, Matrix, Vector0, Vector) :-
		dictionary_lookup(Item1, Index1, IndexDictionary),
		dictionary_lookup(Item2, Index2, IndexDictionary),
		Weight is Item1Wins + Item2Wins,
		Probability is (Item1Wins + 0.5) / (Weight + 1.0),
		probit(Probability, Difference),
		update_system(Index1, Index2, Count, Weight, Difference, Matrix0, Matrix1, Vector0, Vector1),
		build_system(Matchups, IndexDictionary, Count, Matrix1, Matrix, Vector1, Vector).

	update_system(Index1, Index2, Count, Weight, Difference, Matrix0, Matrix, Vector0, Vector) :-
		(   Index1 =:= Count ->
			update_anchor_system(Index2, -Difference, Weight, Matrix0, Matrix, Vector0, Vector)
		;   Index2 =:= Count ->
			update_anchor_system(Index1, Difference, Weight, Matrix0, Matrix, Vector0, Vector)
		;   update_matrix_entry(Matrix0, Index1, Index1, Weight, Matrix1),
			update_matrix_entry(Matrix1, Index2, Index2, Weight, Matrix2),
			update_matrix_entry(Matrix2, Index1, Index2, -Weight, Matrix3),
			update_matrix_entry(Matrix3, Index2, Index1, -Weight, Matrix),
			update_vector_entry(Vector0, Index1, Weight * Difference, Vector1),
			update_vector_entry(Vector1, Index2, -Weight * Difference, Vector)
		).

	update_anchor_system(Index, Difference, Weight, Matrix0, Matrix, Vector0, Vector) :-
		update_matrix_entry(Matrix0, Index, Index, Weight, Matrix),
		update_vector_entry(Vector0, Index, Weight * Difference, Vector).

	zero_matrix(Count, Matrix) :-
		zero_matrix(Count, Count, Matrix).

	zero_matrix(0, _Width, []) :-
		!.
	zero_matrix(RemainingRows, Width, [Row| Matrix]) :-
		RemainingRows > 0,
		zero_vector(Width, Row),
		NextRemainingRows is RemainingRows - 1,
		zero_matrix(NextRemainingRows, Width, Matrix).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Vector]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Vector).

	update_matrix_entry([Row| Matrix], 1, Column, Delta, [UpdatedRow| Matrix]) :-
		!,
		update_vector_entry(Row, Column, Delta, UpdatedRow).
	update_matrix_entry([Row| Matrix], RowIndex, Column, Delta, [Row| UpdatedMatrix]) :-
		RowIndex > 1,
		NextRowIndex is RowIndex - 1,
		update_matrix_entry(Matrix, NextRowIndex, Column, Delta, UpdatedMatrix).

	update_vector_entry([Value| Vector], 1, Delta, [UpdatedValue| Vector]) :-
		!,
		UpdatedValue is Value + Delta.
	update_vector_entry([Value| Vector], Index, Delta, [Value| UpdatedVector]) :-
		Index > 1,
		NextIndex is Index - 1,
		update_vector_entry(Vector, NextIndex, Delta, UpdatedVector).

	solve_linear_system(Matrix, Vector, Solution) :-
		augment_rows(Matrix, Vector, Rows),
		triangularize(Rows, UpperRows),
		back_substitution(UpperRows, Solution),
		validate_solution(Matrix, Vector, Solution).

	augment_rows([], [], []).
	augment_rows([Row| Matrix], [Value| Vector], [row(Row, Value)| Rows]) :-
		augment_rows(Matrix, Vector, Rows).

	triangularize([], []).
	triangularize([Row0| Rows0], [PivotRow| UpperRows]) :-
		select_pivot_row([Row0| Rows0], PivotRow, Rows),
		PivotRow = row([Pivot| PivotTail], PivotValue),
		ensure_non_zero(Pivot),
		eliminate_rows(Pivot, PivotTail, PivotValue, Rows, ReducedRows),
		triangularize(ReducedRows, UpperRows).

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

	ensure_non_zero(Value) :-
		(   abs(Value) > 1.0e-12 ->
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

	back_substitution(UpperRows, Solution) :-
		reverse(UpperRows, ReversedRows),
		back_substitution(ReversedRows, [], Solution).

	back_substitution([], Solution, Solution).
	back_substitution([row([Diagonal], Value)| Rows], KnownSolutions0, KnownSolutions) :-
		!,
		ensure_non_zero(Diagonal),
		Solution is Value / Diagonal,
		back_substitution(Rows, [Solution| KnownSolutions0], KnownSolutions).
	back_substitution([row([Diagonal| Tail], Value)| Rows], KnownSolutions0, KnownSolutions) :-
		ensure_non_zero(Diagonal),
		dot_product(Tail, KnownSolutions0, Correction),
		Solution is (Value - Correction) / Diagonal,
		back_substitution(Rows, [Solution| KnownSolutions0], KnownSolutions).

	validate_solution(Matrix, Vector, Solution) :-
		maximum_residual(Matrix, Vector, Solution, 0.0, MaximumResidual),
		(   MaximumResidual =< 1.0e-8 ->
			true
		;   domain_error(thurstone_mosteller_linear_system_residual, MaximumResidual)
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

	append_anchor_utility([], [0.0]).
	append_anchor_utility([Utility| Utilities], [Utility| AnchoredUtilities]) :-
		append_anchor_utility(Utilities, AnchoredUtilities).

	center_utilities(Utilities0, Utilities) :-
		sum(Utilities0, Total),
		length(Utilities0, Count),
		Mean is Total / Count,
		center_utilities(Utilities0, Mean, Utilities).

	center_utilities([], _Mean, []).
	center_utilities([Utility0| Utilities0], Mean, [Utility| Utilities]) :-
		Utility is Utility0 - Mean,
		center_utilities(Utilities0, Mean, Utilities).

	probit(Probability, Quantile) :-
		Plow = 0.02425,
		Phigh is 1.0 - Plow,
		(   Probability < Plow ->
			Q is sqrt(-2.0 * log(Probability)),
			inverse_tail(Q, Quantile)
		;   Probability =< Phigh ->
			Q is Probability - 0.5,
			R is Q * Q,
			inverse_central(Q, R, Quantile)
		;   Q is sqrt(-2.0 * log(1.0 - Probability)),
			inverse_tail(Q, TailQuantile),
			Quantile is -TailQuantile
		).

	inverse_tail(Q, Quantile) :-
		Numerator is (((((-0.007784894002430293 * Q - 0.3223964580411365) * Q - 2.400758277161838) * Q - 2.549732539343734) * Q + 4.374664141464968) * Q + 2.938163982698783),
		Denominator is ((((0.007784695709041462 * Q + 0.3224671290700398) * Q + 2.445134137142996) * Q + 3.754408661907416) * Q + 1.0),
		Quantile is Numerator / Denominator.

	inverse_central(Q, R, Quantile) :-
		Numerator is (((((-39.69683028665376 * R + 220.9460984245205) * R - 275.9285104469687) * R + 138.3577518672690) * R - 30.66479806614716) * R + 2.506628277459239) * Q,
		Denominator is (((((-54.47609879822406 * R + 161.5858368580409) * R - 155.6989798598866) * R + 66.80131188771972) * R - 13.28068155288572) * R + 1.0),
		Quantile is Numerator / Denominator.

	score_pairs([], [], []).
	score_pairs([Item| Items], [Score| ScoreValues], [Item-Score| Scores]) :-
		score_pairs(Items, ScoreValues, Scores).

	index_items([], _Index, []).
	index_items([Item| Items], Index, [Item-Index| Indices]) :-
		NextIndex is Index + 1,
		index_items(Items, NextIndex, Indices).

	score_ranker_model(thurstone_mosteller).

	score_ranker_label('Thurstone-Mosteller').

	score_ranker_term(Items, Scores, Diagnostics, thurstone_mosteller_ranker(Items, Scores, Diagnostics)).

	valid_score(Score) :-
		number(Score).

:- end_object.

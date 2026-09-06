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

:- category(language_detection_scoring).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Shared portable language detection scoring predicates.'
	]).

	:- protected(cosine_similarity/3).
	:- mode(cosine_similarity(+list(pair(atom,positive_integer)), +list(pair(atom,positive_integer)), -float), one).
	:- info(cosine_similarity/3, [
		comment is 'Computes cosine similarity between two lexically sorted sparse count vectors.',
		argnames is ['Counts1', 'Counts2', 'Similarity']
	]).

	:- protected(normalize_scores/2).
	:- mode(normalize_scores(+list(pair(atom,number)), -list(pair(atom,float))), one).
	:- info(normalize_scores/2, [
		comment is 'Normalizes non-negative scores to sum to one or returns an empty list when their total is zero.',
		argnames is ['RawScores', 'Scores']
	]).

	cosine_similarity(Counts1, Counts2, Similarity) :-
		dot_product(Counts1, Counts2, 0.0, DotProduct),
		square_sum(Counts1, 0.0, SquareSum1),
		square_sum(Counts2, 0.0, SquareSum2),
		Denominator is sqrt(SquareSum1) * sqrt(SquareSum2),
		(	Denominator =< 0.0 ->
			Similarity = 0.0
		;	Similarity is DotProduct / Denominator
		).

	dot_product([], _, DotProduct, DotProduct) :-
		!.
	dot_product(_, [], DotProduct, DotProduct) :-
		!.
	dot_product([Gram1-Count1| Counts1], [Gram2-Count2| Counts2], DotProduct0, DotProduct) :-
		compare(Order, Gram1, Gram2),
		dot_product(Order, Count1, Count2, Counts1, Counts2, Gram1, Gram2, DotProduct0, DotProduct).

	dot_product(=, Count1, Count2, Counts1, Counts2, _, _, DotProduct0, DotProduct) :-
		DotProduct1 is DotProduct0 + Count1 * Count2,
		dot_product(Counts1, Counts2, DotProduct1, DotProduct).
	dot_product(<, _, Count2, Counts1, Counts2, _, Gram2, DotProduct0, DotProduct) :-
		dot_product(Counts1, [Gram2-Count2| Counts2], DotProduct0, DotProduct).
	dot_product(>, Count1, _, Counts1, Counts2, Gram1, _, DotProduct0, DotProduct) :-
		dot_product([Gram1-Count1| Counts1], Counts2, DotProduct0, DotProduct).

	square_sum([], SquareSum, SquareSum).
	square_sum([_-Count| Counts], SquareSum0, SquareSum) :-
		SquareSum1 is SquareSum0 + Count * Count,
		square_sum(Counts, SquareSum1, SquareSum).

	normalize_scores(RawScores, Scores) :-
		sum_scores(RawScores, 0.0, Total),
		(	Total =< 0.0 ->
			Scores = []
		;	normalize_scores(RawScores, Total, Scores)
		).

	sum_scores([], Total, Total).
	sum_scores([_-Score| Scores], Total0, Total) :-
		Total1 is Total0 + Score,
		sum_scores(Scores, Total1, Total).

	normalize_scores([], _, []).
	normalize_scores([Language-RawScore| RawScores], Total, [Language-Score| Scores]) :-
		Score is RawScore / Total,
		normalize_scores(RawScores, Total, Scores).

:- end_category.

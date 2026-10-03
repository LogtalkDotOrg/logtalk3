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


:- object(spearman_similarity,
	imports(similarity_metric_common),
	implements(similarity_metric_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Spearman rank correlation over common keys, using average ranks for numeric ties. Results range from -1.0 to 1.0. Fewer than two common keys or constant common values in either vector gives 0.0. Each key must occur at most once per vector.'
	]).

	:- uses(list, [
		length/2
	]).

	similarity(Vector1, Vector2, Similarity) :-
		^^common_pairs(Vector1, Vector2, Pairs),
		length(Pairs, Count),
		(	Count < 2 ->
			Similarity = 0.0
		;	^^split_pairs(Pairs, Values1, Values2),
			average_ranks(Values1, Values1, Ranks1),
			average_ranks(Values2, Values2, Ranks2),
			^^centered_normalized_values(Ranks1, Normalized1),
			^^centered_normalized_values(Ranks2, Normalized2),
			^^dot_product(Normalized1, Normalized2, Dot),
			^^bounded_similarity(Dot, Similarity)
		).

	average_ranks([], _AllValues, []).
	average_ranks([Value| Values], AllValues, [Rank| Ranks]) :-
		rank_counts(AllValues, Value, 0, 0, Less, Equal),
		Rank is Less + (Equal + 1) / 2,
		average_ranks(Values, AllValues, Ranks).

	rank_counts([], _Value, Less, Equal, Less, Equal).
	rank_counts([Other| Values], Value, Less0, Equal0, Less, Equal) :-
		(	Other < Value ->
			Less1 is Less0 + 1,
			Equal1 = Equal0
		;	(	Other =:= Value ->
				Less1 = Less0,
				Equal1 is Equal0 + 1
			;	Less1 = Less0,
				Equal1 = Equal0
			)
		),
		rank_counts(Values, Value, Less1, Equal1, Less, Equal).

:- end_object.
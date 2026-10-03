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


:- object(msd_similarity,
	imports(similarity_metric_common),
	implements(similarity_metric_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Inverse mean-squared-difference similarity over common keys: 1 / (1 + MSD). Results range from 0.0 to 1.0, with 1.0 for identical common ratings and 0.0 when no keys are common. Scaled differences avoid squaring original magnitudes. Each key must occur at most once per vector.'
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(numberlist, [
		max/2
	]).

	similarity(Vector1, Vector2, Similarity) :-
		^^common_pairs(Vector1, Vector2, Pairs),
		(	Pairs == [] ->
			Similarity = 0.0
		;	half_differences(Pairs, Differences),
			max(Differences, Scale),
			(	Scale =:= 0 ->
				Similarity = 1.0
			;	^^scale_values(Differences, Scaled),
				^^sum_of_squares_list(Scaled, SumSquares),
				length(Pairs, Count),
				Factor is sqrt(SumSquares / Count),
				(	Scale =< 0.5 ->
					Distance is (2 * Scale) * Factor,
					Similarity is 1 / (1 + Distance * Distance)
				;	InverseDistance is (0.5 / Scale) / Factor,
					InverseSquared is InverseDistance * InverseDistance,
					Similarity is InverseSquared / (1 + InverseSquared)
				)
			)
		).

	half_differences([], []).
	half_differences([Value1-Value2| Pairs], [Difference| Differences]) :-
		(	(Value1 >= 0, Value2 >= 0 ; Value1 =< 0, Value2 =< 0) ->
			Difference is abs((Value1 - Value2) / 2)
		;	Difference is abs(Value1) / 2 + abs(Value2) / 2
		),
		half_differences(Pairs, Differences).

:- end_object.
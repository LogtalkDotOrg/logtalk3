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


:- object(correlation_score,
	imports(feature_scoring_common),
	implements(feature_scoring_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Scores a numeric feature against a numeric target using squared Pearson correlation over complete examples. Returns a score in [0.0, 1.0], independent of correlation sign. Returns 0.0 for fewer than two complete examples or an exactly constant feature or target.'
	]).

	:- uses(list, [
		length/2
	]).

	score(Values, Targets, Score) :-
		^^complete_pairs(Values, Targets, Pairs),
		length(Pairs, Count),
		(	Count < 2 ->
			Score = 0.0
		;	^^split_pairs(Pairs, FeatureValues, TargetValues),
			^^scale_values(FeatureValues, ScaledFeature),
			^^scale_values(TargetValues, ScaledTarget),
			^^centered_values(ScaledFeature, FeatureOffsets),
			^^centered_values(ScaledTarget, TargetOffsets),
			^^scale_values(FeatureOffsets, CenteredFeature),
			^^scale_values(TargetOffsets, CenteredTarget),
			^^dot_product(CenteredFeature, CenteredTarget, Dot),
			^^sum_of_squares_list(CenteredFeature, SumSquaresFeature),
			^^sum_of_squares_list(CenteredTarget, SumSquaresTarget),
			(	(SumSquaresFeature =< 0.0 ; SumSquaresTarget =< 0.0) ->
				Score = 0.0
			;	Correlation is Dot / sqrt(SumSquaresFeature) / sqrt(SumSquaresTarget),
				Score is min(1.0, max(0.0, Correlation * Correlation))
			)
		).

:- end_object.

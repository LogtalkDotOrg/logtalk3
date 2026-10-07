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


:- object(anova_f_score,
	imports(feature_scoring_common),
	implements(feature_scoring_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Scores a numeric feature against a categorical target using the one-way ANOVA F-statistic over complete examples, capped at 1.0e10. Returns 0.0 for fewer than two classes, no within-class degrees of freedom, or a constant feature. Perfect class separation scores 1.0e10 and ties with any finite statistic reaching the cap.'
	]).

	score(Values, Targets, Score) :-
		^^complete_pairs(Values, Targets, CompletePairs),
		^^class_scatter(CompletePairs, TotalCount, GroupCount, SumSquaresBetween, SumSquaresWithin),
		(	(GroupCount < 2 ; TotalCount =< GroupCount) ->
			Score = 0.0
		;	DegreesFreedomBetween is GroupCount - 1,
			DegreesFreedomWithin is TotalCount - GroupCount,
			MeanSquareBetween is SumSquaresBetween / DegreesFreedomBetween,
			(	SumSquaresWithin =< 0.0 ->
				(	MeanSquareBetween =< 0.0 ->
					Score = 0.0
				;	Score = 1.0e10
				)
			;	MeanSquareWithin is SumSquaresWithin / DegreesFreedomWithin,
				(	MeanSquareBetween >= 1.0e10 * MeanSquareWithin ->
					Score = 1.0e10
				;	Score is MeanSquareBetween / MeanSquareWithin
				)
			)
		).

:- end_object.

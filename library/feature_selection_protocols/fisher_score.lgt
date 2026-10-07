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


:- object(fisher_score,
	imports(feature_scoring_common),
	implements(feature_scoring_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Scores a numeric feature against categorical targets using between-class scatter divided by within-class scatter, capped at 1.0e10.'
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(type, [
		check/3
	]).

	:- private(check_complete_pairs/2).
	:- mode(check_complete_pairs(+list(pair), +term), one_or_error).
	:- info(check_complete_pairs/2, [
		comment is 'Checks complete numeric feature values and atomic target labels.',
		argnames is ['Pairs', 'Context'],
		exceptions is [
			'A feature value is not numeric' - type_error(number, 'Value'),
			'A target label is not atomic' - type_error(atomic, 'Target')
		]
	]).

	score(Values, Targets, Score) :-
		context(Context),
		check(list, Values, Context),
		check(list, Targets, Context),
		length(Values, ValueCount),
		length(Targets, TargetCount),
		(	ValueCount =:= TargetCount ->
			true
		;	throw(error(consistency_error(list_length, ValueCount, TargetCount), Context))
		),
		^^complete_pairs(Values, Targets, Pairs),
		check_complete_pairs(Pairs, Context),
		^^class_scatter(Pairs, _ObservationCount, ClassCount, Between, Within),
		(	(ClassCount < 2; Between =< 0.0) ->
			Score = 0.0
		;	(	Within =< 0.0 ->
				Score = 1.0e10
			;	(	Between >= 1.0e10 * Within ->
					Score = 1.0e10
				;	Score is Between / Within
				)
			)
		).

	check_complete_pairs([], _Context).
	check_complete_pairs([Value-Target| Pairs], Context) :-
		check(number, Value, Context),
		check(atomic, Target, Context),
		check_complete_pairs(Pairs, Context).

:- end_object.

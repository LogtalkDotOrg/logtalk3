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


:- object(variance_score,
	imports(feature_scoring_common),
	implements(feature_scoring_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Unsupervised feature scoring by population variance: ignores Targets entirely. A near-constant feature (one that takes almost the same value in every example) carries little information for distinguishing examples and so scores low; this is the classic "variance threshold" filter criterion. Scores 0.0 when every value is missing.'
	]).

	:- uses(list, [
		length/2
	]).

	score(Values, _Targets, Score) :-
		^^complete_values(Values, Complete),
		length(Complete, Count),
		(	Count =:= 0 ->
			Score = 0.0
		;	^^variance_values(Complete, Score)
		).

:- end_object.

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


:- category(feature_redundancy,
	extends(feature_discretization)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Mutual information redundancy in bits between aligned, already prepared categorical feature columns.'
	]).

	:- protected(feature_pair_mutual_information/3).
	:- mode(feature_pair_mutual_information(+list(pair), +list(pair), -float), one).
	:- info(feature_pair_mutual_information/3, [
		comment is 'Computes raw mutual information between the feature values in two same-length, complete ``Value-Target`` columns without refitting discretization.',
		argnames is ['LeftPairs', 'RightPairs', 'Information']
	]).

	feature_pair_mutual_information(Left, Right, Information) :-
		aligned_feature_pairs(Left, Right, Pairs),
		^^contingency_counts(Pairs, Counts),
		^^contingency_score(mutual_information, Counts, Information).

	aligned_feature_pairs([], [], []).
	aligned_feature_pairs([Left-_LeftTarget| LeftPairs], [Right-_RightTarget| RightPairs], [Left-Right| Pairs]) :-
		aligned_feature_pairs(LeftPairs, RightPairs, Pairs).

:- end_category.

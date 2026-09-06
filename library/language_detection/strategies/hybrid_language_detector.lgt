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


:- object(hybrid_language_detector,
	implements(language_detection_strategy_protocol),
	imports(language_detection_scoring)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Language detection strategy combining character-trigram and stop-word scores.',
		see_also is [ngram_language_detector, stopword_language_detector]
	]).

	scores(Codes, Candidates, Scores) :-
		ngram_language_detector::scores(Codes, Candidates, NGramScores),
		stopword_language_detector::scores(Codes, Candidates, StopWordScores),
		combine_scores(NGramScores, StopWordScores, Scores).

	combine_scores([], [], []) :-
		!.
	combine_scores([], StopWordScores, StopWordScores) :-
		StopWordScores = [_| _],
		!.
	combine_scores(NGramScores, [], NGramScores) :-
		NGramScores = [_| _],
		!.
	combine_scores(NGramScores, StopWordScores, Scores) :-
		NGramScores = [_| _],
		StopWordScores = [_| _],
		weighted_scores(NGramScores, StopWordScores, RawScores),
		^^normalize_scores(RawScores, Scores).

	weighted_scores([], [], []).
	weighted_scores([Language-NGramScore| NGramScores], [Language-StopWordScore| StopWordScores], [Language-Score| Scores]) :-
		Score is 0.75 * NGramScore + 0.25 * StopWordScore,
		weighted_scores(NGramScores, StopWordScores, Scores).

:- end_object.

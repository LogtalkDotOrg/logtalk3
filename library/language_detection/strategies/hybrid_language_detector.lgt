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
		date is 2026-09-07,
		comment is 'Language detection strategy combining character-trigram, stop-word, and Unicode script scores.',
		see_also is [ngram_language_detector, stopword_language_detector, script_language_detector]
	]).

	scores(Codes, Candidates, Scores) :-
		ngram_language_detector::scores(Codes, Candidates, NGramScores),
		stopword_language_detector::scores(Codes, Candidates, StopWordScores),
		combine_lexical_scores(NGramScores, StopWordScores, LexicalScores),
		script_language_detector::scores(Codes, Candidates, ScriptScores),
		combine_scores(LexicalScores, ScriptScores, Scores).

	combine_lexical_scores([], [], []) :-
		!.
	combine_lexical_scores([], StopWordScores, StopWordScores) :-
		StopWordScores = [_| _],
		!.
	combine_lexical_scores(NGramScores, [], NGramScores) :-
		NGramScores = [_| _],
		!.
	combine_lexical_scores(NGramScores, StopWordScores, Scores) :-
		NGramScores = [_| _],
		StopWordScores = [_| _],
		weighted_scores(NGramScores, 0.75, StopWordScores, 0.25, RawScores),
		^^normalize_scores(RawScores, Scores).

	combine_scores([], [], []) :-
		!.
	combine_scores([], ScriptScores, ScriptScores) :-
		ScriptScores = [_| _],
		!.
	combine_scores(LexicalScores, [], LexicalScores) :-
		LexicalScores = [_| _],
		!.
	combine_scores(LexicalScores, ScriptScores, Scores) :-
		LexicalScores = [_| _],
		ScriptScores = [_| _],
		weighted_scores(LexicalScores, 0.80, ScriptScores, 0.20, RawScores),
		^^normalize_scores(RawScores, Scores).

	weighted_scores([], _, [], _, []).
	weighted_scores([Language-Score1| Scores1], Weight1, [Language-Score2| Scores2], Weight2, [Language-Score| Scores]) :-
		Score is Weight1 * Score1 + Weight2 * Score2,
		weighted_scores(Scores1, Weight1, Scores2, Weight2, Scores).

:- end_object.

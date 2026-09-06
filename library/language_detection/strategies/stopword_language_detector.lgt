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


:- object(stopword_language_detector,
	implements(language_detection_strategy_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Language detection strategy using discriminative stop-word evidence.',
		see_also is [language_detector(_, _), language_detection_tokenizer]
	]).

	:- uses(list, [
		length/2, member/2
	]).

	scores(Codes, Candidates, Scores) :-
		tokenizer(codes, language_detection_tokenizer)::tokenize(Codes, Tokens, [keep_punctuation(false), lowercase(true)]),
		initial_scores(Candidates, InitialScores),
		score_tokens(Tokens, Candidates, InitialScores, RawScores),
		normalize_scores(RawScores, Scores).

	initial_scores([], []).
	initial_scores([Language| Languages], [Language-0.0| Scores]) :-
		initial_scores(Languages, Scores).

	score_tokens([], _, Scores, Scores).
	score_tokens([Token| Tokens], Candidates, Scores0, Scores) :-
		atom_codes(Word, Token),
		matching_languages(Candidates, Word, MatchingLanguages),
		add_evidence(MatchingLanguages, Scores0, Scores1),
		score_tokens(Tokens, Candidates, Scores1, Scores).

	matching_languages([], _, []).
	matching_languages([Language| Languages], Word, MatchingLanguages) :-
		language_profiles::profile(Language, Profile),
		(	Profile::stop_word(Word) ->
			MatchingLanguages = [Language| Rest]
		;	MatchingLanguages = Rest
		),
		matching_languages(Languages, Word, Rest).

	add_evidence([], Scores, Scores) :-
		!.
	add_evidence(MatchingLanguages, Scores0, Scores) :-
		length(MatchingLanguages, Count),
		Weight is 1.0 / Count,
		add_language_evidence(Scores0, MatchingLanguages, Weight, Scores).

	add_language_evidence([], _, _, []).
	add_language_evidence([Language-Score0| Scores0], MatchingLanguages, Weight, [Language-Score| Scores]) :-
		(	member(Language, MatchingLanguages) ->
			Score is Score0 + Weight
		;	Score = Score0
		),
		add_language_evidence(Scores0, MatchingLanguages, Weight, Scores).

	normalize_scores(Scores0, Scores) :-
		sum_scores(Scores0, 0.0, Total),
		(	Total =< 0.0 ->
			Scores = []
		;	normalize_scores(Scores0, Total, Scores)
		).

	sum_scores([], Total, Total).
	sum_scores([_-Score| Scores], Total0, Total) :-
		Total1 is Total0 + Score,
		sum_scores(Scores, Total1, Total).

	normalize_scores([], _, []).
	normalize_scores([Language-Score0| Scores0], Total, [Language-Score| Scores]) :-
		Score is Score0 / Total,
		normalize_scores(Scores0, Total, Scores).

:- end_object.

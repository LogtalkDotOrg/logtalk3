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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Language detection strategy using discriminative stop-word evidence.',
		see_also is [language_detector(_, _), language_detection_tokenizer]
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(avltree, [
		new/1, lookup/3, insert/4
	]).

	scores(Codes, Candidates, Scores) :-
		tokenizer(codes, language_detection_tokenizer)::tokenize(Codes, Tokens, [keep_punctuation(false), lowercase(true)]),
		candidate_profiles(Candidates, Profiles, InitialScores),
		new(Cache),
		score_tokens(Tokens, Profiles, Cache, InitialScores, RawScores),
		normalize_scores(RawScores, Scores).

	candidate_profiles([], [], []).
	candidate_profiles([Language| Languages], [Language-Profile| Profiles], [Language-0.0| Scores]) :-
		language_profiles::profile(Language, Profile),
		candidate_profiles(Languages, Profiles, Scores).

	score_tokens([], _, _, Scores, Scores).
	score_tokens([Token| Tokens], Profiles, Cache0, Scores0, Scores) :-
		atom_codes(Word, Token),
		(	lookup(Word, MatchingLanguages-Weight, Cache0) ->
			Cache1 = Cache0
		;	matching_languages(Profiles, Word, MatchingLanguages),
			evidence_weight(MatchingLanguages, Weight),
			insert(Cache0, Word, MatchingLanguages-Weight, Cache1)
		),
		add_language_evidence(Scores0, MatchingLanguages, Weight, Scores1),
		score_tokens(Tokens, Profiles, Cache1, Scores1, Scores).

	matching_languages([], _, []).
	matching_languages([Language-Profile| Profiles], Word, MatchingLanguages) :-
		(	Profile::stop_word(Word) ->
			MatchingLanguages = [Language| Rest]
		;	MatchingLanguages = Rest
		),
		matching_languages(Profiles, Word, Rest).

	evidence_weight([], 0.0) :-
		!.
	evidence_weight(MatchingLanguages, Weight) :-
		length(MatchingLanguages, Count),
		Weight is 1.0 / Count.

	add_language_evidence(Scores, [], _, Scores) :-
		!.
	add_language_evidence([Language-Score0| Scores0], [Match| Matches], Weight, [Language-Score| Scores]) :-
		(	Language == Match ->
			Score is Score0 + Weight,
			Remaining = Matches
		;	Score = Score0,
			Remaining = [Match| Matches]
		),
		add_language_evidence(Scores0, Remaining, Weight, Scores).

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

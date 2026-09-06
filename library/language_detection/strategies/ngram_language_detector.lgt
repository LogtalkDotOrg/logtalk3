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

:- object(ngram_language_detector,
	implements(language_detection_strategy_protocol),
	imports(language_detection_scoring)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Language detection strategy using character-trigram cosine similarity.',
		see_also is [language_detector(_, _)]
	]).

	scores(Codes, Candidates, Scores) :-
		atom_codes(Text, Codes),
		n_grams(atom)::character_n_grams(3, Text, Trigrams),
		n_grams(atom)::count(standard, Trigrams, InputCounts),
		candidate_scores(Candidates, InputCounts, RawScores),
		^^normalize_scores(RawScores, Scores).

	candidate_scores([], _, []).
	candidate_scores([Language| Languages], InputCounts, [Language-Score| Scores]) :-
		language_profiles::profile(Language, Profile),
		Profile::trigram_counts(ProfileCounts),
		^^cosine_similarity(InputCounts, ProfileCounts, Score),
		candidate_scores(Languages, InputCounts, Scores).

:- end_object.

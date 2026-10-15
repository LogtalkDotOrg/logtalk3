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


:- object(test_language_detection_strategy,
	implements(language_detection_strategy_protocol)).

	scores(_, Candidates, Scores) :-
		scores(Candidates, Scores).

	scores([], []).
	scores([Language| Languages], [Language-1.0| Scores]) :-
		scores(Languages, Scores).

:- end_object.


:- object(fixed_language_detection_strategy(_Case_),
	implements(language_detection_strategy_protocol)).

	scores(_, _, Scores) :-
		response(_Case_, Scores).

	response(empty, []).
	response(ranked, [pt-0.25, en-0.75]).
	response(tie, [pt-0.5, en-0.5]).
	response(boundary, [en-0.5, pt-0.25]).
	response(low_margin, [en-0.5, pt-0.49]).
	response(missing, [en-0.5]).
	response(duplicate, [en-0.5, en-0.5]).
	response(unexpected, [en-0.5, xx-0.5]).
	response(malformed, [invalid]).
	response(non_list, invalid).
	response(non_number, [en-invalid, pt-0.0]).
	response(negative, [en- -0.1, pt-0.0]).
	response(too_large, [en-1.1, pt-0.0]).

:- end_object.


:- object(confident_language_detection_strategy,
	implements(language_detection_strategy_protocol)).

	scores(_, Candidates, Scores) :-
		candidate_scores(Candidates, Scores).

	candidate_scores([], []).
	candidate_scores([Language| Languages], [Language-Score| Scores]) :-
		(	Language == en ->
			Score = 1.0
		;	Score = 0.0
		),
		candidate_scores(Languages, Scores).

:- end_object.


:- object(test_language_detection_scoring,
	imports(language_detection_scoring)).

	:- public([cosine/3, normalize/2]).
	:- mode(cosine(+list, +list, -float), one).
	:- mode(normalize(+list, -list), one).

	cosine(Counts1, Counts2, Score) :-
		^^cosine_similarity(Counts1, Counts2, Score).

	normalize(RawScores, Scores) :-
		^^normalize_scores(RawScores, Scores).

:- end_object.


:- object(derived_language_profile,
	imports(language_profile_common)).

	language(xx).
	stop_word(a).
	stop_word(the).

:- end_object.


:- object(cached_language_profile,
	imports(language_profile_common)).

	language(xx).
	stop_word(a).

	trigram_counts_([cached-3], 9.0).

:- end_object.


:- object(overridden_language_profile,
	imports(language_profile_common)).

	language(xx).
	stop_word(a).

	trigram_counts([custom-2]).

:- end_object.


:- object(custom_en_language_profile,
	imports(language_profile_common)).

	language(en).

	trigram_counts(Counts) :-
		en_language_profile::trigram_counts(Counts).

	stop_word(customword).
	stop_word(Word) :-
		en_language_profile::stop_word(Word).

	:- multifile(language_profiles::custom_profile/2).
	:- multifile(language_detection_scripts::custom_language_scripts/2).

	language_profiles::custom_profile(en, custom_en_language_profile).
	language_detection_scripts::custom_language_scripts(en, ['Greek', 'Latin']).

:- end_object.

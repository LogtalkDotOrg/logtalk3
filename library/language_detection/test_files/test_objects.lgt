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


:- object(custom_en_language_profile,
	implements(language_profile_protocol)).

	language(en).

	trigram_counts(Counts) :-
		en_language_profile::trigram_counts(Counts).

	stop_word(customword).
	stop_word(Word) :-
		en_language_profile::stop_word(Word).

	:- multifile(language_profiles::custom_profile/2).

	language_profiles::custom_profile(en, custom_en_language_profile).

:- end_object.

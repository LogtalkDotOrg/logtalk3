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


	cover(stop_words(_, _)).

	test(stop_words_stop_word_1_01, deterministic) :-
		stop_words::stop_word("the").

	test(stop_words_stop_word_1_02, deterministic(Count == 1294)) :-
		findall(Word, stop_words::stop_word(Word), Words),
		length(Words, Count).

	test(stop_words_stop_word_1_03, deterministic) :-
		stop_words::stop_word("can't").

	test(stop_words_stop_word_1_04, deterministic) :-
		stop_words::stop_word("i.e.").

	test(stop_words_stop_word_1_05, true) :-
		stopwords_en::stop_word(herself),
		stopwords_en::stop_word(himself),
		stopwords_en::stop_word(itself),
		stopwords_en::stop_word(myself).

	test(stop_words_stop_word_1_06, deterministic(Count == UniqueCount)) :-
		findall(Word, stopwords_en::stop_word(Word), Words),
		length(Words, Count),
		sort(Words, UniqueWords),
		length(UniqueWords, UniqueCount).

	test(stop_words_stop_word_1_07, all(atom(Word))) :-
		stopwords_en::stop_word(Word).

	test(stop_words_is_stop_word_1_01, deterministic) :-
		stop_words::is_stop_word("the").

	test(stop_words_is_stop_word_1_02, deterministic) :-
		stop_words::is_stop_word("THE").

	test(stop_words_is_stop_word_1_03, false) :-
		stop_words::is_stop_word("logtalk").

	test(stop_words_is_stop_word_1_04, false) :-
		stop_words(unsupported, stopwords_en)::is_stop_word(the).

	test(stop_words_exclude_2_01, deterministic(Filtered == ["quick", "BROWN", "fox"])) :-
		stop_words::exclude(["The", "quick", "and", "BROWN", "fox"], Filtered).

	test(stop_words_exclude_2_02, deterministic(Filtered == [])) :-
		stop_words::exclude(["a", "THE", "and"], Filtered).

	test(stop_words_exclude_2_03, deterministic(Filtered == [])) :-
		stop_words::exclude([], Filtered).

	test(stop_words_language_provider_01, deterministic) :-
		tiny_stop_words::is_stop_word("THE").

	test(stop_words_language_provider_02, deterministic(Filtered == ["and", "fox"])) :-
		tiny_stop_words::exclude(["a", "and", "the", "fox"], Filtered).

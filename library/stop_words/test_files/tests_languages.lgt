:- encoding('UTF-8').

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


:- object(tests_languages,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Unit tests for the stop-word language providers.'
	]).

	test(stopwords_pt_count, deterministic(Count == 560)) :-
		findall(Word, stopwords_pt::stop_word(Word), Words),
		length(Words, Count).

	test(stopwords_pt_membership, true) :-
		stop_words(atom, stopwords_pt)::is_stop_word(não).

	test(stopwords_es_count, deterministic(Count == 732)) :-
		findall(Word, stopwords_es::stop_word(Word), Words),
		length(Words, Count).

	test(stopwords_es_membership, true) :-
		stop_words(atom, stopwords_es)::is_stop_word(también).

	test(stopwords_fr_count, deterministic(Count == 691)) :-
		findall(Word, stopwords_fr::stop_word(Word), Words),
		length(Words, Count).

	test(stopwords_fr_membership, true) :-
		stop_words(atom, stopwords_fr)::is_stop_word(être).

	test(stopwords_de_count, deterministic(Count == 620)) :-
		findall(Word, stopwords_de::stop_word(Word), Words),
		length(Words, Count).

	test(stopwords_de_membership, true) :-
		stop_words(atom, stopwords_de)::is_stop_word(über).

	test(stopwords_it_count, deterministic(Count == 632)) :-
		findall(Word, stopwords_it::stop_word(Word), Words),
		length(Words, Count).

	test(stopwords_it_membership, true) :-
		stop_words(atom, stopwords_it)::is_stop_word(avrà).

:- end_object.

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


:- category(language_profile_common,
	implements(language_profile_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Shared predicates for language detection profiles.'
	]).

	:- public(trigram_counts/1).
	:- mode(trigram_counts(-list(pair(atom,positive_integer))), one).
	:- info(trigram_counts/1, [
		comment is 'Returns trigram counts derived from the profile stop-word inventory.',
		argnames is ['Counts']
	]).

	:- public(trigram_counts/2).
	:- mode(trigram_counts(-list(pair(atom,positive_integer)), -float), one).
	:- info(trigram_counts/2, [
		comment is 'Returns the profile trigram counts and their squared norm, using cached values when available.',
		argnames is ['Counts', 'SquareSum']
	]).

	:- private(trigram_counts_/2).
	:- mode(trigram_counts_(-list(pair(atom,positive_integer)), -float), zero_or_one).
	:- info(trigram_counts_/2, [
		comment is 'Returns cached profile trigram counts and their squared norm when available.',
		argnames is ['Counts', 'SquareSum']
	]).

	:- uses(list, [
		append/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	trigram_counts(Counts) :-
		(	::trigram_counts_(Counts, _) ->
			true
		;	derive_trigram_counts(Counts)
		).

	trigram_counts(Counts, SquareSum) :-
		(	::trigram_counts_(Counts, SquareSum) ->
			true
		;	::trigram_counts(Counts),
			square_sum(Counts, 0.0, SquareSum)
		).

	derive_trigram_counts(Counts) :-
		findall(Word, ::stop_word(Word), Words),
		word_trigrams(Words, TrigramLists),
		append(TrigramLists, Trigrams),
		n_grams(atom)::count(standard, Trigrams, Counts).

	word_trigrams([], []).
	word_trigrams([Word| Words], [Trigrams| TrigramLists]) :-
		atomic_list_concat([' ', Word, ' '], PaddedWord),
		n_grams(atom)::character_n_grams(3, PaddedWord, Trigrams),
		word_trigrams(Words, TrigramLists).

	square_sum([], Sum, Sum).
	square_sum([_-Count| Counts], Sum0, Sum) :-
		Sum1 is Sum0 + Count * Count,
		square_sum(Counts, Sum1, Sum).

:- end_category.

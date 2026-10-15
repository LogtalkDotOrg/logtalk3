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


:- object(language_profile_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Computes and stores trigram vectors and squared norms when loading language profiles.'
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	term_expansion(
		(stop_word(Word) :- Inventory::stop_word(Word)),
		[
			(stop_word(Word) :- Inventory::stop_word(Word)),
			(trigram_counts_(Counts, SquareSum) :-
				Counts = ProfileCounts,
				SquareSum = ProfileSquareSum
			)
		]
	) :-
		findall(StopWord, Inventory::stop_word(StopWord), Words),
		word_trigrams(Words, Trigrams, []),
		n_grams(atom)::count(standard, Trigrams, ProfileCounts),
		square_sum(ProfileCounts, 0.0, ProfileSquareSum).

	word_trigrams([], Tail, Tail).
	word_trigrams([Word| Words], Trigrams, Tail) :-
		atomic_list_concat([' ', Word, ' '], Padded),
		n_grams(atom)::character_n_grams(3, Padded, WordTrigrams),
		append(WordTrigrams, Rest, Trigrams),
		word_trigrams(Words, Rest, Tail).

	square_sum([], Sum, Sum).
	square_sum([_-Count| Counts], Sum0, Sum) :-
		Sum1 is Sum0 + Count * Count,
		square_sum(Counts, Sum1, Sum).

:- end_object.

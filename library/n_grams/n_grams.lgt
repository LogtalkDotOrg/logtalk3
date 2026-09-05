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


:- object(n_grams(_Representation_),
	implements(n_grams_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Token and character n-gram generation for multiple text representations.',
		parameters is [
			'Representation' - 'Text representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		],
		see_also is [n_grams_protocol]
	]).

	:- uses(list, [
		append/3, drop/3, member/2, occurrences/2, remove_duplicates/2, sort/4, take/3
	]).

	:- uses(type, [
		check/2, valid/2
	]).

	n_grams(N, Tokens, NGrams) :-
		n_grams(N, Tokens, NGrams, []).

	n_grams(N, Tokens, NGrams, UserOptions) :-
		check_representation,
		check(positive_integer, N),
		check_tokens(Tokens),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(padding(Padding), Options),
		^^option(step(Step), Options),
		check_token_padding(Padding),
		pad_sequence(Padding, N, Tokens, PaddedTokens),
		generate_n_grams(PaddedTokens, N, Step, NGrams).

	character_n_grams(N, Text, NGrams) :-
		character_n_grams(N, Text, NGrams, []).

	character_n_grams(N, Text, NGrams, UserOptions) :-
		check_representation,
		check(positive_integer, N),
		check_text(Text),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(padding(Padding), Options),
		^^option(step(Step), Options),
		check_character_padding(Padding),
		text_sequence(Text, Sequence),
		pad_sequence(Padding, N, Sequence, PaddedSequence),
		generate_n_grams(PaddedSequence, N, Step, RawNGrams),
		sequences_text(RawNGrams, NGrams).

	count(NGrams, Counts) :-
		count(first_occurrence, NGrams, Counts).

	count(Order, NGrams, Counts) :-
		check_count_order(Order),
		count_order(Order, NGrams, Counts).

	count_order(first_occurrence, NGrams, Counts) :-
		remove_duplicates(NGrams, UniqueNGrams),
		count_unique(UniqueNGrams, NGrams, Counts).
	count_order(standard, NGrams, Counts) :-
		occurrences(NGrams, Counts).
	count_order(frequency_descending, NGrams, Counts) :-
		occurrences(NGrams, StandardCounts),
		decorate_counts(StandardCounts, DecoratedCounts),
		sort(0, @=<, DecoratedCounts, SortedDecoratedCounts),
		undecorate_counts(SortedDecoratedCounts, Counts).

	check_count_order(Order) :-
		( 	var(Order) ->
			instantiation_error
		;	member(Order, [first_occurrence, standard, frequency_descending]) ->
			true
		;	domain_error(count_order, Order)
		).

	bigrams(Tokens, Bigrams) :-
		n_grams(2, Tokens, Bigrams).

	trigrams(Tokens, Trigrams) :-
		n_grams(3, Tokens, Trigrams).

	default_option(padding(none)).
	default_option(step(1)).

	valid_option(padding(none)).
	valid_option(padding(left(_))).
	valid_option(padding(right(_))).
	valid_option(padding(both(_))).
	valid_option(step(Step)) :-
		valid(positive_integer, Step).

	check_representation :-
		(	var(_Representation_) ->
			instantiation_error
		;	member(_Representation_, [atom, chars, codes]) ->
			true
		;	domain_error(text_representation, _Representation_)
		).

	check_tokens(Tokens) :-
		check(list, Tokens),
		check_token_list(Tokens).

	check_token_list([]).
	check_token_list([Token| Tokens]) :-
		check_text(Token),
		check_token_list(Tokens).

	check_text(Text) :-
		check_text(_Representation_, Text).

	check_text(atom, Text) :-
		check(atom, Text).
	check_text(chars, Text) :-
		check(chars, Text).
	check_text(codes, Text) :-
		check(codes, Text).

	check_token_padding(none).
	check_token_padding(left(Marker)) :-
		check_text(Marker).
	check_token_padding(right(Marker)) :-
		check_text(Marker).
	check_token_padding(both(Marker)) :-
		check_text(Marker).

	check_character_padding(none).
	check_character_padding(left(Marker)) :-
		check_character(Marker).
	check_character_padding(right(Marker)) :-
		check_character(Marker).
	check_character_padding(both(Marker)) :-
		check_character(Marker).

	check_character(Marker) :-
		check_character(_Representation_, Marker).

	check_character(atom, Marker) :-
		check(character, Marker).
	check_character(chars, Marker) :-
		check(character, Marker).
	check_character(codes, Marker) :-
		check(character_code, Marker).

	text_sequence(Text, Sequence) :-
		text_sequence(_Representation_, Text, Sequence).

	text_sequence(atom, Atom, Chars) :-
		atom_chars(Atom, Chars).
	text_sequence(chars, Chars, Chars).
	text_sequence(codes, Codes, Codes).

	sequences_text([], []).
	sequences_text([Sequence| Sequences], [Text| Texts]) :-
		sequence_text(_Representation_, Sequence, Text),
		sequences_text(Sequences, Texts).

	sequence_text(atom, Chars, Atom) :-
		atom_chars(Atom, Chars).
	sequence_text(chars, Chars, Chars).
	sequence_text(codes, Codes, Codes).

	pad_sequence(none, _, Sequence, Sequence).
	pad_sequence(left(Marker), N, Sequence, PaddedSequence) :-
		Count is N - 1,
		padding(Count, Marker, Padding),
		append(Padding, Sequence, PaddedSequence).
	pad_sequence(right(Marker), N, Sequence, PaddedSequence) :-
		Count is N - 1,
		padding(Count, Marker, Padding),
		append(Sequence, Padding, PaddedSequence).
	pad_sequence(both(Marker), N, Sequence, PaddedSequence) :-
		Count is N - 1,
		padding(Count, Marker, Padding),
		append(Padding, Sequence, LeftPaddedSequence),
		append(LeftPaddedSequence, Padding, PaddedSequence).

	padding(0, _, []) :-
		!.
	padding(Count, Marker, [Marker| Padding]) :-
		NextCount is Count - 1,
		padding(NextCount, Marker, Padding).

	generate_n_grams(Sequence, N, Step, NGrams) :-
		(	take(N, Sequence, NGram) ->
			NGrams = [NGram| Rest],
			(	drop(Step, Sequence, Remaining) ->
				generate_n_grams(Remaining, N, Step, Rest)
			;	Rest = []
			)
		;	NGrams = []
		).

	count_unique([], _, []).
	count_unique([NGram| NGrams], AllNGrams, [NGram-Count| Counts]) :-
		count_occurrences(AllNGrams, NGram, 0, Count),
		count_unique(NGrams, AllNGrams, Counts).

	count_occurrences([], _, Count, Count).
	count_occurrences([NGram| NGrams], Current, Count0, Count) :-
		(	NGram == Current ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		count_occurrences(NGrams, Current, Count1, Count).

	decorate_counts([], []).
	decorate_counts([NGram-Count| Counts], [Key-(NGram-Count)| DecoratedCounts]) :-
		Key is -Count,
		decorate_counts(Counts, DecoratedCounts).

	undecorate_counts([], []).
	undecorate_counts([_-Count| DecoratedCounts], [Count| Counts]) :-
		undecorate_counts(DecoratedCounts, Counts).

:- end_object.

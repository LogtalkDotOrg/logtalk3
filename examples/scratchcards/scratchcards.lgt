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


:- object(scratchcards).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-12-05,
		comment is 'Advent of Code 2023 Day 4 problem solution.'
	]).

	:- public([
		points/2,
		total/2
	]).

	:- uses(avltree, [new/1, lookup/3, values/2, insert/4, update/5]).
	:- uses(integer, [between/3]).
	:- uses(list, [length/2, member/2, memberchk/2]).
	:- uses(numberlist, [sum/2]).
	:- uses(blank_grammars(codes), [spaces//0]).
	:- uses(number_grammars(codes), [natural//1]).
	:- uses(reader, [line_to_codes/2]).

	% Part 1

	points(File, Points) :-
		open(File, read, Stream),
		% input data is line-based
		line_to_codes(Stream, Line),
		% use an accumulator pair to compute the sum of all card points
		points(Line, Stream, 0, Points),
		close(Stream).

	% use an explicit list in the second clause to benefit from first-argument indexing
	points(end_of_file, _, Points, Points).
	points([Code| Codes], Stream, Points0, Points) :-
		once(phrase(scratchcard_points(ScratchcardPoints), [Code| Codes])),
		Points1 is Points0 + ScratchcardPoints,
		% next card (if any)
		line_to_codes(Stream, Line),
		points(Line, Stream, Points1, Points).

	scratchcard_points(ScratchcardPoints) -->
		card(WinningNumbers, ScratchcardNumbers),
		{
			findall(
				WinningNumber,
				(	member(WinningNumber, WinningNumbers),
					memberchk(WinningNumber, ScratchcardNumbers)
				),
				Numbers
			),
			length(Numbers, N),
			(	N =:= 0 ->
				ScratchcardPoints is 0
			;	ScratchcardPoints is 2 ^ (N - 1)
			)
		}.

	% Part 2

	total(File, Total) :-
		open(File, read, Stream),
		% input data is line-based
		line_to_codes(Stream, Line),
		% create a dictionary to hold the counts per card
		new(Cards),
		% card numbers start at 1
		total(Line, Stream, 1, Cards, Total),
		close(Stream).

	% use an explicit list in the second clause to benefit from first-argument indexing
	total(end_of_file, _, _, Cards, Total) :-
		values(Cards, Counts),
		sum(Counts, Total).
	total([Code| Codes], Stream, CardNumber0, Cards0, Total) :-
		once(phrase(scratchcards_wins(N), [Code| Codes])),
		CardNumber1 is CardNumber0 + 1,
		Stop is CardNumber0 + N,
		findall(NewCard, between(CardNumber1, Stop, NewCard), NewCards),
		% increment by one the count of the current card and the new cards
		update_counts([CardNumber0| NewCards], 1, Cards0, Cards01),
		% increment the count of the new cards per copy of the current card (if any)
		lookup(CardNumber0, Count0, Cards01),
		Count1 is Count0 - 1,
		(	Count1 > 0 ->
			update_counts(NewCards, Count1, Cards01, Cards1)
		;	% no copies of the current car exist
			Cards1 = Cards01
		),
		% next card (if any)
		line_to_codes(Stream, Line),
		total(Line, Stream, CardNumber1, Cards1, Total).

	scratchcards_wins(N) -->
		card(WinningNumbers, ScratchcardNumbers),
		{
			findall(
				WinningNumber,
				(	member(WinningNumber, WinningNumbers),
					memberchk(WinningNumber, ScratchcardNumbers)
				),
				Numbers
			),
			length(Numbers, N)
		}.

	update_counts([], _, Cards, Cards).
	update_counts([NewCard| NewCards], Count, Cards0, Cards) :-
		(	update(Cards0, NewCard, OldValue, NewValue, Cards1) ->
			NewValue is OldValue + Count
		;	insert(Cards0, NewCard, 1, Cards1)
		),
		update_counts(NewCards, Count, Cards1, Cards).

	% auxiliary grammar rules

	card(WinningNumbers, ScratchcardNumbers) -->
		card, numbers(WinningNumbers), separator, numbers(ScratchcardNumbers).

	card -->
		"Card", spaces, natural(_), ":", spaces.

	separator -->
		spaces, "|", spaces.

	numbers([Number| Numbers]) -->
		natural(Number), spaces, numbers(Numbers).
	numbers([]) -->
		[].

:- end_object.

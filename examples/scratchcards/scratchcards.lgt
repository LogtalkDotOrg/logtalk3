%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Paulo Moura',
		date is 2023-12-04,
		comment is 'Advent of Code 2023 Day 4 problem solution.'
	]).

	:- public(points/2).

	:- uses(list, [member/2, memberchk/2]).
	:- uses(blank_grammars(codes), [spaces//0]).
	:- uses(number_grammars(codes), [natural//1]).
	:- uses(reader, [line_to_codes/2]).

	points(File, Points) :-
		open(File, read, Stream),
		line_to_codes(Stream, Line),
		% use an accumulator pair to compute the sum of all calibration values
		points(Line, Stream, 0, Points),
		close(Stream).

	% use an explicit list in the second clause to benefit from first-argument indexing
	points(end_of_file, _, Points, Points).
	points([Code| Codes], Stream, Points0, Points) :-
		once(phrase(scratchcard_points(ScratchcardPoints), [Code| Codes])),
		Points1 is Points0 + ScratchcardPoints,
		line_to_codes(Stream, Line),
		points(Line, Stream, Points1, Points).

	scratchcard_points(ScratchcardPoints) -->
		card, numbers(WinningNumbers), separator, numbers(ScratchcardNumbers),
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

	card -->
		"Card", spaces, natural(_), ":", spaces.

	separator -->
		spaces, "|", spaces.

	numbers([Number| Numbers]) -->
		natural(Number), spaces, numbers(Numbers).
	numbers([]) -->
		[].

:- end_object.

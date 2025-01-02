%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(trebuchet).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-12-04,
		comment is 'Advent of Code 2023 Day 1 problem solution.'
	]).

	:- public(solution/2).

	solution(File, Solution) :-
		open(File, read, Stream),
		% input data is line-based
		reader::line_to_codes(Stream, Line),
		% use an accumulator pair to compute the sum of all calibration values
		solution(Line, Stream, 0, Solution),
		close(Stream).

	% use an explicit list in the second clause to benefit from first-argument indexing
	solution(end_of_file, _, Solution, Solution).
	solution([Code| Codes], Stream, Solution0, Solution) :-
		once(phrase(calibration(Calibration), [Code| Codes])),
		Solution1 is Solution0 + Calibration,
		% next line calibration (if any)
		reader::line_to_codes(Stream, Line),
		solution(Line, Stream, Solution1, Solution).

	calibration(Calibration) -->
		first_digit(First),
		rest_digits(Digits),
		{
			% when a line contains a single digit, the missing
			% second digit is the same as the first digit
			last(Digits, First, Second),
			Calibration is First*10 + Second
		}.

	first_digit(Digit) -->
		digit(Digit).
	first_digit(Digit) -->
		[_], first_digit(Digit).

	rest_digits([Digit| Digits]) -->
		digit(Digit),
		rest_digits(Digits).
	rest_digits(Digits) -->
		[_],
		rest_digits(Digits).
	rest_digits([]) -->
		[].

	% as numbers
	digit(1) --> "1".
	digit(2) --> "2".
	digit(3) --> "3".
	digit(4) --> "4".
	digit(5) --> "5".
	digit(6) --> "6".
	digit(7) --> "7".
	digit(8) --> "8".
	digit(9) --> "9".
	% as letters (take into account possible overlaps by pushing back
	% the last letter if it can be the first letter of another digit;
	% this allows e.g. "oneight" to be parsed as 18)
	digit(1), "e" --> "one".
	digit(2), "o" --> "two".
	digit(3), "e" --> "three".
	digit(4)      --> "four".
	digit(5), "e" --> "five".
	digit(6)      --> "six".
	digit(7), "n" --> "seven".
	digit(8), "t" --> "eight".
	digit(9), "e" --> "nine".

	last([], Last, Last).
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

:- end_object.

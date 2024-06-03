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


:- object(default_integer_mutations).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-11-24,
		comment is 'Default integer mutations.',
		see_also is [type]
	]).

	:- uses(list, [
		length/2, nth1/4
	]).

	:- uses(fast_random, [
		between/3, permutation/2, random/1, select/3, select/4, swap/2, swap_consecutive/2
	]).

	% swap two consecutive digits
	mutation(integer, Integer, Mutation) :-
		Absolute is abs(Integer),
		Absolute > 9,
		Sign is sign(Integer),
		number_chars(Absolute, Chars),
		swap_consecutive(Chars, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% swap two digits
	mutation(integer, Integer, Mutation) :-
		Absolute is abs(Integer),
		Absolute > 9,
		Sign is sign(Integer),
		number_chars(Absolute, Chars),
		swap(Chars, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% negate integer
	:- if(current_prolog_flag(bounded, true)).
		mutation(integer, Integer, Mutation) :-
			% fail on integer overflow errors
			catch(Mutation is -1 * Integer, _, fail).
	:- else.
		mutation(integer, Integer, Mutation) :-
			Mutation is -1 * Integer.
	:- endif.
	% add random digit
	mutation(integer, Integer, Mutation) :-
		Sign is sign(Integer),
		Absolute is abs(Integer),
		number_chars(Absolute, Chars),
		between(0'0, 0'9, Code),
		char_code(Digit, Code),
		length(Chars, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		nth1(Index, MutationChars, Digit, Chars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% remove random digit
	mutation(integer, Integer, Mutation) :-
		Sign is sign(Integer),
		Absolute is abs(Integer),
		Absolute > 9,
		number_chars(Absolute, Chars),
		select(_Random, Chars, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% replacing a random digit
	mutation(integer, Integer, Mutation) :-
		Sign is sign(Integer),
		Absolute is abs(Integer),
		Absolute > 9,
		number_chars(Absolute, Chars),
		between(0'0, 0'9, Code),
		char_code(Digit, Code),
		select(_Random, Chars, Digit, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% permutation of the integer digits
	mutation(integer, Integer, Mutation) :-
		Sign is sign(Integer),
		Absolute is abs(Integer),
		Absolute > 9,
		number_chars(Absolute, Chars),
		permutation(Chars, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.

:- end_object.

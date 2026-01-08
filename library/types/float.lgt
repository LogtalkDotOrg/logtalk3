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


:- object(float,
	extends(number)).

	:- info([
		version is 1:7:0,
		author is 'Paulo Moura',
		date is 2025-02-25,
		comment is 'Floating point numbers data type predicates.'
	]).

	:- public(between/4).
	:- mode(between(+float, +float, +positive_integer, -float), zero_or_more).
	:- info(between/4, [
		comment is 'Enumerates by backtracking a sequence of ``N`` equally spaced floats in the interval ``[Lower,Upper]``. Assumes ``N > 0`` and ``Lower =< Upper``; fails otherwise.',
		argnames is ['Lower', 'Upper', 'N', 'Float']
	]).

	:- public(sequence/4).
	:- mode(sequence(+float, +float, +positive_integer, -list(float)), zero_or_one).
	:- info(sequence/4, [
		comment is 'Generates a list with the sequence of ``N`` equally spaced floats in the interval ``[Lower,Upper]``. Assumes ``N > 0`` and ``Lower =< Upper``; fails otherwise.',
		argnames is ['Lower', 'Upper', 'N', 'List']
	]).

	:- public(sequence/5).
	:- mode(sequence(+float, +float, +float, -list(float), -positive_integer), zero_or_one).
	:- info(sequence/5, [
		comment is 'Generates a list with the sequence of ``Step`` spaced floats in the interval ``[Lower,Upper]``. Also returns the length of the list. Assumes ``Lower =< Upper``; fails otherwise.',
		argnames is ['Lower', 'Upper', 'Step', 'List', 'Length']
	]).

	between(Lower, Upper, N, Float) :-
		Lower =< Upper,
		N > 0,
		(	N =:= 1 ->
			Float = Lower
		;	Increment is (Upper - Lower) / (N - 1),
			gen_float(N, Lower, Upper, Increment, Float)
		).

	gen_float(1, _, Float, _, Float) :-
		!.
	gen_float(_, Float, _, _, Float).
	gen_float(N, Current, Upper, Increment, Float) :-
		M is N - 1,
		Next is Current + Increment,
		gen_float(M, Next, Upper, Increment, Float).

	sequence(Lower, Upper, N, List) :-
		Lower =< Upper,
		N > 0,
		(	N =:= 1 ->
			List = [Lower]
		;	Increment is (Upper - Lower) / (N - 1),
			gen_sequence(N, Lower, Upper, Increment, List)
		).

	gen_sequence(1, _, Upper, _, [Upper]) :-
		!.
	gen_sequence(N, Current, Upper, Increment, [Current| Tail]) :-
		M is N - 1,
		Next is Current + Increment,
		gen_sequence(M, Next, Upper, Increment, Tail).

	sequence(Lower, Upper, Step, List, Length) :-
		Lower =< Upper,
		gen_sequence(Lower, Upper, Step, List, 1, Length).

	gen_sequence(Current, Upper, Step, [Current], Length, Length) :-
		Current + Step > Upper,
		!.
	gen_sequence(Current, Upper, Step, [Current| Tail], Length0, Length) :-
		Length1 is Length0 + 1,
		Next is Current + Step,
		gen_sequence(Next, Upper, Step, Tail, Length1, Length).

	valid(Float) :-
		float(Float).

	check(Term) :-
		(	float(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(float, Term)
		).

:- end_object.

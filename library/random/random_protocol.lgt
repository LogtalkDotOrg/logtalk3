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


:- protocol(random_protocol).

	:- info([
		version is 3:3:0,
		author is 'Paulo Moura',
		date is 2023-11-24,
		comment is 'Random number generator protocol. The predicates are declared as synchronized when the library is compiled using a backend supporting threads.',
		see_also is [random, backend_random, fast_random]
	]).

	:- public(random/1).
	:- mode(random(-float), one).
	:- info(random/1, [
		comment is 'Returns a new random float value in the interval ``[0.0, 1.0[``.',
		argnames is ['Random']
	]).

	:- public(between/3).
	:- mode(between(+integer, +integer, -integer), zero_or_one).
	:- info(between/3, [
		comment is 'Returns a new random integer in the interval ``[Lower, Upper]``. Fails if ``Lower`` or ``Upper`` are not integers or if ``Lower > Upper``.',
		argnames is ['Lower', 'Upper', 'Random']
	]).

	:- public(member/2).
	:- mode(member(-term, +list(term)), zero_or_one).
	:- info(member/2, [
		comment is 'Returns a random member of a list. Fails if the list is empty.',
		argnames is ['Random', 'List']
	]).

	:- public(select/3).
	:- mode(select(-term, +list(term), -list(term)), zero_or_one).
	:- info(select/3, [
		comment is 'Returns a random member of a list and the rest of the list. Fails if the list is empty.',
		argnames is ['Random', 'List', 'Rest']
	]).

	:- public(select/4).
	:- mode(select(-term, +list(term), @term, -list(term)), zero_or_one).
	:- info(select/4, [
		comment is 'Returns a random member of a list, replacing it with a new element and returning the resulting list.',
		argnames is ['Random', 'OldList', 'New', 'NewList']
	]).

	:- public(swap/2).
	:- mode(swap(-term, +list(term)), zero_or_one).
	:- info(swap/2, [
		comment is 'Swaps two randomly selected elements of a list. Fails if the list is empty or contains a single element.',
		argnames is ['OldList', 'NewList']
	]).

	:- public(swap_consecutive/2).
	:- mode(swap_consecutive(-term, +list(term)), zero_or_one).
	:- info(swap_consecutive/2, [
		comment is 'Swaps two randomly selected consecutive elements of a list. Fails if the list is empty or contains a single element.',
		argnames is ['OldList', 'NewList']
	]).

	:- public(enumerate/2).
	:- mode(enumerate(+list(term), --term), zero_or_more).
	:- info(enumerate/2, [
		comment is 'Enumerates the elements of a list in random order. Fails if the list is empty.',
		argnames is ['List', 'Random']
	]).

	:- public(permutation/2).
	:- mode(permutation(+list, -list), one).
	:- info(permutation/2, [
		comment is 'Returns a random permutation of a list.',
		argnames is ['List', 'Permutation']
	]).

	:- public(sequence/4).
	:- mode(sequence(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- info(sequence/4, [
		comment is 'Returns list of random integers of given length in random order in interval ``[Lower, Upper]``. Fails if ``Length``, ``Lower``, or ``Upper`` are not integers or if ``Lower > Upper``.',
		argnames is ['Length', 'Lower', 'Upper', 'List']
	]).

	:- public(set/4).
	:- mode(set(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- info(set/4, [
		comment is 'Returns ordered set of random integers of given size in interval ``[Lower, Upper]``. Fails if ``Length``, ``Lower``, or ``Upper`` are not integers, if ``Lower > Upper``, or if ``Length > Upper - Lower + 1``.',
		argnames is ['Length', 'Lower', 'Upper', 'Set']
	]).

	:- public(random/3).
	:- mode(random(+integer, +integer, -integer), zero_or_one).
	:- mode(random(+float, +float, -float), zero_or_one).
	:- info(random/3, [
		comment is 'Returns a new random value in the interval ``[Lower, Upper[``. Fails if ``Lower > Upper``. Deprecated. Use ``between/3`` for integers.',
		argnames is ['Lower', 'Upper', 'Random']
	]).

	:- public(randseq/4).
	:- mode(randseq(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randseq(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randseq/4, [
		comment is 'Returns list of random values of given length in random order in interval ``[Lower, Upper[``. Fails if ``Lower > Upper`` or if the arguments are neither integers or floats. Deprecated. Use ``sequence/4`` for integers.',
		argnames is ['Length', 'Lower', 'Upper', 'List']
	]).

	:- public(randset/4).
	:- mode(randset(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randset(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randset/4, [
		comment is 'Returns ordered set of random values of given size in interval ``[Lower, Upper[``. Fails if the arguments are neither integers or floats, ``Lower > Upper``, or ``Length > Upper - Lower`` when arguments are integers. Deprecated. Use ``set/4`` for integers.',
		argnames is ['Length', 'Lower', 'Upper', 'Set']
	]).

	:- public(maybe/0).
	:- mode(maybe, zero_or_one).
	:- info(maybe/0, [
		comment is 'Succeeds or fails with equal probability.'
	]).

	:- public(maybe/1).
	:- mode(maybe(+probability), zero_or_one).
	:- info(maybe/1, [
		comment is 'Succeeds with probability ``Probability`` or fails with probability ``1 - Probability``. Fails if ``Probability`` is not a float or is outside the interval ``[0.0, 1.0]``.',
		argnames is ['Probability']
	]).

	:- public(maybe/2).
	:- mode(maybe(+non_negative_integer, +non_negative_integer), zero_or_one).
	:- info(maybe/2, [
		comment is 'Succeeds with probability ``K/N`` where ``K`` and ``N`` are integers satisfying the equation ``0 =< K =< N``. Fails otherwise.',
		argnames is ['K', 'N']
	]).

	:- public(maybe_call/1).
	:- meta_predicate(maybe_call(0)).
	:- mode(maybe_call(+callable), zero_or_one).
	:- info(maybe_call/1, [
		comment is 'Calls a goal or fails without calling it with equal probability. When the goal is called, it determines if this predicate succeeds once or fails.',
		argnames is ['Goal']
	]).

	:- public(maybe_call/2).
	:- meta_predicate(maybe_call(*, 0)).
	:- mode(maybe_call(+probability, +callable), zero_or_one).
	:- info(maybe_call/2, [
		comment is 'Calls a goal or fails without calling it with probability ``Probability``. When the goal is called, it determines if this predicate succeeds once or fails.',
		argnames is ['Probability', 'Goal']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			random/1, random/3,
			sequence/4, set/4, permutation/2,
			randseq/4, randset/4
		]).
	:- endif.

:- end_protocol.

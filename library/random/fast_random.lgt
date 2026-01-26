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


:- object(fast_random(_Algorithm_),
	implements((pseudo_random_protocol, sampling_protocol))).

	:- info([
		version is 3:0:0,
		author is 'Paulo Moura',
		date is 2026-01-25,
		comment is 'Fast portable random number generator predicates.',
		parameters is [
			'Algorithm' - 'Random number generator algorithm. One of ``as183``, ``splitmix64``, ``xoshiro128pp``, ``xoshiro128ss``, ``xoshiro256pp``, ``xoshiro256ss``.'
		],
		remarks is [
			'Single random number generator' - 'This object provides a faster version of the ``random`` library object but does not support being extended to define multiple random number generators.',
			'Randomness' - 'Loading this object always initializes the random generator seed to the same value, thus providing a pseudo random number generator. The ``randomize/1`` predicate can be used to initialize the seed with a random value.'
		],
		see_also is [fast_random, random(_), random, backend_random]
	]).

	:- initialization(reset_seeds).

	:- public(reset_seed/0).
	:- mode(reset_seed, one).
	:- info(reset_seed/0, [
		comment is 'Resets the random generator seed to its default value. Use ``get_seed/1`` and ``set_seed/1`` instead if you need reproducibility.'
	]).

	:- public(randomize/1).
	:- mode(randomize(+positive_integer), one).
	:- info(randomize/1, [
		comment is 'Randomizes the random generator using a positive integer to compute a new seed. Use of a large integer is recommended. In alternative, when using a small integer argument, discard the first dozen random values.',
		argnames is ['Seed']
	]).

	:- uses(list, [
		length/2, nth1/3
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			random/1, random/3,
			sequence/4, set/4, permutation/2,
			randseq/4, randset/4,
			reset_seed/0, get_seed/1, set_seed/1, randomize/1
		]).
	:- endif.

	:- initialization(reset_seed).

	:- private(seed_/2).
	:- dynamic(seed_/2).
	:- mode(seed_(+atom, -list(integer)), one).
	:- info(seed_/2, [
		comment is 'Stores the current random generator seed values.',
		argnames is ['Algorithm', 'Values']
	]).

	random(Random) :-
		random(_Algorithm_, Random).

	between(Lower, Upper, Random) :-
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		random(_Algorithm_, Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower.

	member(Random, List) :-
		length(List, Length),
		random(_Algorithm_, Float),
		Index is truncate(Float*Length+1),
		nth1(Index, List, Random).

	select(Random, List, Rest) :-
		length(List, Length),
		random(_Algorithm_, Float),
		Index is truncate(Float*Length+1),
		select(1, Index, Random, List, Rest).

	select(Index, Index, Random, [Random| Rest], Rest) :-
		!.
	select(Current, Index, Random, [Head| Tail], [Head| Rest]) :-
		Next is Current + 1,
		select(Next, Index, Random, Tail, Rest).

	select(Random, List, New, Rest) :-
		length(List, Length),
		random(_Algorithm_, Float),
		Index is truncate(Float*Length+1),
		select(1, Index, Random, List, New, Rest).

	select(Index, Index, Random, [Random| Tail], New, [New| Tail]) :-
		!.
	select(Current, Index, Random, [Head| OldTail], New, [Head| NewTail]) :-
		Next is Current + 1,
		select(Next, Index, Random, OldTail, New, NewTail).

	swap(List, Mutation) :-
		length(List, Length),
		Length > 1,
		repeat,
			between(1, Length, N1),
			between(1, Length, N2),
		N1 =\= N2,
		!,
		(	N1 < N2 ->
			swap_1(N1, N2, List, Mutation)
		;	swap_1(N2, N1, List, Mutation)
		).

	swap_1(1, N2, [Element1| Rest0], [Element2| Rest]) :-
		!,
		swap_2(N2, Element1, Element2, Rest0, Rest).
	swap_1(N1, N2, [Head| Tail], [Head| Mutation]) :-
		M1 is N1 - 1,
		M2 is N2 - 1,
		swap_1(M1, M2, Tail, Mutation).

	swap_2(2, Element1, Element2, [Element2| Rest], [Element1| Rest]) :-
		!.
	swap_2(N2, Element1, Element2, [Head| Rest0], [Head| Rest]) :-
		M is N2 - 1,
		swap_2(M, Element1, Element2, Rest0, Rest).

	swap_consecutive(List, Mutation) :-
		length(List, Length),
		Limit is Length - 1,
		between(1, Limit, N),
		swap_consecutive(N, List, Mutation).

	swap_consecutive(1, [Element1, Element2| Rest], [Element2, Element1| Rest]) :-
		!.
	swap_consecutive(N, [Head| Tail], [Head| Mutation]) :-
		M is N - 1,
		swap_consecutive(M, Tail, Mutation).

	enumerate(List, Random) :-
		permutation(List, Permutation),
		list::member(Random, Permutation).

	sequence(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		retract(seed_(_Algorithm_, Values0)),
		sequence(Length, Lower, Upper, Values0, Values, Sequence),
		asserta(seed_(_Algorithm_, Values)).

	sequence(0, _, _, Values, Values, []) :-
		!.
	sequence(N, Lower, Upper, Values0, Values, [Random| Sequence]) :-
		N2 is N - 1,
		random_seeds(_Algorithm_, Values0, Values1, Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower,
		sequence(N2, Lower, Upper, Values1, Values, Sequence).

	set(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		Length =< Upper - Lower + 1,
		retract(seed_(_Algorithm_, Values0)),
		set(Length, Lower, Upper, Values0, Values, [], Set),
		asserta(seed_(_Algorithm_, Values)).

	set(0, _, _, Values, Values, List, Set) :-
		!,
		sort(List, Set).
	set(N, Lower, Upper, Values0, Values, Acc, Set) :-
		random_seeds(_Algorithm_, Values0, Values1, Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower,
		(	not_member(Acc, Random) ->
			N2 is N - 1,
			set(N2, Lower, Upper, Values1, Values, [Random| Acc], Set)
		;	set(N, Lower, Upper, Values1, Values, Acc, Set)
		).

	permutation(List, Permutation) :-
		retract(seed_(_Algorithm_, Values0)),
		add_random_key(List, Values0, Values, KeyList),
		asserta(seed_(_Algorithm_, Values)),
		keysort(KeyList, SortedKeyList),
		remove_random_key(SortedKeyList, Permutation).

	add_random_key([], Values, Values, []).
	add_random_key([Head| Tail], Values0, Values, [Random-Head| KeyTail]) :-
		random_seeds(_Algorithm_, Values0, Values1, Random),
		add_random_key(Tail, Values1, Values, KeyTail).

	remove_random_key([], []).
	remove_random_key([_-Head| KeyTail], [Head| Tail]) :-
		remove_random_key(KeyTail, Tail).

	random(Lower, Upper, Random) :-
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		!,
		random(_Algorithm_, Float),
		Random is truncate((Float * (Upper - Lower) + Lower)).
	random(Lower, Upper, Random) :-
		float(Lower),
		float(Upper),
		Upper >= Lower,
		random(_Algorithm_, Float),
		Random is Float * (Upper-Lower) + Lower.

	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		!,
		retract(seed_(_Algorithm_, Values0)),
		randseq(Length, Lower, Upper, Values0, Values, List),
		asserta(seed_(_Algorithm_, Values)),
		map_truncate(List, Sequence).
	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		retract(seed_(_Algorithm_, Values0)),
		randseq(Length, Lower, Upper, Values0, Values, Sequence),
		asserta(seed_(_Algorithm_, Values)).

	randseq(0, _, _, Values, Values, []) :-
		!.
	randseq(N, Lower, Upper, Values0, Values, [Random| List]) :-
		N2 is N - 1,
		random_seeds(_Algorithm_, Values0, Values1, R),
		Random is R * (Upper-Lower)+Lower,
		randseq(N2, Lower, Upper, Values1, Values, List).

	map_truncate([], []).
	map_truncate([Float| Floats], [Integer| Integers]) :-
		Integer is truncate(Float),
		map_truncate(Floats, Integers).

	randset(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		Length =< Upper - Lower,
		!,
		retract(seed_(_Algorithm_, Values0)),
		randset(Length, Lower, Upper, Values0, Values, [], Set),
		asserta(seed_(_Algorithm_, Values)).
	randset(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		retract(seed_(_Algorithm_, Values0)),
		randset(Length, Lower, Upper, Values0, Values, [], Set),
		asserta(seed_(_Algorithm_, Values)).

	randset(0, _, _, Values, Values, List, Set) :-
		!,
		sort(List, Set).
	randset(N, Lower, Upper, Values0, Values, Acc, Set) :-
		random_seeds(_Algorithm_, Values0, Values1, Float),
		Float2 is Float * (Upper-Lower) + Lower,
		(	integer(Lower) ->
			Random is truncate(Float2)
		;	Random is Float2
		),
		(	not_member(Acc, Random) ->
			N2 is N - 1,
			randset(N2, Lower, Upper, Values1, Values, [Random| Acc], Set)
		;	randset(N, Lower, Upper, Values1, Values, Acc, Set)
		).

	not_member([], _).
	not_member([H| T], R) :-
		H =\= R,
		not_member(T, R).

	reset_seed :-
		reset_seed(_Algorithm_).

	get_seed(seed(_Algorithm_, Values)) :-
		seed_(_Algorithm_, Values).

	set_seed(seed(_Algorithm_, Values)) :-
		retractall(seed_(_Algorithm_, _)),
		asserta(seed_(_Algorithm_, Values)).

	randomize(Seed) :-
		randomize(_Algorithm_, Seed).

	maybe :-
		random(Random),
		Random < 0.5.

	maybe(Probability) :-
		float(Probability),
		0.0 =< Probability, Probability =< 1.0,
		random(_Algorithm_, Random),
		Random < Probability.

	maybe(K, N) :-
		integer(K), integer(N),
		0 =< K, K =< N,
		random(_Algorithm_, Float),
		Random is truncate(Float * N),
		Random < K.

	:- meta_predicate(maybe_call(0)).
	maybe_call(Goal) :-
		random(_Algorithm_, Random),
		Random < 0.5,
		once(Goal).

	:- meta_predicate(maybe_call(*, 0)).
	maybe_call(Probability, Goal) :-
		float(Probability),
		0.0 =< Probability, Probability =< 1.0,
		random(_Algorithm_, Random),
		Random < Probability,
		once(Goal).

	random(_Algorithm_, Random) :-
		retract(seed_(_Algorithm_, Values0)),
		random_seeds(_Algorithm_, Values0, Values, Random),
		asserta(seed_(_Algorithm_, Values)).

	reset_seeds :-
		reset_seed(_),
		fail.
	reset_seeds.

	:- if(current_prolog_flag(bounded, false)).
		:- discontiguous(random/2).
		:- discontiguous(random_seeds/4).
		:- discontiguous(randomize/2).
		:- discontiguous(reset_seed/1).
		:- include(fast_random_algorithms_32_bits).
		:- include(fast_random_algorithms_64_bits).
	:- else.
		:- include(fast_random_algorithms_32_bits).
	:- endif.

	:- include(sampling).

:- end_object.


:- object(fast_random,
	extends(fast_random(as183))).

	:- info([
		version is 3:0:0,
		author is 'Paulo Moura',
		date is 2026-01-25,
		comment is 'Portable random number generator predicates. Core predicates originally written by Richard O''Keefe. Based on algorithm AS 183 from Applied Statistics.',
		remarks is [
			'Single random number generator' - 'This object provides a faster version of the ``random`` library object but does not support being extended to define multiple random number generators.',
			'Randomness' - 'Loading this object always initializes the random generator seed to the same value, thus providing a pseudo random number generator. The ``randomize/1`` predicate can be used to initialize the seed with a random value.'
		],
		see_also is [fast_random, random, random(_), backend_random]
	]).

	:- initialization(::reset_seed).

:- end_object.

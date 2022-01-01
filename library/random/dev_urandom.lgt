%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(dev_urandom,
	implements(random_protocol)).

	:- info([
		version is 2:8:0,
		author is 'Paulo Moura',
		date is 2021-02-21,
		comment is 'Random number generator predicate based on ``/dev/urandom``.',
		remarks is [
			'Single random number generator' - 'This object provides a faster version of the ``random`` library object but does not support being extended to define multiple random number generators.'
		],
		see_also is [random, backend_random]
	]).

	:- uses(list, [
		length/2, nth1/3
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			random/1, random/3,
			sequence/4, set/4, permutation/2,
			randseq/4, randset/4
		]).
	:- endif.

	random(Random) :-
		open('/dev/urandom', read, Stream, [type(binary)]),
		bytes(Stream, 16, Bytes).

	between(Lower, Upper, Random) :-
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		random(Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower.

	member(Random, List) :-
		length(List, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		nth1(Index, List, Random).

	select(Random, List, Rest) :-
		length(List, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		select(1, Index, Random, List, Rest).

	select(Index, Index, Random, [Random| Rest], Rest) :-
		!.
	select(Current, Index, Random, [Head| Tail], [Head| Rest]) :-
		Next is Current + 1,
		select(Next, Index, Random, Tail, Rest).

	enumerate(List, Random) :-
		permutation(List, Permutation),
		list::member(Random, Permutation).

	sequence(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		sequence(Length, Lower, Upper, Sequence).

	sequence(0, _, _, []) :-
		!.
	sequence(N, Lower, Upper, [Random| Sequence]) :-
		N2 is N - 1,
		random(Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower,
		sequence(N2, Lower, Upper, Sequence).

	set(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		Length =< Upper - Lower + 1,
		set(Length, Lower, Upper, [], Set).

	set(0, _, _, List, Set) :-
		!,
		sort(List, Set).
	set(N, Lower, Upper, Acc, Set) :-
		random(Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower,
		(	not_member(Acc, Random) ->
			N2 is N - 1,
			set(N2, Lower, Upper, [Random| Acc], Set)
		;	set(N, Lower, Upper, Acc, Set)
		).

	permutation(List, Permutation) :-
		add_random_key(List, KeyList),
		keysort(KeyList, SortedKeyList),
		remove_random_key(SortedKeyList, Permutation).

	add_random_key([], []).
	add_random_key([Head| Tail], [Random-Head| KeyTail]) :-
		random(Random),
		add_random_key(Tail, KeyTail).

	remove_random_key([], []).
	remove_random_key([_-Head| KeyTail], [Head| Tail]) :-
		remove_random_key(KeyTail, Tail).

	random(Lower, Upper, Random) :-
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		!,
		random(Float),
		Random is truncate((Float * (Upper - Lower) + Lower)).
	random(Lower, Upper, Random) :-
		float(Lower),
		float(Upper),
		Upper >= Lower,
		random(Float),
		Random is Float * (Upper-Lower) + Lower.

	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		!,
		randseq(Length, Lower, Upper, List),
		map_truncate(List, Sequence).
	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		randseq(Length, Lower, Upper, Sequence).

	randseq(0, _, _, []) :-
		!.
	randseq(N, Lower, Upper,  [Random| List]) :-
		N2 is N - 1,
		random(R),
		Random is R * (Upper-Lower)+Lower,
		randseq(N2, Lower, Upper, List).

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
		randset(Length, Lower, Upper, [], Set).
	randset(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		randset(Length, Lower, Upper, [], Set).

	randset(0, _, _, List, Set) :-
		!,
		sort(List, Set).
	randset(N, Lower, Upper, Acc, Set) :-
		random(Float),
		Float2 is Float * (Upper-Lower) + Lower,
		(	integer(Lower) ->
			Random is truncate(Float2)
		;	Random is Float2
		),
		(	not_member(Acc, Random) ->
			N2 is N - 1,
			randset(N2, Lower, Upper, [Random| Acc], Set)
		;	randset(N, Lower, Upper, Acc, Set)
		).

	not_member([], _).
	not_member([H| T], R) :-
		H =\= R,
		not_member(T, R).

	maybe :-
		random(Random),
		Random < 0.5.

	maybe(Probability) :-
		float(Probability),
		0.0 =< Probability, Probability =< 1.0,
		random(Random),
		Random < Probability.

	maybe(K, N) :-
		integer(K), integer(N),
		0 =< K, K =< N,
		random(Float),
		Random is truncate(Float * N),
		Random < K.

	:- meta_predicate(maybe_call(0)).
	maybe_call(Goal) :-
		random(Random),
		Random < 0.5,
		once(Goal).

	:- meta_predicate(maybe_call(*, 0)).
	maybe_call(Probability, Goal) :-
		float(Probability),
		0.0 =< Probability, Probability =< 1.0,
		random(Random),
		Random < Probability,
		once(Goal).

:- end_object.

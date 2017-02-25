%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(frandom,
	implements(randomp)).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2017/02/25,
		comment is 'Fast portable random number generator predicates. Core predicates originally written by Richard O''Keefe.',
		remarks is [
			'Single random number generator' - 'This object provides a faster version of the "random" library object but does not support being extended to define multiple random number generators.'
		]
	]).

	:- uses(list, [
		length/2, nth1/3
	]).

	:- synchronized([
		random/1, random/3,
		sequence/4, set/4, permutation/2,
		randseq/4, randset/4,
		reset_seed/0, get_seed/1, set_seed/1
	]).

	:- initialization(reset_seed).

	:- private(seed_/3).
	:- dynamic(seed_/3).
	:- mode(seed_(-integer, -integer, -integer), one).
	:- info(seed_/3, [
		comment is 'Stores the current random generator seed values.',
		argnames is ['S0', 'S1', 'S2']
	]).

	random(Random) :-
		retract(seed_(A0, A1, A2)),
		random(A0, A1, A2, B0, B1, B2, Random),
		asserta(seed_(B0, B1, B2)).

	random(A0, A1, A2, B0, B1, B2, Random) :-
		B0 is (A0*171) mod 30269,
		B1 is (A1*172) mod 30307,
		B2 is (A2*170) mod 30323,
		Float is A0/30269 + A1/30307 + A2/30323,
		Random is Float - truncate(Float).

	between(Lower, Upper, Random) :-
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		random(Float),
		Random is truncate((Float * (Upper - Lower + 1) + Lower)).

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

	sequence(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		retract(seed_(A0, A1, A2)),
		sequence(Length, Lower, Upper, A0, A1, A2, B0, B1, B2, Sequence),
		asserta(seed_(B0, B1, B2)).

	sequence(0, _, _, S0, S1, S2, S0, S1, S2, []) :-
		!.
	sequence(N, Lower, Upper, A0, A1, A2, S0, S1, S2, [Random| Sequence]) :-
		N2 is N - 1,
		random(A0, A1, A2, B0, B1, B2, Float),
		Random is truncate(Float * (Upper - Lower + 1) + Lower),
		sequence(N2, Lower, Upper, B0, B1, B2, S0, S1, S2, Sequence).

	set(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		Length =< Upper - Lower + 1,
		retract(seed_(A0, A1, A2)),
		set(Length, Lower, Upper, A0, A1, A2, B0, B1, B2, [], Set),
		asserta(seed_(B0, B1, B2)).

	set(0, _, _, S0, S1, S2, S0, S1, S2, List, Set) :-
		!,
		sort(List, Set).
	set(N, Lower, Upper, A0, A1, A2, S0, S1, S2, Acc, Set) :-
		random(A0, A1, A2, B0, B1, B2, Float),
		Random is truncate(Float * (Upper - Lower + 1) + Lower),
		(	not_member(Acc, Random) ->
			N2 is N - 1,
			set(N2, Lower, Upper, B0, B1, B2, S0, S1, S2, [Random| Acc], Set)
		;	set(N, Lower, Upper, B0, B1, B2, S0, S1, S2, Acc, Set)
		).

	permutation(List, Permutation) :-
		retract(seed_(A0, A1, A2)),
		add_random_key(List, A0, A1, A2, B0, B1, B2, KeyList),
		asserta(seed_(B0, B1, B2)),
		keysort(KeyList, SortedKeyList),
		remove_random_key(SortedKeyList, Permutation).

	add_random_key([], S0, S1, S2, S0, S1, S2, []).
	add_random_key([Head| Tail], A0, A1, A2, S0, S1, S2, [Random-Head| KeyTail]) :-
		random(A0, A1, A2, B0, B1, B2, Random),
		add_random_key(Tail, B0, B1, B2, S0, S1, S2, KeyTail).

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
		retract(seed_(A0, A1, A2)),
		randseq(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), List),
		asserta(seed_(B0, B1, B2)),
		map_truncate(List, Sequence).
	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		retract(seed_(A0, A1, A2)),
		randseq(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), Sequence),
		asserta(seed_(B0, B1, B2)).

	randseq(0, _, _, Seed, Seed, []) :-
		!.
	randseq(N, Lower, Upper, (A0, A1, A2), (C0, C1, C2),  [Random| List]) :-
		N2 is N - 1,
		random(A0, A1, A2, B0, B1, B2, R),
		Random is R * (Upper-Lower)+Lower,
		randseq(N2, Lower, Upper, (B0, B1, B2), (C0, C1, C2), List).

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
		retract(seed_(A0, A1, A2)),
		randset(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), [], Set),
		asserta(seed_(B0, B1, B2)).
	randset(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		retract(seed_(A0, A1, A2)),
		randset(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), [], Set),
		asserta(seed_(B0, B1, B2)).

	randset(0, _, _, Seed, Seed, List, Set) :-
		!,
		sort(List, Set).
	randset(N, Lower, Upper, (A0, A1, A2), (C0, C1, C2), Acc, Set) :-
		random(A0, A1, A2, B0, B1, B2, Float),
		Float2 is Float * (Upper-Lower) + Lower,
		(	integer(Lower) ->
			Random is truncate(Float2)
		;	Random is Float2
		),
		(	not_member(Acc, Random) ->
			N2 is N - 1,
			randset(N2, Lower, Upper, (B0, B1, B2), (C0, C1, C2), [Random| Acc], Set)
		;	randset(N, Lower, Upper, (B0, B1, B2), (C0, C1, C2), Acc, Set)
		).

	not_member([], _).
	not_member([H| T], R) :-
		H =\= R,
		not_member(T, R).

	reset_seed :-
		retractall(seed_(_, _, _)),
		asserta(seed_(3172, 9814, 20125)).

	get_seed(seed(S0, S1, S2)) :-
		seed_(S0, S1, S2).

	set_seed(seed(S0, S1, S2)) :-
		retractall(seed_(_, _, _)),
		asserta(seed_(S0, S1, S2)).

	randomize(Seed) :-
		integer(Seed),
		Seed > 0,
		retractall(seed_(_, _, _)),
		S0 is Seed mod 30269,
		S1 is Seed mod 30307,
		S2 is Seed mod 30323,
		asserta(seed_(S0, S1, S2)).

:- end_object.

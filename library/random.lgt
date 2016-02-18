%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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



:- object(random,
	implements(randomp)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2007/10/13,
		comment is 'Random number generator predicates.'
	]).

	:- synchronized([random/1, randseq/4, randset/4, reset_seed/0, set_seed/1]).

	:- initialization(::reset_seed).

	:- private(seed_/3).
	:- dynamic(seed_/3).
	:- mode(seed_(-integer, -integer, -integer), one).
	:- info(seed_/3, [
		comment is 'Stores the current random generator seed values.',
		argnames is ['S0', 'S1', 'S2']
	]).

	random(Random) :-
		::retract(seed_(A0, A1, A2)),
		random(A0, A1, A2, B0, B1, B2, Random),
		::asserta(seed_(B0, B1, B2)).

	random(A0, A1, A2, B0, B1, B2, Random) :-
		B0 is (A0*171) mod 30269,
		B1 is (A1*172) mod 30307,
		B2 is (A2*170) mod 30323,
		Float is A0/30269 + A1/30307 + A2/30323,
		Random is Float - truncate(Float).

	random(Lower, Upper, Random) :-
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		!,
		random(Float),
		Random is truncate((Float * (Upper-Lower)+Lower)).

	random(Lower, Upper, Random) :-
		float(Lower),
		float(Upper),
		Upper >= Lower,
		random(Float),
		Random is Float * (Upper-Lower)+Lower.

	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		!,
		::retract(seed_(A0, A1, A2)),
		randseq(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), List),
		::asserta(seed_(B0, B1, B2)),
		map_truncate(List, Sequence).

	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		::retract(seed_(A0, A1, A2)),
		randseq(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), Sequence),
		::asserta(seed_(B0, B1, B2)).

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
		::retract(seed_(A0, A1, A2)),
		randset(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), [], Set),
		::asserta(seed_(B0, B1, B2)).
	randset(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		::retract(seed_(A0, A1, A2)),
		randset(Length, Lower, Upper, (A0, A1, A2), (B0, B1, B2), [], Set),
		::asserta(seed_(B0, B1, B2)).

	randset(0, _, _, Seed, Seed, List, List) :-
		!.
	randset(N, Lower, Upper, (A0, A1, A2), (C0, C1, C2), Acc, List) :-
		N2 is N - 1,
		random(A0, A1, A2, B0, B1, B2, Float),
		Float2 is Float * (Upper-Lower)+Lower,
		(	integer(Lower) ->
			Random is truncate(Float2)
		;	Random is Float2
		),
		(	not_member(Acc, Random) ->
			add_ordered(Acc, Random, Acc2),
			randset(N2, Lower, Upper, (B0, B1, B2), (C0, C1, C2), Acc2, List)
		;	randset(N, Lower, Upper, (B0, B1, B2), (C0, C1, C2), Acc, List)
		).

	not_member([], _).
	not_member([H| T], R) :-
		H =\= R,
		not_member(T, R).

	add_ordered([], R, [R]).
	add_ordered([H| T], R, L) :-
		(	H > R ->
			L = [R, H| T]
		;	L = [H| T2],
			add_ordered(T, R, T2)
		).

	reset_seed :-
		::retractall(seed_(_, _, _)),
		::asserta(seed_(3172, 9814, 20125)).

	set_seed(Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(_, _, _)),
		S0 is Seed mod 30269,
		S1 is Seed mod 30307,
		S2 is Seed mod 30323,
		::asserta(seed_(S0, S1, S2)).

:- end_object.

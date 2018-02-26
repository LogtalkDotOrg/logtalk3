%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(backend_random,
	implements(randomp)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2018/02/25,
		comment is 'Random number generator predicates using the backend Prolog compiler own random generator.',
		remarks is [
			'Implementation' - 'The backend Prolog compiler own random generator is only used for the basic random/1, get_seed/1, and set_seed/1 predicates.',
			'Portability' - 'B-Prolog, CxProlog, ECLiPSe, JIProlog, Lean Prolog, Qu-Prolog, Quintus Prolog do not provide implementations for the get_seed/1 and set_seed/1 predicates and calling these predicates simply succeed without performing any action.'
		],
		see_also is [random, fast_random]
	]).

	:- uses(list, [
		length/2, nth1/3
	]).

	between(Lower, Upper, Random) :-
		integer(Lower),
		integer(Upper),
		Upper >= Lower,
		random(Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower.

	member(Random, List) :-
		length(List, Length),
		random(Float),
		Index is truncate(Float * Length + 1),
		nth1(Index, List, Random).

	select(Random, List, Rest) :-
		length(List, Length),
		random(Float),
		Index is truncate(Float * Length + 1),
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
		sequence_aux(Length, Lower, Upper, Sequence).

	sequence_aux(0, _, _, []) :-
		!.
	sequence_aux(N, Lower, Upper, [Random| Sequence]) :-
		N2 is N - 1,
		random(Float),
		Random is truncate(Float * (Upper - Lower + 1)) + Lower,
		sequence_aux(N2, Lower, Upper, Sequence).

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
		randseq_aux(Length, Lower, Upper, List),
		map_truncate(List, Sequence).
	randseq(Length, Lower, Upper, Sequence) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		randseq_aux(Length, Lower, Upper, Sequence).

	randseq_aux(0, _, _, []) :-
		!.
	randseq_aux(N, Lower, Upper, [Random| List]) :-
		N2 is N - 1,
		random(Float),
		Random is Float * (Upper-Lower)+Lower,
		randseq_aux(N2, Lower, Upper, List).

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
		randset_aux(Length, Lower, Upper, [], Set).
	randset(Length, Lower, Upper, Set) :-
		integer(Length),
		Length >= 0,
		float(Lower),
		float(Upper),
		Upper >= Lower,
		randset_aux(Length, Lower, Upper, [], Set).

	randset_aux(0, _, _, Set, List) :-
		!,
		sort(Set, List).
	randset_aux(N, Lower, Upper, Acc, List) :-
		random(Float),
		Float2 is Float * (Upper-Lower) + Lower,
		(	integer(Lower) ->
			Random is truncate(Float2)
		;	Random is Float2
		),
		(	not_member(Acc, Random) ->
			N2 is N - 1,
			randset_aux(N2, Lower, Upper, [Random| Acc], List)
		;	randset_aux(N, Lower, Upper, Acc, List)
		).

	not_member([], _).
	not_member([H| T], R) :-
		H =\= R,
		not_member(T, R).

	:- if(current_logtalk_flag(prolog_dialect, b)).
		get_seed(_).
		set_seed(_).
		random(Random) :- {Random is random}.
	:- elif(current_logtalk_flag(prolog_dialect, cx)).
		get_seed(_).
		set_seed(_).
		random(Random) :- {Random is random}.
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		get_seed(_).
		set_seed(_).
		random(Random) :- {frandom(Random)}.
	:- elif(current_logtalk_flag(prolog_dialect, gnu)).
		get_seed(Seed) :- {get_seed(Seed)}.
		set_seed(Seed) :- {set_seed(Seed)}.
		random(Random) :- {random(Random)}.
	:- elif(current_logtalk_flag(prolog_dialect, ji)).
		get_seed(_).
		set_seed(_).
		random(Random) :- {Random is rand}.
	:- elif(current_logtalk_flag(prolog_dialect, lean)).
		get_seed(_).
		set_seed(_).
		random(Random) :- {Random is random}.
	:- elif(current_logtalk_flag(prolog_dialect, qp)).
		get_seed(Seed) :- {srandom(Seed)}.
		set_seed(Seed) :- {srandom(Seed)}.
		random(Random) :- {random(Random)}.
	:- elif(current_logtalk_flag(prolog_dialect, quintus)).
		get_seed(_).
		set_seed(_).
		{:- use_module(library(random), [random/1])}.
		random(Random) :- {random(Random)}.
	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).
		{:- use_module(library(random), [])}.
		get_seed(Seed) :- {random:getrand(Seed)}.
		set_seed(Seed) :- {random:setrand(Seed)}.
		random(Random) :- {random:random(Random)}.
	:- elif(current_logtalk_flag(prolog_dialect, swi)).
		get_seed(Seed) :- {random_property(state(Seed))}.
		set_seed(Seed) :- {set_random(seed(Seed))}.
		random(Random) :- {Random is random_float}.
	:- elif(current_logtalk_flag(prolog_dialect, xsb)).
		{:- import(from(/(getrand,1), random))}.
		{:- import(from(/(setrand,1), random))}.
		{:- import(from(/(random,1), random))}.
		get_seed(Seed) :- {getrand(Seed)}.
		set_seed(Seed) :- {setrand(Seed)}.
		random(Random) :- {random(Random)}.
	:- elif(current_logtalk_flag(prolog_dialect, yap)).
		{:- use_module(library(random), [])}.
		get_seed(Seed) :- {random:getrand(Seed)}.
		set_seed(Seed) :- {random:setrand(Seed)}.
		random(Random) :- {random:random(Random)}.
	:- endif.

:- end_object.

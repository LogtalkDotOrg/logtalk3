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


:- object(buffer(_MaxCapacity_)).

	:- info([
		version is 3:0:0,
		author is 'Paulo Moura',
		date is 2025-05-08,
		comment is 'Producer-consumer problem with a bounded buffer.'
	]).

	:- threaded.

	:- public(put/1).
	:- mode(put(?integer), one).
	:- info(put/1, [
		comment is 'Put an item in the buffer.'
	]).

	:- public(get/1).
	:- mode(get(?integer), one).
	:- info(get/1, [
		comment is 'Get an item from the buffer.'
	]).

	% public predicates just for testing
	:- public(produced/1).
	:- dynamic(produced/1).

	:- public(consumed/1).
	:- dynamic(consumed/1).

	:- private(item_/1).
	:- dynamic(item_/1).

	:- private(size_/1).
	:- dynamic(size_/1).

	:- uses(format, [format/2]).

	size_(0).

	:- synchronized([put_item/1, get_item/1]).

	put_item(Item) :-
		assertz(item_(Item)),
		retract(size_(N)),
		N2 is N + 1,
		assertz(size_(N2)),
		format('produced item ~w (~w/~w items in the buffer)~n', [Item, N2, _MaxCapacity_]),
		assertz(produced(Item)),
		(	N =:= 0 ->
			threaded_notify(not_empty)
		;	true
		).

	get_item(Item) :-
		retract(item_(Item)),
		retract(size_(N)),
		N2 is N - 1,
		assertz(size_(N2)),
		format('consumed item ~w (~w/~w items in the buffer)~n', [Item, N2, _MaxCapacity_]),
		assertz(consumed(Item)),
		(	N =:= _MaxCapacity_ ->
			threaded_notify(vacancy)
		;	true
		).

	put(Item) :-
		(	size_(_MaxCapacity_) ->
			% maximum buffer capacity have been reached;
			% wait until an item is consumed
			threaded_wait(vacancy),
			% be sure to consume all "vacancy" notifications before proceeding
			put(Item)
		;	put_item(Item)
		).

	get(Item) :-
		(	size_(0) ->
			% buffer is empty, wait until an item is produced
			threaded_wait(not_empty),
			% be sure to consume all "not_empty" notifications before proceeding
			get(Item)
		;	get_item(Item)
		).

:- end_object.


:- object(producer(_MaxCapacity_, _MaxTime_)).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		% simulate a variable amount of time to produce a new item
		random::random(0.1, _MaxTime_, Random),
		thread_sleep(Random),
		buffer(_MaxCapacity_)::put(M),
		M2 is M + 1,
		run(M2, N).

:- end_object.


:- object(consumer(_MaxCapacity_, _MaxTime_)).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		% simulate a variable amount of time to consume an item
		random::random(0.1, _MaxTime_, Random),
		thread_sleep(Random),
		buffer(_MaxCapacity_)::get(_Item),
		M2 is M + 1,
		run(M2, N).

:- end_object.

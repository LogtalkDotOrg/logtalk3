%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(pmq).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-11-19,
		comment is 'An implementation of a priority queue using a threaded engine.'
	]).

	:- threaded.

	% initialize a perpetual threaded engine holding a priority queue
	:- initialization((
		maxheap::new(Heap),
		threaded_engine_create(none, loop(Heap), pq)
	)).

	% send a message to the priority queue
	:- public(send/1).
	send(Term) :-
		threaded_engine_post(pq, Term).

	% retrieve a list of messages ordered top priority first
	:- public(messages/1).
	messages(Messages) :-
		threaded_engine_post(pq, messages),
		threaded_engine_next(pq, Messages).

	loop(Heap0) :-
		threaded_engine_fetch(Term),
		(	Term == messages ->
			maxheap::as_list(Heap0, Pairs),
			pairs::values(Pairs, Messages),
			threaded_engine_yield(Messages),
			maxheap::new(Heap1)
		;	Term = Priority-Message,
			maxheap::insert(Priority, Message, Heap0, Heap1)
		),
		loop(Heap1).

:- end_object.

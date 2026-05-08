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


:- object(pool).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-02-15,
		comment is 'Example of using threaded engines to implement thread pools.'
	]).

	:- threaded.

	:- public(new/2).
	:- mode(new(+atom, +positive_integer), one).
	:- info(new/2, [
		comment is 'Create a new thread pool with the given name and number of workers threads.',
		argnames is ['Name', 'Workers']
	]).

	:- public(stop/1).
	:- mode(stop(+atom), one).
	:- info(stop/1, [
		comment is 'Stop the given thread pool, terminating all workers threads.',
		argnames is ['Name']
	]).

	:- public(submit/2).
	:- mode(submit(+atom, @callable), one).
	:- info(submit/2, [
		comment is 'Submit and collects an unit of work to/from the given new thread pool.',
		argnames is ['Controller', 'Goal']
	]).

	:- public(spawn/2).
	:- mode(spawn(+atom, @callable), one).
	:- info(spawn/2, [
		comment is 'Spawns an unit of work to the given new thread pool.',
		argnames is ['Controller', 'Goal']
	]).

	:- public(collect/2).
	:- mode(collect(+atom, @callable), one).
	:- info(collect/2, [
		comment is 'Collects the goal result from the thread pool.',
		argnames is ['Controller', 'Goal']
	]).

	new(Controller, N) :-
		message_queue_create(Queue),
		threaded_engine_create(true, controller_loop(Queue), Controller),
		forall(
			(	integer::between(1, N, _),
				threaded_engine_create(true, worker_loop(Queue), Worker)
			),
			thread_send_message(Queue, worker(Worker))
		).

	stop(Controller) :-
		threaded_engine_post(Controller, stop),
		threaded_engine_next(Controller, done),
		threaded_engine_destroy(Controller).

	submit(Controller, Goal) :-
		threaded_engine_post(Controller, goal(Goal)),
		threaded_engine_next(Controller, result(Goal)).

	spawn(Controller, Goal) :-
		threaded_engine_post(Controller, goal(Goal)).

	collect(Controller, Goal) :-
		threaded_engine_next(Controller, result(Goal)).

	controller_loop(Queue) :-
		threaded_engine_fetch(Work),
		(	Work == stop ->
			stop_works(Queue),
			threaded_engine_yield(done)
		;	Work = goal(Goal),
			thread_get_message(Queue, worker(Worker)),
			thread_send_message(Queue, busy(Worker)),
			threaded_engine_post(Worker, goal(Goal)),
			controller_loop(Queue)
		).

	stop_works(Queue) :-
		findall(
			Worker,
			(	(	thread_get_message(Queue, worker(Worker))
				;	thread_get_message(Queue, busy(Worker))
				),
				threaded_engine_post(Worker, stop)
			),
			Workers
		),
		forall(
			list::member(Worker, Workers),
			threaded_engine_destroy(Worker)
		).

	worker_loop(Queue) :-
		threaded_engine_fetch(Work),
		(	Work == stop ->
			true
		;	Work = goal(Goal),
			once(Goal),
			thread_get_message(Queue, busy(Worker)),
			thread_send_message(Queue, result(Goal)),
			thread_send_message(Queue, worker(Worker)),
			worker_loop(Queue)
		).

:- end_object.

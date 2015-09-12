%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(metered_concurrency).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2013/11/02,
		comment is 'Simple example of using multi-threading notifications, which use a per-object FIFO message queue, thus avoiding the need of idle-loops, for implementing a counting semaphore.'
	]).

	:- threaded.

	:- public(run/2).
	run(Workers, Max) :-
		% start the semaphore and the workers
		threaded_ignore(semaphore(Max, Max)),
		forall(
			integer::between(1, Workers, Worker),
			threaded_call(worker(Worker))
		),
		% wait for the workers to finish
		forall(
			integer::between(1, Workers, Worker),
			threaded_exit(worker(Worker))
		),
		% tell the semaphore thread to stop
		threaded_notify(worker(stop, _)).

	:- public(run/0).
	run :-
		% default values: 7 workers, 2 concurrent workers
		run(7, 2).

	:- synchronized([
		acquired_semaphore_message/1,
		releasing_semaphore_message/1
	]).

	semaphore(N, Max) :-
		threaded_wait(worker(Action, Worker)),
		(	Action == acquire, N > 0 ->
			M is N - 1,
			threaded_notify(semaphore(acquired, Worker)),
			semaphore(M, Max)
		;	Action == release ->
			M is N + 1,
			threaded_notify(semaphore(released, Worker)),
			semaphore(M, Max)
		;	Action == stop ->
			true
		;	% Action == acquire, N =:= 0,
			threaded_wait(worker(release, OtherWorker)),
			threaded_notify(semaphore(released, OtherWorker)),
			threaded_notify(semaphore(acquired, Worker)),
			semaphore(N, Max)
		).

	worker(Worker) :-
		% use a random setup time for the worker
		random::random(0.0, 2.0, Setup),
		thread_sleep(Setup),
		threaded_notify(worker(acquire, Worker)),
		threaded_wait(semaphore(acquired, Worker)),
		write('Worker '), write(Worker), write(' acquired semaphore\n'),
		thread_sleep(2),
		threaded_notify(worker(release, Worker)),
		write('Worker '), write(Worker), write(' releasing semaphore\n'),
		threaded_wait(semaphore(released, Worker)).

	acquired_semaphore_message(Worker) :-
		write('Worker '), write(Worker), write(' acquired semaphore\n').

	releasing_semaphore_message(Worker) :-
		write('Worker '), write(Worker), write(' releasing semaphore\n').

:- end_object.

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


:- object(beatles).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-02-06,
		comment is 'Simple example of using a barrier to synchronize a set of threads.'
	]).

	:- threaded.

	:- public(sing_along/0).
	:- mode(sing_along, one).
	:- info(sing_along/0, [
		comment is 'Wait for all threads to say "hello" and then proceed with the threads saying "goodbye".'
	]).

	% for testing
	:- public([hello/1, goodbye/1]).
	:- dynamic([hello/1, goodbye/1]).

	:- uses(random, [random/3]).

	sing(Thread) :-
		% spend some time before saying hello
		random(1, 3, BusyHello), thread_sleep(BusyHello),
		write(hello(Thread)), flush_output,
		assertz(hello(Thread)),
		% notify barrier that you have arrived
		threaded_notify(ready(Thread)),
		% wait for green light to cross the barrier
		threaded_wait(go(Thread)),
		% spend some time before saying goodbye
		random(1, 3, BusyGoodbye), thread_sleep(BusyGoodbye),
		write(goodbye(Thread)), flush_output,
		assertz(goodbye(Thread)),
		threaded_notify(done(Thread)).

	sing_along :-
		% clean testing data
		retractall(hello(_)),
		retractall(goodbye(_)),
		% start the threads
		threaded_ignore(sing(1)),
		threaded_ignore(sing(2)),
		threaded_ignore(sing(3)),
		threaded_ignore(sing(4)),
		% wait for all threads to reach the barrier
		threaded_wait([ready(1), ready(2), ready(3), ready(4)]),
		nl, write('Enough of hellos! Time for goodbyes!'), nl,
		% give green light to all threads to cross the barrier
		threaded_notify([go(1), go(2), go(3), go(4)]),
		threaded_wait([done(1), done(2), done(3), done(4)]),
		nl.

:- end_object.

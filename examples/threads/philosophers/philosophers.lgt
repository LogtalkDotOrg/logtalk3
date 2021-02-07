%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- category(chopstick).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2007-3-19,
		comment is 'Dining philosophers problem: chopstick representation.'
	]).

	:- public(pick_up/0).
	:- mode(pick_up, zero_or_one).
	:- info(pick_up/0, [
		comment is 'A Philosopher picks up the chopstick.'
	]).

	:- public(put_down/0).
	:- mode(put_down, zero_or_one).
	:- info(put_down/0, [
		comment is 'A Philosopher puts down the chopstick.'
	]).

	% chopstick actions (picking up and putting down) are synchronized using a notification
	% such that a chopstick can only be handled by a single philosopher at a time:

	pick_up :-
		threaded_wait(available).

	put_down :-
		threaded_notify(available).

:- end_category.


:- object(cs1,
	imports(chopstick)).

	:- threaded.
	:- initialization(threaded_notify(available)).

:- end_object.


:- object(cs2,
	imports(chopstick)).

	:- threaded.
	:- initialization(threaded_notify(available)).

:- end_object.


:- object(cs3,
	imports(chopstick)).

	:- threaded.
	:- initialization(threaded_notify(available)).

:- end_object.


:- object(cs4,
	imports(chopstick)).

	:- threaded.
	:- initialization(threaded_notify(available)).

:- end_object.


:- object(cs5,
	imports(chopstick)).

	:- threaded.
	:- initialization(threaded_notify(available)).

:- end_object.



:- protocol(philosopherp).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2013-02-09,
		comment is 'Dining philosophers ptotocol.'
	]).

	:- public(left_chopstick/1).
	:- mode(left_chopstick(?object_identifier), zero_or_one).
	:- info(left_chopstick/1, [
		comment is 'Chopstick at the left of a philosopher.',
		argnames is ['Chopstick']
	]).

	:- public(right_chopstick/1).
	:- mode(right_chopstick(?object_identifier), zero_or_one).
	:- info(right_chopstick/1, [
		comment is 'Chopstick at the right of a philosopher.',
		argnames is ['Chopstick']
	]).

	:- public(run/2).
	:- mode(run(+integer, +integer), one).
	:- info(run/2, [
		comment is 'Runs Count number of thinking/eating cycles, with each activity taking MaxTime (in seconds).',
		argnames is ['Count', 'MaxTime']
	]).

:- end_protocol.



:- category(philosopher,
	implements(philosopherp)).

	:- info([
		version is 2:2:0,
		author is 'Paulo Moura',
		date is 2013-02-09,
		comment is 'Dining philosophers problem: philosopher representation.'
	]).

	:- private(message/1).
	:- synchronized(message/1).
	:- mode(message(+list), one).
	:- info(message/1, [
		comment is 'Writes all the terms on a list as an atomic operation.',
		argnames is ['Atoms']
	]).

	:- uses(random, [random/3]).

	run(0, _) :-
		this(Philosopher),
		message([Philosopher, ' terminated.'
	]).

	run(Count, MaxTime) :-
		Count > 0,
		think(MaxTime),
		eat(MaxTime),
		Count2 is Count - 1,
		run(Count2, MaxTime).

	think(MaxTime) :-
		this(Philosopher),
		random(1, MaxTime, ThinkTime),
		message(['Philosopher ', Philosopher, ' thinking for ', ThinkTime, ' seconds.']),
		thread_sleep(ThinkTime).

	eat(MaxTime) :-
		this(Philosopher),
		random(1, MaxTime, EatTime),
		::left_chopstick(LeftStick),
		::right_chopstick(RightStick),
		LeftStick::pick_up,
		RightStick::pick_up,
		message(['Philosopher ', Philosopher, ' eating for ', EatTime, ' seconds with chopsticks ', LeftStick, ' and ', RightStick, '.']),
		thread_sleep(EatTime),
		LeftStick::put_down,
		RightStick::put_down.

	% writing a message needs to be synchronized as it's accomplished
	% using a combination of individual write/1 and nl/0 calls:
	message([]) :-
		nl,
		flush_output.
	message([Atom| Atoms]) :-
		write(Atom),
		message(Atoms).

:- end_category.


:- object(p1,
	imports(philosopher)).

	left_chopstick(cs1).
	right_chopstick(cs2).

:- end_object.


:- object(p2,
	imports(philosopher)).

	left_chopstick(cs2).
	right_chopstick(cs3).

:- end_object.


:- object(p3,
	imports(philosopher)).

	left_chopstick(cs3).
	right_chopstick(cs4).

:- end_object.


:- object(p4,
	imports(philosopher)).

	left_chopstick(cs4).
	right_chopstick(cs5).

:- end_object.


:- object(p5,
	imports(philosopher)).

	% change order so that the chopsticks are picked
	% in different order from the other philosophers
	left_chopstick(cs1).
	right_chopstick(cs5).

:- end_object.


:- object(philosopher(_Philosopher_, _LeftChopstick_, _RightChopstick_),
	implements(philosopherp)).

	:- info([
		version is 2:2:0,
		author is 'Paulo Moura',
		date is 2021-02-07,
		comment is 'Dining philosophers problem: philosopher representation.'
	]).

	:- private(message/1).
	:- synchronized(message/1).
	:- mode(message(+list), one).
	:- info(message/1, [
		comment is 'Writes all the terms on a list as an atomic operation.',
		argnames is ['Atoms']
	]).

	:- uses(random, [random/3]).

	left_chopstick(_LeftChopstick_).

	right_chopstick(_RightChopstick_).

	run(0, _) :-
		message([_Philosopher_, ' terminated.']).

	run(Count, MaxTime) :-
		Count > 0,
		think(MaxTime),
		eat(MaxTime),
		Count2 is Count - 1,
		run(Count2, MaxTime).

	think(MaxTime) :-
		random(1, MaxTime, ThinkTime),
		message(['Philosopher ', _Philosopher_, ' thinking for ', ThinkTime, ' seconds.']),
		thread_sleep(ThinkTime).

	eat(MaxTime) :-
		random(1, MaxTime, EatTime),
		_LeftChopstick_::pick_up,
		_RightChopstick_::pick_up,
		message(['Philosopher ', _Philosopher_, ' eating for ', EatTime, ' seconds with chopsticks ', _LeftChopstick_, ' and ', _RightChopstick_, '.']),
		thread_sleep(EatTime),
		_LeftChopstick_::put_down,
		_RightChopstick_::put_down.

	% writing a message needs to be synchronized as it's accomplished
	% using a combination of individual write/1 and nl/0 calls:
	message([]) :-
		nl,
		flush_output.
	message([Atom| Atoms]) :-
		write(Atom),
		message(Atoms).

:- end_object.

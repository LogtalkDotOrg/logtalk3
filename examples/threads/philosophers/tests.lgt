%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2025-04-16,
		comment is 'Unit tests for the "threads/philosophers" example.'
	]).

	:- threaded.

	test(philosophers_1, true((p1::terminated,p2::terminated,p3::terminated,p4::terminated,p5::terminated))) :-
		^^suppress_text_output,
		p1::retractall(terminated),
		p2::retractall(terminated),
		p3::retractall(terminated),
		p4::retractall(terminated),
		p5::retractall(terminated),
		threaded_once(p1::run(5, 5)),
		threaded_once(p2::run(5, 5)),
		threaded_once(p3::run(5, 5)),
		threaded_once(p4::run(5, 5)),
		threaded_once(p5::run(5, 5)),
		threaded_exit(p1::run(5, 5)),
		threaded_exit(p2::run(5, 5)),
		threaded_exit(p3::run(5, 5)),
		threaded_exit(p4::run(5, 5)),
		threaded_exit(p5::run(5, 5)).

	test(philosophers_2, true(Philosophers == [p1,p2,p3,p4,p5])) :-
		^^suppress_text_output,
		philosopher(_,_,_)::retractall(terminated(_)),
		threaded_once(philosopher(p1,cs1,cs2)::run(5, 5)),
		threaded_once(philosopher(p2,cs2,cs3)::run(5, 5)),
		threaded_once(philosopher(p3,cs3,cs4)::run(5, 5)),
		threaded_once(philosopher(p4,cs4,cs5)::run(5, 5)),
		threaded_once(philosopher(p5,cs1,cs5)::run(5, 5)),
		threaded_exit(philosopher(p1,cs1,cs2)::run(5, 5)),
		threaded_exit(philosopher(p2,cs2,cs3)::run(5, 5)),
		threaded_exit(philosopher(p3,cs3,cs4)::run(5, 5)),
		threaded_exit(philosopher(p4,cs4,cs5)::run(5, 5)),
		threaded_exit(philosopher(p5,cs1,cs5)::run(5, 5)),
		findall(Philosopher, philosopher(_,_,_)::terminated(Philosopher), Philosophers0),
		sort(Philosophers0, Philosophers).

:- end_object.

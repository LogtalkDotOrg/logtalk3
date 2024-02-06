%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2024-02-06,
		comment is 'Unit tests for the "threads/philosophers" example.'
	]).

	:- threaded.

	test(philosophers_1, true) :-
		^^suppress_text_output,
		threaded_call(p1::run(5, 5)),
		threaded_call(p2::run(5, 5)),
		threaded_call(p3::run(5, 5)),
		threaded_call(p4::run(5, 5)),
		threaded_call(p5::run(5, 5)),
		threaded_exit(p1::run(5, 5)),
		threaded_exit(p2::run(5, 5)),
		threaded_exit(p3::run(5, 5)),
		threaded_exit(p4::run(5, 5)),
		threaded_exit(p5::run(5, 5)).

	test(philosophers_2, true(Philosophers == [p1,p2,p3,p4,p5])) :-
		^^suppress_text_output,
		philosopher(_,_,_)::retractall(terminated(_)),
		threaded_call(philosopher(p1,cs1,cs2)::run(5, 5)),
		threaded_call(philosopher(p2,cs2,cs3)::run(5, 5)),
		threaded_call(philosopher(p3,cs3,cs4)::run(5, 5)),
		threaded_call(philosopher(p4,cs4,cs5)::run(5, 5)),
		threaded_call(philosopher(p5,cs1,cs5)::run(5, 5)),
		threaded_exit(philosopher(p1,cs1,cs2)::run(5, 5)),
		threaded_exit(philosopher(p2,cs2,cs3)::run(5, 5)),
		threaded_exit(philosopher(p3,cs3,cs4)::run(5, 5)),
		threaded_exit(philosopher(p4,cs4,cs5)::run(5, 5)),
		threaded_exit(philosopher(p5,cs1,cs5)::run(5, 5)),
		findall(Philosopher, philosopher(_,_,_)::terminated(Philosopher), Philosophers0),
		sort(Philosophers0, Philosophers).

:- end_object.

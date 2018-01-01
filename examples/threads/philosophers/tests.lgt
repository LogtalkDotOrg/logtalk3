%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/08/06,
		comment is 'Unit tests for the "threads/philosophers" example.'
	]).

	:- threaded.

	cover(chopstick).
	cover(philosopher).

	cover(philosopher(_, _, _)).

	test(philosophers_1) :-
		threaded_ignore(p1::run(5, 5)),
		threaded_ignore(p2::run(5, 5)),
		threaded_ignore(p3::run(5, 5)),
		threaded_ignore(p4::run(5, 5)),
		threaded_ignore(p5::run(5, 5)).

	test(philosophers_2) :-
		threaded_ignore(philosopher(p1,cs1,cs2)::run(5, 5)),
		threaded_ignore(philosopher(p2,cs2,cs3)::run(5, 5)),
		threaded_ignore(philosopher(p3,cs3,cs4)::run(5, 5)),
		threaded_ignore(philosopher(p4,cs4,cs5)::run(5, 5)),
		threaded_ignore(philosopher(p5,cs1,cs5)::run(5, 5)).

:- end_object.

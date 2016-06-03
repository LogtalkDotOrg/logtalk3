%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		date is 2016/06/03,
		comment is 'Unit tests for the "ebench" example.'
	]).

	:- threaded.

	:- uses(lgtunit, [benchmark/2]).

	note('creation and stopping of 1000 threaded engines').

	test(ebench_1, true, [note(create(true)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,I),atom_number(E,I),threaded_engine_create(_,true,E),fail;true),
			Time
		).

	test(ebench_2, true, [note(stop(true)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,I),atom_number(E,I),threaded_engine_stop(E),fail;true),
			Time
		).

	test(ebench_3, true, [note(create(repeat)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,I),atom_number(E,I),threaded_engine_create(_,repeat,E),fail;true),
			Time
		).

	test(ebench_4, true, [note(stop(repeat)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,I),atom_number(E,I),threaded_engine_stop(E),fail;true),
			Time
		).

	test(ebench_5, true, [note(create(loop)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,I),atom_number(E,I),threaded_engine_create(_,loop,E),fail;true),
			Time
		).

	test(ebench_6, true, [note(stop(loop)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,I),atom_number(E,I),threaded_engine_stop(E),fail;true),
			Time
		).

	% auxiliary predicates

	loop :-
		threaded_engine_fetch(_),
		loop.

:- end_object.

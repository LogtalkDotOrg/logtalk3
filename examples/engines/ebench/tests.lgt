%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2019-01-19,
		comment is 'Unit tests for the "ebench" example.'
	]).

	:- threaded.

	:- uses(lgtunit, [benchmark/2]).
	:- uses(integer, [between/3]).

	note('total times to create and destroy 1000 threaded engines').

	test(ebench_1, true, [note(create(true)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,_),threaded_engine_create(_,true,_),fail;true),
			Time
		).

	test(ebench_2, true, [note(destroy(true)-seconds(Time))]) :-
		benchmark(
			(threaded_engine(Engine),threaded_engine_destroy(Engine),fail;true),
			Time
		).

	test(ebench_3, true, [note(create(repeat)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,_),threaded_engine_create(_,repeat,_),fail;true),
			Time
		).

	test(ebench_4, true, [note(destroy(repeat)-seconds(Time))]) :-
		benchmark(
			(threaded_engine(Engine),threaded_engine_destroy(Engine),fail;true),
			Time
		).

	test(ebench_5, true, [note(create(loop)-seconds(Time))]) :-
		benchmark(
			(between(1,1000,_),threaded_engine_create(_,loop,_),fail;true),
			Time
		).

	test(ebench_6, true, [note(destroy(loop)-seconds(Time))]) :-
		benchmark(
			(threaded_engine(Engine),threaded_engine_destroy(Engine),fail;true),
			Time
		).

	% auxiliary predicates

	loop :-
		threaded_engine_fetch(_),
		loop.

:- end_object.

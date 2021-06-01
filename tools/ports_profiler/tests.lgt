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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2021-06-01,
		comment is 'Unit tests for the "ports_profiler" tool.'
	]).

	:- uses(ports_profiler, [
		data/0, data/1, port/5,
		reset/0, reset/1
	]).

	setup :-
		foo::solutions.

	test(ports_profiler_data_0_01, deterministic) :-
		^^suppress_text_output,
		data.

	test(ports_profiler_data_1_01, deterministic) :-
		^^suppress_text_output,
		data(foo).

	test(ports_profiler_data_1_02, deterministic) :-
		^^suppress_text_output,
		data(non_existent).

	test(ports_profiler_port_5_01, true(Functor/Arity-Count == solutions/0-1)) :-
		port(fact, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_02, true(Functor/Arity-Count == solutions/0-1)) :-
		port(rule, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_03, true(Functor/Arity-Count == solutions/0-1)) :-
		port(call, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_04, true(Functor/Arity-Count == solutions/0-1)) :-
		port(exit, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_05, true(Data == [baz/1-2, qux/1-6])) :-
		setof(Functor/Arity-Count, port(fact, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_06, true(Data == [bar/2-1, baz/1-1, qux/1-2])) :-
		setof(Functor/Arity-Count, port(call, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_07, true(Data == [bar/2-1, baz/1-1, qux/1-2])) :-
		setof(Functor/Arity-Count, port(exit, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_08, true(Data == [bar/2-5, baz/1-1, qux/1-4])) :-
		setof(Functor/Arity-Count, port(nd_exit, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_09, true(Data == [bar/2-5, baz/1-1, qux/1-4])) :-
		setof(Functor/Arity-Count, port(redo, bar, Functor, Arity, Count), Data).

	test(ports_profiler_reset_0_01, deterministic) :-
		reset.

	test(ports_profiler_reset_1_01, deterministic) :-
		reset(foo).

	test(ports_profiler_reset_1_02, deterministic) :-
		reset(non_existent).

:- end_object.

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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2018-02-28,
		comment is 'Unit tests for the "ports_profiler" tool.'
	]).

	:- uses(ports_profiler, [
		data/0, data/1, port/5,
		reset/0, reset/1
	]).

	setup :-
		foo::solutions.

	deterministic(ports_profiler_data_0_01) :-
		data.

	deterministic(ports_profiler_data_1_01) :-
		data(foo).

	deterministic(ports_profiler_data_1_02) :-
		data(non_existent).

	succeeds(ports_profiler_port_5_01) :-
		port(fact, foo, Functor, Arity, Count),
		Functor/Arity == solutions/0, Count == 1.

	succeeds(ports_profiler_port_5_02) :-
		port(rule, foo, Functor, Arity, Count),
		Functor/Arity == solutions/0, Count == 1.

	succeeds(ports_profiler_port_5_03) :-
		port(call, foo, Functor, Arity, Count),
		Functor/Arity == solutions/0, Count == 1.

	succeeds(ports_profiler_port_5_04) :-
		port(exit, foo, Functor, Arity, Count),
		Functor/Arity == solutions/0, Count == 1.

	succeeds(ports_profiler_port_5_05) :-
		port(fact, bar, Functor, Arity, Count),
		Functor/Arity == qux/1, Count == 6.

	succeeds(ports_profiler_port_5_06) :-
		port(call, bar, Functor, Arity, Count),
		Functor/Arity == qux/1, Count == 2.

	succeeds(ports_profiler_port_5_07) :-
		port(exit, bar, Functor, Arity, Count),
		Functor/Arity == qux/1, Count == 2.

	succeeds(ports_profiler_port_5_08) :-
		port(nd_exit, bar, Functor, Arity, Count),
		Functor/Arity == qux/1, Count == 4.

	succeeds(ports_profiler_port_5_09) :-
		port(redo, bar, Functor, Arity, Count),
		Functor/Arity == qux/1, Count == 4.

	deterministic(ports_profiler_reset_0_01) :-
		reset.

	deterministic(ports_profiler_reset_1_01) :-
		reset(foo).

	deterministic(ports_profiler_reset_1_02) :-
		reset(non_existent).

:- end_object.

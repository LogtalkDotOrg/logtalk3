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


:- object(timer).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2006-9-17,
		comment is 'Call executing time profiler.'
	]).

	:- uses(time, [cpu_time/1]).
	:- uses(loop, [forto/3]).

	:- public(timer/2).
	:- meta_predicate(timer(0, *)).
	:- mode(timer(+callable, -number), one).
	:- info(timer/2, [
		comment is 'Returns time to execute a call.',
		argnames is ['Call', 'Time']
	]).

	:- public(timer/3).
	:- meta_predicate(timer(0, *, *)).
	:- mode(timer(+callable, +integer, -float), one).
	:- info(timer/3, [
		comment is 'Returns the average time needed to to execute a call.',
		argnames is ['Call', 'Times', 'Time']
	]).

	timer(Call, Time) :-
		cpu_time(Start),
		(call(Call) -> true; true),
		cpu_time(End),
		Time is End - Start.

	timer(Call, Times, Time) :-
		cpu_time(Start),
		forto(1, Times, Call),
		cpu_time(End),
		cpu_time(Start2),
		forto(1, 0, true),
		cpu_time(End2),
		Overhead is End2 - Start2,
		Time is (End - Start - Overhead) / Times.

:- end_object.

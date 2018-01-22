%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


:- object(yield).

	:- info([
		version is 1.0,
		author is 'Jan Wielemaker. Adapted to Logtalk by Paulo Moura',
		date is 2016/06/16,
		comment is 'Example of fetching engine answers returned using the threaded_engine_yield/1 predicate.'
	]).

	:- threaded.

	:- public(yield/2).
	:- mode(yield(+natural, -list(natural)), one).
	:- info(yield/2, [
		comment is 'Fetchs answers from an engine that returns them using threaded_engine_yield/1. Note that yield_loop/2 eventually fails. If we succeed we would extract one more answer from the engine.',
		argnames is ['Length', 'List']
	]).

	yield(Length, List) :-
		threaded_engine_create(_, yield_loop(1,Length), Engine),
		get_answers(Engine, List),
		threaded_engine_destroy(Engine).

	yield_loop(I, M) :-
		I =< M,
		threaded_engine_yield(I),
		I2 is I+1,
		yield_loop(I2, M).

	get_answers(Engine, [Head| Tail]) :-
		threaded_engine_next(Engine, Head),
		!,
		get_answers(Engine, Tail).
	get_answers(_, []).

:- end_object.

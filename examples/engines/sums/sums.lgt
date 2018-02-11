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


:- object(sums).

	:- info([
		version is 1.0,
		author is 'Jan Wielemaker. Adapted to Logtalk by Paulo Moura',
		date is 2016/06/15,
		comment is 'Example of using engines to accumulate state.'
	]).

	:- threaded.

	:- public(rd/2).
	:- mode(rd(+natural, -list(natural)), one).
	:- info(rd/2, [
		comment is 'Use an engine to accumulate state.',
		argnames is ['Length', 'Sums']
	]).

	rd(N, Sums) :-
		integer::sequence(1, N, List),
		threaded_engine_create(_, sum(0), Engine),
		meta::maplist(list_to_sums(Engine), List, Sums),
		threaded_engine_destroy(Engine).

	sum(Sum) :-
		threaded_engine_fetch(New),
		Sum1 is New + Sum,
		threaded_engine_yield(Sum1),
		sum(Sum1).

	list_to_sums(Engine, N, Sum) :-
		threaded_engine_post(Engine, N),
		threaded_engine_next(Engine, Sum).

:- end_object.

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


:- object(lazy).

	:- info([
		version is 1.0,
		author is 'Paul Tarau and Paulo Moura',
		date is 2016/06/09,
		comment is 'Lazy meta-predicates implemented using coroutining and threaded engines.'
	]).

	:- threaded.

	:- public(find_all/3).
	:- meta_predicate(find_all(*, 0, *)).
	:- mode(find_all(@term, +callable, -list), one).
	:- info(find_all/3, [
		comment is 'Lazy findall/3 alternative using coroutining and threaded engines.',
		argnames is ['Template', 'Goal', 'LazyList']
	]).

	find_all(Template, Goal, LazyList):-
		threaded_engine_create(Template, Goal, Engine),
		(	threaded_engine_answer(Engine, Head) ->
			freeze(LazyList, source_lazy_list(LazyList, Head, Engine))
		;	threaded_engine_stop(Engine)
		).

	source_lazy_list([Head| LazyTail], Head, Engine) :-
		(	threaded_engine_answer(Engine, Next) ->
			freeze(LazyTail, source_lazy_list(LazyTail, Next, Engine))
		;	LazyTail = [],
			threaded_engine_stop(Engine)
		).

:- end_object.

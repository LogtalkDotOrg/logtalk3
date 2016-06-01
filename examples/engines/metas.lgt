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


:- object(metas).

	:- info([
		version is 1.0,
		author is 'Paul Tarau and Paulo Moura',
		date is 2016/06/01,
		comment is 'Examples of implementing meta-predicates using threaded engines.'
	]).

	:- threaded.

	:- public(best_of/3).
	:- meta_predicate(best_of(*, 2, 0)).
	:- mode(best_of(?term, +callable, +callable), zero_or_one).
	:- info(best_of/3, [
		comment is 'Find the best solution for a goal given a comparator.',
		argnames is ['Answer', 'Comparator', 'Generator']
	]).

	:- public(engine_findall/3).
	:- meta_predicate(engine_findall(*, 0, *)).
	:- mode(engine_findall(@term, +callable, -list), one).
	:- info(engine_findall/3, [
		comment is 'A findall/3 implementation using threaded engines.',
		argnames is ['Template', 'Goal', 'List']
	]).

	best_of(Answer, Comparator, Generator) :-
		threaded_engine_create(Answer, Generator, E),
		efoldl(E, compare_answers(Comparator), no, Best),
		Answer = Best.

	:- meta_predicate(efoldl(*, 3, *, *)).
	efoldl(Engine, F, R1, R2):-
		threaded_engine_answer(Engine, X),
		!,
		call(F, R1, X, R),
		efoldl(Engine, F, R, R2).
	efoldl(_Engine, _F, R, R).

	:- meta_predicate(compare_answers(2, *, *, *)).
	compare_answers(Comparator, A1, A2, Best):-
		(	A1 \== no,
			call(Comparator, A1, A2) ->
			Best = A1
		;	Best = A2
		).

	engine_findall(Template, Goal, List) :-
		threaded_engine_create(Template, Goal, Tag),
		collect(Tag, List0),
		threaded_engine_stop(Tag),
		List = List0.

	collect(Tag, [X| Xs]) :-
		threaded_engine_answer(Tag, X),
		!,
		collect(Tag, Xs).
	collect(_, []).

:- end_object.

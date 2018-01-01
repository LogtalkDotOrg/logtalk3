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


:- object(emetas).

	:- info([
		version is 1.1,
		author is 'Paul Tarau and Paulo Moura',
		date is 2016/06/18,
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

	:- public(find_all/3).
	:- meta_predicate(find_all(*, 0, *)).
	:- mode(find_all(@term, +callable, -list), one).
	:- info(find_all/3, [
		comment is 'A findall/3 implementation using threaded engines.',
		argnames is ['Template', 'Goal', 'List']
	]).

	:- public(find_all_reified/3).
	:- meta_predicate(find_all_reified(*, 0, *)).
	:- mode(find_all_reified(@term, +callable, -list), one).
	:- info(find_all_reified/3, [
		comment is 'A findall/3 implementation using threaded engines but with reified answers.',
		argnames is ['Template', 'Goal', 'List']
	]).

	:- public(find_at_most/4).
	:- meta_predicate(find_at_most(*, *, 0, *)).
	:- mode(find_at_most(+integer, @term, +callable, -list), one).
	:- info(find_at_most/4, [
		comment is 'Similar to find_all/3 but finding at most N solutions.',
		argnames is ['N', 'Template', 'Goal', 'List']
	]).

	best_of(Answer, Comparator, Generator) :-
		threaded_engine_create(Answer, Generator, Engine),
		efoldl(Engine, compare_answers(Comparator), no, Best),
		Answer = Best.

	:- meta_predicate(efoldl(*, 3, *, *)).
	efoldl(Engine, F, R1, R2):-
		threaded_engine_next(Engine, X),
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

	find_all(Template, Goal, List) :-
		threaded_engine_create(Template, Goal, Engine),
		collect_all(Engine, List0),
		threaded_engine_destroy(Engine),
		List = List0.

	collect_all(Engine, [X| Xs]) :-
		threaded_engine_next(Engine, X),
		!,
		collect_all(Engine, Xs).
	collect_all(_, []).

	find_all_reified(Template, Goal, List) :-
		threaded_engine_create(Template, Goal, Engine),
		threaded_engine_next_reified(Engine, Answer),
		collect_all_reifeid(Answer, Engine, List0),
		threaded_engine_destroy(Engine),
		List = List0.

	collect_all_reifeid(no, _, []).
	collect_all_reifeid(the(X), Engine, [X| Xs]) :-
		threaded_engine_next_reified(Engine, Answer),
		collect_all_reifeid(Answer, Engine, Xs).

	find_at_most(N, Template, Goal, List) :-
		threaded_engine_create(Template, Goal, Engine),
		collect_at_most(N, Engine, List0),
		threaded_engine_destroy(Engine),
		List = List0.

	collect_at_most(N, Engine, [X| Xs]) :-
		N > 0,
		threaded_engine_next(Engine, X),
		!,
		M is N - 1,
		collect_at_most(M, Engine, Xs).
	collect_at_most(_, _, []).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- initialization(sorting::sort(compare, [3,1,2,6,4,8,0], _)).


:- object(sorting).

	:- public(sort/3).
	:- meta_predicate(sort(3, *, *)).

	:- meta_predicate(merge(3, *, *, *)).

	sort(_, [], []) :- !.
	sort(_, [X], [X]) :- !.
	sort(Closure, [X, Y| Xs], Ys) :-
		split([X, Y| Xs], X1s, X2s),
		sort(Closure, X1s, Y1s),
		sort(Closure, X2s, Y2s),
		merge(Closure, Y1s, Y2s, Ys).

	split([], [], []).
	split([X| Xs], [X| Ys], Zs) :-
		split(Xs, Zs, Ys).

	merge(Closure, [X| Xs], [Y| Ys], Zs) :- !,
		call(Closure, Order, X, Y),
		merge(Order, Closure, [X| Xs], [Y| Ys], Zs).
	merge(_, [], Xs, Xs) :- !.
	merge(_, Xs, [], Xs).

	merge(<, Closure, [X| Xs], [Y| Ys], [X| Zs]) :-
		merge(Closure, Xs, [Y| Ys], Zs).
	merge(=, Closure, [X| Xs], [Y| Ys], [X| Zs]) :-
		merge(Closure, Xs, [Y| Ys], Zs).
	merge(>, Closure, [X| Xs], [Y| Ys], [Y| Zs]) :-
		merge(Closure, [X | Xs], Ys, Zs).

:- end_object.

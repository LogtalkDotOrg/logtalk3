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


:- object(paths,
	imports(assumptions)).

	:- info([
		version is 1.0,
		author is 'Orginal example by Paul Tarau et al. Adapted to Logtalk by Paulo Moura.',
		date is 2014/06/26,
		comment is 'Find paths avoiding loops using linear assumptions.'
	]).

	:- public(init/0).
	init :-
		^^assumel(c(1,[2,3])),
		^^assumel(c(2,[1,4])),
		^^assumel(c(3,[1,5])),
		^^assumel(c(4,[1,5])).

	:- public(init/1).
	init([]).
	init([Start-Ends| Edges]) :-
		^^assumel(c(Start,Ends)),
		init(Edges).

	:- public(reset/0).
	reset :-
		retractall(c(_,_)).

	:- public(path/3).
	path(X, X, [X]).
	path(X, Z, [X| Xs]) :-
		linked(X, Y),
		path(Y, Z, Xs).

	linked(X, Y) :-
		c(X, Ys),
		member(Y, Ys).

	:- private(c/2).
	:- dynamic(c/2).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.

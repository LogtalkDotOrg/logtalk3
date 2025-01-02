%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(graph).

	:- info([
		version is 1:0:0,
		author is 'Theofrastos Mantadelis',
		date is 2010-11-14,
		comment is 'Example of a fully connected graph path search problem for benchmarking tests.'
	]).

	:- public(path/3).

	path(X, Y, L) :-
		path(X, Y, [X], L).

	path(X, Y, L, [Y| L]) :-
		\+ member(Y, L),
		edge(X, Y).
	path(X, Y, L, R) :-
		edge(X, Z),
		Z =\= Y,
		\+ member(Z, L),
		path(Z, Y, [Z| L], R).

	edge(X, Y) :-
		node(X),
		node(Y),
		X =\= Y.

	node(0).
	node(1).
	node(2).
	node(3).
	node(4).
%	node(5).
%	node(6).
%	node(7).
%	node(8).
%	node(9).
%	node(10).

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.

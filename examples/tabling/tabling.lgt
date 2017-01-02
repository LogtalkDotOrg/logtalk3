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


:- object(paths).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/05/14,
		comment is 'Simple tabling example using graph paths.',
		source is 'Direct conversion to Logtalk of a XSB tabling example.'
	]).

	:- public(path/2).
	:- table(path/2).

	path(X, Y) :- path(X, Z), edge(Z, Y).
	path(X, Y) :- edge(X, Y).

	edge(1, 2).
	edge(2, 2).
	edge(2, 4).
	edge(2, 3).
	edge(3, 5).

:- end_object.


:- object(fibonacci).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/05/28,
		comment is 'Simple tabling example using Fibonacci numbers.',
		source is 'Direct conversion to Logtalk of a B-Prolog tabling example.'
	]).

	:- public(fib/2).
	:- table(fib/2).

	fib(0, 1).
	fib(1, 1).
	fib(N,F) :-
		N > 1,
		N1 is N - 1,
		N2 is N - 2,
		fib(N1, F1),
		fib(N2, F2),
		F is F1 + F2.

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, yap)).

	:- set_prolog_flag(tabling_mode, local).

	:- object(mdt_paths_first).

		:- info([
			version is 1.0,
			author is 'Joao Santos and Ricardo Rocha. Adapted to Logtalk by Paulo Moura',
			date is 2012/10/20,
			comment is 'Simple tabling example using graph paths.',
			source is 'SLATE 2012 paper on mode directed tabling.'
		]).

		:- public(path/3).
		:- table(path(index, index, first)).

		path(X, Y, N) :- path(X, Z, N1), edge(Z, Y), N is N1 + 1.
		path(X, Y, 1) :- edge(X, Y).

		edge(a, b).
		edge(b, a).

	:- end_object.


	:- object(mdt_paths_min).

		:- info([
			version is 1.0,
			author is 'Joao Santos and Ricardo Rocha. Adapted to Logtalk by Paulo Moura',
			date is 2012/10/20,
			comment is 'Simple tabling example using graph paths.',
			source is 'SLATE 2012 paper on mode directed tabling.'
		]).

		:- public(path/3).
		:- table(path(index, index, min)).

		path(X, Y, C) :- path(X, Z, C1), edge(Z, Y, C2), C is C1 + C2.
		path(X, Y, C) :- edge(X, Y, C).

		edge(a, b, 1).
		edge(b, c, 1).
		edge(b, d, 4).
		edge(c, d, 1).

	:- end_object.


	:- object(mdt_paths_min_all).

		:- info([
			version is 1.0,
			author is 'Joao Santos and Ricardo Rocha. Adapted to Logtalk by Paulo Moura',
			date is 2012/10/20,
			comment is 'Simple tabling example using graph paths.',
			source is 'SLATE 2012 paper on mode directed tabling.'
		]).

		:- public(path/4).
		:- table(path(index, index, min, all)).

		path(X, Z, C, N) :- path(X, Y, C1, N1), edge(Y, Z, C2), C is C1 + C2, N is N1 + 1.
		path(X, Z, C, 1) :- edge(X, Z, C).

		edge(a, b, 2).
		edge(a, c, 1).
		edge(c, b, 1).

	:- end_object.

:- endif.

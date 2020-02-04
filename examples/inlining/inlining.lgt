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


% simple plain Prolog table for the inlining examples
t(1,  a,  'A').
t(2,  b,  'B').
t(3,  c,  'C').
t(4, 'A', 'A').
t(5, 'B', 'B').
t(6, 'C', 'C').


:- object(inlining).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2019-01-05,
		comment is 'Simple object for illustrating and testing inlining of predicate definitions.'
	]).

	:- public([
		integer/1, map/2, a/2, any/2, member/2
	]).

	:- uses(user, [
		t/3
	]).

	% the following clause defining the local predicate integer/1
	% simply calls the same predicate in "user"; although we use this
	% standard Prolog predicate here for example portability, in
	% practical cases these linking clauses are common when writing
	% portable code and when abstracting foreign library resources

	% the compiler generates a predicate definition table entry that
	% directly calls user::integer/1, thus inlining the predicate
	% definition, and discards the clause (assuming compilation with
	% the "optimize" flag turned on)

	integer(Term) :-
		user::integer(Term).

	% a common case occurs with meta-predicate definitions like map/N
	% that take a list on its second argument but uses a linking clause
	% to move the list to the first argument to take advantage of
	% first-argument indexing

	map(Closure, List) :-
		map_(List, Closure).

	map_(_, _).

	% another common case is to define accessors to a plain Prolog table
	% (see the uses/2 directive above)
	
	a(N, C) :-
		t(N, C, 'A').

	% but we cannot inline predicates when the body contains anonymous
	% variables as replacing calling the clause head by calling directly
	% the clause body would potentially break calls to the predicate
	% from bagof/3 and setof/3 goals

	any(N, C) :-
		t(N, C, _).

	% yet another common case is linking clauses that call Prolog module predicates

	:- if(current_logtalk_flag(modules, supported)).

		:- if(current_module(lists)).

			member(X, L) :-
				% use canonical syntax to avoid compilation errors when using
				% a backend Prolog compiler that does not support modules
				':'(lists, member(X, L)).

		:- endif.

	:- endif.

:- end_object.

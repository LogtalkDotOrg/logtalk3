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


% simple plain Prolog table for the inlining examples
a(1, a, 'A').
a(2, b, 'B').
a(3, c, 'C').


:- object(inlining).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/10/23,
		comment is 'Simple object for illustrating and testing inlining of predicate definitions.'
	]).

	:- public([
		integer/1, map/2, a/2
	]).

	:- uses(user, [
		a/3
	]).

	% the following clause defining the local predicate integer/1
	% simply calls the same predicate in "user"; although we use this
	% standard Prolog predicate here for example portability, in
	% practical cases these linking clauses are common when writing
	% portable code and when abstracting foreign library resources

	% the compiler generates a predicate definition table entry that
	% directly calls user::between/2, thus inlining the predicate
	% definition, and discars the clause (assuming compilation with
	% the "optimize" flag turned on)

	integer(Term) :-
		user::integer(Term).

	% a common case occurs with meta-predicate definitions like
	% map/N that take a list on its second argument but uses a linking
	% clause to move the list to the first argument to better exploit
	% indexing

	map(Closure, List) :-
		map_(List, Closure).

	map_(_, _).

	% another common case is to define accessors to a plain Prolog table
	
	a(N, C) :-
		a(N, C, _).

:- end_object.

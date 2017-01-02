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


:- object(tak(_Threads)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2011/04/03,
		comment is 'Takeuchi function (recursive arithmetic).',
		parameters is ['Threads' - 'Number of threads to use. Valid values are 1, 3, 9, 27, 81, etc.']
	]).

	:- threaded.

	:- public(tak/4).
	:- mode(tak(+integer, +integer, +integer, -integer), one).
	:- info(tak/4, [
		comment is 'Takeuchi function.',
		argnames is ['X', 'Y', 'Z', 'A']
	]).

	tak(X, Y, Z, A) :-
		parameter(1, Threads),
		Threads > 0,
		tak_mt(Threads, X, Y, Z, A).

	tak_mt(1, X, Y, Z, A) :-
		!,
		tak_st(X, Y, Z, A).
	tak_mt(_, X, Y, Z, A) :-
		X =< Y, !,
		Z = A.
	tak_mt(Threads, X, Y, Z, A) :-
		Threads3 is Threads//3,
		%X > Y,
		X1 is X - 1,
		Y1 is Y - 1,
		Z1 is Z - 1,
		threaded((
			tak_mt(Threads3, X1, Y, Z, A1),
			tak_mt(Threads3, Y1, Z, X, A2),
			tak_mt(Threads3, Z1, X, Y, A3)
		)),
		tak_st(A1, A2, A3, A).

	tak_st(X, Y, Z, A) :-
		X =< Y, !,
		Z = A.
	tak_st(X, Y, Z, A) :-
		%X > Y,
		X1 is X - 1,
		tak_st(X1, Y, Z, A1),
		Y1 is Y - 1,
		tak_st(Y1, Z, X, A2),
		Z1 is Z - 1,
		tak_st(Z1, X, Y, A3),
		tak_st(A1, A2, A3, A).

:- end_object.

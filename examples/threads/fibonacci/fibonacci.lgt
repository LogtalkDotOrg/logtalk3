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


:- object(fibonacci(_Threads)).

	:- info([
		version is 1.1,
		date is 2007/12/27,
		author is 'Paulo Moura',
		comment is 'Multi-threaded version of the computation of Fibonacci numbers.',
		parameters is ['Threads' - 'Number of threads to use. Valid values are 1, 2, 4, 8, 16, etc.']
	]).

	:- threaded.

	:- public(fib/2).
	:- mode(fib(+integer, -integer), one).
	:- info(fib/2, [
		comment is 'Calculates the Nth Fibonacci number.',
		argnames is ['Nth', 'Number']
	]).

	fib(N, F) :-
		parameter(1, Threads),
		mt_fib(Threads, N, F),
		!.

	mt_fib(_, 0, 1) :- !.
	mt_fib(_, 1, 1) :- !.
	mt_fib(1, N, F) :- !,
		st_fib(N, F).
	mt_fib(Threads, N, F) :-
		Threads > 1,
		Threads2 is Threads//2,
		N > 1,
		N1 is N - 1,
		N2 is N - 2,
		threaded((
			mt_fib(Threads2, N1, F1),
			mt_fib(Threads2, N2, F2)
		)),
		F is F1 + F2.

	st_fib(0, 1) :- !.
	st_fib(1, 1) :- !.
	st_fib(N, F) :-
		N > 1,
		N1 is N - 1,
		N2 is N - 2,
		st_fib(N1, F1),
		st_fib(N2, F2),
		F is F1 + F2.

:- end_object.

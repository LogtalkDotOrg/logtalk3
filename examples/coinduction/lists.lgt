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


:- object(lists).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2012/09/11,
		comment is 'Coinductive infinite list predicates example.'
	]).

	:- public(append/3).
	:- coinductive(append/3).

	append([], X, X).
	append([H| T], Y, [H| Z]) :-
		append(T, Y, Z).

	% Are there "occurrences" of arg1 in arg2?
	:- public(member/2).
	:- coinductive(member/2).

	member(X, [X| _]).
	member(X, [_| T]) :-
		member(X, T).

	% Are there infinitely many "occurrences" of arg1 in arg2?
	:- public(comember/2).
	:- coinductive(comember/2).

	% definition contributed by Davide Ancona
	comember(X, [_| T]) :-
		comember(X, T).

%	% alternative definition by Gopal et al; requires tabling of rational terms
%	comember(X, L) :-
%		drop(X, L, L1),
%		comember(X, L1).
%
%	% Drop some prefix of arg2 upto an "occurrence" of arg1 from arg2,
%	% yielding arg3.
%	% ("Occurrence" of X = something unifiable with X.)
%	%:- table(drop/3).	% not working; needs tabling supporting cyclic terms!
%	drop(H, [H| T], T).
%	drop(H, [_| T], T1) :-
%		drop(H, T, T1).

	:- public(absent/2).
	:- coinductive(absent/2).

	absent(X, [Y| T]) :-
		X \= Y,
		absent(X, T).

	% see the CO-LP 2012 paper by Davide Ancona and Elena Zucca
	% for the idea behind this hook predicate
	coinductive_success_hook(member(_, _)) :-
		fail.
	coinductive_success_hook(comember(X, L)) :-
		member(X, L).

:- end_object.


/* Example queries:

:- initialization((
	write('Query1'), nl,
	X = [0, s(0), s(s(0))],
	member2(s(0), X),
	write('Yes1 !'), nl
)).

:- initialization((
	write('Query2'), nl,
	X = [0, s(0), s(s(0))],
	member1(s( 0), X),
	write('Yes2 !'), nl
)).

:- initialization((
	(	write('Query3'), nl,
	X = [0, s(0), s(s(0))],
	comember(s(0), X) ->
	write('WHAT? SHOULD HAVE FAILED !'), nl
	;	true
	)
)).

:- initialization((
	write('Query4'), nl,
	X = [0, s(0), s(s(0))| X], write(' HERE!'),
	once(comember(s(0), X)),
	write('Yes4 !'), nl,
	write_term(X, [max_depth(10)]), nl
)).

*/

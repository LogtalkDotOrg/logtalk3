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


% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% written by Markus Triska (August 2008)


% Written 2006 by Markus Triska triska@gmx.at
% Public domain code.

/*
    A   B   C
  D   E   F   G
H   I   J   K   L
  M   N   O   P
    Q   R   S
*/

:- object(hexagon).

	:- use_module(library(clpfd), [all_different/1, ins/2, labeling/2, sum/3, (#=)/2]).
	:- uses(meta, [succeeds/2::maplist/2]).

	:- public(mhex/1).

	sum38(Vs) :- sum(Vs, #=, 38).

	mhex(Vs) :-
		Vs = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
		Vs ins 1..19,
		all_different(Vs),
		maplist(sum38, [
			[A,B,C], [D,E,F,G], [H,I,J,K,L], [M,N,O,P], [Q,R,S],
			[H,D,A], [M,I,E,B], [Q,N,J,F,C], [R,O,K,G], [S,P,L],
			[C,G,L], [B,F,K,P], [A,E,J,O,S], [D,I,N,R], [H,M,Q]
		]),
		labeling([ff], Vs).

	%?- mhex(Vs).
	%@ Vs = [3, 17, 18, 19, 7, 1, 11, 16, 2, 5, 6, 9, 12, 4, 8, 14, 10, 13, 15] ;
	%@ Vs = [3, 19, 16, 17, 7, 2, 12, 18, 1, 5, 4, 10, 11, 6, 8, 13, 9, 14, 15] ;
	%@ Vs = [9, 11, 18, 14, 6, 1, 17, 15, 8, 5, 7, 3, 13, 4, 2, 19, 10, 12, 16] ;
	%@ etc.

:- end_object.

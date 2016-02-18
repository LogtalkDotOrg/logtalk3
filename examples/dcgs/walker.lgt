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


:- object(walker).

	:- info([
		version is 1.0,
		date is 2004/4/29,
		author is 'Paulo Moura',
		comment is 'Walker movements.'
	]).

	:- public(walk/2).
	:- mode(walk(@list, -position), one).
	:- info(walk/2, [
		comment is 'Parses a sequence of walker moves, returning ending position.',
		argnames is ['Moves', 'Ending']
	]).

	walk(Moves, Ending) :-
		phrase(walk(Ending), Moves).

	walk(Ending) -->
		moves((0, 0), Ending).

	moves(Start, Ending) -->
		move(Start, Temp), moves(Temp, Ending). 
	moves(Ending, Ending) -->
		[]. 

	move((X0, Y0), (X, Y)) --> [ n(S)], {X is X0, Y is Y0 + S}.
	move((X0, Y0), (X, Y)) --> [ne(S)], {X is X0 + S / sqrt(2), Y is Y0 + S / sqrt(2)}.
	move((X0, Y0), (X, Y)) --> [ e(S)], {X is X0 + S, Y = Y0}.
	move((X0, Y0), (X, Y)) --> [se(S)], {X is X0 + S / sqrt(2), Y is Y0 - S / sqrt(2)}.
	move((X0, Y0), (X, Y)) --> [ s(S)], {X is X0, Y is Y0 - S}.
	move((X0, Y0), (X, Y)) --> [sw(S)], {X is X0 - S / sqrt(2), Y is Y0 - S / sqrt(2)}.
	move((X0, Y0), (X, Y)) --> [ w(S)], {X is X0 - S, Y = Y0}.
	move((X0, Y0), (X, Y)) --> [nw(S)], {X is X0 - S / sqrt(2), Y is Y0 + S / sqrt(2)}.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


/*
The description of this puzzle is easily found on the web. For example:

http://www.csci.csusb.edu/dick/cs320/prolog/Potions.htm
*/


:- object(potions).

	:- info([
		version is 1.2,
		date is 2006/3/26,
		author is 'Paulo Moura',
		comment is 'Harry Potter potions logical puzzle.'
	]).

	:- uses(list, [select/3]).

	:- public(potions/7).
	:- mode(potions(?atom, ?atom, ?atom, ?atom, ?atom, ?atom, ?atom), zero_or_one).
	:- info(potions/7, [
		comment is 'Contents of the seven potions.',
		argnames is ['P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7']
	]).

	contents([wine, wine, poison, poison, poison, forward, backwards]).

	potions(P1, P2, P3, P4, P5, P6, P7) :-
		contents(H1),
		select(P1, H1, H2),
		select(P7, H2, H3),
		P1 \== P7, P1 \== forward, P7 \== forward,				% second clue
		select(P2, H3, H4),
		P2 \== poison,
		select(P3, H4, H5),
		P3 \== poison,											% third clue
		select(P6, H5, H6),										% fourth clue
		P2 == P6,
		select(P4, H6, H7),
		select(P5, H7, []),
		two_pairs_poison_wine([P1, P2, P3, P4, P5, P6, P7]).	% first clue

	two_pairs_poison_wine(S) :-
		poison_wine_pair(S, R),
		poison_wine_pair(R, _).

	poison_wine_pair([poison, wine| R], R) :-
		!.
	poison_wine_pair([_| L], R) :-
		poison_wine_pair(L, R).

:- end_object.

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


/* Houses logical puzzle: who owns the zebra and who drinks water?

	 1) Five colored houses in a row, each with an owner, a pet, cigarettes, and a drink.
	 2) The English lives in the red house. 
	 3) The Spanish has a dog. 
	 4) They drink coffee in the green house. 
	 5) The Ukrainian drinks tea. 
	 6) The green house is next to the white house. 
	 7) The Winston smoker has a serpent. 
	 8) In the yellow house they smoke Kool. 
	 9) In the middle house they drink milk.
	10) The Norwegian lives in the first house from the left. 
	11) The Chesterfield smoker lives near the man with the fox. 
	12) In the house near the house with the horse they smoke Kool. 
	13) The Lucky Strike smoker drinks juice. 
	14) The Japanese smokes Kent. 
	15) The Norwegian lives near the blue house. 

Who owns the zebra and who drinks water?
*/

:- object(houses).

	:- info([
		version is 1.1,
		date is 2014/09/27,
		author is 'Paulo Moura and Markus Triska',
		comment is 'Houses logical puzzle']).

	:- public(houses/1).
	:- mode(houses(-list), one).
	:- info(houses/1, [
		comment is 'Solution to the puzzle satisfying all constraints.',
		argnames is ['Solution']
	]).

	:- public(zebra_owner/1).
	:- mode(zebra_owner(?atom), zero_or_one).
	:- info(zebra_owner/1, [
		comment is 'Zebra owner.',
		argnames is ['Owner']
	]).

	:- public(water_drinker/1).
	:- mode(water_drinker(?atom), zero_or_one).
	:- info(water_drinker/1, [
		comment is 'Water drinker.',
		argnames is ['Drinker']
	]).

	:- public(print/1).
	:- mode(print(+list), one).
	:- info(print/1, [
		comment is 'Pretty print solution to the puzzle.',
		argnames is ['Solution']
	]).

	houses(Solution) :-
		template(Solution),                                                 %  1
		member(h(english, _, _, _, red), Solution),                         %  2
		member(h(spanish, dog, _, _, _), Solution),                         %  3
		member(h(_, _, _, coffee, green), Solution),                        %  4
		member(h(ukrainian, _, _, tea, _), Solution),                       %  5 
		near(h(_, _, _, _, green), h(_, _, _, _, white), Solution),         %  6
		member(h(_, snake, winston, _, _), Solution),                       %  7
		member(h(_, _, kool, _, yellow), Solution),                         %  8
		Solution = [_, _, h(_, _, _, milk, _), _, _],                       %  9
		Solution = [h(norwegian, _, _, _, _)| _],                           % 10
		near(h(_, fox, _, _, _), h(_, _, chesterfield, _, _), Solution),    % 11
		near(h(_, _, kool, _, _), h(_, horse, _, _, _), Solution),          % 12
		member(h(_, _, lucky, juice, _), Solution),                         % 13
		member(h(japonese, _, kent, _, _), Solution),                       % 14
		near(h(norwegian, _, _, _, _), h(_, _, _, _, blue), Solution),      % 15
		member(h(_, _, _, water, _), Solution),  	% one of them drinks water
		member(h(_, zebra, _, _, _), Solution).  	% one of them owns a zebra

	zebra_owner(Owner) :-
		houses(Solution),
		member(h(Owner,zebra,_,_,_), Solution).

	water_drinker(Drinker) :-
		houses(Solution),
		member(h(Drinker,_,_,water,_), Solution).

	print([]).
	print([House| Houses]) :-
		write(House), nl,
		print(Houses).

	% auxiliary predicates

	% h(Nationality, Pet, Cigarette, Drink, Color)
	template([h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _)]).

	member(A, [A, _, _, _, _]).
	member(B, [_, B, _, _, _]).
	member(C, [_, _, C, _, _]).
	member(D, [_, _, _, D, _]).
	member(E, [_, _, _, _, E]).

	near(A, B, Solution) :-
		next(A, B, Solution).
	near(A, B, Solution) :-
		next(B, A, Solution).

	next(A, B, [A, B, _, _, _]).
	next(B, C, [_, B, C, _, _]).
	next(C, D, [_, _, C, D, _]).
	next(D, E, [_, _, _, D, E]).

:- end_object.

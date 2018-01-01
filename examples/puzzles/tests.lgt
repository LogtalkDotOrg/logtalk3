%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "puzzles" example.'
	]).

	test(puzzles_1) :-
		potions::potions(P1, P2, P3, P4, P5, P6, P7),
		P1 == poison,
		P2 == wine,
		P3 == forward,
		P4 == poison,
		P5 == poison,
		P6 == wine,
		P7 == backwards.

	test(puzzles_2) :-
		horses::horses(S),
		S == [h(doc,gelding,chestnut,april,1),h(danny,gelding,bay,sue,2),h(gopher,gelding,gray,doc,3),h(april,mare,white,danny,4),h(sue,mare,black,gopher,5)].

	test(puzzles_3) :-
		jam_thief::thief(Thief),
		Thief == hare.

	test(puzzles_4) :-
		jam_thief::thief(Thief, Why),
		Thief == hare,
		Why == [trusty(dormouse),liar(hare),trusty(hatter)].

	test(puzzles_5) :-
		houses::houses(S),
		S == [h(norwegian,fox,kool,water,yellow),h(ukrainian,horse,chesterfield,tea,blue),h(english,snake,winston,milk,red),h(japonese,zebra,kent,coffee,green),h(spanish,dog,lucky,juice,white)].

	test(puzzles_6) :-
		note::students(S),
		S == [s(mary,english,red,1),s(paul,math,yellow,2),s(josephine,science,green,3),s(derrick,french,blue,4),s(alexis,reading,black,5)].

	test(puzzles_7) :-
		camp_swampy::beds(S),
		S == [b(tim,thomas,maine,1),b(sam,franklin,north_carolina,2),b(mac,miller,virginia,3),b(fred,james,florida,4),b(john,smith,arkansas,5)].

:- end_object.

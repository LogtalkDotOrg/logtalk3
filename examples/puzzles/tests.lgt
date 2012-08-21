%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "puzzles" example.']).

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

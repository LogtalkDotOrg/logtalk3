%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/02,
		comment is 'Unit tests for the "recipes" example.'
	]).

	test(recipes_1) :-
		green_soup::ingredients(Ingredients),
		Ingredients == [peas,cream,water,oil].

	test(recipes_2) :-
		mashed_peas::steps(Steps),
		Steps == [1-'Boil the peas.',2-'Mash the peas',3-'Add salt and mix.'].

	test(recipes_3) :-
		setof(Recipe, (conforms_to_protocol(Recipe, recipep), Recipe::ingredient(peas)), Recipes),
		Recipes == [green_soup,mashed_peas].

	test(recipes_4) :-
		green_soup::assertz(level(hard)),
		green_soup::level(Level),
		Level == hard.
		
	test(recipes_5) :-
		mashed_peas::level(Level),
		Level == easy.

:- end_object.

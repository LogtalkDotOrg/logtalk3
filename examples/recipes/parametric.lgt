%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% some recipes represented as a table of Prolog facts

recipe(
	'Chocolate Chip Cookies', [
		flour-500-gr,
		butter-20-gr,
		chocolate-200-gr,
		sugar-65-gr,
		eggs-2-units
	], [
		'mix flour, butter and sugar'-10-min,
		'add eggs and mix'-10-min,
		'make chocolate chips'-5-min,
		'split in small portions'-5-min,
		'cook in the oven'-25-min
	]
).
recipe(
	'Berries and cream', [
		cream-500-ml,
		sugar-50-gr,
		strawberries-300-gr, 
		chocolate-100-gr
	], [
		'mix whipping cream add sugar'-5-min,
		'slice strawberries'-5-min,
		'place alternate layers of cream and strawberries in dessert dishes'-10-min,
		'make chocolate chips'-5-min,
		'top dishes with chocolate chips'-5-min
	]
).


% a parametric object to bridge the fact and the object representations

:- object(recipe(_Name,_Ingredients,_Steps),
	implements(recipep),
	extends(proto_recipe)).

	:- uses(list, [
		member/2, nth1/3
	]).

	name(Name) :-
		parameter(1, Name).

	ingredient(Ingredient, Quantity, Units) :-
		parameter(2, Ingredients),
		member(Ingredient-Quantity-Units, Ingredients).

	step(Order, Step, StepTime) :-
		parameter(3, Steps),
		nth1(Order, Steps, Step-StepTime-_).

:- end_object.


% define an abstraction predicate to enumerate recipes

recipe(Recipe) :-
	conforms_to_protocol(Recipe, recipep),
	(	atom(Recipe) ->
		true
	;	call(Recipe)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% the recipe descriptors are specified in the following protocol

:- protocol(recipep).

    :- public([
        name/1, ingredient/3, step/3
    ]).

:- end_protocol.


% next, we define a prototypical recipe object, which implements the recipe
% protocol and adds some convinient predicates

:- object(proto_recipe).

	% allow this object to be (hot) patched
	:- set_logtalk_flag(complements, allow).

    :- public([
        ingredient/1, ingredients/1, steps/1,
		cooking_time/1
    ]).

    ingredient(Ingredient) :-
        ::ingredient(Ingredient,_,_).

    ingredients(Ingredients) :-
        findall(Ingredient, ::ingredient(Ingredient,_,_), Ingredients).

    steps(Steps) :-
        findall(Order-Step, ::step(Order,Step,_), Steps).

	cooking_time(CookingTime) :-
		findall(StepTime, ::step(_,_,StepTime), StepTimes),
		sum(StepTimes, CookingTime).

	% auxiliary predicates

	sum(Numbers, Total) :-
		sum(Numbers, 0, Total).

	sum([], Total, Total).
	sum([Number| Numbers], Total0, Total) :-
		Total1 is Total0 + Number,
		sum(Numbers, Total1, Total).

:- end_object.

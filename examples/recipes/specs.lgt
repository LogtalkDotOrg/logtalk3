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

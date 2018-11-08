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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the flyweight design pattern:
%
% https://en.wikipedia.org/wiki/Flyweight_pattern


% our flyweight object stores cheese brands and their correspoding unit
% cost, which we assume are the same for all shops selling cheese 

:- object(cheese_brands).

	:- public(brand/2).
	brand(Brand, Cost) :-
		brand_(Brand, Cost).

	:- public(add_brand/2).
	add_brand(Brand, Cost) :-
		retractall(brand_(Brand, _)),
		assertz(brand_(Brand, Cost)).

	:- private(brand_/2).
	:- dynamic(brand_/2).

:- end_object.


% the cheese shops only store information about units sold per cheese
% brand while the available brands and correspoding costs are stored
% and shared using the cheese_brands flyweight object

:- object(cheese_shop).

	:- uses(numberlist, [sum/2]).

	:- public(stock_cheese/2).
	stock_cheese(Brand, Cost) :-
		cheese_brands::add_brand(Brand, Cost).

	:- public(cheese/2).
	cheese(Brand, Cost) :-
		cheese_brands::brand(Brand, Cost).

	:- public(sell_cheese/2).
	sell_cheese(Brand, Units) :-
		(	::retract(order_(Brand, UnitsSofar)) ->
			true
		;	UnitsSofar is 0	
		),
		UpdatedUnits is UnitsSofar + Units,
		::assertz(order_(Brand, UpdatedUnits)).

	:- public(total_units_sold/1).
	total_units_sold(Total) :-
		findall(Units, ::order_(_, Units), List),
		sum(List, Total).

	:- public(total_income/1).
	total_income(Total) :-
		findall(Brand-Units, ::order_(Brand, Units), List),
		total_income(List, 0, Total).

	total_income([], Total, Total).
	total_income([Brand-Units| Tail], Total0, Total) :-
		cheese_brands::brand(Brand, Cost),
		Total1 is Total0 + Units * Cost,
		total_income(Tail, Total1, Total).

	:- private(order_/2).
	:- dynamic(order_/2).

:- end_object.


% we define two shops for testing our implementation

:- object(shop1,
	extends(cheese_shop)).

:- end_object.


:- object(shop2,
	extends(cheese_shop)).

:- end_object.

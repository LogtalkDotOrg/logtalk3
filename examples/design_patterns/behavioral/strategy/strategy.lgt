%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
% page on the Strategy design pattern:
%
% https://en.wikipedia.org/wiki/Strategy_pattern


:- object(customer).

	% a simple predicate to dynamically create new
	% customers with a given initial strategy
	:- public(new/2).
	new(Customer, Strategy) :-
		self(Self),
		create_object(Customer, [extends(Self)], [], [strategy_(Strategy)]).

	% customer drinks consumption, whose price
	% depends on the currently chosen strategy
	:- public(add/2).
	add(Price, Quantity) :-
		::strategy_(Strategy),
		Strategy::get_actual_price(Price * Quantity, ActualPrice),
		::assertz(drinks_(ActualPrice)).

	% compute and print customer bill
	:- public(print_bill/0).
	print_bill :-
		findall(Price, ::drinks_(Price), Prices),
		numberlist::sum(Prices, Total),
		write('Total due: '), write(Total), nl.

	% set a new strategy going forward
	:- public(set_strategy/1).
	set_strategy(Strategy) :-
		::retractall(strategy_(_)),
		::assertz(strategy_(Strategy)).

	% customer state 

	:- private(drinks_/1).
	:- dynamic(drinks_/1).

	:- private(strategy_/1).
	:- dynamic(strategy_/1).

:- end_object.


% define a common protocol for the strategies

:- protocol(billing_strategy).

	:- public(get_actual_price/2).

:- end_protocol.


% define a couple of strategies

:- object(normal_strategy,
	implements(billing_strategy)).

	get_actual_price(Price, ActualPrice) :-
		% full price
		ActualPrice is Price.

:- end_object.


:- object(happy_hour_strategy,
	implements(billing_strategy)).

	get_actual_price(Price, ActualPrice) :-
		% 50% discount
		ActualPrice is Price * 0.5.

:- end_object.

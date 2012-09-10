%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% The viral marketing example consists of a social network of friend relations. You have to decide which persons to market. Sending marketing has a cost of 2, but might cause people to buy your product, giving you a profit of 5. When someone buys the product, it becomes more likely that his friends also buy the product.

:- object(vm,
	imports(problog_decision_theory)).

	% Decisions
	(?) ~ marketed(P) :- person(P).

	% Utility attributes
	buys(P) => 5 :- person(P).
	marketed(P) => -2 :- person(P).

	% Probabilistic facts
	0.2 ~ buy_from_marketing(_).
	0.3 ~ buy_from_trust(_,_).

	% Background knowledge
	person(bernd).
	person(ingo).
	person(theo).
	person(angelika).
	person(guy).
	person(martijn).
	person(laura).
	person(kurt).

	trusts(X,Y) :- trusts_directed(X,Y).
	trusts(X,Y) :- trusts_directed(Y,X).

	trusts_directed(bernd,ingo).
	trusts_directed(ingo,theo).
	trusts_directed(theo,angelika).
	trusts_directed(bernd,martijn).
	trusts_directed(ingo,martijn).
	trusts_directed(martijn,guy).
	trusts_directed(guy,theo).
	trusts_directed(guy,angelika).
	trusts_directed(laura,ingo).
	trusts_directed(laura,theo).
	trusts_directed(laura,guy).
	trusts_directed(laura,martijn).
	trusts_directed(kurt,bernd).

	buys(X) :- buys(X,[X]).

	buys(X, _) :-
	        marketed(X),
	        buy_from_marketing(X).
	buys(X, Visited) :-
	        trusts(X,Y),
	        buy_from_trust(X,Y),
	        absent(Y,Visited),
	        buys(Y, [Y|Visited]).

	absent(_,[]).
	absent(X,[Y|Z]) :-X \= Y, absent(X,Z).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLE USE::
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Find the globally optimal strategy.
%
% ?- vm::dtproblog_solve(Strategy,ExpectedValue).
% ExpectedValue = 3.21097,
% Strategy = [marketed(martijn),marketed(guy),marketed(theo),marketed(ingo)]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute the expected value for a given strategy.
%
% ?- vm::dtproblog_ev([marketed(martijn),marketed(laura)],ExpectedValue).
% ExpectedValue = 2.35771065
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Find a locally optimal strategy.
%
% ?- flags:set_problog_flag(optimization, local), vm::dtproblog_solve(Strategy,ExpectedValue).
% ExpectedValue = 3.19528,
% Strategy = [marketed(martijn),marketed(laura),marketed(guy),marketed(ingo)]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Find all ground utility facts in the theory.
%
% ?- vm::dtproblog_utility_facts(Facts).
% Facts = [buys(bernd)=>5, buys(ingo)=>5, buys(theo)=>5, buys(angelika)=>5, buys(guy)=>5, buys(martijn)=>5, buys(laura)=>5, buys(kurt)=>5, marketed(bernd)=> -2, marketed(ingo)=> -2, marketed(theo)=> -2, marketed(angelika)=> -2, marketed(guy)=> -2, marketed(martijn)=> -2, marketed(laura)=> -2, marketed(kurt)=> -2]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Find all ground decisions relevant to the utility attributes.
%
% ?- vm::dtproblog_decisions(Decisions).
% Decisions = [marketed(angelika), marketed(theo), marketed(kurt), marketed(ingo), marketed(laura), marketed(martijn), marketed(guy), marketed(bernd)]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Set the inference method to K-best to limit the complexity. This means that only the K most likely proofs for each utility attribute are considered as an underestimate of the probabilities and utilities. In the viral marketing example, this means that the probability that someone buys the product only depends on a limited number of other people in the social network,  regardless of the size of the social network.
% Finding the globally optimal strategy under these simplifying assumptions yields a good but suboptimal strategy.
%
% ?- flags:set_problog_flag(inference,20-best), vm::dtproblog_solve(Strategy,ExpectedValue).
% ExpectedValue = 2.62531,
% Strategy = [marketed(martijn),marketed(guy),marketed(ingo),marketed(laura)]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The expected value returned in the previous example is an underestimate of the real expected value of the strategy found, which can be computed as
%
% ?- flags:set_problog_flag(inference,exact), vm::dtproblog_ev([marketed(martijn), marketed(guy), marketed(ingo), marketed(laura)], ExpectedValue).
% ExpectedValue = 3.1952798
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

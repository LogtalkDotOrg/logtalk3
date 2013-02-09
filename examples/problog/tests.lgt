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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2010/11/09,
		comment is 'Unit tests for the "problog" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	:- if(current_object(graph)).

	% explanation probability (and facts involved)
	test(problog_graph_1) :-
		graph::problog_max(path(1,4), Prob, FactsUsed),
		FactsUsed == [dir_edge(1,2),dir_edge(2,3),dir_edge(3,4)],
		Prob =~= 0.432.

	% success probability
	test(problog_graph_2) :-
		graph::problog_exact(path(1,4), Prob, Status),
		% 8 proofs
		Prob =~= 0.53864,
		Status == ok.

	% lower bound using 4 best proofs
	test(problog_graph_3) :-
		graph::problog_kbest(path(1,4),4, Prob, Status),
		% 4 proofs
		Prob =~= 0.517344,
		Status == ok.

	% approximation using monte carlo, to reach 95%-confidence interval width 0.01
	test(problog_graph_4) :-
		graph::problog_montecarlo(path(1,4), 0.01, Prob),
		abs(Prob - 0.536475) =< 0.05.

	% upper and lower bound using iterative deepening, final interval width 0.01
	test(problog_graph_5) :-
		graph::problog_delta(path(1,4), 0.01, Bound_low, Bound_up, Status),
		Bound_low =~= 0.5354096,
		Bound_up =~= 0.53864,
		Status == ok.

	% upper and lower bound obtained cutting the sld tree at probability 0.1 for each branch
	test(problog_graph_6) :-
		graph::problog_threshold(path(1,4),0.1,Bound_low,Bound_up,Status),
		% 4 proofs
		Bound_low =~= 0.517344,
		Bound_up =~= 0.563728,
		Status == ok.

	% lower bound obtained cutting the sld tree at probability 0.2 for each branch
	test(problog_graph_7) :-
		graph::problog_low(path(1,4),0.2,Bound_low,Status),
		% 1 proofs
		Bound_low =~= 0.432,
		Status == ok.

	:- endif.

	:- if(current_object(office)).

	test(problog_office_1) :-
		office::problog_exact(room_has_window, Prob, Status),
		Prob =~= 0.01517076,
		Status == ok.

	:- endif.

	:- if(current_object(vm)).

	test(problog_vm_1) :-
		vm::dtproblog_solve(Strategy,ExpectedValue),
		ExpectedValue =~= 3.21097,
		Strategy == [marketed(martijn),marketed(guy),marketed(theo),marketed(ingo)].

	test(problog_vm_2) :-
		vm::dtproblog_ev([marketed(martijn),marketed(laura)],ExpectedValue),
		ExpectedValue =~= 2.35771065.

	test(problog_vm_3) :-
		flags:set_problog_flag(optimization, local),
		vm::dtproblog_solve(Strategy,ExpectedValue),
		flags:set_problog_flag(optimization, global),
		ExpectedValue =~= 3.19528,
		Strategy == [marketed(martijn),marketed(laura),marketed(guy),marketed(ingo)].

	test(problog_vm_4) :-
		vm::dtproblog_utility_facts(Facts),
		Facts == [buys(bernd)=>5,buys(ingo)=>5,buys(theo)=>5,buys(angelika)=>5,buys(guy)=>5,buys(martijn)=>5,buys(laura)=>5,buys(kurt)=>5,marketed(bernd)=> -2,marketed(ingo)=> -2,marketed(theo)=> -2,marketed(angelika)=> -2,marketed(guy)=> -2,marketed(martijn)=> -2,marketed(laura)=> -2,marketed(kurt)=> -2].

	test(problog_vm_5) :-
		vm::dtproblog_decisions(Decisions),
		Decisions == [marketed(angelika),marketed(theo),marketed(kurt),marketed(ingo),marketed(laura),marketed(martijn),marketed(guy),marketed(bernd)].

	test(problog_vm_6) :-
		flags:set_problog_flag(inference,20-best), vm::dtproblog_solve(Strategy,ExpectedValue),
		ExpectedValue =~= 2.62531,
		Strategy = [marketed(martijn),marketed(guy),marketed(ingo),marketed(laura)].

	test(problog_vm_7) :-
		flags:set_problog_flag(inference,exact), vm::dtproblog_ev([marketed(martijn), marketed(guy), marketed(ingo), marketed(laura)], ExpectedValue),
		ExpectedValue =~= 3.1952798.

	:- endif.

	:- if(current_object(graph_tabled)).

	% success probability
	test(problog_graph_tabled_1) :-
		graph_tabled::problog_exact(path(1,4),Prob,Status),
		Prob =~= 0.53864,
		Status == ok.

	% approximation using monte carlo, to reach 95%-confidence interval width 0.01
	test(problog_graph_tabled_2) :-
		graph_tabled::problog_montecarlo(path(1,4),0.01,Prob),
		abs(Prob - 0.537525) =< 0.05.

	% success probability of negation
	test(problog_graph_tabled_3) :-
		graph_tabled::problog_exact(problog_neg(path(1,4)),Prob,Status),
		Prob =~= 0.46136,
		Status == ok.

	:- endif.

	:- if(current_object(vmt)).

	test(problog_vmt_1) :-
		vmt::dtproblog_solve(Strategy,ExpectedValue),
		ExpectedValue =~= 3.21097,
		Strategy == [marketed(martijn),marketed(guy),marketed(theo),marketed(ingo)].

	% Compute the expected value for a given strategy.
	test(problog_vmt_2) :-
		vmt::dtproblog_ev([marketed(martijn),marketed(laura)],ExpectedValue),
		ExpectedValue =~= 2.35771065.

	% Find a locally optimal strategy.
	test(problog_vmt_3) :-
		flags:set_problog_flag(optimization, local), vmt::dtproblog_solve(Strategy,ExpectedValue),
		ExpectedValue =~= 3.19528,
		Strategy == [marketed(martijn),marketed(laura),marketed(guy),marketed(ingo)].

	% Find all ground utility facts in the theory.
	test(problog_vmt_4) :-
		vmt::dtproblog_utility_facts(Facts),
		Facts == [buys(bernd)=>5, buys(ingo)=>5, buys(theo)=>5, buys(angelika)=>5, buys(guy)=>5, buys(martijn)=>5, buys(laura)=>5, buys(kurt)=>5, marketed(bernd)=> -2, marketed(ingo)=> -2, marketed(theo)=> -2, marketed(angelika)=> -2, marketed(guy)=> -2, marketed(martijn)=> -2, marketed(laura)=> -2, marketed(kurt)=> -2].

	% Find all ground decisions relevant to the utility attributes.
	test(problog_vmt_5) :-
		vmt::dtproblog_decisions(Decisions),
		Decisions == [marketed(angelika), marketed(theo), marketed(kurt), marketed(ingo), marketed(laura), marketed(martijn), marketed(guy), marketed(bernd)].

	:- endif.

:- end_object.

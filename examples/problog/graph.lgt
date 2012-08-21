%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ProbLog program describing a probabilistic graph
% (running example from ProbLog presentations)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(graph,
	imports(problog_inference)).

	:- public(path/2).

	%%%%
	% background knowledge
	%%%%
	% definition of acyclic path using list of visited nodes
	path(X,Y) :- path(X,Y,[X],_).

	path(X,X,A,A).
	path(X,Y,A,R) :-
		X\==Y,
		edge(X,Z),
		absent(Z,A),
		path(Z,Y,[Z|A],R).

	% using directed edges in both directions
	edge(X,Y) :- dir_edge(Y,X).
	edge(X,Y) :- dir_edge(X,Y).

	% checking whether node hasn't been visited before
	absent(_,[]).
	absent(X,[Y|Z]):-X \= Y, absent(X,Z).

	%%%%
	% probabilistic facts
	%%%%
	0.9 ~ dir_edge(1,2).
	0.8 ~ dir_edge(2,3).
	0.6 ~ dir_edge(3,4).
	0.7 ~ dir_edge(1,6).
	0.5 ~ dir_edge(2,6).
	0.4 ~ dir_edge(6,5).
	0.7 ~ dir_edge(5,3).
	0.2 ~ dir_edge(5,4).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% example queries about path(1,4)
%
%%% explanation probability (and facts involved)
%     ?- graph::problog_max(path(1,4),Prob,FactsUsed).
%  FactsUsed = [dir_edge(1,2),dir_edge(2,3),dir_edge(3,4)],
%  Prob = 0.432 ?
%  yes
%%% success probability
%     ?- graph::problog_exact(path(1,4),Prob,Status).
%  8 proofs
%  Prob = 0.53864,
%  Status = ok ?
%  yes
%%% lower bound using 4 best proofs
%     ?- graph::problog_kbest(path(1,4),4,Prob,Status).
%  4 proofs
%  Prob = 0.517344,
%  Status = ok ?
%  yes
%%% approximation using monte carlo, to reach 95%-confidence interval width 0.01
%     ?- graph::problog_montecarlo(path(1,4),0.01,Prob).
%  Prob = 0.537525 ?
%  yes
%%% upper and lower bound using iterative deepening, final interval width 0.01
%    ?- graph::problog_delta(path(1,4),0.01,Bound_low,Bound_up,Status).
%  Bound_low = 0.5354096,
%  Bound_up = 0.53864,
%  Status = ok ?
%  yes
%%% upper and lower bound obtained cutting the sld tree at probability 0.1 for each branch
%     ?- graph::problog_threshold(path(1,4),0.1,Bound_low,Bound_up,Status).
%  4 proofs
%  Bound_low = 0.517344,
%  Bound_up = 0.563728,
%  Status = ok ?
%  yes
%%% lower bound obtained cutting the sld tree at probability 0.2 for each branch
%     ?- graph::problog_low(path(1,4),0.2,Bound_low,Status).
%  1 proofs
%  Bound_low = 0.432,
%  Status = ok ?
%  yes
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

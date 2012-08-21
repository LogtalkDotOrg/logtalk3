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
% 
% example for parameter learning with LeProbLog
%
% training and test examples are included at the end of the file
% 
% query ?- learn_graph::do_learning(20).
% will run 20 iterations of learning with default settings 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(learn_graph,
	imports(problog_learning)).

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
	% - probability represented by t/1 term means learnable parameter
	% - argument of t/1 is real value (used to compare against in evaluation when known), use t(_) if unknown
	%%%%
	t(0.9) ~ dir_edge(1,2).
	t(0.8) ~ dir_edge(2,3).
	t(0.6) ~ dir_edge(3,4).
	t(0.7) ~ dir_edge(1,6).
	t(0.5) ~ dir_edge(2,6).
	t(0.4) ~ dir_edge(6,5).
	t(0.7) ~ dir_edge(5,3).
	t(0.2) ~ dir_edge(5,4).

	%%%%%%%%%%%%%%
	% training examples of form example(ID,Query,DesiredProbability) 
	%%%%%%%%%%%%%%

	example(1,path(1,2),0.94).
	example(2,path(1,3),0.81).
	example(3,path(1,4),0.54).
	example(4,path(1,5),0.70).
	example(5,path(1,6),0.87).
	example(6,path(2,3),0.85).
	example(7,path(2,4),0.57).
	example(8,path(2,5),0.72).
	example(9,path(2,6),0.86).
	example(10,path(3,4),0.66).
	example(11,path(3,5),0.80).
	example(12,path(3,6),0.75).
	example(13,path(4,5),0.57).
	example(14,path(4,6),0.51).
	example(15,path(5,6),0.69).
	% some examples for learning from proofs:
	example(16,(dir_edge(2,3),dir_edge(2,6),dir_edge(6,5),dir_edge(5,4)),0.032).
	example(17,(dir_edge(1,6),dir_edge(2,6),dir_edge(2,3),dir_edge(3,4)),0.168).
	example(18,(dir_edge(5,3),dir_edge(5,4)),0.14).
	example(19,(dir_edge(2,6),dir_edge(6,5)),0.2).
	example(20,(dir_edge(1,2),dir_edge(2,3),dir_edge(3,4)),0.432).

	%%%%%%%%%%%%%%
	% test examples of form test_example(ID,Query,DesiredProbability) 
	% note: ID namespace is shared with training example IDs
	%%%%%%%%%%%%%%

	test_example(21,path(2,1),0.94).
	test_example(22,path(3,1),0.81).
	test_example(23,path(4,1),0.54).
	test_example(24,path(5,1),0.70).
	test_example(25,path(6,1),0.87).
	test_example(26,path(3,2),0.85).
	test_example(27,path(4,2),0.57).
	test_example(28,path(5,2),0.72).
	test_example(29,path(6,2),0.86).
	test_example(30,path(4,3),0.66).
	test_example(31,path(5,3),0.80).
	test_example(32,path(6,3),0.75).
	test_example(33,path(5,4),0.57).
	test_example(34,path(6,4),0.51).
	test_example(35,path(6,5),0.69).

	:- multifile(user::example/3).
	user::example(A, B, C) :-
		example(A, B, C).

	:- multifile(user::test_example/3).
	user::test_example(A, B, C) :-
		test_example(A, B, C).

	:- multifile(user::path/2).
	user::path(A, B) :-
		path(A, B).

	:- multifile(user::dir_edge/2).
	user::dir_edge(A, B) :-
		dir_edge(A, B).

:- end_object.

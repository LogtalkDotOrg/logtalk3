%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(solver).

	:- info([
		version is 1.1,
		date is 2008/10/2,
		author is 'Paulo Moura',
		comment is 'Simple meta-interpreter for pure Prolog with only conjunctions as clause bodies.']).

	:- public(solve/1).
	:- mode(solve(+goal), zero_or_more).
	:- info(solve/1, [
		comment is 'Proves goal.',
		argnames is ['Goal']]).

	solve(true) :-
		!.
	solve((A, B)) :-
		!,
		solve(A),
		solve(B).
	solve(A) :-
		clause(A, B),   % retrieves clauses in "this", i.e. in the database of the object importing the category
		solve(B).

:- end_category.


:- category(proof_tree).

	:- info([
		version is 1.1,
		date is 2008/10/2,
		author is 'Paulo Moura',
		comment is 'Meta-interpreter for pure Prolog with only conjunctions as clause bodies.']).

	:- public(proof_tree/2).
	:- mode(proof_tree(+goal, -tree), zero_or_more).
	:- info(proof_tree/2, [
		comment is 'Constructs a proof tree for a goal.',
		argnames is ['Goal', 'Tree']]).

	proof_tree(true, true) :-
		!.
	proof_tree((A, B), (PA, PB)) :-
		!,
		proof_tree(A, PA),
		proof_tree(B, PB).
	proof_tree(A, (A :- PB)) :-
		clause(A, B),   % retrieves clauses in "this", i.e. in the database of the object importing the category
		proof_tree(B, PB).

:- end_category.


:- category(tracer).

	:- info([
		version is 1.1,
		date is 2008/10/2,
		author is 'Paulo Moura',
		comment is 'A simple tracer meta-interpreter for pure Prolog with only conjunctions as clause bodies.']).

	:- public(trace/1).
	:- mode(trace(+goal), zero_or_more).
	:- info(trace/1, [
		comment is 'Traces goal proof.',
		argnames is ['Goal']]).

	trace(Goal) :-
		trace(Goal, 1).

	trace(true, _) :-
		!.
	trace((A, B), Depth) :-
		!, 
		trace(A, Depth),
		trace(B, Depth). 
	trace(A, Depth) :-
		write_trace(call, A, Depth),
		clause(A, B),   % retrieves clauses in "this", i.e. in the database of the object importing the category
		Depth2 is Depth + 1,
		trace(B, Depth2),
		(	write_trace(exit, A, Depth)
		;   write_trace(redo, A, Depth),
			fail
		).
	trace(A, Depth) :-
		write_trace(fail, A, Depth),
		fail.

	write_trace(Port, Goal, Depth) :-
		write(Depth), write(' '), write(Port), write(': '), writeq(Goal), nl.

:- end_category.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- category(solver).

	:- info([
		version is 1.1,
		date is 2008/10/2,
		author is 'Paulo Moura',
		comment is 'Simple meta-interpreter for pure Prolog with only conjunctions as clause bodies.'
	]).

	:- public(solve/1).
	:- mode(solve(+goal), zero_or_more).
	:- info(solve/1, [
		comment is 'Proves goal.',
		argnames is ['Goal']
	]).

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
		comment is 'Meta-interpreter for pure Prolog with only conjunctions as clause bodies.'
	]).

	:- public(proof_tree/2).
	:- mode(proof_tree(+goal, -tree), zero_or_more).
	:- info(proof_tree/2, [
		comment is 'Constructs a proof tree for a goal.',
		argnames is ['Goal', 'Tree']
	]).

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
		comment is 'A simple tracer meta-interpreter for pure Prolog with only conjunctions as clause bodies.'
	]).

	:- public(trace/1).
	:- mode(trace(+goal), zero_or_more).
	:- info(trace/1, [
		comment is 'Traces goal proof.',
		argnames is ['Goal']
	]).

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

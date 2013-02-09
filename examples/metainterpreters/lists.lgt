%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(counter).

	:- info([
		version is 1.0,
		date is 2010/02/23,
		author is 'Paulo Moura',
		comment is 'Simple meta-interpreter for pure Prolog for counting resolution steps.'
	]).

	:- public(steps/2).
	:- mode(steps(+goal, -integer), zero_or_more).
	:- info(steps/2, [
		comment is 'Proves goal.',
		argnames is ['Goal', 'Steps']
	]).

	steps(Goal, Steps) :-
		steps(Goal, 0, Steps).

	steps(true, Steps, Steps) :-
		!.
	steps((A, B), Steps0, Steps) :-
		!,
		steps(A, Steps0, Steps1),
		steps(B, Steps1, Steps).
	steps(A, Steps0, Steps) :-
		Steps1 is Steps0 + 1,
		clause(A, B),   % retrieves clauses in "this", i.e. in the database of the object importing the category
		steps(B, Steps1, Steps).

:- end_category.


:- object(lists,
	imports(counter)).

	:- public(append/3).
	:- dynamic(append/3).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	:- public(reverse/2).
	:- dynamic(reverse/2).

	reverse([], []).
	reverse([Head| Tail], Reversed) :-
		reverse(Tail, ReversedTail),
		append(ReversedTail, [Head], Reversed).

:- end_object.

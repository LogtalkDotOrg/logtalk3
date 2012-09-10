%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(lists).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Coinductive infinite list predicates example.']).

	:- public(append/3).
	:- coinductive(append/3).

	append([], X, X).
	append([H| T], Y, [H| Z]) :-
		append(T, Y, Z).

	% Similar to member/2.
	:- coinductive(member1/2).

	member1(X, [X| _]).
	member1(X, [_| T]) :-
		member1(X, T).

	% Ditto, but not coinductive
	member2(X, [X| _]).
	member2(X, [_| T]) :-
		member2(X, T).

	% Are there infinitely many "occurrences" of arg1 in arg2?
	:- public(comember/2).
	:- coinductive(comember/2).

	comember(X, L) :-
		drop(X, L, L1),
		comember(X, L1).

	% Drop some prefix of arg2 upto an "occurrence" of arg1 from arg2,
	% yielding arg3.
	% ("Occurrence" of X = something unifiable with X.)
	%:- table(drop/3).	% not working; needs tabling supporting cyclic terms!
	drop(H, [H| T], T).
	drop(H, [_| T], T1) :-
		drop(H, T, T1).

	:- public(absent/2).
	:- coinductive(absent/2).

	absent(X, [Y| T]) :-
		X \= Y,
		absent(X, T).

:- end_object.


/* Example queries:

:- initialization((
	write('Query1'), nl,
    X = [0, s(0), s(s(0))],
    member2(s(0), X),
    write('Yes1 !'), nl
)).

:- initialization((
	write('Query2'), nl,
    X = [0, s(0), s(s(0))],
    member1(s( 0), X),
    write('Yes2 !'), nl
)).

:- initialization((
	(	write('Query3'), nl,
    	X = [0, s(0), s(s(0))],
    	comember(s(0), X) ->
    	write('WHAT? SHOULD HAVE FAILED !'), nl
	;	true
	)
)).

:- initialization((
	write('Query4'), nl,
    X = [0, s(0), s(s(0))| X], write(' HERE!'),
    once(comember(s(0), X)),
    write('Yes4 !'), nl,
    write_term(X, [max_depth(10)]), nl
)).

*/

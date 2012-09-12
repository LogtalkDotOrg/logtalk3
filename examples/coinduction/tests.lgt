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
		version is 1.3,
		author is 'Paulo Moura',
		date is 2012/09/12,
		comment is 'Unit tests for the "coinduction" example.']).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).

	succeeds(coinduction_simple_1) :-
		simple::p.

	succeeds(coinduction_simple_2) :-
    	simple::p(hoho).

	succeeds(coinduction_simple_3) :-
		simple::p(hoho, X),
		X == hoho.

	succeeds(coinduction_binary_1) :-
		L = [1| L], binary::p(L).

	succeeds(coinduction_binary_2) :-
		L = [0| L], binary::p(L).

	succeeds(coinduction_binary_3) :-
		L = [1,0,1| L], binary::p(L).

	succeeds(coinduction_streams_1) :-
		bagof(T, streams::nat_stream([0, s(0), s(s(0))| T]), [T1, T2, T3]),
		T1 == [s(s(0))| T1],
		T2 == [s(0), s(s(0))| T2],
		T3 == [0, s(0), s(s(0))| T3].

	succeeds(coinduction_streams_2) :-
		X = [0, 1, 1, 0| X], streams::bit_stream(X).

	succeeds(coinduction_filter_1) :-
		L = [0, s(0), s(s(0))| L], filter::filter(L, F),
		F == [0, s(s(0))| F].

	succeeds(coinduction_lists_1) :-
		X = [1, 2, 3| X], lists::comember(2, X).

	fails(coinduction_lists_2) :-
		X = [1, 2, 3, 1, 2, 3], lists::comember(2, X).

	fails(coinduction_lists_3) :-
		X = [0, s(0), s(s(0))], lists::comember(s(0), X).

	succeeds(coinduction_lists_4) :-
		X = [0, s(0), s(s(0))| X], lists::comember(s(0), X).

	succeeds(coinduction_lists_5) :-
		X = [1, 2, 3| X],
		bagof(E, lists::comember(E, X), [E1, E2, E3]),
		E1 == 1, E2 == 2, E3 == 3.

	succeeds(coinduction_lists_6) :-
		Y = [1, 2, 3| Y], X = [0| Y],
		bagof(E, lists::comember(E, X), [E1, E2, E3]),
		E1 == 1, E2 == 2, E3 == 3.

	succeeds(coinduction_automata_1) :-
		bagof(X, automaton::automaton(s0, X), [X1, X2]),
		X1 == [a, b, c, d| X1],
		X2 == [a, b, e| X2].

	succeeds(coinduction_nested_1) :-
		bagof(X, (nested::state(s0, X), lists::absent(s2, X)), [X1, X2]),
		L = [s1| L], X1 == [s0| L],
		X2 == [s0, s3| X2].

	succeeds(coinduction_counter_1) :-
		counter::verify.

	:- if(\+ current_logtalk_flag(prolog_dialect, eclipse)).

	succeeds(coinduction_sieve_1) :-
		sieve::primes(20, P),
		P = [2, 3, 5, 7, 11, 13, 17, 19| P].

	succeeds(coinduction_cyclic_paths_1) :-
		bagof(P, cp1::path(a, P), [P1, P2, P3]),
		X1 = [b| X1], P1 == [a| X1],
		X2 = [d| X2], P2 == [a, b, c| X2],
		X3 = [b, c, a| X3], P3 == [a| X3].

	succeeds(coinduction_cyclic_paths_2) :-
		bagof(P, cp2::path(a, P), [P1, P2]),
		X1 = [b, c, a| X1], P1 == [a| X1],
		X2 = [b, c, d, a| X2], P2 == [a| X2].

	succeeds(coinduction_shared_paths_1) :-
		bagof(P, shared_paths::path(a, P), [P1, P2, P3, P4]),
		X1 = [a, b, c, d, e, f| X1], P1 == X1,
		X2 = [c, d, e, f| X2], P2 == [a, b| X2],
		X3 = [a, b, c, f| X3], P3 == X3,
		X4 = [c, f| X4], P4 == [a, b| X4].

	succeeds(coinduction_tangle_1) :-
		bagof(P, tangle::p(P), [P1, P2]),
		X1 = [a, b| X1], P1 == X1,
		X2 = [c, d| X2], P2 == X2.

	:- if(current_object(train)).
	succeeds(coinduction_train_1) :-
		bagof((X, R), train::driver(s0, s0, s0, X, R), [(X1, R1), (X2, R2)]),
		XS1 = [down, in, out, exit, raise, approach, up, lower| XS1], X1 == [approach, lower| XS1],
		RS1 = [(down, _), (in, _), (out, _), (exit, _), (raise, _), (approach, _), (up, _), (lower, 1.0)| RS1], R1 = [(approach, 0), (lower, 1.0)| RS1],
		XS2 = [lower, down, in, out, exit, raise, up, approach| XS2], X2 == [approach| XS2],
		RS2 = [(lower, 1.0), (down, _), (in, _), (out, _), (exit, _), (raise, _), (up, _), (approach, 0)| RS2], R2 = [(approach, 0)| RS2].
	:- endif.

	:- if(current_object(cotrain)).
	succeeds(coinduction_cotrain_1) :-
		cotrain::comain(A, B, C),
		A = [approach, in, out, exit| A],
		B = [approach, exit| B],
		C = [lower, raise| C].
	:- endif.

	:- endif.

:- end_object.

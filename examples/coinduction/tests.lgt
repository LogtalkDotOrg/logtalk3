%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2021-11-29,
		comment is 'Unit tests for the "coinduction" example.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	test(coinduction_arithmetic_1, true) :-
		N1 = [0| Eights], Eights = [8| Eights],
		N2 = [0| Ones], Ones = [1| Ones],
		bagof((R, O), arithmetic::add(N1, N2, R, O), [(R1, O1), (R2, O2)]),
		T1 = [9| T1], assertion(R1 == [0| T1]), assertion(O1 == 0),
		T2 = [0| T2], assertion(R2 == [1| T2]), assertion(O2 == 0).

	test(coinduction_arithmetic_2, true(M == 3)) :-
		L = [1,2,3,2,1| L],
		arithmetic::max(L, M).

	test(coinduction_simple_1, true) :-
		simple::p.

	test(coinduction_simple_2, true) :-
		simple::p(hoho).

	test(coinduction_simple_3, true(X == hoho)) :-
		simple::p(hoho, X).

	test(coinduction_binary_1, true) :-
		L = [1| L], binary::p(L).

	test(coinduction_binary_2, true) :-
		L = [0| L], binary::p(L).

	test(coinduction_binary_3, true) :-
		L = [1,0,1| L], binary::p(L).

	test(coinduction_streams_1, true) :-
		bagof(T, streams::nat_stream([0, s(0), s(s(0))| T]), [T1, T2, T3]),
		assertion(T1 == [s(s(0))| T1]),
		assertion(T2 == [s(0), s(s(0))| T2]),
		assertion(T3 == [0, s(0), s(s(0))| T3]).

	test(coinduction_streams_2, true) :-
		X = [0, 1, 1, 0| X], streams::bit_stream(X).

	test(coinduction_filter_1, true) :-
		L = [0, s(0), s(s(0))| L], filter::filter(L, F),
		assertion(F == [0, s(s(0))| F]).

	test(coinduction_lists_1, true) :-
		X = [1, 2, 3| X], lists::comember(2, X).

	test(coinduction_lists_2, false) :-
		X = [1, 2, 3, 1, 2, 3], lists::comember(2, X).

	test(coinduction_lists_3, false) :-
		X = [0, s(0), s(s(0))], lists::comember(s(0), X).

	test(coinduction_lists_4, true) :-
		X = [0, s(0), s(s(0))| X], lists::comember(s(0), X).

	test(coinduction_lists_5, true) :-
		X = [1, 2, 3| X],
		bagof(E, lists::comember(E, X), [E1, E2, E3]),
		assertion(E1 == 1),
		assertion(E2 == 2),
		assertion(E3 == 3).

	test(coinduction_lists_6, true) :-
		Y = [1, 2, 3| Y], X = [0| Y],
		bagof(E, lists::comember(E, X), [E1, E2, E3]),
		assertion(E1 == 1),
		assertion(E2 == 2),
		assertion(E3 == 3).

	test(coinduction_automata_1, true) :-
		bagof(X, automaton::automaton(s0, X), [X1, X2]),
		assertion(X1 == [a, b, c, d| X1]),
		assertion(X2 == [a, b, e| X2]).

	test(coinduction_nested_1, true) :-
		bagof(X, (nested::state(s0, X), lists::absent(s2, X)), [X1, X2]),
		L = [s1| L],
		assertion(X1 == [s0| L]),
		assertion(X2 == [s0, s3| X2]).

	test(coinduction_counter_1, true) :-
		counter::verify.

	test(coinduction_sieve_1, true) :-
		sieve::primes(20, P),
		P = [2, 3, 5, 7, 11, 13, 17, 19| P].

	test(coinduction_cyclic_paths_1, true) :-
		bagof(P, cp1::path(a, P), [P1, P2, P3]),
		X1 = [b| X1],       assertion(P1 == [a| X1]),
		X2 = [d| X2],       assertion(P2 == [a, b, c| X2]),
		X3 = [b, c, a| X3], assertion(P3 == [a| X3]).

	test(coinduction_cyclic_paths_2, true) :-
		bagof(P, cp2::path(a, P), [P1, P2]),
		X1 = [b, c, a| X1],    assertion(P1 == [a| X1]),
		X2 = [b, c, d, a| X2], assertion(P2 == [a| X2]).

	test(coinduction_shared_paths_1, true) :-
		bagof(P, shared_paths::path(a, P), [P1, P2, P3, P4]),
		X1 = [a, b, c, d, e, f| X1], assertion(P1 == X1),
		X2 = [c, d, e, f| X2],       assertion(P2 == [a, b| X2]),
		X3 = [a, b, c, f| X3],       assertion(P3 == X3),
		X4 = [c, f| X4],             assertion(P4 == [a, b| X4]).

	test(coinduction_tangle_1, true) :-
		bagof(P, tangle::p(P), [P1, P2]),
		X1 = [a, b| X1], assertion(P1 == X1),
		X2 = [c, d| X2], assertion(P2 == X2).

	test(coinduction_graph_1, true) :-
		% graph found on the Wikipedia page http://en.wikipedia.org/wiki/Bipartite_graph
		A = v(a, [F]), B = v(b, [F, G]), C = v(c, [H, I]), D = v(d, [G]), E = v(e, [F, I]),
		F = v(f, [A, B]), G = v(g, [B, D]), H = v(h, [C]), I = v(i, [C, E]),
		graph::bipartite(A).

	test(coinduction_graph_2, true) :-
		V0 = v(0, [V1, V3, V5, V7]), V1 = v(1, [V0, V2, V4, V6]), V2 = v(2, [V1, V3, V5, V7]), V3 = v(3, [V0, V2, V4, V6]),
		V4 = v(4, [V1, V3, V5, V7]), V5 = v(5, [V0, V2, V4, V6]), V6 = v(6, [V1, V3, V5, V7]), V7 = v(7, [V0, V2, V4, V6]),
		graph::bipartite(V0).

	test(coinduction_graph_3, false) :-
		V0 = v(0, [V0]),
		graph::bipartite(V0).

	test(coinduction_graph_4, false) :-
		V0 = v(0, [V1, V4]), V1 = v(1, [V0, V2]), V2 = v(2, [V1, V3]), V3 = v(3, [V2, V4]), V4 = v(4, [V0, V3]),
		graph::bipartite(V4).

	:- if(current_object(train)).

		test(coinduction_train_1, true) :-
			bagof((X, R), train::driver(s0, s0, s0, X, R), [(X1, R1), (X2, R2)]),
			X1 = [approach| TX1], TX1 = [lower, down, in, out, exit, raise, approach, up| TX1],
			R1 = [(approach, 0)| TR1], TR1 = [ (lower, 1.0), (down, _), (in, _), (out, _), (exit, _), (raise, _), (approach, _), (up, _)| TR1],
			X2 = [approach| TX2], TX2 = [lower, down, in, out, exit, raise, up, approach| TX2],
			R2 = [(approach, 0)| TR2], TR2 = [ (lower, 1.0), (down, _), (in, _), (out, _), (exit, _), (raise, _), (up, _), (approach, _)| TR2].

	:- endif.

	:- if(current_object(cotrain)).

		test(coinduction_cotrain_1, true) :-
			cotrain::comain(A, B, C),
			A = [approach, in, out, exit| A],
			B = [approach, exit| B],
			C = [lower, raise| C].

	:- endif.

:- end_object.

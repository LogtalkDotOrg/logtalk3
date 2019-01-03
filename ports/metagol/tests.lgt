%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  
%  Copyright 2016 Metagol authors
%  Copyright 2018-2019 Paulo Moura
%  All rights reserved.
%  
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions
%  are met:
%  
%  1. Redistributions of source code must retain the above copyright
%     notice, this list of conditions and the following disclaimer.
%  
%  2. Redistributions in binary form must reproduce the above copyright
%     notice, this list of conditions and the following disclaimer in
%     the documentation and/or other materials provided with the
%     distribution.
%  
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2019/01/03,
		comment is 'Unit tests for the "metagol" example.'
	]).

	cover(metagol).

	test(metagol_constants1_1) :-
		constants1::learn(Clauses), !,
		^^variant(
			Clauses,
			[p(4, _), p(_, 2), p(1, _)]
		).

	test(metagol_constants2_1) :-
		constants2::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(q(4, A) :-num(4), num(A)),
				(q(B, 2) :-num(B), num(2)),
				(q(1, C) :-num(1), num(C))
			]
		).

	test(metagol_constants3_1) :-
		constants3::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(f(andy, A) :- p(patrick, A)),
				(f(andy, B) :- p(spongebob, B))
			]
		).

	test(metagol_family_1) :-
		family::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(grandparent(A,C) :- grandparent_1(A,B), grandparent_1(B,C)),
				(grandparent_1(D,E) :- father(D,E)),
				(grandparent_1(F,G) :- mother(F,G))
			]
		).

	test(metagol_find_duplicate_1) :-
		find_duplicate::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(f(A, B) :- head(A, B), f_1(A, B)),
				(f(C, E) :- tail(C, D), f(D, E)),
				(f_1(F, H) :- tail(F, G), element(G, H))
			]
		).

	test(metagol_graph_reachability_1) :-
		graph_reachability::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(p(A, C) :- edge(A, B), p(B, C)),
				(p(D, E) :- edge(D, E))
			]
		).

	test(metagol_higher_order1_1) :-
		higher_order1::learn(Clauses), !,
		^^variant(
			Clauses,
			[(f(A, B):-map(A, B, my_succ))]
		).

	test(metagol_higher_order2_1) :-
		higher_order2::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(f(A, B) :- map(A, B, f_1)),
				(f_1(C, E) :- my_length(C, D), my_double(D, E))
			]
		).

	test(metagol_higher_order3_1) :-
		higher_order3::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(f(A, B) :- filter(A, B, f_1)),
				(f_1(C) :- divisible5(C)),
				(f_1(D) :- divisible2(D))
			]
		).

	test(metagol_kinship1_1) :-
		kinship1::learn1(Clauses), !,
		^^variant(
			Clauses,
			[	(grandparent(A,C) :- grandparent_1(A,B), grandparent_1(B,C)),
				(grandparent_1(D,E) :- father(D,E)),
				(grandparent_1(F,G) :- mother(F,G))
			]
		).

	test(metagol_kinship1_2) :-
		kinship1::learn2, !.

	test(metagol_kinship2_1) :-
		kinship2::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(ancestor(A, C) :- parent(A, B), ancestor(B, C)),
				(ancestor(D, E) :- parent(D, E))
			]
		).

	test(metagol_mutual_recursion_1) :-
		mutual_recursion::learn(Clauses), !,
		^^variant(
			Clauses,
			[	even(0),
				(even(A) :- s(A, B), even_1(B)),
				(even_1(C) :- s(C, D), even(D))
			]
		).

	test(metagol_robots_1) :-
		robots::learn1(Clauses), !,
		^^variant(
			Clauses,
			[	(f(A,E) :- grab_ball(A,B), f_3(B,C), f_3(C,D), drop_ball(D,E)),
				(f_3(F,H) :- move_right(F,G), move_forwards(G,H))
			]
		).

	test(metagol_robots_2) :-
		robots::learn2(Clauses), !,
		^^variant(
			Clauses,
			[	(f(A,E) :- grab_ball(A,B), f_3(B,C), f_3(C,D), drop_ball(D,E)),
				(f_3(F,H) :- f_4(F,G), f_4(G,H)),
				(f_4(I,K) :- move_right(I,J), move_forwards(J,K))
			]
		).

	test(metagol_sequential_1) :-
		sequential::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(grandparent(A,C) :- parent(A,B), parent(B,C)),
				(great_grandparent(D,F) :- parent(D,E), grandparent(E,F)),
				(parent(G,H) :- father(G,H)),
				(parent(I,J) :- mother(I,J))
			]
		).

	test(metagol_sequential1_1) :-
		sequential1::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(grandparent(A,C) :- parent(A,B), parent(B,C)),
				(great_grandparent(D,F) :- parent(D,E), grandparent(E,F)),
				(parent(G,H) :- father(G,H)),
				(parent(I,J) :- mother(I,J))
			]
		).

	test(metagol_strings1_1) :-
		strings1::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(f(A, C) :- copy1(A, B), skip1(B, C)),
				(f(D, F) :- skip1(D, E), f(E, F)),
				(f(G, I) :- copy1(G, H), f(H, I))
			]
		).

	test(metagol_strings2_1) :-
		strings2::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(f(_3908, _3914) :- f_1(_3908, _3946), f_2(_3946, _3914)),
				(f_1(_4198, _4204) :- next_empty(_4198), f_2(_4198, _4204)),
				(f_1(_4476, _4482) :- f_2(_4476, _4514), f_1(_4514, _4482)),
				(f_2(_4812, _4818) :- write1(_4812, _4818, d)),
				(f_2(_5046, _5052) :- copy1(_5046, _5084), skip1(_5084, _5052))
			]
		).

	test(metagol_strings3_1) :-
		strings3::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(f(_1670, _1676) :- copy1(_1670, _1688), f_1(_1688, _1676)),
				(f_1(_1724, _1730) :- next_empty(_1724), f_3(_1724, _1730)),
				(f_1(_1778, _1784) :- f_2(_1778, _1796), f_1(_1796, _1784)),
				(f_2(_1856, _1862) :- skip1(_1856, _1874), copy1(_1874, _1862)),
				(f_3(_1934, _1940) :- write1(_1934, _1940, d))
			]
		).

	test(metagol_trains_1) :-
		trains::learn(Clauses), !,
		^^variant(
			Clauses,
			[	(e(A) :- has_car(A, B), e_1(B)),
				(e_1(C) :- short(C), closed(C))
			]
		).

:- end_object.

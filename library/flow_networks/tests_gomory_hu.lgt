%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests_gomory_hu(_Dictionary_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Unit tests for Gomory-Hu cut trees.',
		parnames is ['Dictionary']
	]).

	:- uses(flow_network(_Dictionary_), [
		new/2, new/3, vertices/2, cut_capacity/3
	]).

	:- uses(gomory_hu(_Dictionary_), [
		tree/2, min_cut_value/4
	]).

	:- uses(list, [
		member/2, append/3
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(numberlist, [
		min/2 as minimum/2
	]).

	:- uses(fast_random(as183), [
		between/3 as random_between/3, get_seed/1, set_seed/1, randomize/1
	]).

	cover(gomory_hu(_Dictionary_)).

	test(empty_tree, deterministic(Tree == tree([],[]))) :-
		new([], Network),
		tree(Network, Tree).

	test(singleton_tree, deterministic(Tree == tree([f(1)],[]))) :-
		new([f(1)], [arc(f(1),f(1),3,-2)], Network),
		tree(Network, Tree).

	test(disconnected_tree, deterministic(Value == 0)) :-
		new([a,b,c], [arc(a,b,3),arc(b,a,3)], Network),
		tree(Network, Tree),
		min_cut_value(Tree, a, c, Value),
		check_tree(Network, Tree).

	test(parallel_symmetry, deterministic(Value == 5)) :-
		new([arc(a,b,2),arc(a,b,3),arc(b,a,5),arc(a,a,7)], Network),
		tree(Network, Tree),
		min_cut_value(Tree, a, b, Value),
		check_tree(Network, Tree).

	test(reparent_processed_subtree, deterministic) :-
		new([arc(a,c,3),arc(c,a,3),arc(a,d,6),arc(d,a,6),
			arc(a,e,3),arc(e,a,3),arc(b,d,1),arc(d,b,1),
			arc(b,e,3),arc(e,b,3),arc(c,d,4),arc(d,c,4),
			arc(c,e,4),arc(e,c,4),arc(d,e,5),arc(e,d,5)], Network),
		tree(Network, Tree),
		check_tree(Network, Tree).

	test(asymmetric_network, error(domain_error(symmetric_capacities, _))) :-
		new([arc(a,b,2),arc(b,a,1)], Network),
		tree(Network, _).

	test(nonzero_lower, error(domain_error(zero_lower_bounds, _))) :-
		new([arc(a,b,1,2,0),arc(b,a,2)], Network),
		tree(Network, _).

	test(zero_asymmetry, deterministic(Value == 0)) :-
		new([arc(a,b,0)], Network),
		tree(Network, Tree),
		min_cut_value(Tree, a, b, Value).

	test(nonground_query, error(instantiation_error)) :-
		min_cut_value(tree([a,b],[edge(a,b,1)]), _, b, _).

	test(unknown_terminal, error(existence_error(vertex, c))) :-
		min_cut_value(tree([a,b],[edge(a,b,1)]), a, c, _).

	test(equal_terminals, error(consistency_error(distinct_vertices, a, a))) :-
		min_cut_value(tree([a],[]), a, a, _).

	test(invalid_tree, error(domain_error(gomory_hu_tree, _))) :-
		min_cut_value(tree([a,b,c],[edge(a,b,1),edge(a,b,2)]), a, b, _).

	test(random_symmetric_networks, all(check_tree(Network, Tree)), [
		setup(save_seed(Saved)), cleanup(set_seed(Saved))
	]) :-
		between(1, 40, _),
		symmetric_arcs([a,b,c,d,e], Arcs),
		new([a,b,c,d,e], Arcs, Network),
		tree(Network, Tree).

	% auxiliary predicates

	save_seed(Saved) :-
		get_seed(Saved),
		randomize(20261116).

	symmetric_arcs([], []).
	symmetric_arcs([Vertex| Vertices], Arcs) :-
		pair_arcs(Vertices, Vertex, Arcs0),
		symmetric_arcs(Vertices, Arcs1),
		append(Arcs0, Arcs1, Arcs).

	pair_arcs([], _, []).
	pair_arcs([To| Vertices], From, [arc(From,To,A),arc(From,To,B),arc(To,From,C)| Arcs]) :-
		random_between(0, 4, A),
		random_between(0, 4, B),
		C is A + B,
		pair_arcs(Vertices, From, Arcs).

	check_tree(Network, Tree) :-
		vertices(Network, Vertices),
		\+ (
			member(Source, Vertices), member(Sink, Vertices), Source @< Sink,
			\+ check_pair(Network, Tree, Vertices, Source, Sink)
		),
		Tree = tree(_, Edges),
		\+ (
			member(edge(From, To, Weight), Edges),
			reach_tree([From], Edges, From-To, [], Side),
			\+ cut_capacity(Network, Side, Weight)
		).

	check_pair(Network, Tree, Vertices, Source, Sink) :-
		min_cut_value(Tree, Source, Sink, Value),
		dinic(_Dictionary_)::min_cut(Network, Source, Sink, Value, _),
		findall(Capacity, (
			subset(Vertices, Side),
			member(Source, Side), \+ member(Sink, Side),
			cut_capacity(Network, Side, Capacity)
		), Capacities),
		minimum(Capacities, Value).

	subset([], []).
	subset([Head| Tail], [Head| Rest]) :-
		subset(Tail, Rest).
	subset([_| Tail], Rest) :-
		subset(Tail, Rest).

	reach_tree([], _, _, Reached, Reached).
	reach_tree([Vertex| Pending], Edges, Removed, Reached0, Reached) :-
		(	member(Vertex, Reached0) ->
			reach_tree(Pending, Edges, Removed, Reached0, Reached)
		;	findall(To, (
				member(edge(A,B,_), Edges),
				A-B \== Removed,
				( A == Vertex -> To = B ; B == Vertex, To = A )
			), Neighbors),
			append(Neighbors, Pending, Pending1),
			reach_tree(Pending1, Edges, Removed, [Vertex| Reached0], Reached)
		).

:- end_object.

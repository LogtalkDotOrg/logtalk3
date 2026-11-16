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


:- object(gomory_hu(_Dictionary_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Gomory-Hu cut trees for networks with symmetric aggregate capacities. Uses n-1 calls to Dinic for n vertices, with no calls for empty networks. Costs and self-loops do not affect cuts.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the flow network.'
		],
		see_also is [gomory_hu, dinic(_), flow_network(_)]
	]).

	:- public(tree/2).
	:- mode(tree(+compound, -compound), one_or_error).
	:- info(tree/2, [
		comment is 'Returns ``tree(Vertices, Edges)`` where each ``edge(Vertex, Parent, Capacity)`` is a weighted tree edge, in vertex order. Requires zero lower bounds and equal total capacity in both directions for every pair of distinct vertices. Parallel multiplicities may differ. Disconnected components are linked by zero-capacity edges.',
		argnames is ['Network', 'Tree'],
		exceptions is [
			'A lower bound is non-zero' - domain_error(zero_lower_bounds, 'Network'),
			'Aggregate capacities are asymmetric' - domain_error(symmetric_capacities, 'Network')
		]
	]).

	:- public(min_cut_value/4).
	:- mode(min_cut_value(+compound, +ground, +ground, -integer), one_or_error).
	:- info(min_cut_value/4, [
		comment is 'Returns the minimum edge capacity on the unique tree path between distinct vertices. Validates the supplied weighted tree. Adjacency construction and traversal take O(V*log(V)) with balanced dictionaries.',
		argnames is ['Tree', 'Source', 'Sink', 'Value'],
		exceptions is [
			'An input vertex or the tree is not ground' - instantiation_error,
			'The tree is malformed' - domain_error(gomory_hu_tree, 'Tree'),
			'A terminal is not a tree vertex' - existence_error(vertex, 'Vertex'),
			'The terminals are equal' - consistency_error(distinct_vertices, 'Source', 'Sink')
		]
	]).

	:- uses(_Dictionary_, [
		new/1 as dict_new/1, insert/4 as dict_insert/4, lookup/3 as dict_lookup/3, as_list/2 as dict_as_list/2
	]).

	:- uses(list, [
		member/2, append/3, length/2
	]).

	:- uses(type, [
		valid/2 as valid_type/2
	]).

	tree(Network, tree(Vertices, Edges)) :-
		context(Context),
		flow_network(_Dictionary_)::vertices(Network, Vertices),
		flow_network(_Dictionary_)::arcs(Network, Arcs),
		dict_new(Empty),
		capacities(Arcs, Empty, Capacities, Network, Context),
		dict_as_list(Capacities, Pairs),
		check_symmetry(Pairs, Capacities, Network, Context),
		(	Vertices = [Root| Rest] ->
			initialize(Vertices, Root, Empty, Parents0, Empty, Weights0),
			cuts(Rest, Network, Parents0, Parents, Weights0, Weights),
			tree_edges(Rest, Parents, Weights, Edges)
		;	Edges = []
		).

	capacities([], Capacities, Capacities, _, _).
	capacities([arc(From, To, Lower, Capacity, _)| Arcs], Capacities0, Capacities, Network, Context) :-
		(	Lower =:= 0 ->
			true
		;	throw(error(domain_error(zero_lower_bounds, Network), Context))
		),
		(	From == To ->
			Capacities1 = Capacities0
		;	(	dict_lookup(From-To, Previous, Capacities0) ->
				true
			;	Previous = 0
			),
			Total is Previous + Capacity,
			dict_insert(Capacities0, From-To, Total, Capacities1)
		),
		capacities(Arcs, Capacities1, Capacities, Network, Context).

	check_symmetry([], _, _, _).
	check_symmetry([(From-To)-Capacity| Pairs], Capacities, Network, Context) :-
		(	dict_lookup(To-From, Reverse0, Capacities) ->
			Reverse = Reverse0
		;	Reverse = 0
		),
		(	Capacity =:= Reverse ->
			true
		;	throw(error(domain_error(symmetric_capacities, Network), Context))
		),
		check_symmetry(Pairs, Capacities, Network, Context).

	initialize([], _, Parents, Parents, Weights, Weights).
	initialize([Vertex| Vertices], Root, Parents0, Parents, Weights0, Weights) :-
		dict_insert(Parents0, Vertex, Root, Parents1),
		dict_insert(Weights0, Vertex, 0, Weights1),
		initialize(Vertices, Root, Parents1, Parents, Weights1, Weights).

	cuts([], _, Parents, Parents, Weights, Weights).
	cuts([Source| Sources], Network, Parents0, Parents, Weights0, Weights) :-
		dict_lookup(Source, Sink, Parents0),
		dinic(_Dictionary_)::min_cut(Network, Source, Sink, Value, cut(SourceSide, _, _)),
		dict_as_list(Parents0, ParentPairs),
		% Reattach processed children too, preserving the tree-edge cut partitions.
		reparent(ParentPairs, Sink, Source, SourceSide, Parents0, Parents1),
		dict_lookup(Sink, Grandparent, Parents1),
		(	member(Grandparent, SourceSide) ->
			dict_insert(Parents1, Source, Grandparent, Parents2),
			dict_insert(Parents2, Sink, Source, Parents3),
			dict_lookup(Sink, OldValue, Weights0),
			dict_insert(Weights0, Source, OldValue, Weights1),
			dict_insert(Weights1, Sink, Value, Weights2)
		;	Parents3 = Parents1,
			dict_insert(Weights0, Source, Value, Weights2)
		),
		cuts(Sources, Network, Parents3, Parents, Weights2, Weights).

	reparent([], _, _, _, Parents, Parents).
	reparent([Vertex-Parent| Vertices], Old, New, Side, Parents0, Parents) :-
		(	Vertex \== New,
			Parent == Old,
			member(Vertex, Side) ->
			dict_insert(Parents0, Vertex, New, Parents1)
		;	Parents1 = Parents0
		),
		reparent(Vertices, Old, New, Side, Parents1, Parents).

	tree_edges([], _, _, []).
	tree_edges([Vertex| Vertices], Parents, Weights, [edge(Vertex, Parent, Value)| Edges]) :-
		dict_lookup(Vertex, Parent, Parents),
		dict_lookup(Vertex, Value, Weights),
		tree_edges(Vertices, Parents, Weights, Edges).

	min_cut_value(Tree, Source, Sink, Value) :-
		context(Context),
		(	ground(Tree-Source-Sink) ->
			true
		;	throw(error(instantiation_error, Context))
		),
		(	valid_tree(Tree, Vertices, Adjacency) ->
			true
		;	throw(error(domain_error(gomory_hu_tree, Tree), Context))
		),
		(	member(Source, Vertices) ->
			true
		;	throw(error(existence_error(vertex, Source), Context))
		),
		(	member(Sink, Vertices) ->
			true
		;	throw(error(existence_error(vertex, Sink), Context))
		),
		(	Source \== Sink ->
			true
		;	throw(error(consistency_error(distinct_vertices, Source, Sink), Context))
		),
		path_value(Source, Sink, Adjacency, none, none, Value),
		!.

	valid_tree(tree(Vertices, Edges), Vertices, Adjacency) :-
		valid_type(list, Vertices),
		sort(Vertices, Vertices),
		valid_type(list, Edges),
		length(Vertices, VertexCount),
		length(Edges, EdgeCount),
		EdgeCount =:= max(0, VertexCount - 1),
		dict_new(Empty),
		vertex_adjacency(Vertices, Empty, Adjacency0),
		edge_adjacency(Edges, Adjacency0, Adjacency),
		(	Vertices = [Root| _] ->
			reach([Root], Adjacency, Empty, Reached),
			dict_as_list(Reached, ReachedPairs),
			length(ReachedPairs, VertexCount)
		;	true
		).

	vertex_adjacency([], Adjacency, Adjacency).
	vertex_adjacency([Vertex| Vertices], Adjacency0, Adjacency) :-
		dict_insert(Adjacency0, Vertex, [], Adjacency1),
		vertex_adjacency(Vertices, Adjacency1, Adjacency).

	edge_adjacency([], Adjacency, Adjacency).
	edge_adjacency([edge(From, To, Capacity)| Edges], Adjacency0, Adjacency) :-
		From \== To,
		integer(Capacity), Capacity >= 0,
		dict_lookup(From, FromNeighbors, Adjacency0),
		dict_lookup(To, ToNeighbors, Adjacency0),
		dict_insert(Adjacency0, From, [To-Capacity| FromNeighbors], Adjacency1),
		dict_insert(Adjacency1, To, [From-Capacity| ToNeighbors], Adjacency2),
		edge_adjacency(Edges, Adjacency2, Adjacency).

	reach([], _, Reached, Reached).
	reach([Vertex| Pending], Adjacency, Reached0, Reached) :-
		(	dict_lookup(Vertex, _, Reached0) ->
			reach(Pending, Adjacency, Reached0, Reached)
		;	dict_insert(Reached0, Vertex, true, Reached1),
			dict_lookup(Vertex, Neighbors, Adjacency),
			neighbor_vertices(Neighbors, Next),
			append(Next, Pending, Pending1),
			reach(Pending1, Adjacency, Reached1, Reached)
		).

	neighbor_vertices([], []).
	neighbor_vertices([Vertex-_| Neighbors], [Vertex| Vertices]) :-
		neighbor_vertices(Neighbors, Vertices).

	path_value(Vertex, Sink, Adjacency, Previous, Minimum0, Value) :-
		(	Vertex == Sink ->
			Value = Minimum0
		;	dict_lookup(Vertex, Neighbors, Adjacency),
			member(To-Capacity, Neighbors),
			Previous \== previous(To),
			(	Minimum0 == none ->
				Minimum = Capacity
			;	Minimum is min(Minimum0, Capacity)
			),
			path_value(To, Sink, Adjacency, previous(Vertex), Minimum, Value)
		).

:- end_object.


:- object(gomory_hu,
	extends(gomory_hu(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Gomory-Hu cut trees using the ``avltree`` dictionary.',
		see_also is [gomory_hu(_), dinic]
	]).

:- end_object.

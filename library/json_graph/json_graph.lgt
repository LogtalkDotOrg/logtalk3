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


:- object(json_graph,
	implements(json_graph_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'JSON Graph Format v2 parser and generator.'
	]).

	:- uses(list, [
		append/3, member/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	parse(Source, Terms) :-
		(   var(Source) ->
			instantiation_error
		;   Source = object(Object) ->
			object_terms(Object, Terms)
		;   catch(json::parse(Source, JSON), error(domain_error(json_source, Source), _), domain_error(json_graph_source, Source)) ->
			(   json_terms(JSON, Terms) ->
				true
			;   domain_error(json_graph, JSON)
			)
		;   domain_error(json_graph_source, Source)
		).

	generate(Sink, Terms) :-
		(   var(Sink) ->
			instantiation_error
		;   Sink = object(Object) ->
			terms_object(Terms, Object)
		;   terms_json(Terms, JSON) ->
			json::generate(Sink, JSON)
		;   domain_error(json_graph_terms, Terms)
		).

	object_terms(Object, Terms) :-
		(   conforms_to_protocol(Object, json_graph_data_protocol) ->
			findall(graph(GraphId, Properties), Object::graph(GraphId, Properties), GraphTerms),
			findall(node(GraphId, NodeId, Properties), Object::node(GraphId, NodeId, Properties), NodeTerms),
			findall(edge(GraphId, EdgeId, Source, Target, Properties), Object::edge(GraphId, EdgeId, Source, Target, Properties), EdgeTerms),
			findall(hyperedge(GraphId, HyperedgeId, Nodes, Properties), Object::hyperedge(GraphId, HyperedgeId, Nodes, Properties), UndirectedHyperedgeTerms),
			findall(hyperedge(GraphId, HyperedgeId, Sources, Targets, Properties), Object::hyperedge(GraphId, HyperedgeId, Sources, Targets, Properties), DirectedHyperedgeTerms),
			append(GraphTerms, NodeTerms, PartialTerms),
			append(PartialTerms, EdgeTerms, PartialTerms2),
			append(PartialTerms2, UndirectedHyperedgeTerms, PartialTerms3),
			append(PartialTerms3, DirectedHyperedgeTerms, Terms0),
			canonical_terms(Terms0, Terms)
		;   domain_error(json_graph_source, object(Object))
		).

	terms_object(Terms, Object) :-
		(   conforms_to_protocol(Object, json_graph_data_protocol) ->
			canonical_terms(Terms, CanonicalTerms),
			assert_terms(CanonicalTerms, Object)
		;   domain_error(json_graph_sink, object(Object))
		).

	assert_terms([], _).
	assert_terms([Term| Terms], Object) :-
		Object::assertz(Term),
		assert_terms(Terms, Object).

	json_terms({graph-Graph}, Terms) :-
		!,
		graph_json_terms(1, Graph, Terms).
	json_terms({graphs-Graphs}, Terms) :-
		!,
		graphs_unique_ids(Graphs),
		graphs_json_terms(Graphs, 1, Terms).
	json_terms(JSON, _) :-
		domain_error(json_graph, JSON).

	graphs_json_terms([], _, []).
	graphs_json_terms([Graph| Graphs], N, Terms) :-
		graph_json_terms(N, Graph, GraphTerms),
		M is N + 1,
		graphs_json_terms(Graphs, M, OtherTerms),
		append(GraphTerms, OtherTerms, Terms).

	graph_json_terms(DefaultGraphId, Graph, Terms) :-
		valid_graph_json(Graph),
		graph_id(Graph, DefaultGraphId, GraphId),
		graph_properties(Graph, GraphProperties),
		nodes_json_terms(GraphId, Graph, NodeTerms),
		edges_json_terms(GraphId, Graph, EdgeTerms),
		hyperedges_json_terms(GraphId, Graph, HyperedgeTerms),
		Terms = [graph(GraphId, GraphProperties)| RestTerms],
		append(NodeTerms, EdgeTerms, RestTerms0),
		append(RestTerms0, HyperedgeTerms, RestTerms),
		valid_graph_terms(GraphId, Terms).

	graph_id(Graph, _, GraphId) :-
		graph_member(id, Graph, JSONGraphId),
		canonical_json_id(JSONGraphId, GraphId),
		!.
	graph_id(_, DefaultGraphId, GraphId) :-
		atomic_concat(g, DefaultGraphId, GraphId).

	graph_properties(Graph, Properties) :-
		findall(Property, graph_property(Graph, Property), Properties).

	graph_property(Graph, directed(true)) :-
		\+ graph_member(directed, Graph, _).
	graph_property(Graph, directed(Directed)) :-
		graph_member(directed, Graph, JSONDirected),
		parse_boolean(JSONDirected, Directed).
	graph_property(Graph, type(Type)) :-
		graph_member(type, Graph, Type).
	graph_property(Graph, label(Label)) :-
		graph_member(label, Graph, Label).
	graph_property(Graph, metadata(Metadata)) :-
		graph_member(metadata, Graph, Metadata),
		json_object(Metadata).

	nodes_json_terms(GraphId, Graph, Terms) :-
		(   graph_member(nodes, Graph, Nodes) ->
			node_members(Nodes, GraphId, Terms)
		;   Terms = []
		).

	node_members({}, _, []) :-
		!.
	node_members({Pairs}, GraphId, Terms) :-
		!,
		pairs_list(Pairs, PairList),
		node_pairs_terms(PairList, GraphId, Terms).
	node_members(JSON, _, _) :-
		domain_error(json_graph, JSON).

	node_pairs_terms([], _, []).
	node_pairs_terms([JSONNodeId-Node| Pairs], GraphId, [node(GraphId, NodeId, Properties)| Terms]) :-
		canonical_json_id(JSONNodeId, NodeId),
		node_properties(Node, Properties),
		node_pairs_terms(Pairs, GraphId, Terms).

	node_properties(Node, Properties) :-
		json_object(Node),
		findall(Property, node_property(Node, Property), Properties).

	node_property(Node, label(Label)) :-
		json_member(label, Node, Label).
	node_property(Node, metadata(Metadata)) :-
		json_member(metadata, Node, Metadata),
		json_object(Metadata).

	edges_json_terms(GraphId, Graph, Terms) :-
		(   graph_member(edges, Graph, Edges) ->
			edge_list_terms(Edges, GraphId, 1, Terms)
		;   Terms = []
		).

	edge_list_terms([], _, _, []).
	edge_list_terms([Edge| Edges], GraphId, N, [edge(GraphId, EdgeId, Source, Target, Properties)| Terms]) :-
		edge_id(Edge, N, EdgeId),
		required_member(source, Edge, JSONSource),
		required_member(target, Edge, JSONTarget),
		canonical_json_id(JSONSource, Source),
		canonical_json_id(JSONTarget, Target),
		edge_properties(Edge, Properties),
		M is N + 1,
		edge_list_terms(Edges, GraphId, M, Terms).

	edge_id(Edge, _, EdgeId) :-
		json_member(id, Edge, JSONEdgeId),
		canonical_json_id(JSONEdgeId, EdgeId),
		!.
	edge_id(_, DefaultEdgeId, EdgeId) :-
		atomic_concat(e, DefaultEdgeId, EdgeId).

	edge_properties(Edge, Properties) :-
		findall(Property, edge_property(Edge, Property), Properties).

	edge_property(Edge, relation(Relation)) :-
		json_member(relation, Edge, Relation).
	edge_property(Edge, directed(true)) :-
		\+ json_member(directed, Edge, _).
	edge_property(Edge, directed(Directed)) :-
		json_member(directed, Edge, JSONDirected),
		parse_boolean(JSONDirected, Directed).
	edge_property(Edge, label(Label)) :-
		json_member(label, Edge, Label).
	edge_property(Edge, metadata(Metadata)) :-
		json_member(metadata, Edge, Metadata),
		json_object(Metadata).

	hyperedges_json_terms(GraphId, Graph, Terms) :-
		(   graph_member(hyperedges, Graph, Hyperedges) ->
			hyperedge_list_terms(Hyperedges, GraphId, 1, Terms)
		;   Terms = []
		).

	hyperedge_list_terms([], _, _, []).
	hyperedge_list_terms([Hyperedge| Hyperedges], GraphId, N, [Term| Terms]) :-
		hyperedge_id(Hyperedge, N, HyperedgeId),
		hyperedge_term(GraphId, HyperedgeId, Hyperedge, Term),
		M is N + 1,
		hyperedge_list_terms(Hyperedges, GraphId, M, Terms).

	hyperedge_id(Hyperedge, _, HyperedgeId) :-
		json_member(id, Hyperedge, JSONHyperedgeId),
		canonical_json_id(JSONHyperedgeId, HyperedgeId),
		!.
	hyperedge_id(_, DefaultHyperedgeId, HyperedgeId) :-
		atomic_concat(he, DefaultHyperedgeId, HyperedgeId).

	hyperedge_term(GraphId, HyperedgeId, Hyperedge, hyperedge(GraphId, HyperedgeId, Nodes, Properties)) :-
		json_member(nodes, Hyperedge, JSONNodes),
		!,
		json_ids(JSONNodes, Nodes),
		hyperedge_properties(Hyperedge, Properties).
	hyperedge_term(GraphId, HyperedgeId, Hyperedge, hyperedge(GraphId, HyperedgeId, Sources, Targets, Properties)) :-
		required_member(source, Hyperedge, JSONSources),
		required_member(target, Hyperedge, JSONTargets),
		json_ids(JSONSources, Sources),
		json_ids(JSONTargets, Targets),
		hyperedge_properties(Hyperedge, Properties).

	json_ids([], []).
	json_ids([JSONId| JSONIds], [Id| Ids]) :-
		canonical_json_id(JSONId, Id),
		json_ids(JSONIds, Ids).

	hyperedge_properties(Hyperedge, Properties) :-
		findall(Property, hyperedge_property(Hyperedge, Property), Properties).

	hyperedge_property(Hyperedge, relation(Relation)) :-
		json_member(relation, Hyperedge, Relation).
	hyperedge_property(Hyperedge, label(Label)) :-
		json_member(label, Hyperedge, Label).
	hyperedge_property(Hyperedge, metadata(Metadata)) :-
		json_member(metadata, Hyperedge, Metadata),
		json_object(Metadata).

	terms_json(Terms, JSON) :-
		canonical_terms(Terms, CanonicalTerms),
		terms_graphs(CanonicalTerms, Graphs),
		(   Graphs = [Graph] ->
			JSON = {graph-Graph}
		;   JSON = {graphs-Graphs}
		).

	terms_graphs(Terms, Graphs) :-
		graph_ids(Terms, GraphIds),
		graphs_from_ids(GraphIds, Terms, Graphs).

	graph_ids([], []).
	graph_ids([graph(GraphId, _)| Terms], GraphIds) :-
		!,
		graph_ids(Terms, RestGraphIds),
		add_unique(GraphId, RestGraphIds, GraphIds).
	graph_ids([_| Terms], GraphIds) :-
		graph_ids(Terms, GraphIds).

	graphs_from_ids([], _, []).
	graphs_from_ids([GraphId| GraphIds], Terms, [Graph| Graphs]) :-
		graph_from_id(GraphId, Terms, Graph),
		graphs_from_ids(GraphIds, Terms, Graphs).

	graph_from_id(GraphId, Terms, Graph) :-
		member(graph(GraphId, Properties), Terms),
		graph_nodes(GraphId, Terms, Nodes),
		graph_edges(GraphId, Terms, Edges),
		graph_hyperedges(GraphId, Terms, Hyperedges),
		graph_pairs(GraphId, Properties, Nodes, Edges, Hyperedges, Pairs),
		Graph = {Pairs}.

	graph_pairs(GraphId, Properties, Nodes, Edges, Hyperedges, Pairs) :-
		base_graph_pairs(GraphId, Properties, Nodes, Edges, Hyperedges, BasePairs),
		pairs_term(BasePairs, Pairs).

	base_graph_pairs(GraphId, Properties, Nodes, Edges, [], [id-JSONGraphId| Tail]) :-
		json_id(GraphId, JSONGraphId),
		graph_properties_pairs(Properties, PropertyPairs),
		append(PropertyPairs, [nodes-Nodes, edges-Edges], Tail).
	base_graph_pairs(GraphId, Properties, Nodes, _Edges, Hyperedges, [id-JSONGraphId| Tail]) :-
		Hyperedges \== [],
		json_id(GraphId, JSONGraphId),
		graph_properties_pairs(Properties, PropertyPairs),
		append(PropertyPairs, [nodes-Nodes, hyperedges-Hyperedges], Tail).

	graph_nodes(GraphId, Terms, Nodes) :-
		findall(JSONNodeId-Node, (member(node(GraphId, NodeId, Properties), Terms), json_id(NodeId, JSONNodeId), node_json(Properties, Node)), NodesList),
		pairs_object(NodesList, Nodes).

	graph_edges(GraphId, Terms, Edges) :-
		findall(Edge, (member(edge(GraphId, EdgeId, Source, Target, Properties), Terms), edge_json(EdgeId, Source, Target, Properties, Edge)), Edges).

	graph_hyperedges(GraphId, Terms, Hyperedges) :-
		findall(Hyperedge, (member(hyperedge(GraphId, HyperedgeId, Nodes, Properties), Terms), undirected_hyperedge_json(HyperedgeId, Nodes, Properties, Hyperedge)), UndirectedHyperedges),
		findall(Hyperedge, (member(hyperedge(GraphId, HyperedgeId, Sources, Targets, Properties), Terms), directed_hyperedge_json(HyperedgeId, Sources, Targets, Properties, Hyperedge)), DirectedHyperedges),
		append(UndirectedHyperedges, DirectedHyperedges, Hyperedges).

	node_json(Properties, Node) :-
		node_properties_pairs(Properties, PairList),
		pairs_object(PairList, Node).

	edge_json(EdgeId, Source, Target, Properties, {Pairs}) :-
		json_id(EdgeId, JSONEdgeId),
		json_id(Source, JSONSource),
		json_id(Target, JSONTarget),
		edge_properties_pairs(Properties, PropertyPairs),
		PairList = [id-JSONEdgeId, source-JSONSource, target-JSONTarget| PropertyPairs],
		pairs_term(PairList, Pairs).

	undirected_hyperedge_json(HyperedgeId, Nodes, Properties, {Pairs}) :-
		json_id(HyperedgeId, JSONHyperedgeId),
		json_ids(Nodes, JSONNodes),
		hyperedge_properties_pairs(Properties, PropertyPairs),
		PairList = [id-JSONHyperedgeId, nodes-JSONNodes| PropertyPairs],
		pairs_term(PairList, Pairs).

	directed_hyperedge_json(HyperedgeId, Sources, Targets, Properties, {Pairs}) :-
		json_id(HyperedgeId, JSONHyperedgeId),
		json_ids(Sources, JSONSources),
		json_ids(Targets, JSONTargets),
		hyperedge_properties_pairs(Properties, PropertyPairs),
		PairList = [id-JSONHyperedgeId, source-JSONSources, target-JSONTargets| PropertyPairs],
		pairs_term(PairList, Pairs).

	graph_properties_pairs(Properties, Pairs) :-
		property_pair(directed, Properties, DirectedPairs),
		property_pair(type, Properties, TypePairs),
		property_pair(label, Properties, LabelPairs),
		property_pair(metadata, Properties, MetadataPairs),
		append(DirectedPairs, TypePairs, Pairs0),
		append(Pairs0, LabelPairs, Pairs1),
		append(Pairs1, MetadataPairs, Pairs).

	node_properties_pairs(Properties, Pairs) :-
		property_pair(label, Properties, LabelPairs),
		property_pair(metadata, Properties, MetadataPairs),
		append(LabelPairs, MetadataPairs, Pairs).

	edge_properties_pairs(Properties, Pairs) :-
		property_pair(relation, Properties, RelationPairs),
		property_pair(directed, Properties, DirectedPairs),
		property_pair(label, Properties, LabelPairs),
		property_pair(metadata, Properties, MetadataPairs),
		append(RelationPairs, DirectedPairs, Pairs0),
		append(Pairs0, LabelPairs, Pairs1),
		append(Pairs1, MetadataPairs, Pairs).

	hyperedge_properties_pairs(Properties, Pairs) :-
		property_pair(relation, Properties, RelationPairs),
		property_pair(label, Properties, LabelPairs),
		property_pair(metadata, Properties, MetadataPairs),
		append(RelationPairs, LabelPairs, Pairs0),
		append(Pairs0, MetadataPairs, Pairs).

	property_pair(directed, Properties, [directed-JSONDirected]) :-
		member(directed(Directed), Properties),
		!,
		generate_boolean(Directed, JSONDirected).
	property_pair(type, Properties, [type-Type]) :-
		member(type(Type), Properties),
		!.
	property_pair(label, Properties, [label-Label]) :-
		member(label(Label), Properties),
		!.
	property_pair(relation, Properties, [relation-Relation]) :-
		member(relation(Relation), Properties),
		!.
	property_pair(metadata, Properties, [metadata-Metadata]) :-
		member(metadata(Metadata), Properties),
		!.
	property_pair(_, _, []).

	canonical_terms(Terms, Terms) :-
		(   var(Terms) ->
			instantiation_error
		;   valid_terms(Terms),
			valid_terms_semantics(Terms) ->
			true
		;   domain_error(json_graph_terms, Terms)
		).

	valid_terms([]).
	valid_terms([Term| Terms]) :-
		valid_term(Term),
		valid_terms(Terms).

	valid_term(graph(GraphId, Properties)) :-
		valid_identifier(GraphId),
		valid_properties(Properties).
	valid_term(node(GraphId, NodeId, Properties)) :-
		valid_identifier(GraphId),
		valid_identifier(NodeId),
		valid_properties(Properties).
	valid_term(edge(GraphId, EdgeId, Source, Target, Properties)) :-
		valid_identifier(GraphId),
		valid_identifier(EdgeId),
		valid_identifier(Source),
		valid_identifier(Target),
		valid_properties(Properties).
	valid_term(hyperedge(GraphId, HyperedgeId, Nodes, Properties)) :-
		valid_identifier(GraphId),
		valid_identifier(HyperedgeId),
		valid_identifier_list(Nodes),
		valid_properties(Properties).
	valid_term(hyperedge(GraphId, HyperedgeId, Sources, Targets, Properties)) :-
		valid_identifier(GraphId),
		valid_identifier(HyperedgeId),
		valid_identifier_list(Sources),
		valid_identifier_list(Targets),
		valid_properties(Properties).

	valid_properties([]).
	valid_properties([Property| Properties]) :-
		valid_property(Property),
		valid_properties(Properties).

	valid_property(directed(Directed)) :-
		valid_logtalk_boolean(Directed).
	valid_property(type(Type)) :-
		nonvar(Type).
	valid_property(label(Label)) :-
		nonvar(Label).
	valid_property(relation(Relation)) :-
		nonvar(Relation).
	valid_property(metadata(Metadata)) :-
		json_object(Metadata).

	valid_terms_semantics(Terms) :-
		graph_ids(Terms, GraphIds),
		GraphIds \== [],
		valid_graph_ids(GraphIds, Terms).

	valid_graph_ids([], _).
	valid_graph_ids([GraphId| GraphIds], Terms) :-
		valid_graph_terms(GraphId, Terms),
		valid_graph_ids(GraphIds, Terms).

	valid_graph_terms(GraphId, Terms) :-
		findall(Properties, member(graph(GraphId, Properties), Terms), Graphs),
		Graphs = [GraphProperties],
		findall(NodeId, member(node(GraphId, NodeId, _), Terms), NodeIds),
		unique_list(NodeIds),
		findall(EdgeId, member(edge(GraphId, EdgeId, _, _, _), Terms), EdgeIds),
		unique_list(EdgeIds),
		findall(HyperedgeId, member(hyperedge(GraphId, HyperedgeId, _, _), Terms), UndirectedHyperedgeIds),
		unique_list(UndirectedHyperedgeIds),
		findall(HyperedgeId, member(hyperedge(GraphId, HyperedgeId, _, _, _), Terms), DirectedHyperedgeIds),
		unique_list(DirectedHyperedgeIds),
		append(UndirectedHyperedgeIds, DirectedHyperedgeIds, HyperedgeIds),
		unique_list(HyperedgeIds),
		valid_graph_shape(GraphProperties, EdgeIds, UndirectedHyperedgeIds, DirectedHyperedgeIds),
		valid_edge_references(GraphId, Terms, NodeIds),
		valid_hyperedge_references(GraphId, Terms, NodeIds).

	valid_graph_shape(_, _, [], []).
	valid_graph_shape(GraphProperties, [], UndirectedHyperedgeIds, []) :-
		UndirectedHyperedgeIds \== [],
		member(directed(false), GraphProperties).
	valid_graph_shape(GraphProperties, [], [], DirectedHyperedgeIds) :-
		DirectedHyperedgeIds \== [],
		\+ member(directed(false), GraphProperties).

	valid_edge_references(GraphId, Terms, NodeIds) :-
		findall(Source-Target, member(edge(GraphId, _, Source, Target, _), Terms), References),
		(   NodeIds == [] ->
			References == []
		;   valid_references(References, NodeIds)
		).

	valid_references([], _).
	valid_references([Source-Target| References], NodeIds) :-
		member(Source, NodeIds),
		member(Target, NodeIds),
		valid_references(References, NodeIds).

	valid_hyperedge_references(GraphId, Terms, NodeIds) :-
		findall(Nodes, member(hyperedge(GraphId, _, Nodes, _), Terms), UndirectedNodeLists),
		findall(Sources, member(hyperedge(GraphId, _, Sources, _, _), Terms), SourceLists),
		findall(Targets, member(hyperedge(GraphId, _, _, Targets, _), Terms), TargetLists),
		valid_node_lists(UndirectedNodeLists, NodeIds),
		valid_node_lists(SourceLists, NodeIds),
		valid_node_lists(TargetLists, NodeIds).

	valid_node_lists([], _).
	valid_node_lists([Nodes| NodeLists], NodeIds) :-
		valid_node_references(Nodes, NodeIds),
		valid_node_lists(NodeLists, NodeIds).

	valid_node_references([], _).
	valid_node_references([Node| Nodes], NodeIds) :-
		member(Node, NodeIds),
		valid_node_references(Nodes, NodeIds).

	valid_logtalk_boolean(true).
	valid_logtalk_boolean(false).

	valid_identifier(Id) :-
		atom(Id).

	valid_identifier_list([Id| Ids]) :-
		valid_identifier(Id),
		valid_identifiers(Ids),
		unique_list([Id| Ids]).

	valid_identifiers([]).
	valid_identifiers([Id| Ids]) :-
		valid_identifier(Id),
		valid_identifiers(Ids).

	valid_json_identifier(Id) :-
		atom(Id).

	parse_boolean(@true, true).
	parse_boolean(@false, false).
	parse_boolean(true, true).
	parse_boolean(false, false).

	generate_boolean(true, @true).
	generate_boolean(false, @false).

	json_id(Id, Id).

	canonical_json_id(Id, Id).

	json_object({}).
	json_object({_}).

	valid_graph_json(Graph) :-
		json_object(Graph),
		known_keys(Graph, [id, label, directed, type, metadata, nodes, edges, hyperedges]),
		(   graph_member(id, Graph, GraphId) ->
			valid_json_identifier(GraphId)
		;   true
		),
		(   graph_member(directed, Graph, Directed) ->
			parse_boolean(Directed, _)
		;   true
		),
		(   graph_member(metadata, Graph, Metadata) ->
			json_object(Metadata)
		;   true
		),
		(   graph_member(nodes, Graph, Nodes) ->
			valid_nodes_json(Nodes)
		;   true
		),
		(   graph_member(edges, Graph, Edges) ->
			valid_edges_json(Edges)
		;   true
		),
		valid_graph_shape_json(Graph).

	valid_graph_shape_json(Graph) :-
		(   graph_member(hyperedges, Graph, Hyperedges) ->
			\+ graph_member(edges, Graph, _),
			graph_directed_json(Graph, Directed),
			valid_hyperedges_json(Directed, Hyperedges)
		;   true
		).

	graph_directed_json(Graph, Directed) :-
		(   graph_member(directed, Graph, JSONDirected) ->
			parse_boolean(JSONDirected, Directed)
		;   Directed = true
		).

	valid_nodes_json(Nodes) :-
		json_object(Nodes),
		pairs_list_from_object(Nodes, Pairs),
		pairs_keys(Pairs, NodeIds),
		valid_json_identifiers(NodeIds),
		unique_list(NodeIds),
		valid_node_pairs(Pairs).

	valid_node_pairs([]).
	valid_node_pairs([_-Node| Pairs]) :-
		json_object(Node),
		known_keys(Node, [label, metadata]),
		(   json_member(metadata, Node, Metadata) ->
			json_object(Metadata)
		;   true
		),
		valid_node_pairs(Pairs).

	valid_edges_json([]).
	valid_edges_json([Edge| Edges]) :-
		json_object(Edge),
		known_keys(Edge, [id, source, target, relation, directed, label, metadata]),
		required_member(source, Edge, Source),
		valid_json_identifier(Source),
		required_member(target, Edge, Target),
		valid_json_identifier(Target),
		(   json_member(id, Edge, EdgeId) ->
			valid_json_identifier(EdgeId)
		;   true
		),
		(   json_member(directed, Edge, Directed) ->
			parse_boolean(Directed, _)
		;   true
		),
		(   json_member(metadata, Edge, Metadata) ->
			json_object(Metadata)
		;   true
		),
		valid_edges_json(Edges).

	valid_hyperedges_json(_, []).
	valid_hyperedges_json(false, [Hyperedge| Hyperedges]) :-
		valid_undirected_hyperedge_json(Hyperedge),
		valid_hyperedges_json(false, Hyperedges).
	valid_hyperedges_json(true, [Hyperedge| Hyperedges]) :-
		valid_directed_hyperedge_json(Hyperedge),
		valid_hyperedges_json(true, Hyperedges).

	valid_undirected_hyperedge_json(Hyperedge) :-
		json_object(Hyperedge),
		known_keys(Hyperedge, [id, nodes, relation, label, metadata]),
		required_member(nodes, Hyperedge, Nodes),
		valid_json_identifier_list(Nodes),
		valid_hyperedge_common_json(Hyperedge).

	valid_directed_hyperedge_json(Hyperedge) :-
		json_object(Hyperedge),
		known_keys(Hyperedge, [id, source, target, relation, label, metadata]),
		required_member(source, Hyperedge, Sources),
		valid_json_identifier_list(Sources),
		required_member(target, Hyperedge, Targets),
		valid_json_identifier_list(Targets),
		valid_hyperedge_common_json(Hyperedge).

	valid_hyperedge_common_json(Hyperedge) :-
		(   json_member(id, Hyperedge, HyperedgeId) ->
			valid_json_identifier(HyperedgeId)
		;   true
		),
		(   json_member(relation, Hyperedge, Relation) ->
			valid_json_identifier(Relation)
		;   true
		),
		(   json_member(label, Hyperedge, Label) ->
			valid_json_identifier(Label)
		;   true
		),
		(   json_member(metadata, Hyperedge, Metadata) ->
			json_object(Metadata)
		;   true
		).

	graphs_unique_ids(Graphs) :-
		findall(GraphId, (member(Graph, Graphs), json_member(id, Graph, GraphId)), GraphIds),
		unique_list(GraphIds).

	valid_json_identifiers([]).
	valid_json_identifiers([Id| Ids]) :-
		valid_json_identifier(Id),
		valid_json_identifiers(Ids).

	valid_json_identifier_list([Id| Ids]) :-
		valid_json_identifier(Id),
		valid_json_identifiers(Ids),
		unique_list([Id| Ids]).

	known_keys(JSON, AllowedKeys) :-
		pairs_list_from_object(JSON, Pairs),
		pairs_keys(Pairs, Keys),
		all_known_keys(Keys, AllowedKeys).

	all_known_keys([], _).
	all_known_keys([Key| Keys], AllowedKeys) :-
		member(Key, AllowedKeys),
		all_known_keys(Keys, AllowedKeys).

	pairs_list_from_object({}, []).
	pairs_list_from_object({Pairs}, PairList) :-
		pairs_list(Pairs, PairList).

	pairs_keys([], []).
	pairs_keys([Key-_| Pairs], [Key| Keys]) :-
		pairs_keys(Pairs, Keys).

	unique_list([]).
	unique_list([Item| Items]) :-
		\+ member(Item, Items),
		unique_list(Items).

	graph_member(Key, Graph, Value) :-
		json_member(Key, Graph, Value).

	required_member(Key, JSON, Value) :-
		(   json_member(Key, JSON, Value) ->
			true
		;   domain_error(json_graph, JSON)
		).

	json_member(Key, {Pairs}, Value) :-
		pairs_list(Pairs, PairList),
		member(Key-Value, PairList).

	pairs_list((Pair, Pairs), [Pair| PairList]) :-
		!,
		pairs_list(Pairs, PairList).
	pairs_list(Pair, [Pair]).

	pairs_term([Pair], Pair) :-
		!.
	pairs_term([Pair| Pairs], (Pair, Tail)) :-
		pairs_term(Pairs, Tail).
	pairs_term([], {}).

	pairs_object([], {}) :-
		!.
	pairs_object(Pairs, {PairsTerm}) :-
		pairs_term(Pairs, PairsTerm).

	add_unique(Term, Terms, Terms) :-
		member(Term, Terms),
		!.
	add_unique(Term, Terms, [Term| Terms]).

:- end_object.

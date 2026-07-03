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


:- object(test_graph_store,
	implements(json_graph_data_protocol)).

	:- dynamic([
		graph/2,
		node/3,
		edge/5,
		hyperedge/4,
		hyperedge/5
	]).

:- end_object.


:- object(source_graph,
	implements(json_graph_data_protocol)).

	graph(g1, [label('Example'), directed(true)]).
	node(g1, n1, [label('one')]).
	edge(g1, e1, n1, n1, [directed(true)]).

:- end_object.


:- object(source_hypergraphs,
	implements(json_graph_data_protocol)).

	graph(g2, [label('Directed hypergraph'), directed(true)]).
	graph(g3, [label('Undirected hypergraph'), directed(false)]).

	node(g2, a, []).
	node(g2, b, []).
	node(g2, c, []).
	node(g3, x, []).
	node(g3, y, []).

	hyperedge(g2, he1, [a], [b, c], [relation(links)]).
	hyperedge(g3, he1, [x, y], [metadata({weight-1})]).

:- end_object.


:- object(non_graph_source).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'Tests for the JSON Graph Format v2 library.'
	]).

	cover(json_graph).

	:- uses(list, [
		member/2
	]).

	setup :-
		test_graph_store::retractall(graph(_, _)),
		test_graph_store::retractall(node(_, _, _)),
		test_graph_store::retractall(edge(_, _, _, _, _)),
		test_graph_store::retractall(hyperedge(_, _, _, _)),
		test_graph_store::retractall(hyperedge(_, _, _, _, _)),
		^^clean_file('test_files/roundtrip_output.json').

	cleanup :-
		setup.

	test(parse_single_graph_atom, true(Terms == [graph(g, [directed(true), label('Example')]), node(g, n1, [label(one)]), edge(g, e1, n1, n1, [directed(true)])])) :-
		JSON = '{"graph":{"id":"g","label":"Example","nodes":{"n1":{"label":"one"}},"edges":[{"source":"n1","target":"n1"}]}}',
		json_graph::parse(atom(JSON), Terms).

	test(generate_single_graph_atom, true(JSON == '{"graph":{"id":"g","directed":true,"label":"Example","nodes":{"n1":{"label":"one"}},"edges":[{"id":"e1","source":"n1","target":"n1","directed":true}]}}')) :-
		Terms = [graph(g, [label('Example'), directed(true)]), node(g, n1, [label(one)]), edge(g, e1, n1, n1, [directed(true)])],
		json_graph::generate(atom(JSON), Terms).

	test(parse_multiple_graphs_atom, true(Terms == [graph(g1, [directed(true), label(one)]), node(g1, n1, []), graph(g2, [directed(false), label(two)]), node(g2, n2, [metadata({flag- @true})]), edge(g2, e1, n2, n2, [directed(false)])])) :-
		JSON = '{"graphs":[{"id":"g1","label":"one","nodes":{"n1":{}},"edges":[]},{"label":"two","directed":false,"nodes":{"n2":{"metadata":{"flag":true}}},"edges":[{"id":"e1","source":"n2","target":"n2","directed":false}]}]}',
		json_graph::parse(atom(JSON), Terms).

	test(generate_multiple_graphs_atom, true(JSON == '{"graphs":[{"id":"g1","directed":true,"label":"one","nodes":{"n1":{}},"edges":[]},{"id":"g2","directed":false,"label":"two","nodes":{"n2":{"metadata":{"flag":true}}},"edges":[{"id":"e1","source":"n2","target":"n2","directed":false}]}]}')) :-
		Terms = [graph(g1, [label(one), directed(true)]), node(g1, n1, []), graph(g2, [label(two), directed(false)]), node(g2, n2, [metadata({flag- @true})]), edge(g2, e1, n2, n2, [directed(false)])],
		json_graph::generate(atom(JSON), Terms).

	test(parse_object_source, true(Terms == [graph(g1, [label('Example'), directed(true)]), node(g1, n1, [label('one')]), edge(g1, e1, n1, n1, [directed(true)])])) :-
		json_graph::parse(object(source_graph), Terms).

	test(parse_invalid_source_throws, error(domain_error(json_graph_source, xml('<graph/>')))) :-
		json_graph::parse(xml('<graph/>'), _).

	test(parse_non_graph_object_source_throws, error(domain_error(json_graph_source, object(non_graph_source)))) :-
		json_graph::parse(object(non_graph_source), _).

	test(generate_object_sink, true) :-
		Terms = [graph(g2, [label(example)]), node(g2, n2, [label(two)]), edge(g2, e7, n2, n2, [directed(true)])],
		json_graph::generate(object(test_graph_store), Terms),
		test_graph_store::graph(g2, [label(example)]),
		test_graph_store::node(g2, n2, [label(two)]),
		test_graph_store::edge(g2, e7, n2, n2, [directed(true)]).

	test(parse_object_source_directed_hyperedge, true) :-
		json_graph::parse(object(source_hypergraphs), Terms),
		member(hyperedge(g2, he1, [a], [b, c], [relation(links)]), Terms).

	test(parse_object_source_undirected_hyperedge, true) :-
		json_graph::parse(object(source_hypergraphs), Terms),
		member(hyperedge(g3, he1, [x, y], [metadata({weight-1})]), Terms).

	test(generate_object_sink_directed_hyperedge, true) :-
		Terms = [graph(g4, [directed(true)]), node(g4, a, []), node(g4, b, []), hyperedge(g4, he1, [a], [b], [relation(points_to)])],
		json_graph::generate(object(test_graph_store), Terms),
		test_graph_store::hyperedge(g4, he1, [a], [b], [relation(points_to)]).

	test(generate_object_sink_undirected_hyperedge, true) :-
		Terms = [graph(g5, [directed(false)]), node(g5, a, []), node(g5, b, []), hyperedge(g5, he1, [a, b], [metadata({weight-2})])],
		json_graph::generate(object(test_graph_store), Terms),
		test_graph_store::hyperedge(g5, he1, [a, b], [metadata({weight-2})]).

	test(roundtrip_single_graph_terms, true(Terms == RoundtripTerms)) :-
		Terms = [graph(g, [directed(true), label('Example')]), node(g, n1, [label(one)]), edge(g, e1, n1, n1, [directed(true)])],
		roundtrip_via_atom(Terms, RoundtripTerms).

	test(roundtrip_multiple_graphs_terms, true(Terms == RoundtripTerms)) :-
		Terms = [graph(g1, [directed(true), label(one)]), node(g1, n1, []), graph(g2, [directed(false), label(two)]), node(g2, n2, [metadata({flag- @true})]), edge(g2, e1, n2, n2, [directed(false)])],
		roundtrip_via_atom(Terms, RoundtripTerms).

	test(roundtrip_generated_file, true(RoundtripTerms == [graph(g3, [directed(true), type(example), label(example), metadata({kind-test})]), node(g3, n3, [metadata({x-1, y-2})]), edge(g3, e5, n3, n3, [relation(self), directed(true), metadata({weight-1})])])) :-
		Terms = [graph(g3, [directed(true), type(example), label(example), metadata({kind-test})]), node(g3, n3, [metadata({x-1, y-2})]), edge(g3, e5, n3, n3, [relation(self), directed(true), metadata({weight-1})])],
		roundtrip_via_file(Terms, RoundtripTerms).

	test(roundtrip_public_fixture_usual_suspects, true(Terms == RoundtripTerms)) :-
		roundtrip_fixture_via_atom('test_files/usual_suspects.json', Terms, RoundtripTerms).

	test(parse_public_fixture_empty_test, true(Terms == [graph('', [directed(true), type('schema test'), label('empty test'), metadata({})])])) :-
		^^file_path('test_files/empty_test.json', Path),
		json_graph::parse(file(Path), Terms).

	test(roundtrip_public_fixture_empty_test, true(Terms == RoundtripTerms)) :-
		roundtrip_fixture_via_atom('test_files/empty_test.json', Terms, RoundtripTerms).

	test(roundtrip_public_fixture_car_graphs, true(Terms == RoundtripTerms)) :-
		roundtrip_fixture_via_atom('test_files/car_graphs.json', Terms, RoundtripTerms).

	test(roundtrip_public_fixture_test_network, true(Terms == RoundtripTerms)) :-
		roundtrip_fixture_via_atom('test_files/test.network.json', Terms, RoundtripTerms).

	test(roundtrip_public_fixture_les_miserables, true(Terms == RoundtripTerms)) :-
		roundtrip_fixture_via_atom('test_files/les_miserables.json', Terms, RoundtripTerms).

	test(parse_public_fixture_hyper_directed, true) :-
		^^file_path('test_files/hyper-directed.json', Path),
		json_graph::parse(file(Path), Terms),
		member(graph('1', [directed(true), type('weighted directed network'), label('None')]), Terms),
		member(hyperedge('1', he1, [a, b, c], [f, g], [metadata({weight-17})]), Terms).

	test(roundtrip_public_fixture_hyper_directed, true(Terms == RoundtripTerms)) :-
		roundtrip_fixture_via_atom('test_files/hyper-directed.json', Terms, RoundtripTerms).

	test(parse_public_fixture_hyper_undirected, true) :-
		^^file_path('test_files/hyper-undirected.json', Path),
		json_graph::parse(file(Path), Terms),
		member(graph('1', [directed(false), type('weighted network'), label('None')]), Terms),
		member(hyperedge('1', he1, [a, b, x], [metadata({weight-17})]), Terms).

	test(roundtrip_public_fixture_hyper_undirected, true(Terms == RoundtripTerms)) :-
		roundtrip_fixture_via_atom('test_files/hyper-undirected.json', Terms, RoundtripTerms).

	test(generate_directed_hyperedge_atom, true(JSON == '{"graph":{"id":"g","directed":true,"nodes":{"a":{},"b":{},"c":{}},"hyperedges":[{"id":"he1","source":["a"],"target":["b","c"],"relation":"links","metadata":{"weight":1}}]}}')) :-
		Terms = [graph(g, [directed(true)]), node(g, a, []), node(g, b, []), node(g, c, []), hyperedge(g, he1, [a], [b, c], [relation(links), metadata({weight-1})])],
		json_graph::generate(atom(JSON), Terms).

	test(generate_undirected_hyperedge_atom, true(JSON == '{"graph":{"id":"g","directed":false,"nodes":{"a":{},"b":{}},"hyperedges":[{"id":"he1","nodes":["a","b"],"metadata":{"weight":1}}]}}')) :-
		Terms = [graph(g, [directed(false)]), node(g, a, []), node(g, b, []), hyperedge(g, he1, [a, b], [metadata({weight-1})])],
		json_graph::generate(atom(JSON), Terms).

	test(parse_duplicate_graph_ids_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graphs":[{"id":"g","nodes":{},"edges":[]},{"id":"g","nodes":{},"edges":[]}]}',
		json_graph::parse(atom(JSON), _).

	test(parse_missing_edge_target_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"edges":[{"source":"n1"}]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_malformed_node_metadata_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{"metadata":1}},"edges":[]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_malformed_edge_metadata_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"edges":[{"source":"n1","target":"n1","metadata":1}]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_malformed_graph_metadata_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","metadata":1,"nodes":{},"edges":[]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_unknown_top_level_key_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{},"edges":[]},"extra":1}',
		json_graph::parse(atom(JSON), _).

	test(parse_unknown_graph_level_key_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{},"edges":[],"extra":1}}',
		json_graph::parse(atom(JSON), _).

	test(parse_unknown_edge_level_key_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"edges":[{"source":"n1","target":"n1","extra":1}]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_unknown_hyperedge_level_key_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"hyperedges":[{"source":["n1"],"target":["n1"],"extra":1}]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_numeric_edge_id_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"edges":[{"id":1,"source":"n1","target":"n1"}]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_numeric_graph_id_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":1,"nodes":{},"edges":[]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_numeric_hyperedge_id_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"hyperedges":[{"id":1,"source":["n1"],"target":["n1"]}]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_empty_hyperedge_sources_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"hyperedges":[{"source":[],"target":["n1"]}]}}',
		json_graph::parse(atom(JSON), _).

	test(parse_mixed_edges_and_hyperedges_throws, error(domain_error(json_graph, _))) :-
		JSON = '{"graph":{"id":"g","nodes":{"n1":{}},"edges":[{"source":"n1","target":"n1"}],"hyperedges":[{"source":["n1"],"target":["n1"]}]}}',
		json_graph::parse(atom(JSON), _).

	test(generate_integer_graph_identifier_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(1, [directed(true)]), node(n1, n1, [])],
		json_graph::generate(atom(_), Terms).

	test(generate_integer_node_identifier_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(g, [directed(true)]), node(g, 1, [])],
		json_graph::generate(atom(_), Terms).

	test(generate_integer_edge_identifier_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(g, [directed(true)]), node(g, n1, []), edge(g, 1, n1, n1, [])],
		json_graph::generate(atom(_), Terms).

	test(generate_duplicate_edge_ids_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(g, [directed(true)]), node(g, n1, []), edge(g, e1, n1, n1, []), edge(g, e1, n1, n1, [])],
		json_graph::generate(atom(_), Terms).

	test(generate_unknown_node_reference_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(g, [directed(true)]), node(g, n1, []), edge(g, e1, n1, n2, [])],
		json_graph::generate(atom(_), Terms).

	test(generate_unknown_hyperedge_node_reference_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(g, [directed(true)]), node(g, n1, []), hyperedge(g, he1, [n1], [n2], [])],
		json_graph::generate(atom(_), Terms).

	test(generate_mixed_edges_and_hyperedges_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(g, [directed(true)]), node(g, n1, []), edge(g, e1, n1, n1, []), hyperedge(g, he1, [n1], [n1], [])],
		json_graph::generate(atom(_), Terms).

	test(generate_undirected_hyperedge_without_undirected_graph_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(g, [directed(true)]), node(g, n1, []), hyperedge(g, he1, [n1], [])],
		json_graph::generate(atom(_), Terms).

	test(generate_compound_identifier_throws, error(domain_error(json_graph_terms, _))) :-
		Terms = [graph(graph(g), [directed(true)]), node(graph(g), n1, []), edge(graph(g), e1, n1, n1, [])],
		json_graph::generate(atom(_), Terms).

	% auxiliary predicates

	roundtrip_via_atom(Terms, RoundtripTerms) :-
		json_graph::generate(atom(JSON), Terms),
		json_graph::parse(atom(JSON), RoundtripTerms).

	roundtrip_via_file(Terms, RoundtripTerms) :-
		^^file_path('test_files/roundtrip_output.json', Path),
		json_graph::generate(file(Path), Terms),
		json_graph::parse(file(Path), RoundtripTerms).

	roundtrip_fixture_via_atom(File, Terms, RoundtripTerms) :-
		^^file_path(File, Path),
		json_graph::parse(file(Path), Terms),
		roundtrip_via_atom(Terms, RoundtripTerms).

:- end_object.

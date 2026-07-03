________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`json_graph`
============

The `json_graph` library provides predicates for parsing and generating
JSON Graph Format (JGF) version 2 graphs:

https://jsongraphformat.info

The library main object, `json_graph`, converts between JGF JSON and a
canonical list of terms:

- `graph(GraphId, Properties)`
- `node(GraphId, NodeId, Properties)`
- `edge(GraphId, EdgeId, Source, Target, Properties)`
- `hyperedge(GraphId, HyperedgeId, Nodes, Properties)`
- `hyperedge(GraphId, HyperedgeId, Sources, Targets, Properties)`

Canonical graph, node, source, target, and edge identifiers are always atoms.
When the library needs to generate missing graph, edge, or hyperedge
identifiers while parsing JGF input, it uses atoms in the `gN`, `eN`, and
`heN` formats. Node identifiers are not generated because JGF nodes are stored
as object members keyed by their identifiers.

Undirected hyperedges use `hyperedge/4` and require their graph to have a
`directed(false)` property. Directed hyperedges use `hyperedge/5` and preserve
the JGF `source` and `target` node sets as separate lists. A graph uses either
ordinary edges or hyperedges, matching the JGF schema shape.

It also supports object-backed sources and sinks using the
`json_graph_data_protocol` protocol. Objects used as sinks should implement
that protocol and locally declare the `graph/2`, `node/3`, `edge/5`,
`hyperedge/4`, and `hyperedge/5` predicates as dynamic.


API documentation
-----------------

Open the [../../apis/library_index.html#json_graph](../../apis/library_index.html#json_graph)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(json_graph(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(json_graph(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(json_graph(tester_performance)).


Usage
-----

A JSON graph can be parsed into a list of canonical terms:

	| ?- json_graph::parse(atom('{"graph":{"id":"g","label":"Example","nodes":{"n1":{"label":"one"}},"edges":[{"source":"n1","target":"n1"}]}}'), Terms).

An object-backed graph can be exported by implementing the
`json_graph_data_protocol` protocol and defining `graph/2`, `node/3`,
`edge/5`, `hyperedge/4`, and `hyperedge/5` predicates.


Roundtrip and fixture coverage
------------------------------

The test suite includes roundtrip checks for generated single-graph and
multi-graph term representations, file sink roundtrips, and public JGF example
files saved under the `test_files/` directory.

The bundled public fixtures are sourced from the upstream JGF specification
examples repository:

- `test_files/car_graphs.json`
- `test_files/empty_test.json`
- `test_files/hyper-directed.json`
- `test_files/hyper-undirected.json`
- `test_files/les_miserables.json`
- `test_files/test.network.json`
- `test_files/usual_suspects.json`

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


:- protocol(max_flow_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Maximum flow and minimum cut protocol.',
		see_also is [flow_network_protocol, edmonds_karp(_), dinic(_)]
	]).

	:- public(max_flow/5).
	:- mode(max_flow(+compound, +ground, +ground, -integer, -list(compound)), one_or_error).
	:- info(max_flow/5, [
		comment is 'Computes a maximum flow from ``Source`` to ``Sink``. ``Flow`` is a list of ``flow(ArcId, From, To, Amount)`` terms with a positive amount, one per arc carrying flow, in the order of the network arcs. ``ArcId`` is the one-based position of the arc in the input list used to construct the network. Maximum flow currently requires zero lower bounds.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Flow'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(multi_source_max_flow/5).
	:- mode(multi_source_max_flow(+compound, +list(ground), +list(ground), -integer, -list(compound)), one_or_error).
	:- info(multi_source_max_flow/5, [
		comment is 'Computes a maximum flow from a non-empty set of distinct sources to a non-empty, disjoint set of distinct sinks. ``Flow`` uses the original network arc IDs and endpoints. Requires zero lower bounds.',
		argnames is ['Network', 'Sources', 'Sinks', 'Value', 'Flow'],
		exceptions is [
			'A terminal is not ground' - instantiation_error,
			'A terminal set is empty' - domain_error(non_empty_terminal_set, 'Terminals'),
			'A terminal is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'A terminal set contains duplicates' - domain_error(unique_terminals, 'Terminals'),
			'A source is also a sink' - consistency_error(disjoint_terminals, 'Sources', 'Sinks'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(node_capacity_max_flow/6).
	:- mode(node_capacity_max_flow(+compound, +ground, +ground, +list(pair), -integer, -list(compound)), one_or_error).
	:- info(node_capacity_max_flow/6, [
		comment is 'Computes a maximum flow subject to the given intermediate vertex capacities, represented as ``Vertex-Capacity`` pairs. Source and sink capacities are not constrained. ``Flow`` uses the original network arc IDs and endpoints. Requires zero lower bounds.',
		argnames is ['Network', 'Source', 'Sink', 'NodeCapacities', 'Value', 'Flow'],
		exceptions is [
			'A node capacity vertex is not ground' - instantiation_error,
			'A node capacity is not a non-negative integer' - domain_error(non_negative_integer, 'Capacity'),
			'A node capacity vertex is not in the network' - existence_error(vertex, 'Vertex'),
			'A node capacity is specified for a terminal' - domain_error(intermediate_vertex, 'Vertex'),
			'A node capacity is specified more than once for a vertex' - domain_error(unique_node_capacities, 'Vertex'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(min_cut/5).
	:- mode(min_cut(+compound, +ground, +ground, -integer, -compound), one_or_error).
	:- info(min_cut/5, [
		comment is 'Computes a minimum cut separating ``Source`` from ``Sink``. ``Cut`` is a ``cut(SourceSide, SinkSide, Arcs)`` term where ``SourceSide`` and ``SinkSide`` are sorted lists of vertices and ``Arcs`` is the list of ``arc(From, To, Capacity)`` terms for the arcs with positive capacity going from the source side to the sink side. ``Value`` is both the maximum flow value and the capacity of the cut. Minimum cut currently requires zero lower bounds.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Cut'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(sink_closest_min_cut/5).
	:- mode(sink_closest_min_cut(+compound, +ground, +ground, -integer, -compound), one_or_error).
	:- info(sink_closest_min_cut/5, [
		comment is 'Computes the sink-closest minimum cut. Its source side contains exactly the vertices that cannot reach the sink in the final residual graph. Requires zero lower bounds.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Cut'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(unique_min_cut/4).
	:- mode(unique_min_cut(+compound, +ground, +ground, -boolean), one_or_error).
	:- info(unique_min_cut/4, [
		comment is 'Checks whether the minimum cut separating the given source and sink is unique. ``Unique`` is ``true`` or ``false``. Requires zero lower bounds.',
		argnames is ['Network', 'Source', 'Sink', 'Unique'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(min_cuts/5).
	:- mode(min_cuts(+compound, +ground, +ground, -integer, -compound), zero_or_more_or_error).
	:- info(min_cuts/5, [
		comment is 'Enumerates all minimum cuts on backtracking as ``cut(SourceSide, SinkSide, Arcs)`` terms. Enumeration uses the strongly connected components of the final residual graph. Requires zero lower bounds.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Cut'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

:- end_protocol.

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


:- protocol(flow_network_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Protocol for flow network construction and inspection. A flow network is a directed graph where each arc has a non-negative integer lower bound and capacity, with the lower bound not exceeding the capacity, and an integer cost.',
		see_also is [flow_network(_), flow_network, max_flow_protocol, min_cost_flow_protocol]
	]).

	:- public(new/2).
	:- mode(new(+list(compound), --compound), one_or_error).
	:- info(new/2, [
		comment is 'Constructs a flow network from a list of arcs. Arcs are ``arc(From, To, Capacity)``, ``arc(From, To, Capacity, Cost)``, or ``arc(From, To, Lower, Capacity, Cost)`` terms. The default cost and lower bound are zero. Vertices can be any ground term. Parallel, antiparallel, and self-loop arcs are allowed.',
		argnames is ['Arcs', 'Network'],
		exceptions is [
			'``Arcs`` is a partial list or contains a non-ground vertex' - instantiation_error,
			'``Arcs`` is neither a variable nor a list' - type_error(list, 'Arcs'),
			'An element of ``Arcs`` is not an ``arc/3``, ``arc/4``, or ``arc/5`` term' - domain_error(flow_arc, 'Arc'),
			'A lower bound is not a non-negative integer' - domain_error(non_negative_integer, 'Lower'),
			'A lower bound exceeds the capacity' - consistency_error(arc_lower_bound_and_capacity, 'Lower', 'Capacity'),
			'A capacity is not a non-negative integer' - domain_error(non_negative_integer, 'Capacity'),
			'A cost is not an integer' - type_error(integer, 'Cost')
		]
	]).

	:- public(from_graph/2).
	:- mode(from_graph(+compound, --compound), one_or_error).
	:- info(from_graph/2, [
		comment is 'Constructs a flow network from a weighted directed graph (created using the ``graphs`` library with the same dictionary object). Edge weights are interpreted as capacities and all costs are zero.',
		argnames is ['Graph', 'Network']
	]).

	:- public(vertices/2).
	:- mode(vertices(+compound, -list), one).
	:- info(vertices/2, [
		comment is 'Returns the sorted list of vertices of a flow network (including vertices that only occur in self-loops).',
		argnames is ['Network', 'Vertices']
	]).

	:- public(arcs/2).
	:- mode(arcs(+compound, -list(compound)), one).
	:- info(arcs/2, [
		comment is 'Returns the arcs of a flow network as ``arc(From, To, Lower, Capacity, Cost)`` terms in the order they were given to ``new/2``, including self-loops.',
		argnames is ['Network', 'Arcs']
	]).

	:- public(flow_cost/3).
	:- mode(flow_cost(+compound, +list(compound), -integer), one_or_error).
	:- info(flow_cost/3, [
		comment is 'Computes the cost of a capacity-valid flow, without requiring conservation or optimality. Each entry must be a ``flow(ArcId, From, To, Amount)`` term with a unique one-based arc ID, matching endpoints, and a positive integer amount. Every arc lower and upper bound must be respected, including omitted mandatory arcs. Entries may be in any order. Self-loop flows contribute their full cost.',
		argnames is ['Network', 'Flow', 'Cost'],
		exceptions is [
			'``Flow`` is a variable or partial list, or an entry has a variable field' - instantiation_error,
			'``Flow`` is neither a variable nor a list' - type_error(list, 'Flow'),
			'An entry is not a ``flow/4`` term' - domain_error(flow, 'Term'),
			'An arc ID or amount is not an integer' - type_error(integer, 'Value'),
			'An arc ID or amount is not positive' - domain_error(positive_integer, 'Value'),
			'An arc ID is not in the network' - existence_error(arc, 'ArcId'),
			'An entry has incorrect endpoints' - domain_error(flow_arc_endpoints, 'Term'),
			'Arc IDs are repeated or an arc bound is violated' - domain_error(capacity_valid_flow, 'Flow')
		]
	]).

	:- public(flow_balances/3).
	:- mode(flow_balances(+compound, +list(compound), -list(pair)), one_or_error).
	:- info(flow_balances/3, [
		comment is 'Returns sorted ``Vertex-NetOutflow`` pairs for every vertex, including zero balances. Positive balances indicate supplies and negative balances indicate demands. Self-loops contribute zero net outflow. Requires a capacity-valid flow with the same validation and exceptions as ``flow_cost/3``, but does not require conservation against particular supplies.',
		argnames is ['Network', 'Flow', 'Balances']
	]).

	:- public(cut_capacity/3).
	:- mode(cut_capacity(+compound, +list(ground), -integer), one_or_error).
	:- info(cut_capacity/3, [
		comment is 'Computes the capacity of the cut whose source side is the given vertex set and whose sink side is its complement. Input order is immaterial; vertices must be unique and belong to the network. Empty and full sets are allowed. Counts parallel crossing arcs individually and excludes self-loops. Requires zero lower bounds.',
		argnames is ['Network', 'SourceSide', 'Capacity'],
		exceptions is [
			'``SourceSide`` is a variable or partial list or contains a non-ground vertex' - instantiation_error,
			'``SourceSide`` is neither a variable nor a list' - type_error(list, 'SourceSide'),
			'A vertex is not in the network' - existence_error(vertex, 'Vertex'),
			'A vertex is repeated' - domain_error(unique_cut_vertices, 'Vertex'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(valid_cut/5).
	:- mode(valid_cut(+compound, +ground, +ground, +integer, +compound), zero_or_one).
	:- info(valid_cut/5, [
		comment is 'Checks a ground ``cut(SourceSide, SinkSide, Arcs)`` candidate and its reported capacity. The sides must be sorted, duplicate-free lists forming a complete disjoint partition of the network vertices, with distinct existing source and sink on their respective sides. ``Arcs`` must list every positive-capacity crossing arc in network input order, including parallel arcs. Requires zero lower bounds and fails for invalid candidates. Cut validity does not imply minimum-cut optimality.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Cut']
	]).

	:- public(decompose_flow/3).
	:- mode(decompose_flow(+compound, +list(compound), -list(compound)), zero_or_one).
	:- info(decompose_flow/3, [
		comment is 'Decomposes a feasible flow into a list of ``path(Vertices, ArcIds, Amount)`` and ``cycle(Vertices, ArcIds, Amount)`` terms. Path endpoints are inferred from the flow balances. Vertex lists for cycles repeat the first vertex at the end. Arc IDs are one-based input arc positions.',
		argnames is ['Network', 'Flow', 'Decomposition']
	]).

	:- public(valid_flow/3).
	:- mode(valid_flow(+compound, +list(pair), +list(compound)), zero_or_one).
	:- info(valid_flow/3, [
		comment is 'Checks that ``Flow`` respects every arc lower and upper capacity bound and that vertex net outflows match the given supplies and demands. Duplicate supply entries are added.',
		argnames is ['Network', 'Supplies', 'Flow']
	]).

	:- public(valid_st_flow/5).
	:- mode(valid_st_flow(+compound, +ground, +ground, +integer, +list(compound)), zero_or_one).
	:- info(valid_st_flow/5, [
		comment is 'Checks that ``Flow`` respects capacities and conservation for a source-to-sink flow of the given value. Fails if either terminal is not ground.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Flow']
	]).

	:- public(valid_min_cost_flow/4).
	:- mode(valid_min_cost_flow(+compound, +list(pair), +list(compound), +integer), zero_or_one).
	:- info(valid_min_cost_flow/4, [
		comment is 'Checks flow feasibility, the reported cost, and min-cost optimality by checking for negative-cost cycles in the residual graph.',
		argnames is ['Network', 'Supplies', 'Flow', 'Cost']
	]).

	:- public(valid_min_cost_flow/5).
	:- mode(valid_min_cost_flow(+compound, +list(pair), +list(compound), +integer, +list(pair)), zero_or_one).
	:- info(valid_min_cost_flow/5, [
		comment is 'Checks flow feasibility and cost, and validates a potential certificate by verifying non-negative reduced costs on every positive-capacity residual arc.',
		argnames is ['Network', 'Supplies', 'Flow', 'Cost', 'Potentials']
	]).

:- end_protocol.

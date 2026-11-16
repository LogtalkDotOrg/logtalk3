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


:- protocol(min_cost_flow_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Minimum cost flow protocol.',
		see_also is [flow_network_protocol, max_flow_protocol, successive_shortest_paths(_)]
	]).

	:- public(min_cost_flow/4).
	:- mode(min_cost_flow(+compound, +list(pair), -integer, -list(compound)), zero_or_one_or_error).
	:- info(min_cost_flow/4, [
		comment is 'Computes a minimum cost flow satisfying the given supplies and demands, including arc lower bounds. ``Supplies`` is a list of ``Vertex-Amount`` pairs where a positive amount is a supply and a negative amount is a demand; vertices not listed are transshipment vertices. Duplicate vertices are added. Fails if there is no feasible flow. Negative arc costs (including negative cost cycles) are supported. ``Flow`` is a list of ``flow(ArcId, From, To, Amount)`` terms with a positive amount, one per arc carrying flow, in the order of the network arcs. ``ArcId`` is the one-based position of the arc in the input list used to construct the network.',
		argnames is ['Network', 'Supplies', 'Cost', 'Flow'],
		exceptions is [
			'``Supplies`` is a partial list or contains a variable vertex' - instantiation_error,
			'``Supplies`` is not a list of ``Vertex-Amount`` pairs' - type_error(list(pair), 'Supplies'),
			'An amount is not an integer' - type_error(integer, 'Amount'),
			'A vertex of ``Supplies`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'The supplies and demands do not add up to zero' - domain_error(balanced_supplies, 'Supplies')
		]
	]).

	:- public(min_cost_flow/5).
	:- mode(min_cost_flow(+compound, +list(pair), -integer, -list(compound), -list(pair)), zero_or_one_or_error).
	:- info(min_cost_flow/5, [
		comment is 'As ``min_cost_flow/4``, also returning the final vertex potentials as ``Vertex-Potential`` pairs in vertex order. The potentials certify non-negative reduced costs on the residual arcs.',
		argnames is ['Network', 'Supplies', 'Cost', 'Flow', 'Potentials'],
		exceptions is [
			'``Supplies`` is a partial list or contains a variable vertex' - instantiation_error,
			'``Supplies`` is not a list of ``Vertex-Amount`` pairs' - type_error(list(pair), 'Supplies'),
			'An amount is not an integer' - type_error(integer, 'Amount'),
			'A vertex of ``Supplies`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'The supplies and demands do not add up to zero' - domain_error(balanced_supplies, 'Supplies')
		]
	]).

	:- public(min_cost_max_flow/6).
	:- mode(min_cost_max_flow(+compound, +ground, +ground, -integer, -integer, -list(compound)), one_or_error).
	:- info(min_cost_max_flow/6, [
		comment is 'Computes a minimum cost flow among the maximum flows from ``Source`` to ``Sink``. Always succeeds for valid terminals. Currently requires zero lower bounds.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Cost', 'Flow'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

	:- public(min_cost_max_flow/7).
	:- mode(min_cost_max_flow(+compound, +ground, +ground, -integer, -integer, -list(compound), -list(pair)), one_or_error).
	:- info(min_cost_max_flow/7, [
		comment is 'As ``min_cost_max_flow/6``, also returning final vertex potentials as ``Vertex-Potential`` pairs in vertex order.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Cost', 'Flow', 'Potentials'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink'),
			'The network contains a non-zero lower bound' - domain_error(zero_lower_bounds, 'Network')
		]
	]).

:- end_protocol.

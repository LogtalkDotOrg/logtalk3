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


:- protocol(interval_constraint_network_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Allen interval constraint-network protocol built on canonical relation sets, intended for small-to-medium symbolic networks.',
		see_also is [interval_relation_set_protocol, interval_relation_set, interval_algebra_protocol, interval_algebra]
	]).

	:- public(network/1).
	:- mode(network(?compound), zero_or_more).
	:- info(network/1, [
		comment is 'True if Network is a valid interval constraint network represented as a network(Nodes, Constraints) term.',
		argnames is ['Network']
	]).

	:- public(new/2).
	:- mode(new(+list, -compound), zero_or_one).
	:- info(new/2, [
		comment is 'Constructs a new network for a list of distinct nodes. Distinct node pairs are initialized with the universal relation set.',
		argnames is ['Nodes', 'Network']
	]).

	:- public(nodes/2).
	:- mode(nodes(+compound, -list), zero_or_one).
	:- info(nodes/2, [
		comment is 'Returns the list of nodes in a network.',
		argnames is ['Network', 'Nodes']
	]).

	:- public(relation/4).
	:- mode(relation(+compound, @term, @term, -list(atom)), zero_or_one).
	:- info(relation/4, [
		comment is 'Returns the current relation set between two nodes in a network. A node is always equal to itself.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet']
	]).

	:- public(entails/4).
	:- mode(entails(+compound, @term, @term, +list(atom)), zero_or_one).
	:- info(entails/4, [
		comment is 'True if the current relation set between two nodes is a subset of a queried relation set.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet']
	]).

	:- public(entails/5).
	:- mode(entails(+compound, @term, @term, +list(atom), -compound), zero_or_one).
	:- info(entails/5, [
		comment is 'True if the current relation set between two nodes is a subset of a queried relation set and returns an explanation term describing either a direct constraint or a supporting propagation path.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet', 'Explanation']
	]).

	:- public(possible/4).
	:- mode(possible(+compound, @term, @term, +list(atom)), zero_or_one).
	:- info(possible/4, [
		comment is 'True if the current relation set between two nodes intersects a queried relation set.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet']
	]).

	:- public(excluded/4).
	:- mode(excluded(+compound, @term, @term, +list(atom)), zero_or_one).
	:- info(excluded/4, [
		comment is 'True if the current relation set between two nodes is disjoint from a queried relation set.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet']
	]).

	:- public(contradiction/2).
	:- mode(contradiction(+compound, -compound), zero_or_more).
	:- info(contradiction/2, [
		comment is 'Returns a simple contradiction explanation for an inconsistent network as contradiction(Node1, Node2, Cause).',
		argnames is ['Network', 'Explanation']
	]).

	:- public(entailment_explanations/5).
	:- mode(entailment_explanations(+compound, @term, @term, +list(atom), -list(compound)), zero_or_one).
	:- info(entailment_explanations/5, [
		comment is 'Returns the list of immediate explanations supporting an entailed relation query, using identity, direct, and propagated explanation terms.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet', 'Explanations']
	]).

	:- public(contradiction_explanations/2).
	:- mode(contradiction_explanations(+compound, -list(compound)), zero_or_one).
	:- info(contradiction_explanations/2, [
		comment is 'Returns the list of contradiction explanations for all empty explicit pair constraints in a network.',
		argnames is ['Network', 'Explanations']
	]).

	:- public(refine/5).
	:- mode(refine(+compound, @term, @term, +list(atom), -compound), zero_or_one).
	:- info(refine/5, [
		comment is 'Refines the relation set between two distinct nodes by intersecting it with a new relation set.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet', 'RefinedNetwork']
	]).

	:- public(consistent/1).
	:- mode(consistent(+compound), zero_or_one).
	:- info(consistent/1, [
		comment is 'True if all explicit pair constraints in a network are non-empty.',
		argnames is ['Network']
	]).

	:- public(propagate/2).
	:- mode(propagate(+compound, -compound), zero_or_one).
	:- info(propagate/2, [
		comment is 'Computes a path-consistency closure and succeeds only if the resulting network is consistent. Intended for small-to-medium symbolic networks; not a large-scale temporal CSP solver.',
		argnames is ['Network', 'Closure']
	]).

	:- public(propagate/3).
	:- mode(propagate(+compound, -compound, -list(compound)), zero_or_one).
	:- info(propagate/3, [
		comment is 'Computes a path-consistency closure, returns the list of propagated changes, and succeeds only if the resulting network is consistent. Intended for small-to-medium symbolic networks; not a large-scale temporal CSP solver.',
		argnames is ['Network', 'Closure', 'Changes']
	]).

	:- public(refine_propagate/5).
	:- mode(refine_propagate(+compound, @term, @term, +list(atom), -compound), zero_or_one).
	:- info(refine_propagate/5, [
		comment is 'Refines the relation set between two distinct nodes and then propagates the consequences, succeeding only if the resulting network is consistent. Intended for small-to-medium symbolic networks; not a large-scale temporal CSP solver.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet', 'Closure']
	]).

	:- public(refine_propagate/6).
	:- mode(refine_propagate(+compound, @term, @term, +list(atom), -compound, -list(compound)), zero_or_one).
	:- info(refine_propagate/6, [
		comment is 'Refines the relation set between two distinct nodes, propagates the consequences incrementally, and returns the list of direct and propagated changes, succeeding only if the resulting network is consistent. Intended for small-to-medium symbolic networks; not a large-scale temporal CSP solver.',
		argnames is ['Network', 'Node1', 'Node2', 'RelationSet', 'Closure', 'Changes']
	]).

	:- public(refine_propagate/3).
	:- mode(refine_propagate(+compound, +list(compound), -compound), zero_or_one).
	:- info(refine_propagate/3, [
		comment is 'Applies a batch of relation-set refinements represented as constraint(Node1, Node2, RelationSet) terms and then propagates the combined consequences, succeeding only if the resulting network is consistent. Intended for small-to-medium symbolic networks; not a large-scale temporal CSP solver.',
		argnames is ['Network', 'Constraints', 'Closure']
	]).

	:- public(refine_propagate/4).
	:- mode(refine_propagate(+compound, +list(compound), -compound, -list(compound)), zero_or_one).
	:- info(refine_propagate/4, [
		comment is 'Applies a batch of relation-set refinements, propagates the combined consequences incrementally, and returns the list of direct and propagated changes, succeeding only if the resulting network is consistent. Intended for small-to-medium symbolic networks; not a large-scale temporal CSP solver.',
		argnames is ['Network', 'Constraints', 'Closure', 'Changes']
	]).

	:- public(propagation_triple/2).
	:- mode(propagation_triple(+compound, -compound), zero_or_one).
	:- info(propagation_triple/2, [
		comment is 'Extracts the causing node triple triple(Node1, Node3, Node2) from a propagated change term.',
		argnames is ['Change', 'Triple']
	]).

	:- public(propagation_triples/2).
	:- mode(propagation_triples(+list(compound), -list(compound)), zero_or_one).
	:- info(propagation_triples/2, [
		comment is 'Collects the unique node triples that caused propagated refinements in a list of change terms.',
		argnames is ['Changes', 'Triples']
	]).

	:- public(path_consistency/2).
	:- mode(path_consistency(+compound, -compound), zero_or_one).
	:- info(path_consistency/2, [
		comment is 'Computes the path-consistency closure of a network by iterative refinement using relation-set composition and intersection. Intended for small-to-medium symbolic networks; not a large-scale temporal CSP solver.',
		argnames is ['Network', 'Closure']
	]).

	:- public(constraints/2).
	:- mode(constraints(+compound, -list(compound)), zero_or_one).
	:- info(constraints/2, [
		comment is 'Returns the explicit pair constraints in canonical storage order using constraint(Node1, Node2, RelationSet) terms.',
		argnames is ['Network', 'Constraints']
	]).

	:- public(subsumes/2).
	:- mode(subsumes(+compound, +compound), zero_or_one).
	:- info(subsumes/2, [
		comment is 'True if the first network is at least as general as the second network, i.e. every pair relation set in the second network is a subset of the corresponding relation set in the first network.',
		argnames is ['Network1', 'Network2']
	]).

	:- public(equivalent/2).
	:- mode(equivalent(+compound, +compound), zero_or_one).
	:- info(equivalent/2, [
		comment is 'True if two networks contain the same nodes and mutually subsume each other.',
		argnames is ['Network1', 'Network2']
	]).

:- end_protocol.

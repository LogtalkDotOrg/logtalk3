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


:- protocol(atms_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'Portable, functional Assumption-based Truth Maintenance System API.',
		remarks is [
			'State' - 'All update predicates take an input ATMS state and return a new state. This permits several ATMS instances without relying on backend-specific mutable databases.',
			'Environments' - 'The public API uses ordered lists of node identifiers. The representation selected when creating a state is internal.'
		]
	]).

	:- public(new/1).
	:- mode(new(-atms_state), one).
	:- info(new/1, [
		comment is 'Creates a new empty ATMS using the default ordered-list environment representation.',
		argnames is ['State']
	]).

	:- public(new/2).
	:- mode(new(+object, -atms_state), zero_or_one).
	:- info(new/2, [
		comment is 'Creates a new empty ATMS using the specified environment representation object. Fails if the object does not conform to the environment protocol.',
		argnames is ['Representation', 'State']
	]).

	:- public(clear/2).
	:- mode(clear(+atms_state, -atms_state), one).
	:- info(clear/2, [
		comment is 'Returns a new empty ATMS using the same environment representation as the given state.',
		argnames is ['State', 'ClearedState']
	]).

	:- public(representation/2).
	:- mode(representation(+atms_state, ?object), one).
	:- info(representation/2, [
		comment is 'Returns the environment representation object used by the given state.',
		argnames is ['State', 'Representation']
	]).

	:- public(create_node/4).
	:- mode(create_node(+term, +atms_state, -atms_node, -atms_state), one).
	:- info(create_node/4, [
		comment is 'Creates an ordinary node for the given datum and returns the updated state.',
		argnames is ['Datum', 'State', 'Node', 'NewState']
	]).

	:- public(create_assumption/4).
	:- mode(create_assumption(+term, +atms_state, -atms_node, -atms_state), one).
	:- info(create_assumption/4, [
		comment is 'Creates an assumption node for the given datum and returns the updated state.',
		argnames is ['Datum', 'State', 'Assumption', 'NewState']
	]).

	:- public(create_contradiction/4).
	:- mode(create_contradiction(+term, +atms_state, -atms_node, -atms_state), one).
	:- info(create_contradiction/4, [
		comment is 'Creates a contradiction node for the given datum and returns the updated state.',
		argnames is ['Datum', 'State', 'Contradiction', 'NewState']
	]).

	:- public(justify/5).
	:- mode(justify(+atms_node, +list(atms_node), +term, +atms_state, -atms_state), zero_or_one).
	:- info(justify/5, [
		comment is 'Adds a Horn justification from the antecedent nodes to the consequent node, incrementally propagating label changes. Fails if any referenced node is unknown.',
		argnames is ['Consequent', 'Antecedents', 'Info', 'State', 'NewState']
	]).

	:- public(node/2).
	:- mode(node(?atms_node, +atms_state), zero_or_more).
	:- info(node/2, [
		comment is 'Enumerates or tests the nodes in the given state.',
		argnames is ['Node', 'State']
	]).

	:- public(assumption/2).
	:- mode(assumption(?atms_node, +atms_state), zero_or_more).
	:- info(assumption/2, [
		comment is 'Enumerates or tests the assumption nodes in the given state.',
		argnames is ['Assumption', 'State']
	]).

	:- public(contradiction/2).
	:- mode(contradiction(?atms_node, +atms_state), zero_or_more).
	:- info(contradiction/2, [
		comment is 'Enumerates or tests the contradiction nodes in the given state.',
		argnames is ['Contradiction', 'State']
	]).

	:- public(nodes/2).
	:- mode(nodes(+atms_state, -list(pair)), one).
	:- info(nodes/2, [
		comment is 'Returns the node-datum pairs in node identifier order.',
		argnames is ['State', 'Nodes']
	]).

	:- public(assumptions/2).
	:- mode(assumptions(+atms_state, -list(atms_node)), one).
	:- info(assumptions/2, [
		comment is 'Returns the assumption nodes in node identifier order.',
		argnames is ['State', 'Assumptions']
	]).

	:- public(justifications/2).
	:- mode(justifications(+atms_state, -list(term)), one).
	:- info(justifications/2, [
		comment is 'Returns all justifications in insertion order.',
		argnames is ['State', 'Justifications']
	]).

	:- public(label/3).
	:- mode(label(+atms_node, +atms_state, -list(list(atms_node))), zero_or_one).
	:- info(label/3, [
		comment is 'Returns the minimal consistent environments supporting the given node. Fails if the node is unknown.',
		argnames is ['Node', 'State', 'Environments']
	]).

	:- public(in_label/3).
	:- mode(in_label(+atms_node, +list(atms_node), +atms_state), zero_or_one).
	:- info(in_label/3, [
		comment is 'True iff the given environment is a member of the node label.',
		argnames is ['Node', 'Environment', 'State']
	]).

	:- public(consistent/2).
	:- mode(consistent(+list(atms_node), +atms_state), zero_or_one).
	:- info(consistent/2, [
		comment is 'True iff the given environment contains no recorded nogood.',
		argnames is ['Environment', 'State']
	]).

	:- public(nogood/2).
	:- mode(nogood(?list(atms_node), +atms_state), zero_or_more).
	:- info(nogood/2, [
		comment is 'Enumerates or tests the minimal nogood environments in the given state.',
		argnames is ['Environment', 'State']
	]).

	:- public(interpretations/2).
	:- mode(interpretations(+atms_state, -list(list(atms_node))), one).
	:- info(interpretations/2, [
		comment is 'Returns the maximal consistent environments over all assumptions.',
		argnames is ['State', 'Interpretations']
	]).

	:- public(why/3).
	:- mode(why(+atms_node, +atms_state, -list(term)), one).
	:- info(why/3, [
		comment is 'Returns the direct justifications for the given node in insertion order.',
		argnames is ['Node', 'State', 'Justifications']
	]).

:- end_protocol.

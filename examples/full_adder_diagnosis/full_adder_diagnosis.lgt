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


:- object(full_adder_diagnosis).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'Model-based diagnosis of the classical multiplier-adder circuit (Reiter 1987; de Kleer & Williams 1987) using the ``atms`` library.',
		remarks is [
			'Circuit' - 'Multipliers M1, M2, M3 compute X1=A*C, X2=B*D, X3=B*E. Adders A1, A2 compute Y1=X1+X2, Y2=X2+X3.',
			'Scenario' - 'Inputs A=3, B=2, C=2, D=3, E=3. Observed outputs are Y1=10 and Y2=12, but a correctly functioning circuit predicts Y1=12.',
			'Diagnosis' - 'Each component has an "ok" assumption. The observed/predicted mismatch on Y1 is recorded as a contradiction node, whose environments become nogoods. The maximal consistent environments (``atms::interpretations/2``) then correspond directly to the minimal single-fault diagnoses {M1}, {M2}, {A1}.'
		],
		see_also is [atms]
	]).

	:- public(build/1).
	:- mode(build(-atms_state), one).
	:- info(build/1, [
		comment is 'Builds the ATMS state for the circuit and scenario, including the recorded observations and the Y1 contradiction.',
		argnames is ['State']
	]).

	:- public(diagnoses/2).
	:- mode(diagnoses(+atms_state, -list(list(atom))), one).
	:- info(diagnoses/2, [
		comment is 'Computes the minimal single-fault diagnoses (as sorted lists of suspect component identifiers) from the maximal consistent environments over the health assumptions.',
		argnames is ['State', 'Diagnoses']
	]).

	:- public(node_data/3).
	:- mode(node_data(+list(atms_node), +list(pair), -list(term)), one).
	:- info(node_data/3, [
		comment is 'Translates a list of opaque node identifiers into the corresponding data, given the node/datum pairs returned by atms::nodes/2.',
		argnames is ['Nodes', 'Pairs', 'Data']
	]).

	:- public(report/0).
	:- mode(report, one).
	:- info(report/0, [
		comment is 'Builds the circuit ATMS and prints the nodes, the recorded nogoods, and the resulting minimal single-fault diagnoses.'
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		member/2, memberchk/2, subtract/3
	]).

	% known input readings for the scenario
	input(a, 3).
	input(b, 2).
	input(c, 2).
	input(d, 3).
	input(e, 3).

	build(State) :-
		atms::new(S0),
		atms::create_assumption(ok(m1), S0, OkM1, S1),
		atms::create_assumption(ok(m2), S1, OkM2, S2),
		atms::create_assumption(ok(m3), S2, OkM3, S3),
		atms::create_assumption(ok(a1), S3, OkA1, S4),
		atms::create_assumption(ok(a2), S4, OkA2, S5),
		multiplier(x1, a, c, OkM1, S5, X1, S6),
		multiplier(x2, b, d, OkM2, S6, X2, S7),
		multiplier(x3, b, e, OkM3, S7, X3, S8),
		adder(y1, X1, X2, OkA1, S8, Y1, Y1Value, S9),
		adder(y2, X2, X3, OkA2, S9, Y2, Y2Value, S10),
		check_observation(y1, Y1, Y1Value, 10, S10, S11),
		check_observation(y2, Y2, Y2Value, 12, S11, State).

	% multiplier(Wire, InputA, InputB, OkAssumption, State0, Node, State)
	%
	% A multiplier predicts its output from the known inputs whenever it
	% is assumed to be behaving normally.
	multiplier(Wire, InputA, InputB, OkAssumption, State0, Node, State) :-
		input(InputA, ValueA),
		input(InputB, ValueB),
		Product is ValueA * ValueB,
		atms::create_node(val(Wire, Product), State0, Node, State1),
		atms::justify(Node, [OkAssumption], multiply(InputA, InputB), State1, State).

	% adder(Wire, InNode1, InNode2, OkAssumption, State0, Node, Value, State)
	%
	% An adder predicts its output from its (already derived) input node
	% values whenever it is assumed to be behaving normally.
	adder(Wire, InNode1, InNode2, OkAssumption, State0, Node, Value, State) :-
		atms::nodes(State0, Nodes),
		memberchk(InNode1-val(_, Value1), Nodes),
		memberchk(InNode2-val(_, Value2), Nodes),
		Value is Value1 + Value2,
		atms::create_node(val(Wire, Value), State0, Node, State1),
		atms::justify(Node, [InNode1, InNode2, OkAssumption], add(InNode1, InNode2), State1, State).

	% check_observation(Wire, ModelNode, ModelValue, ObservedValue, State0, State)
	%
	% Records the observed reading for a wire as a premise (a node holding
	% in the empty environment). When it disagrees with the value the
	% model predicts, also records a contradiction node connecting the two;
	% every environment supporting that contradiction becomes a nogood.
	check_observation(Wire, ModelNode, ModelValue, ObservedValue, State0, State) :-
		atms::create_node(obs(Wire, ObservedValue), State0, ObsNode, State1),
		atms::justify(ObsNode, [], observed, State1, State2),
		(	ModelValue == ObservedValue ->
			State = State2
		;	atms::create_contradiction(contra(Wire), State2, ContraNode, State3),
			atms::justify(ContraNode, [ModelNode, ObsNode], conflict(Wire, ModelValue, ObservedValue), State3, State)
		).

	diagnoses(State, Diagnoses) :-
		atms::assumptions(State, Assumptions),
		atms::nodes(State, Nodes),
		atms::interpretations(State, Interpretations),
		findall(
			Diagnosis,
			(	member(Interpretation, Interpretations),
				subtract(Assumptions, Interpretation, FaultyNodes),
				node_data(FaultyNodes, Nodes, Diagnosis)
			),
			Diagnoses0
		),
		sort(Diagnoses0, Diagnoses).

	% node_data(Nodes, Pairs, Data)
	%
	% Translates opaque node identifiers back into the user-level data
	% (here, the "ok(Component)" terms) using the node/datum pairs
	% returned by atms::nodes/2.
	node_data([], _, []).
	node_data([Node| Nodes], Pairs, [Datum| Data]) :-
		memberchk(Node-Datum, Pairs),
		node_data(Nodes, Pairs, Data).

	report :-
		build(State),
		atms::nodes(State, Nodes),
		format('Nodes:~n', []),
		forall(
			member(Node-Datum, Nodes),
			format('  ~w -> ~w~n', [Node, Datum])
		),
		format('~nNogoods (minimal conflict sets):~n', []),
		forall(
			atms::nogood(Nogood, State),
			(	node_data(Nogood, Nodes, Data),
				format('  ~w~n', [Data])
			)
		),
		diagnoses(State, Diagnoses),
		format('~nMinimal single-fault diagnoses:~n', []),
		forall(
			member(Diagnosis, Diagnoses),
			format('  ~w~n', [Diagnosis])
		).

:- end_object.

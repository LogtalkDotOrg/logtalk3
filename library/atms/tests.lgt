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


:- object(tests(_Representation_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Unit tests for the "atms" library.',
		parameters is [
			'Representation' - 'ATMS environment representation under test.'
		]
	]).

	:- uses(atms, [
		new/2, create_node/4, create_assumption/4, create_contradiction/4, justify/5, node/2, nodes/2,
		label/3, consistent/2, nogood/2, interpretations/2, assumptions/2, justifications/2, why/3,
		retract_justification/5, retract_justifications/3, retract_node/3, retract_nodes/3, retract_batch/4,
		assumption/2, contradiction/2, in_label/3, representation/2, clear/2
	]).

	:- uses(list, [
		member/2, memberchk/2, append/3, length/2, nth1/3
	]).

	cover(atms).
	cover(atms_bitset_environment).
	cover(atms_ordered_list_environment).
	cover(atms_segmented_bitset_environment).

	test(atms_default_constructor_01, deterministic((Representation == atms_ordered_list_environment, Label == [[Assumption]]))) :-
		atms::new(State0),
		representation(State0, Representation),
		nodes(State0, []),
		assumptions(State0, []),
		justifications(State0, []),
		interpretations(State0, [[]]),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, State2),
		justify(Head, [Assumption], support, State2, State),
		label(Head, State, Label).

	test(atms_environment_union_01, deterministic(Union == [node(0), node(1), node(2), node(3)])) :-
		_Representation_::from_list([node(0), node(2), node(3)], Environment1),
		_Representation_::from_list([node(1), node(2)], Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(atms_environment_union_empty_01, deterministic(Union == [node(0), node(2)])) :-
		_Representation_::from_list([node(0), node(2)], Environment1),
		_Representation_::empty(Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(atms_environment_subset_01, deterministic(IsSubset == true)) :-
		_Representation_::from_list([node(0), node(2)], Environment1),
		_Representation_::from_list([node(0), node(1), node(2), node(3)], Environment2),
		(	_Representation_::subset(Environment1, Environment2) ->
			IsSubset = true
		;	IsSubset = false
		).

	test(atms_environment_not_subset_01, deterministic(IsSubset == false)) :-
		_Representation_::from_list([node(0), node(2)], Environment1),
		_Representation_::from_list([node(0), node(1), node(3)], Environment2),
		(	_Representation_::subset(Environment1, Environment2) ->
			IsSubset = true
		;	IsSubset = false
		).

	test(atms_environment_canonicalization_01, deterministic(Nodes == [node(0), node(15), node(16), node(31), node(32)])) :-
		_Representation_::from_list([node(32), node(15), node(16), node(15), node(31), node(0)], Environment),
		_Representation_::to_list(Environment, Nodes).

	test(atms_environment_equal_01, deterministic(Equal == true)) :-
		_Representation_::from_list([node(31), node(15), node(16), node(15)], Environment1),
		_Representation_::from_list([node(15), node(16), node(31)], Environment2),
		( 	_Representation_::equal(Environment1, Environment2) ->
			Equal = true
		; 	Equal = false
		).

	test(atms_environment_empty_and_exact_subset_01, deterministic(Subset == true)) :-
		_Representation_::empty(Empty),
		_Representation_::from_list([node(15), node(16), node(31), node(32)], Environment),
		( 	_Representation_::subset(Empty, Environment),
			_Representation_::subset(Environment, Environment) ->
			Subset = true
		; 	Subset = false
		).

	test(atms_environment_cross_block_subset_01, deterministic(Subset == true)) :-
		_Representation_::from_list([node(15), node(31)], Environment1),
		_Representation_::from_list([node(0), node(15), node(16), node(31), node(32)], Environment2),
		( 	_Representation_::subset(Environment1, Environment2) ->
			Subset = true
		; 	Subset = false
		).

	test(atms_environment_cross_block_union_01, deterministic(Union == [node(15), node(16), node(31), node(32)])) :-
		_Representation_::from_list([node(15), node(16), node(31)], Environment1),
		_Representation_::from_list([node(16), node(32)], Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(atms_environment_sparse_blocks_01, deterministic(Union == [node(0), node(16), node(32), node(48)])) :-
		_Representation_::from_list([node(0), node(48)], Environment1),
		_Representation_::from_list([node(16), node(32)], Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(atms_empty_system_01, deterministic(Assumptions == [])) :-
		new(_Representation_, State),
		assumptions(State, Assumptions).

	test(atms_assumption_label_01, deterministic(Label == [[node(0)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, _, State),
		label(node(0), State, Label).

	test(atms_state_collection_order_01, deterministic([EnumeratedNodes, Nodes, Assumptions, Justifications, Why] == [
		[node(0), node(1), node(2)],
		[node(0)-a, node(1)-b, node(2)-h],
		[node(0), node(1)],
		[justification(node(2),[node(0)],first), justification(node(2),[node(1)],second)],
		[justification(node(2),[node(0)],first), justification(node(2),[node(1)],second)]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		justify(H, [A], first, State3, State4),
		justify(H, [B], second, State4, State),
		findall(Node, node(Node, State), EnumeratedNodes),
		nodes(State, Nodes),
		assumptions(State, Assumptions),
		justifications(State, Justifications),
		why(H, State, Why).

	test(atms_unconditional_justification_01, deterministic(Label == [[]])) :-
		new(_Representation_, State0),
		create_node(fact, State0, Fact, State1),
		justify(Fact, [], axiom, State1, State),
		label(Fact, State, Label).

	test(atms_incremental_delta_01, deterministic(Label == [[node(0)], [node(1)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(c, State1, C, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		justify(H, [A], first, State4, State5),
		justify(K, [H], inherited, State5, State6),
		justify(H, [C], alternative, State6, State),
		label(K, State, Label).

	test(atms_subsumed_label_update_01, deterministic([LabelH, LabelK] == [[[node(0)]], [[node(0)]]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		justify(H, [A], minimal, State4, State5),
		justify(K, [H], dependent, State5, State6),
		justify(H, [A, B], subsumed, State6, State),
		label(H, State, LabelH),
		label(K, State, LabelK).

	test(atms_cartesian_products_01, deterministic(Label == [
		[node(0),node(2)], [node(0),node(3)], [node(1),node(2)], [node(1),node(3)]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_assumption(d, State3, D, State4),
		create_node(x, State4, X, State5),
		create_node(y, State5, Y, State6),
		create_node(z, State6, Z, State7),
		justify(X, [A], x_a, State7, State8),
		justify(X, [B], x_b, State8, State9),
		justify(Y, [C], y_c, State9, State10),
		justify(Y, [D], y_d, State10, State11),
		justify(Z, [X, Y], product, State11, State),
		label(Z, State, Label).

	test(atms_cartesian_product_minimality_01, deterministic(Label == [[node(0)], [node(1),node(2)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_node(x, State3, X, State4),
		create_node(y, State4, Y, State5),
		create_node(z, State5, Z, State6),
		justify(X, [A], x_a, State6, State7),
		justify(X, [B], x_b, State7, State8),
		justify(Y, [A], y_a, State8, State9),
		justify(Y, [C], y_c, State9, State10),
		justify(Z, [X, Y], product, State10, State),
		label(Z, State, Label).

	test(atms_cartesian_product_nogood_pruning_01, deterministic(Label == [
		[node(0),node(3)], [node(1),node(2)], [node(1),node(3)]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_assumption(d, State3, D, State4),
		create_node(x, State4, X, State5),
		create_node(y, State5, Y, State6),
		create_node(z, State6, Z, State7),
		create_contradiction(false, State7, Bottom, State8),
		justify(X, [A], x_a, State8, State9),
		justify(X, [B], x_b, State9, State10),
		justify(Y, [C], y_c, State10, State11),
		justify(Y, [D], y_d, State11, State12),
		justify(Bottom, [A, C], clash, State12, State13),
		justify(Z, [X, Y], product, State13, State),
		label(Z, State, Label).

	test(atms_empty_antecedent_product_01, deterministic([LabelX, LabelTarget] == [
		[[node(1)], [node(2)]], []
	])) :-
		new(_Representation_, State0),
		create_node(unsupported, State0, Unsupported, State1),
		create_assumption(a, State1, A, State2),
		create_assumption(b, State2, B, State3),
		create_node(x, State3, X, State4),
		create_node(target, State4, Target, State5),
		justify(X, [A], x_a, State5, State6),
		justify(Target, [Unsupported, X], blocked, State6, State7),
		justify(X, [B], x_b, State7, State),
		label(X, State, LabelX),
		label(Target, State, LabelTarget).

	test(atms_cyclic_justifications_01, deterministic([LabelB, LabelC] == [[[node(0)]], [[node(0)]]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(b, State1, B, State2),
		create_node(c, State2, C, State3),
		justify(C, [B], b_implies_c, State3, State4),
		justify(B, [C], c_implies_b, State4, State5),
		justify(B, [A], a_implies_b, State5, State),
		label(B, State, LabelB),
		label(C, State, LabelC).

	test(atms_minimality_01, deterministic(Label == [[node(0)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(c, State1, C, State2),
		create_node(h, State2, H, State3),
		justify(H, [A, C], conjunction, State3, State4),
		justify(H, [A], simpler, State4, State),
		label(H, State, Label).

	test(atms_nogood_pruning_01, deterministic([Label, IsConsistent, Nogood] == [[], false, [node(0),node(1)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(c, State1, C, State2),
		create_node(h, State2, H, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [A, C], support, State4, State5),
		justify(Bottom, [A, C], clash, State5, State),
		label(H, State, Label),
		(	consistent([A,C], State) ->
			IsConsistent = true
		;	IsConsistent = false
		),
		findall(Environment, nogood(Environment, State), [Nogood]).

	test(atms_sequential_nogood_pruning_01, deterministic([Label1, Label2] == [
		[[node(0),node(2)], [node(1),node(2)]],
		[[node(0),node(2)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_node(h, State3, H, State4),
		create_contradiction(false_ab, State4, BottomAB, State5),
		create_contradiction(false_bc, State5, BottomBC, State6),
		justify(H, [A, B], support_ab, State6, State7),
		justify(H, [A, C], support_ac, State7, State8),
		justify(H, [B, C], support_bc, State8, State9),
		justify(BottomAB, [A, B], clash_ab, State9, State10),
		label(H, State10, Label1),
		justify(BottomBC, [B, C], clash_bc, State10, State),
		label(H, State, Label2).

	test(atms_interpretations_01, deterministic(Interpretations == [[node(0)], [node(1)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(c, State1, C, State2),
		create_contradiction(false, State2, Bottom, State3),
		justify(Bottom, [A, C], clash, State3, State),
		interpretations(State, Interpretations).

	test(atms_interpretations_without_nogoods_01, deterministic(Interpretations == [[node(0), node(1), node(2)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, _, State1),
		create_assumption(b, State1, _, State2),
		create_assumption(c, State2, _, State),
		interpretations(State, Interpretations).

	test(atms_interpretations_empty_system_01, deterministic(Interpretations == [[]])) :-
		new(_Representation_, State),
		interpretations(State, Interpretations).

	test(atms_interpretations_inconsistent_system_01, deterministic(Interpretations == [])) :-
		new(_Representation_, State0),
		create_contradiction(false, State0, Bottom, State1),
		justify(Bottom, [], contradiction, State1, State),
		interpretations(State, Interpretations).

	test(atms_interpretations_multiple_nogoods_01, deterministic(Interpretations == [[node(0),node(2)], [node(1)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_contradiction(false_ab, State3, BottomAB, State4),
		create_contradiction(false_bc, State4, BottomBC, State5),
		justify(BottomAB, [A, B], clash_ab, State5, State6),
		justify(BottomBC, [B, C], clash_bc, State6, State),
		interpretations(State, Interpretations).

	test(atms_why_01, deterministic(Why == [justification(node(1),[node(0)],rule)])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		justify(H, [A], rule, State2, State),
		why(H, State, Why).

	test(atms_retract_justification_subsumed_support_01, deterministic([LabelH, LabelK] == [
		[[node(0),node(1)]], [[node(0),node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		justify(H, [A, B], larger, State4, State5),
		justify(K, [H], dependent, State5, State6),
		justify(H, [A], smaller, State6, State7),
		retract_justification(H, [A], smaller, State7, State),
		label(H, State, LabelH),
		label(K, State, LabelK).

	test(atms_retract_justification_missing_01, deterministic(State == State1)) :-
		new(_Representation_, State0),
		create_node(h, State0, H, State1),
		retract_justification(H, [], missing, State1, State2),
		retract_justification(node(99), [node(98)], missing, State2, State).

	test(atms_retract_justification_canonicalization_01, deterministic([Label, Rules, SameState] == [[], [], true])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		justify(H, [A, B], rule, State3, State4),
		retract_justification(H, [B, A, B], rule, State4, State5),
		retract_justification(H, [A, B], rule, State5, State),
		label(H, State, Label),
		justifications(State, Rules),
		( 	State == State5 ->
			SameState = true
		;	SameState = false
		).

	test(atms_retract_node_incident_rules_01, deterministic([Nodes, Rules, Why, Label, OldLabel, NextNode] == [
		[node(0)-a, node(2)-k], [], [], [], [[node(0)]], node(3)
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(k, State2, K, State3),
		justify(H, [A], incoming, State3, State4),
		justify(K, [H], outgoing, State4, State5),
		retract_node(H, State5, State6),
		nodes(State6, Nodes),
		justifications(State6, Rules),
		why(H, State6, Why),
		label(K, State6, Label),
		label(K, State5, OldLabel),
		\+ node(H, State6),
		\+ label(H, State6, _),
		\+ in_label(H, [A], State6),
		create_node(next, State6, NextNode, _).

	test(atms_retract_node_assumption_01, deterministic([Assumptions, Label, Interpretations] == [
		[node(1)], [[node(1)]], [[node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		justify(H, [A], first, State3, State4),
		justify(H, [B], second, State4, State5),
		retract_node(A, State5, State),
		\+ assumption(A, State),
		assumptions(State, Assumptions),
		label(H, State, Label),
		interpretations(State, Interpretations).

	test(atms_retract_node_contradiction_01, deterministic([Label, Nogoods, Interpretations] == [
		[[node(0)]], [], [[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_contradiction(false, State2, Bottom, State3),
		justify(H, [A], support, State3, State4),
		justify(Bottom, [A], clash, State4, State5),
		retract_node(Bottom, State5, State),
		\+ contradiction(Bottom, State),
		label(H, State, Label),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations).

	test(atms_retract_node_missing_01, deterministic([State, State3] == [State0, State2])) :-
		new(_Representation_, State0),
		retract_node(node(99), State0, State),
		create_node(isolated, State0, Node, State1),
		retract_node(Node, State1, State2),
		retract_node(Node, State2, State3).

	test(atms_retract_justification_chain_and_product_01, deterministic([LabelH, LabelK, LabelProduct, OldLabel, Representation] == [
		[], [], [], [[node(0),node(1)]], _Representation_
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		create_node(product, State4, Product, State5),
		justify(H, [A], support, State5, State6),
		justify(K, [H], chain, State6, State7),
		justify(Product, [K, B], product, State7, State8),
		retract_justification(H, [A], support, State8, State),
		label(H, State, LabelH),
		label(K, State, LabelK),
		label(Product, State, LabelProduct),
		label(Product, State8, OldLabel),
		representation(State, Representation).

	test(atms_retract_justification_identity_and_order_01, deterministic([Label, Rules, Why] == [
		[[node(0)], [node(1)]],
		[justification(node(2),[node(0)],first), justification(node(2),[node(1)],third)],
		[justification(node(2),[node(0)],first), justification(node(2),[node(1)],third)]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		justify(H, [A], first, State3, State4),
		justify(H, [A], second, State4, State5),
		justify(H, [B], third, State5, State6),
		retract_justification(H, [A], _Info, State6, Unchanged),
		Unchanged == State6,
		retract_justification(H, [A], second, State6, State),
		label(H, State, Label),
		justifications(State, Rules),
		why(H, State, Why).

	test(atms_retract_justification_variable_info_01, deterministic([Label, Rules] == [[], []])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		justify(H, [A], info(Detail), State2, State3),
		retract_justification(H, [A], info(_OtherDetail), State3, Unchanged),
		Unchanged == State3,
		var(Detail),
		retract_justification(H, [A], info(Detail), State3, State),
		label(H, State, Label),
		justifications(State, Rules).

	test(atms_retract_justification_duplicate_01, deterministic([Label, Rules] == [[], []])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		justify(H, [A], rule, State2, State3),
		justify(H, [A, A], rule, State3, State4),
		retract_justification(H, [A], rule, State4, State),
		label(H, State, Label),
		justifications(State, Rules).

	test(atms_retract_justification_unconditional_01, deterministic([Label, DependentLabel] == [
		[[node(0)]], [[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(k, State2, K, State3),
		justify(H, [A], conditional, State3, State4),
		justify(H, [], axiom, State4, State5),
		justify(K, [H], dependent, State5, State6),
		retract_justification(H, [], axiom, State6, State),
		label(H, State, Label),
		label(K, State, DependentLabel).

	test(atms_retract_justification_cycle_01, deterministic([LabelH, LabelK, RestoredH, RestoredK] == [
		[], [], [[node(0)]], [[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(k, State2, K, State3),
		justify(H, [K], backward, State3, State4),
		justify(K, [H], forward, State4, State5),
		justify(H, [H], self, State5, State6),
		justify(H, [A], external, State6, State7),
		retract_justification(H, [A], external, State7, State8),
		label(H, State8, LabelH),
		label(K, State8, LabelK),
		justify(K, [A], replacement, State8, State),
		label(H, State, RestoredH),
		label(K, State, RestoredK).

	test(atms_retract_justification_cycle_alternative_01, deterministic([LabelH, LabelK] == [
		[[node(1)]], [[node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		justify(H, [K], backward, State4, State5),
		justify(K, [H], forward, State5, State6),
		justify(H, [A], external_a, State6, State7),
		justify(K, [B], external_b, State7, State8),
		retract_justification(H, [A], external_a, State8, State),
		label(H, State, LabelH),
		label(K, State, LabelK).

	test(atms_retract_justification_nogood_restoration_01, deterministic([Label, Nogoods, Interpretations] == [
		[[node(0),node(1)]], [], [[node(0),node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [A, B], support, State4, State5),
		justify(Bottom, [H], clash, State5, State6),
		retract_justification(Bottom, [H], clash, State6, State),
		label(H, State, Label),
		consistent([A, B], State),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations).

	test(atms_retract_justification_indirect_nogood_01, deterministic([LabelH, LabelK, Nogoods] == [
		[], [[node(0)]], []
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(k, State2, K, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [A], support, State4, State5),
		justify(K, [A], independent, State5, State6),
		justify(Bottom, [H], clash, State6, State7),
		retract_justification(H, [A], support, State7, State),
		label(H, State, LabelH),
		label(K, State, LabelK),
		findall(Environment, nogood(Environment, State), Nogoods).

	test(atms_retract_justification_shared_nogood_01, deterministic([Label, Nogoods, RestoredLabel] == [
		[], [[node(0)]], [[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_contradiction(false, State2, Bottom, State3),
		justify(H, [A], support, State3, State4),
		justify(Bottom, [A], first, State4, State5),
		justify(Bottom, [A], second, State5, State6),
		retract_justification(Bottom, [A], first, State6, State7),
		label(H, State7, Label),
		findall(Environment, nogood(Environment, State7), Nogoods),
		retract_justification(Bottom, [A], second, State7, State),
		label(H, State, RestoredLabel).

	test(atms_retract_justification_subsumed_nogood_01, deterministic([Label, Nogoods, Interpretations] == [
		[[node(0)]], [[node(0),node(1)]], [[node(0)], [node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [A], support, State4, State5),
		justify(Bottom, [A, B], larger, State5, State6),
		justify(Bottom, [A], smaller, State6, State7),
		retract_justification(Bottom, [A], smaller, State7, State),
		label(H, State, Label),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations).

	test(atms_retract_justification_unconditional_contradiction_01, deterministic([Label, Nogoods, Interpretations] == [
		[[node(0)]], [], [[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_contradiction(false, State2, Bottom, State3),
		justify(Bottom, [], inconsistent, State3, State4),
		justify(H, [A], support, State4, State5),
		retract_justification(Bottom, [], inconsistent, State5, State),
		label(H, State, Label),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations).

	test(atms_retract_node_shared_nogood_01, deterministic([Nogoods, Interpretations, RestoredLabel] == [
		[[node(0)]], [[]], [[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_contradiction(first, State1, First, State2),
		create_contradiction(second, State2, Second, State3),
		justify(First, [A], first, State3, State4),
		justify(Second, [A], second, State4, State5),
		retract_node(First, State5, State6),
		findall(Environment, nogood(Environment, State6), Nogoods),
		interpretations(State6, Interpretations),
		retract_node(Second, State6, State),
		label(A, State, RestoredLabel).

	test(atms_retract_node_indirect_nogood_01, deterministic([Label, Rules, Nogoods] == [
		[[node(0)]], [justification(node(2),[node(0)],independent)], []
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(k, State2, K, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [A], support, State4, State5),
		justify(K, [A], independent, State5, State6),
		justify(Bottom, [H], clash, State6, State7),
		retract_node(H, State7, State),
		label(K, State, Label),
		justifications(State, Rules),
		findall(Environment, nogood(Environment, State), Nogoods).

	test(atms_retract_node_assumption_nogood_01, deterministic([Assumptions, Label, Nogoods, Interpretations] == [
		[node(1)], [[node(1)]], [], [[node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [B], support, State4, State5),
		justify(Bottom, [A, B], clash, State5, State6),
		retract_node(A, State6, State),
		assumptions(State, Assumptions),
		label(H, State, Label),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations).

	test(atms_retract_node_cycle_01, deterministic([Label, Rules] == [
		[], [justification(node(1),[node(2)],backward), justification(node(2),[node(1)],forward)]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(k, State2, K, State3),
		justify(H, [K], backward, State3, State4),
		justify(K, [H], forward, State4, State5),
		justify(H, [A], external, State5, State6),
		retract_node(A, State6, State),
		label(K, State, Label),
		justifications(State, Rules).

	test(atms_retract_node_self_justification_01, deterministic(Rules == [])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		justify(A, [A], self, State1, State2),
		retract_node(A, State2, State),
		justifications(State, Rules).

	test(atms_retract_node_update_and_clear_01, deterministic([Label, NewNode, Nodes, Representation] == [
		[[node(3)]], node(0), [], _Representation_
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(k, State2, K, State3),
		justify(H, [A], first, State3, State4),
		justify(K, [H], dependent, State4, State5),
		retract_node(A, State5, State6),
		create_assumption(b, State6, B, State7),
		justify(H, [B], replacement, State7, State8),
		label(K, State8, Label),
		clear(State8, Cleared),
		nodes(Cleared, Nodes),
		representation(Cleared, Representation),
		create_node(next, Cleared, NewNode, _).

	test(atms_retract_node_identifier_gaps_01, deterministic([Label, Assumptions, NextNode] == [
		[[node(31),node(32)]], [node(15),node(31),node(32)], node(34)
	])) :-
		new(_Representation_, State0),
		create_padding_nodes(15, State0, State1),
		create_assumption(a, State1, A, State2),
		create_assumption(b, State2, B, State3),
		create_padding_nodes(14, State3, State4),
		create_assumption(c, State4, C, State5),
		create_assumption(d, State5, D, State6),
		create_node(h, State6, H, State7),
		justify(H, [A, B], first, State7, State8),
		justify(H, [C, D], second, State8, State9),
		retract_node(B, State9, State10),
		retract_node(node(0), State10, State),
		label(H, State, Label),
		assumptions(State, Assumptions),
		create_node(next, State, NextNode, _).

	test(atms_retract_justification_reconstruction_01, deterministic(Snapshot == ExpectedSnapshot)) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		create_contradiction(false, State4, Bottom, State5),
		justify(H, [A, B], larger, State5, State6),
		justify(K, [H], dependent, State6, State7),
		justify(Bottom, [A, B], clash, State7, State8),
		justify(H, [A], smaller, State8, State9),
		retract_justification(Bottom, [A, B], clash, State9, State10),
		retract_justification(H, [A], smaller, State10, State),
		new(_Representation_, Expected0),
		create_assumption(a, Expected0, A, Expected1),
		create_assumption(b, Expected1, B, Expected2),
		create_node(h, Expected2, H, Expected3),
		create_node(k, Expected3, K, Expected4),
		create_contradiction(false, Expected4, Bottom, Expected5),
		justify(H, [A, B], larger, Expected5, Expected6),
		justify(K, [H], dependent, Expected6, Expected),
		state_snapshot(State, [], Snapshot),
		state_snapshot(Expected, [], ExpectedSnapshot).

	test(atms_rebuild_aliased_justifications_01, deterministic([Rules, Why, Label] == [
		[justification(node(1),[node(0)],info(Detail)), justification(node(1),[node(0)],info(Detail))],
		[justification(node(1),[node(0)],info(Detail)), justification(node(1),[node(0)],info(Detail))],
		[[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(unrelated, State2, Unrelated, State3),
		justify(H, [A], info(Detail), State3, State4),
		justify(H, [A], info(OtherDetail), State4, State5),
		justify(Unrelated, [], axiom, State5, State6),
		Detail = OtherDetail,
		retract_justification(Unrelated, [], axiom, State6, State),
		var(Detail),
		justifications(State, Rules),
		why(H, State, Why),
		label(H, State, Label).

	test(atms_retract_justifications_no_matches_01, deterministic(State == State1)) :-
		new(_Representation_, State0),
		create_node(h, State0, H, State1),
		retract_justifications([], State1, State2),
		retract_justifications([justification(H, [], missing), justification(node(99), [node(98)], missing)], State2, State).

	test(atms_retract_justifications_normalization_01, deterministic([Rules, Label, SameState] == [
		[justification(node(2),[node(0)],retained)], [[node(0)]], true
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		justify(H, [A, B], removed, State3, State4),
		justify(H, [B], removed_too, State4, State5),
		justify(H, [A], retained, State5, State6),
		Targets = [justification(H, [B, A, B], removed), justification(H, [A, B], removed), justification(H, [B], removed_too)],
		retract_justifications(Targets, State6, State7),
		retract_justifications(Targets, State7, State),
		justifications(State, Rules),
		label(H, State, Label),
		(	State == State7 ->
			SameState = true
		;	SameState = false
		).

	test(atms_retract_justifications_aliased_occurrences_01, deterministic([Before, After, Label, Repeated] == [
		[justification(node(1),[node(0)],info(Detail)), justification(node(1),[node(0)],info(Detail))], [], [], true
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		justify(H, [A], info(Detail), State2, State3),
		justify(H, [A], info(OtherDetail), State3, State4),
		Detail = OtherDetail,
		justifications(State4, Before),
		retract_justifications([justification(H, [A], info(_Different))], State4, Unchanged),
		Unchanged == State4,
		var(Detail),
		retract_justifications([justification(H, [A], info(Detail))], State4, State5),
		retract_justifications([justification(H, [A], info(Detail))], State5, State),
		justifications(State, After),
		label(H, State, Label),
		(	State == State5 ->
			Repeated = true
		;	Repeated = false
		).

	test(atms_retract_justifications_identity_and_order_01, deterministic([Rules, Why, Label] == [
		[justification(node(2),[node(0)],first), justification(node(2),[node(1)],last)],
		[justification(node(2),[node(0)],first), justification(node(2),[node(1)],last)],
		[[node(0)], [node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		justify(H, [A], first, State3, State4),
		justify(H, [A], middle, State4, State5),
		justify(H, [B], last, State5, State6),
		retract_justifications([justification(H, [A], middle), justification(H, [], missing)], State6, State),
		justifications(State, Rules),
		why(H, State, Why),
		label(H, State, Label).

	test(atms_retract_nodes_no_matches_01, deterministic(State == State1)) :-
		new(_Representation_, State0),
		create_node(h, State0, _, State1),
		retract_nodes([], State1, State2),
		retract_nodes([node(99), node(99)], State2, State).

	test(atms_retract_nodes_mixed_roles_01, deterministic([Nodes, Assumptions, Rules, Label, Nogoods, Interpretations, NextNode] == [
		[node(1)-b, node(3)-k], [node(1)], [justification(node(3),[node(1)],alternative)],
		[[node(1)]], [], [[node(1)]], node(5)
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		create_contradiction(false, State4, Bottom, State5),
		justify(H, [A], incoming, State5, State6),
		justify(K, [H, A], outgoing, State6, State7),
		justify(Bottom, [A, B], clash, State7, State8),
		justify(K, [B], alternative, State8, State9),
		retract_nodes([Bottom, H, A, H, node(99)], State9, State10),
		retract_nodes([A, H, Bottom], State10, State),
		State == State10,
		nodes(State, Nodes),
		assumptions(State, Assumptions),
		justifications(State, Rules),
		label(K, State, Label),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations),
		create_node(next, State, NextNode, _).

	test(atms_retract_nodes_all_01, deterministic([Nodes, Assumptions, Rules, Nogoods, Interpretations, NextNode] == [
		[], [], [], [], [[]], node(3)
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_contradiction(false, State2, Bottom, State3),
		justify(H, [A], support, State3, State4),
		justify(Bottom, [H], clash, State4, State5),
		retract_nodes([H, Bottom, A], State5, State),
		nodes(State, Nodes),
		assumptions(State, Assumptions),
		justifications(State, Rules),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations),
		create_node(next, State, NextNode, _).

	test(atms_rebuild_aliased_justifications_node_01, deterministic(Rules == [
		justification(node(1),[node(0)],info(Detail)), justification(node(1),[node(0)],info(Detail))
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		create_node(unrelated, State2, Unrelated, State3),
		justify(H, [A], info(Detail), State3, State4),
		justify(H, [A], info(OtherDetail), State4, State5),
		justify(Unrelated, [], axiom, State5, State6),
		Detail = OtherDetail,
		retract_node(Unrelated, State6, State),
		justifications(State, Rules).

	test(atms_retract_nodes_isolated_fast_path_01, deterministic([Rules, Label, Nogoods, Interpretations, OldLabel, NextNode] == [
		[justification(node(2),[node(0)],support), justification(node(3),[node(0),node(1)],clash)],
		[[node(0)]], [[node(0),node(1)]], [[node(0)], [node(1)]], [[node(0)]], node(6)
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [A], support, State4, State5),
		justify(Bottom, [A, B], clash, State5, State6),
		create_node(isolated, State6, Isolated, State7),
		create_contradiction(isolated_false, State7, IsolatedFalse, State8),
		retract_nodes([Isolated, IsolatedFalse], State8, State),
		\+ node(Isolated, State),
		\+ label(Isolated, State, _),
		\+ contradiction(IsolatedFalse, State),
		justifications(State, Rules),
		label(H, State, Label),
		label(H, State8, OldLabel),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations),
		create_node(next, State, NextNode, _).

	test(atms_retract_nodes_incoming_axiom_01, deterministic([Rules, Label] == [[], []])) :-
		new(_Representation_, State0),
		create_node(fact, State0, Fact, State1),
		create_node(dependent, State1, Dependent, State2),
		justify(Fact, [], axiom, State2, State3),
		justify(Dependent, [Fact], dependent, State3, State4),
		retract_nodes([Fact], State4, State),
		justifications(State, Rules),
		label(Dependent, State, Label).

	test(atms_retract_nodes_unconditional_contradiction_01, deterministic([Label, Nogoods, Interpretations] == [
		[[node(0)]], [], [[node(0)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_contradiction(false, State1, Bottom, State2),
		justify(Bottom, [], axiom, State2, State3),
		retract_nodes([Bottom], State3, State),
		label(A, State, Label),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations).

	test(atms_retract_nodes_isolated_assumption_01, deterministic([Assumptions, Interpretations] == [[], [[]]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		retract_nodes([A], State1, State),
		assumptions(State, Assumptions),
		interpretations(State, Interpretations).

	test(atms_retract_justification_aliased_single_occurrence_01, deterministic(Rules == [
		justification(node(1),[node(0)],info(Detail))
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		justify(H, [A], info(Detail), State2, State3),
		justify(H, [A], info(OtherDetail), State3, State4),
		Detail = OtherDetail,
		retract_justification(H, [A], info(Detail), State4, State),
		justifications(State, Rules).

	test(atms_retract_justifications_reconstruction_01, deterministic([BatchSnapshot, SequentialSnapshot] == [
		ExpectedSnapshot, ExpectedSnapshot
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_node(h, State3, H, State4),
		create_node(k, State4, K, State5),
		create_contradiction(false_ab, State5, BottomAB, State6),
		create_contradiction(false_bc, State6, BottomBC, State7),
		justify(H, [K], backward, State7, State8),
		justify(K, [H], forward, State8, State9),
		justify(H, [B, C], larger, State9, State10),
		justify(H, [A], smaller, State10, State11),
		justify(BottomAB, [A, B], clash_ab, State11, State12),
		justify(BottomBC, [B, C], clash_bc, State12, State13),
		retract_justifications([justification(H, [A], smaller), justification(BottomAB, [B, A], clash_ab)], State13, Batch),
		retract_justification(H, [A], smaller, State13, Sequential0),
		retract_justification(BottomAB, [A, B], clash_ab, Sequential0, Sequential),
		new(_Representation_, Expected0),
		create_assumption(a, Expected0, A, Expected1),
		create_assumption(b, Expected1, B, Expected2),
		create_assumption(c, Expected2, C, Expected3),
		create_node(h, Expected3, H, Expected4),
		create_node(k, Expected4, K, Expected5),
		create_contradiction(false_ab, Expected5, BottomAB, Expected6),
		create_contradiction(false_bc, Expected6, BottomBC, Expected7),
		justify(H, [K], backward, Expected7, Expected8),
		justify(K, [H], forward, Expected8, Expected9),
		justify(H, [B, C], larger, Expected9, Expected10),
		justify(BottomBC, [B, C], clash_bc, Expected10, Expected),
		state_snapshot(Batch, [], BatchSnapshot),
		state_snapshot(Sequential, [], SequentialSnapshot),
		state_snapshot(Expected, [], ExpectedSnapshot).

	test(atms_retract_nodes_reconstruction_01, deterministic([BatchSnapshot, SequentialSnapshot, NextNode] == [
		ExpectedSnapshot, ExpectedSnapshot, node(6)
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		create_contradiction(false, State4, Bottom, State5),
		create_node(isolated, State5, Isolated, State6),
		justify(H, [A], support, State6, State7),
		justify(K, [H], dependent, State7, State8),
		justify(K, [B], alternative, State8, State9),
		justify(Bottom, [A, B], clash, State9, State10),
		retract_nodes([A, H, Isolated, A], State10, Batch),
		retract_node(A, State10, Sequential0),
		retract_node(H, Sequential0, Sequential1),
		retract_node(Isolated, Sequential1, Sequential),
		new(_Representation_, Expected0),
		create_node(padding, Expected0, _, Expected1),
		create_assumption(b, Expected1, B, Expected2),
		create_node(padding, Expected2, _, Expected3),
		create_node(k, Expected3, K, Expected4),
		create_contradiction(false, Expected4, Bottom, Expected5),
		create_node(padding, Expected5, _, Expected6),
		justify(K, [B], alternative, Expected6, Expected),
		state_snapshot(Batch, [], BatchSnapshot),
		state_snapshot(Sequential, [], SequentialSnapshot),
		state_snapshot(Expected, [A, H, Isolated], ExpectedSnapshot),
		\+ node(A, Batch),
		\+ node(H, Batch),
		\+ node(Isolated, Batch),
		create_node(next, Batch, NextNode, _).

	test(atms_retract_nodes_batch_identifier_gaps_01, deterministic([Label, Assumptions, NextNode] == [
		[[node(31),node(32)]], [node(15),node(31),node(32)], node(34)
	])) :-
		new(_Representation_, State0),
		create_padding_nodes(15, State0, State1),
		create_assumption(a, State1, A, State2),
		create_assumption(b, State2, B, State3),
		create_padding_nodes(14, State3, State4),
		create_assumption(c, State4, C, State5),
		create_assumption(d, State5, D, State6),
		create_node(h, State6, H, State7),
		justify(H, [A, B], first, State7, State8),
		justify(H, [C, D], second, State8, State9),
		retract_nodes([node(0), B, B], State9, State),
		label(H, State, Label),
		assumptions(State, Assumptions),
		create_node(next, State, NextNode, _).

	test(atms_retract_justifications_restored_support_01, deterministic([Label, Nogoods, Interpretations] == [
		[[node(0),node(1)]], [], [[node(0),node(1)]]
	])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_node(h, State2, H, State3),
		create_contradiction(false, State3, Bottom, State4),
		justify(H, [A, B], larger, State4, State5),
		justify(H, [A], smaller, State5, State6),
		justify(Bottom, [A, B], clash, State6, State7),
		retract_justifications([justification(H, [A], smaller), justification(Bottom, [A, B], clash)], State7, State),
		label(H, State, Label),
		findall(Environment, nogood(Environment, State), Nogoods),
		interpretations(State, Interpretations).

	test(atms_counting_environment_reset_01, deterministic([Calls, Singletons, Nodes, ResetCalls, ResetSingletons] == [2, 1, [node(0)], 0, 0])) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		Counter::singleton(node(0), Environment),
		Counter::to_list(Environment, Nodes),
		Counter::counts(Calls, Singletons),
		Counter::reset,
		Counter::counts(ResetCalls, ResetSingletons).

	test(atms_retraction_empty_batches_01, deterministic((State == State0, Calls == 0))) :-
		counted_fixture(State0, _, _, _),
		counting_environment(_Representation_)::reset,
		retract_justifications([], State0, State1),
		retract_nodes([], State1, State),
		counting_environment(_Representation_)::counts(Calls, _).

	test(atms_retraction_empty_state_01, deterministic(State == State0)) :-
		new(_Representation_, State0),
		retract_justifications([], State0, State1),
		retract_nodes([node(99), node(99)], State1, State2),
		retract_justifications([justification(node(99), [], missing)], State2, State).

	test(atms_retraction_missing_batches_01, deterministic((State == State0, Calls == 0, var(Info)))) :-
		counted_fixture(State0, Assumption, Head, Info),
		counting_environment(_Representation_)::reset,
		retract_justifications([justification(Head, [Assumption], info(_)), justification(node(99), [], missing)], State0, State1),
		retract_nodes([node(99), node(100), node(99)], State1, State),
		counting_environment(_Representation_)::counts(Calls, _).

	test(atms_retraction_filter_suffix_01, deterministic((Remaining == [Oldest], Removed == [Newest], var(Info)))) :-
		Oldest = justification(node(1), [node(0)], info(Info)),
		Newest = justification(node(2), [node(0)], newest),
		atms<<remove_matching_justifications([Newest, Oldest], linear([Newest]), Remaining, Removed).

	test(atms_retraction_filter_middle_01, deterministic((Remaining == [Newest, Oldest], Removed == [Middle]))) :-
		Oldest = justification(node(1), [], oldest),
		Middle = justification(node(2), [], middle),
		Newest = justification(node(3), [], newest),
		atms<<remove_matching_justifications([Newest, Middle, Oldest], linear([Middle]), Remaining, Removed).

	test(atms_retraction_filter_unchanged_01, deterministic((Remaining == Original, Removed == []))) :-
		Original = [justification(node(1), [], first), justification(node(2), [], second)],
		atms<<remove_matching_justifications(Original, linear([]), Remaining, Removed).

	test(atms_retraction_isolated_no_rebuild_01, deterministic((Calls == 0, Label == [[Assumption]]))) :-
		counted_fixture(State0, Assumption, Head, _),
		create_node(isolated, State0, Isolated, State1),
		counting_environment(_Representation_)::reset,
		retract_nodes([Isolated, Isolated], State1, State),
		counting_environment(_Representation_)::counts(Calls, _),
		label(Head, State, Label).

	test(atms_retraction_index_ground_01, deterministic((Rules == Expected, Explanation == Expected, Unchanged == Original))) :-
		numbered_fixture(24, State0, Assumption, Head),
		numbered_targets(30, Assumption, Head, Targets),
		justifications(State0, Original),
		linear_rule_survivors(Original, Targets, Expected),
		length(Expected, 12),
		retract_justifications(Targets, State0, State),
		justifications(State, Rules),
		why(Head, State, Explanation),
		label(Head, State, [[Assumption]]),
		justifications(State0, Unchanged),
		label(Head, State0, [[Assumption]]).

	test(atms_retraction_index_full_info_01, deterministic(Rules == [justification(Head, [Assumption], retained)])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, State2),
		justify(Head, [Assumption], removed, State2, State3),
		justify(Head, [Assumption], retained, State3, State4),
		retract_justifications([justification(Head, [Assumption, Assumption], removed), justification(Head, [Assumption], missing), justification(Head, [Assumption], removed)], State4, State),
		justifications(State, Rules).

	test(atms_retraction_index_mixed_identity_01, deterministic((Rules == [justification(Head, [Assumption], info(Other))], var(Info), var(Other), Info \== Other))) :-
		new(_Representation_, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, State2),
		justify(Head, [Assumption], ground, State2, State3),
		justify(Head, [Assumption], info(Info), State3, State4),
		justify(Head, [Assumption], info(Other), State4, State5),
		retract_justifications([justification(Head, [Assumption], ground), justification(Head, [Assumption], info(Info)), justification(Head, [Assumption], info(_))], State5, State),
		justifications(State, Rules).

	test(atms_retraction_index_aliased_01, deterministic((Rules == [], var(Info)))) :-
		new(_Representation_, State0),
		create_node(h, State0, Head, State1),
		justify(Head, [], info(Info), State1, State2),
		justify(Head, [], info(Other), State2, State3),
		Info = Other,
		retract_justifications([justification(Head, [], info(Info)), justification(Head, [], info(_))], State3, State),
		justifications(State, Rules).

	test(atms_retraction_index_nodes_01, deterministic(Snapshot == Expected)) :-
		numbered_fixture(20, State0, Assumption, Head),
		create_node(isolated, State0, Isolated, State1),
		retract_nodes([Head, Isolated, Head, node(999)], State1, State),
		retract_node(Head, State1, Sequential0),
		retract_node(Isolated, Sequential0, Sequential),
		state_snapshot(State, [], Snapshot),
		state_snapshot(Sequential, [], Expected),
		assumptions(State, [Assumption]).

	test(atms_retraction_index_partition_01, deterministic((GroundFound == true, MissingFound == false, VariableFound == true, OtherFound == false, var(Info)))) :-
		Target = justification(node(1), [], info(Info)),
		atms<<target_index([Target, justification(node(1), [], ground), Target], Index),
		( atms<<matches_target(justification(node(1), [], ground), Index) -> GroundFound = true; GroundFound = false ),
		( atms<<matches_target(justification(node(1), [], absent), Index) -> MissingFound = true; MissingFound = false ),
		( atms<<matches_target(Target, Index) -> VariableFound = true; VariableFound = false ),
		( atms<<matches_target(justification(node(1), [], info(_)), Index) -> OtherFound = true; OtherFound = false ).

	test(atms_retract_batch_noop_01, deterministic((State == State0, Calls == 0, var(Info)))) :-
		counted_fixture(State0, Assumption, Head, Info),
		counting_environment(_Representation_)::reset,
		retract_batch([], [], State0, State1),
		retract_batch([node(999), node(999)], [justification(Head, [Assumption], missing)], State1, State),
		counting_environment(_Representation_)::counts(Calls, _).

	test(atms_retract_batch_one_rebuild_01, deterministic((Singletons == 1, Label == [], OldLabel == [[Assumption], [Backup]], Next == node(3)))) :-
		counted_fixture(State0, Assumption, Head, Info),
		create_assumption(b, State0, Backup, State1),
		justify(Head, [Backup], backup, State1, State2),
		counting_environment(_Representation_)::reset,
		retract_batch([Backup, Backup], [justification(Head, [Assumption], info(Info)), justification(Head, [Backup], backup)], State2, State),
		counting_environment(_Representation_)::counts(_, Singletons),
		label(Head, State, Label),
		label(Head, State2, OldLabel),
		create_node(next, State, Next, _).

	test(atms_retract_batch_isolated_01, deterministic((Calls == 0, Label == [[Assumption]], Roles == []))) :-
		counted_fixture(State0, Assumption, Head, _),
		create_node(isolated, State0, Isolated, State1),
		create_contradiction(false, State1, Bottom, State2),
		counting_environment(_Representation_)::reset,
		retract_batch([Isolated, Bottom], [justification(Head, [], missing)], State2, State),
		counting_environment(_Representation_)::counts(Calls, _),
		label(Head, State, Label),
		findall(Node, contradiction(Node, State), Roles).

	test(atms_retract_batch_overlap_identity_01, deterministic((Rules == [], Nodes == [Assumption-a], var(Info)))) :-
		counted_fixture(State0, Assumption, Head, Info),
		justify(Head, [Assumption], info(Other), State0, State1),
		Info = Other,
		retract_batch([Head], [justification(Head, [Assumption, Assumption], info(Info)), justification(Head, [Assumption], info(Info))], State1, State),
		justifications(State, Rules),
		nodes(State, Nodes).

	test(atms_retract_batch_degenerate_01, deterministic((NodeSnapshot == ExpectedNodes, RuleSnapshot == ExpectedRules))) :-
		numbered_fixture(8, State0, Assumption, Head),
		Target = justification(Head, [Assumption], index(2)),
		retract_batch([Assumption], [], State0, NodesState),
		retract_nodes([Assumption], State0, NodeReference),
		retract_batch([], [Target, Target], State0, RulesState),
		retract_justifications([Target, Target], State0, RuleReference),
		state_snapshot(NodesState, [], NodeSnapshot),
		state_snapshot(NodeReference, [], ExpectedNodes),
		state_snapshot(RulesState, [], RuleSnapshot),
		state_snapshot(RuleReference, [], ExpectedRules).

	test(atms_retract_batch_reconstruction_01, deterministic((Snapshot == Expected, Snapshot == SequentialSnapshot, Label == [[Assumption]]))) :-
		new(_Representation_, State0),
		create_assumption(a, State0, Assumption, State1),
		create_assumption(b, State1, Backup, State2),
		create_node(h, State2, Head, State3),
		create_node(cycle, State3, Cycle, State4),
		create_contradiction(false, State4, Bottom, State5),
		justify(Head, [Assumption], support, State5, State6),
		justify(Cycle, [Head], forward, State6, State7),
		justify(Head, [Cycle], backward, State7, State8),
		justify(Bottom, [Assumption], clash, State8, State9),
		justify(Bottom, [Assumption, Backup], subsumed, State9, State10),
		Targets = [justification(Bottom, [Assumption], clash)],
		retract_batch([Backup], Targets, State10, State),
		retract_nodes([Backup], State10, Sequential0),
		retract_justifications(Targets, Sequential0, Sequential),
		new(_Representation_, Oracle0),
		create_assumption(a, Oracle0, Assumption, Oracle1),
		create_node(padding, Oracle1, Backup, Oracle2),
		create_node(h, Oracle2, Head, Oracle3),
		create_node(cycle, Oracle3, Cycle, Oracle4),
		create_contradiction(false, Oracle4, Bottom, Oracle5),
		justify(Head, [Assumption], support, Oracle5, Oracle6),
		justify(Cycle, [Head], forward, Oracle6, Oracle7),
		justify(Head, [Cycle], backward, Oracle7, Oracle),
		state_snapshot(State, [], Snapshot),
		state_snapshot(Sequential, [], SequentialSnapshot),
		state_snapshot(Oracle, [Backup], Expected),
		create_assumption(c, State, NewAssumption, State11),
		justify(Head, [NewAssumption], new_support, State11, State12),
		retract_justification(Head, [NewAssumption], new_support, State12, State13),
		label(Head, State13, Label).

	test(atms_redundant_single_rule_01, deterministic((Calls == 0, Label == [[Assumption]], Rules == [justification(Head, [Assumption], info(Info))], var(Info)))) :-
		counted_fixture(State0, Assumption, Head, Info),
		justify(Head, [Assumption], redundant, State0, State1),
		counting_environment(_Representation_)::reset,
		retract_justification(Head, [Assumption, Assumption], redundant, State1, State),
		counting_environment(_Representation_)::counts(Calls, _),
		label(Head, State, Label),
		justifications(State, Rules).

	test(atms_redundant_batch_rules_01, deterministic((Calls == 0, Label == [[Assumption]], Rules == [justification(Head, [Assumption], retained)]))) :-
		counted_fixture(State0, Assumption, Head, Info),
		justify(Head, [Assumption], retained, State0, State1),
		justify(Head, [Assumption], removed, State1, State2),
		counting_environment(_Representation_)::reset,
		retract_batch([node(999)], [justification(Head, [Assumption], info(Info)), justification(Head, [Assumption], removed)], State2, State),
		counting_environment(_Representation_)::counts(Calls, _),
		label(Head, State, Label),
		justifications(State, Rules).

	test(atms_redundant_last_signature_01, deterministic((Singletons == 1, Label == [], Rules == []))) :-
		counted_fixture(State0, Assumption, Head, Info),
		justify(Head, [Assumption], second, State0, State1),
		counting_environment(_Representation_)::reset,
		retract_justifications([justification(Head, [Assumption], info(Info)), justification(Head, [Assumption], second)], State1, State),
		counting_environment(_Representation_)::counts(_, Singletons),
		label(Head, State, Label),
		justifications(State, Rules).

	test(atms_redundant_equal_labels_fallback_01, deterministic((Singletons == 1, Label == [[Assumption]]))) :-
		counted_fixture(State0, Assumption, Head, Info),
		create_node(bridge, State0, Bridge, State1),
		justify(Bridge, [Assumption], bridge, State1, State2),
		justify(Head, [Bridge], different_signature, State2, State3),
		counting_environment(_Representation_)::reset,
		retract_justification(Head, [Assumption], info(Info), State3, State),
		counting_environment(_Representation_)::counts(_, Singletons),
		label(Head, State, Label).

	test(atms_redundant_contradiction_01, deterministic((Calls == 0, Nogoods == [[Assumption]], Label == [], Interpretations == [[]]))) :-
		counted_fixture(State0, Assumption, Head, _),
		create_contradiction(false, State0, Bottom, State1),
		justify(Bottom, [Assumption], first, State1, State2),
		justify(Bottom, [Assumption], second, State2, State3),
		counting_environment(_Representation_)::reset,
		retract_justification(Bottom, [Assumption], first, State3, State),
		counting_environment(_Representation_)::counts(Calls, _),
		findall(Environment, nogood(Environment, State), Nogoods),
		label(Head, State, Label),
		interpretations(State, Interpretations).

	test(atms_redundant_unconditional_01, deterministic((Calls == 0, Label == [[]]))) :-
		counted_fixture(State0, _, Head, _),
		justify(Head, [], first, State0, State1),
		justify(Head, [], second, State1, State2),
		counting_environment(_Representation_)::reset,
		retract_justification(Head, [], first, State2, State),
		counting_environment(_Representation_)::counts(Calls, _),
		label(Head, State, Label).

	test(atms_redundant_cycles_and_assumption_01, deterministic((Calls == 0, Label == [[Assumption]], AssumptionLabel == [[Assumption]]))) :-
		counted_fixture(State0, Assumption, Head, _),
		justify(Head, [Head], self_first, State0, State1),
		justify(Head, [Head], self_second, State1, State2),
		justify(Assumption, [Head], cycle_first, State2, State3),
		justify(Assumption, [Head], cycle_second, State3, State4),
		counting_environment(_Representation_)::reset,
		retract_justifications([justification(Head, [Head], self_first), justification(Assumption, [Head], cycle_first)], State4, State),
		counting_environment(_Representation_)::counts(Calls, _),
		label(Head, State, Label),
		label(Assumption, State, AssumptionLabel).

	test(atms_redundant_alias_occurrences_01, deterministic((FirstCalls == 0, BatchCalls == 0, Rules == [justification(Head, [Assumption], retained)], var(Info)))) :-
		counted_fixture(State0, Assumption, Head, Info),
		justify(Head, [Assumption], info(Other), State0, State1),
		justify(Head, [Assumption], retained, State1, State2),
		Info = Other,
		counting_environment(_Representation_)::reset,
		retract_justification(Head, [Assumption], info(Info), State2, State3),
		counting_environment(_Representation_)::counts(FirstCalls, _),
		justifications(State3, [justification(Head, [Assumption], info(Info)), justification(Head, [Assumption], retained)]),
		counting_environment(_Representation_)::reset,
		retract_justifications([justification(Head, [Assumption], info(Info))], State3, State),
		counting_environment(_Representation_)::counts(BatchCalls, _),
		justifications(State, Rules).

	test(atms_duplicate_index_candidates_01, deterministic((Candidates == [Rule], Same == State, Calls == 0, var(Info)))) :-
		counted_fixture(State0, Assumption, Head, Info),
		add_unrelated_rules(20, State0, State),
		Rule = justification(Head, [Assumption], info(Info)),
		atms<<duplicate_candidates(Rule, State, Candidates),
		counting_environment(_Representation_)::reset,
		justify(Head, [Assumption, Assumption], info(Info), State, Same),
		counting_environment(_Representation_)::counts(Calls, _).

	test(atms_duplicate_after_grouped_removal_01, deterministic((Same == State2, Rules == [justification(Head, [Assumption], retained)]))) :-
		new(_Representation_, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, Initial),
		justify(Head, [Assumption], removed, Initial, First),
		justify(Head, [Assumption], retained, First, Both),
		retract_justifications([justification(Head, [Assumption], removed)], Both, State2),
		justify(Head, [Assumption], retained, State2, Same),
		justifications(Same, Rules),
		retract_justification(Head, [Assumption], retained, Same, Rebuilt),
		justify(Head, [Assumption], retained, Rebuilt, Readded),
		label(Head, Readded, [[Assumption]]),
		clear(Readded, Cleared),
		create_node(new_head, Cleared, NewHead, EmptyState),
		justify(NewHead, [], unconditional, EmptyState, Unconditional),
		justify(NewHead, [], unconditional, Unconditional, Unconditional).

	test(atms_product_smallest_first_01, deterministic((SortedCalls < UnsortedCalls, Actual == Expected))) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		add_free_assumptions(9, State0, [A, B, C, D, E, F, G, H, I], State1),
		create_contradiction(bottom, State1, Bottom, State2),
		justify(Bottom, [A, I], first, State2, State3),
		justify(Bottom, [B, I], second, State3, State4),
		justify(Bottom, [C, I], third, State4, State),
		singleton_environments([A, B, C, D], Counter, First),
		singleton_environments([E, F, G, H], Counter, Second),
		singleton_environments([I], Counter, Third),
		Counter::from_list([A, I], FirstNogood),
		Counter::from_list([B, I], SecondNogood),
		Counter::from_list([C, I], ThirdNogood),
		Lists = [First, Second, Third],
		Counter::reset,
		atms<<combine_state_environment_lists(Lists, State, Ordered),
		Counter::operation_count(union, SortedCalls),
		Counter::reset,
		Counter::empty(Empty),
		atms<<combine_environment_lists(Lists, [Empty], [ThirdNogood, SecondNogood, FirstNogood], Counter, Unordered),
		Counter::operation_count(union, UnsortedCalls),
		atms<<external_environments(Ordered, Counter, External),
		atms<<external_environments(Unordered, Counter, UnsortedExternal),
		sort(External, Actual),
		sort(UnsortedExternal, Actual),
		Expected = [[D, E, I], [D, F, I], [D, G, I], [D, H, I]].

	test(atms_product_stable_sizes_01, deterministic(Ordered == [[short], [first, first], [second, second]])) :-
		atms<<size_environment_lists([[first, first], [short], [second, second]], Sized),
		keysort(Sized, Sorted),
		atms<<unsize_environment_lists(Sorted, Ordered).

	test(atms_product_ordered_trigger_delta_01, deterministic(Actual == [[A, B, C], [A, B, D]])) :-
		new(_Representation_, State0),
		add_free_assumptions(4, State0, [A, B, C, D], State1),
		create_node(trigger, State1, Trigger, State2),
		create_node(other, State2, Other, State3),
		justify(Trigger, [A], first, State3, State4),
		justify(Trigger, [B], second, State4, State5),
		justify(Other, [C], third, State5, State6),
		justify(Other, [D], fourth, State6, State),
		_Representation_::singleton(B, Delta),
		atms<<candidates_with_delta([A, Trigger, Other], Trigger, [Delta], State, Candidates),
		atms<<external_environments(Candidates, _Representation_, External),
		sort(External, Actual).

	test(atms_product_empty_identity_01, deterministic(Unions == 0)) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_assumption(a, State0, A, State1),
		create_contradiction(bottom, State1, Bottom, State2),
		justify(Bottom, [A], forbidden, State2, InconsistentState),
		Counter::empty(Empty),
		Counter::singleton(A, Environment),
		Counter::reset,
		atms<<combine_state_environment_lists([[Environment]], State1, [Environment]),
		atms<<combine_environment_sets([Environment], [Empty], [], Counter, [Environment]),
		atms<<combine_environment_sets([Empty], [Environment], [Environment], Counter, []),
		atms<<combine_state_environment_lists([[Environment]], InconsistentState, []),
		Counter::operation_count(union, Unions).

	test(atms_product_empty_operands_01, deterministic(Unions == 0)) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State),
		Counter::singleton(node(0), Environment),
		Counter::reset,
		atms<<combine_state_environment_lists([[], [Environment]], State, []),
		atms<<combine_state_environment_lists([[Environment], [], [Environment]], State, []),
		atms<<combine_state_environment_lists([[Environment], [Environment], []], State, []),
		Counter::operation_count(union, Unions).

	test(atms_product_inconsistent_prefix_01, deterministic(Unions =< 2)) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_contradiction(bottom, State3, Bottom, State4),
		justify(Bottom, [A, B], clash, State4, State),
		Counter::singleton(A, First),
		Counter::singleton(B, Second),
		Counter::singleton(C, Third),
		Counter::reset,
		atms<<combine_state_environment_lists([[First], [Second], [Third]], State, []),
		Counter::operation_count(union, Unions).

	test(atms_nogood_batch_final_antichain_01, deterministic((BatchCalls < SequentialCalls, Same == Batch))) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_node(head, State0, Head, State1),
		add_label_alternatives(8, Head, State1, State2),
		Counter::from_list([node(1), node(2)], Larger),
		Counter::from_list([node(1)], Smaller),
		Counter::reset,
		atms<<record_nogoods([Larger, Smaller], State2, Batch),
		Counter::counts(BatchCalls, _),
		Counter::reset,
		atms<<record_nogoods([Larger], State2, Sequential0),
		atms<<record_nogoods([Smaller], Sequential0, Sequential),
		Counter::counts(SequentialCalls, _),
		state_snapshot(Batch, [], Snapshot),
		state_snapshot(Sequential, [], Snapshot),
		atms<<record_nogoods([Larger, Smaller, Smaller], Batch, Same).

	test(atms_nogood_batch_propagation_01, deterministic) :-
		verify_edit_history([
			create(ordinary),
			add(justification(node(4), [node(0)], first)),
			add(justification(node(4), [node(1)], second)),
			add(justification(node(3), [node(4)], batch)),
			add(justification(node(2), [node(4)], stale)),
			remove_rule(justification(node(3), [node(4)], batch))
		]).

	test(atms_mixed_missing_nodes_shortcut_01, deterministic((Unchanged == State0, Calls == 0, var(Info)))) :-
		counted_fixture(Initial, Assumption, Head, Info),
		add_numbered_rules(20, Assumption, Head, Initial, State0),
		counting_environment(_Representation_)::reset,
		retract_batch([node(999), node(998), node(999)], [], State0, Unchanged),
		counting_environment(_Representation_)::counts(Calls, _),
		retract_batch([node(999)], [justification(Head, [Assumption], info(Info))], State0, State),
		justifications(State, Rules),
		length(Rules, 20),
		label(Head, State, [[Assumption]]).

	test(atms_grouped_dependency_order_01, deterministic(Actual == Expected)) :-
		numbered_fixture(12, State0, Assumption, Head),
		numbered_targets(6, Assumption, Head, EvenTargets),
		Targets = [justification(Head, [Assumption], index(1))| EvenTargets],
		atms<<dependents(Assumption, State0, Original),
		linear_rule_survivors(Original, Targets, Expected),
		retract_justifications(Targets, State0, State),
		atms<<dependents(Assumption, State, Actual),
		label(Head, State, [[Assumption]]).

	test(atms_redundant_dependency_occurrences_01, deterministic((Entries == [node(0)-[Rule], node(1)-[Rule]], EmptyEntries == [], var(Info)))) :-
		Rule = justification(node(2), [node(0), node(1)], info(Info)),
		OtherRule = justification(node(2), [node(0), node(1)], info(Other)),
		avltree::new(Empty),
		atms<<index_justification([node(0), node(1)], Rule, Empty, Index0),
		atms<<index_justification([node(0), node(1)], OtherRule, Index0, Index1),
		Info = Other,
		atms<<unindex_justifications([Rule], Index1, Index2),
		avltree::as_list(Index2, Entries),
		atms<<unindex_justifications([Rule], Index2, Index),
		avltree::as_list(Index, EmptyEntries).

	test(atms_redundant_future_propagation_01, deterministic((RemovalCalls == 0, ActualCalls == ExpectedCalls, Snapshot == Expected))) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(parent, State1, Parent, State2),
		create_node(h, State2, Head, State3),
		justify(Head, [Parent], first, State3, State4),
		justify(Head, [Parent], second, State4, State5),
		Counter::reset,
		retract_justification(Head, [Parent], first, State5, State6),
		Counter::counts(RemovalCalls, _),
		Counter::reset,
		justify(Parent, [Assumption], activate, State6, State),
		Counter::counts(ActualCalls, _),
		justify(Head, [Parent], second, State3, Oracle0),
		Counter::reset,
		justify(Parent, [Assumption], activate, Oracle0, Oracle),
		Counter::counts(ExpectedCalls, _),
		state_snapshot(State, [], Snapshot),
		state_snapshot(Oracle, [], Expected).

	test(atms_redundant_mixed_signature_fallback_01, deterministic((Singletons == 1, FirstLabel == [[Assumption]], SecondLabel == []))) :-
		counted_fixture(State0, Assumption, Head, Info),
		justify(Head, [Assumption], retained, State0, State1),
		create_node(other, State1, Other, State2),
		justify(Other, [Assumption], sole_support, State2, State3),
		counting_environment(_Representation_)::reset,
		retract_justifications([justification(Head, [Assumption], info(Info)), justification(Other, [Assumption], sole_support)], State3, State),
		counting_environment(_Representation_)::counts(_, Singletons),
		label(Head, State, FirstLabel),
		label(Other, State, SecondLabel).

	quick_check(edit_sequences_generated_01, edit_sequence(+list(byte,20)), [
		n(25), l(edit_sequence_labels),
		setup(reset_sequence_seed(Saved)), cleanup(fast_random::set_seed(Saved))
	]).

	test(atms_edit_sequences_all_operations_01, deterministic) :-
		edit_sequence([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]).

	test(atms_edit_sequences_restored_nogoods_01, deterministic) :-
		verify_edit_history([
			add(justification(node(2), [node(0), node(1)], larger)),
			remove_rule(justification(node(2), [node(0)], first)),
			remove_rules([justification(node(2), [node(1)], second)]),
			add(justification(node(3), [node(0)], small)),
			add(justification(node(3), [node(0)], shared)),
			remove_rule(justification(node(3), [node(0)], small)),
			remove_rules([justification(node(3), [node(0)], shared), justification(node(3), [node(0), node(1)], clash)]),
			add(justification(node(2), [], unconditional)),
			remove_batch([node(1), node(1)], [justification(node(2), [], unconditional)]),
			remove_nodes([]),
			remove_rules([]),
			remove_batch([node(999)], [justification(node(999), [], missing)]),
			create(assumption),
			add(justification(node(2), [node(4)], later))
		]).

	test(atms_edit_sequences_cycles_and_inconsistency_01, deterministic) :-
		verify_edit_history([
			create(ordinary),
			add(justification(node(4), [node(2)], forward)),
			add(justification(node(2), [node(4)], backward)),
			add(justification(node(4), [node(4)], self)),
			remove_rules([justification(node(2), [node(0)], first), justification(node(2), [node(1)], second)]),
			add(justification(node(2), [], unconditional)),
			remove_node(node(4)),
			add(justification(node(3), [], inconsistent)),
			remove_node(node(3)),
			create(contradiction),
			add(justification(node(5), [node(0)], clash)),
			remove_batch([node(0)], [justification(node(2), [], unconditional)]),
			clear,
			create(ordinary),
			add(justification(node(0), [], restarted))
		]).

	test(atms_edit_sequences_creation_limits_01, deterministic) :-
		verify_edit_history([
			create(assumption), create(assumption), create(assumption),
			create(ordinary), create(contradiction), create(ordinary),
			create(assumption), remove_node(node(0)), create(assumption),
			remove_rule(justification(node(999), [], missing))
		]).

	test(atms_verified_evidence_01, deterministic) :-
		history_fixture(State, Model),
		verify_model(State, Model, Evidence),
		ground(Evidence),
		matches_verified_state(State, Evidence).

	test(atms_verified_evidence_rules_01, deterministic) :-
		history_fixture(State0, Model),
		verify_model(State0, Model, Evidence),
		justify(node(2), [node(0)], extra, State0, State),
		label(node(2), State0, Label),
		label(node(2), State, Label),
		\+ matches_verified_state(State, Evidence),
		matches_verified_state(State0, Evidence).

	test(atms_verified_evidence_next_id_01, deterministic) :-
		history_fixture(State0, Model),
		verify_model(State0, Model, Evidence),
		State0 = atms_state(Representation, Next, Nodes, Assumptions, Contradictions, Rules, Labels, Nogoods, Dependents),
		WrongNext is Next + 1,
		State = atms_state(Representation, WrongNext, Nodes, Assumptions, Contradictions, Rules, Labels, Nogoods, Dependents),
		state_snapshot(State0, [], Snapshot),
		state_snapshot(State, [], Snapshot),
		\+ matches_verified_state(State, Evidence).

	test(atms_verified_evidence_ghost_label_01, deterministic) :-
		history_fixture(State0, Model0),
		apply_model_edit(create(ordinary), State0, Model0, State1, Model1),
		apply_model_edit(remove_node(node(4)), State1, Model1, State, Model),
		verify_model(State, Model, Evidence),
		State1 = atms_state(_, _, _, _, _, _, GhostLabels, _, _),
		State = atms_state(Representation, Next, Nodes, Assumptions, Contradictions, Rules, _, Nogoods, Dependents),
		Ghost = atms_state(Representation, Next, Nodes, Assumptions, Contradictions, Rules, GhostLabels, Nogoods, Dependents),
		state_snapshot(State, [], Snapshot),
		state_snapshot(Ghost, [], Snapshot),
		\+ matches_verified_state(Ghost, Evidence),
		matches_verified_state(State, Evidence).

	test(atms_verified_evidence_consistency_01, deterministic) :-
		history_fixture(State, Model),
		verify_model(State, Model, verified(Snapshot, Next, Absent, [truth(Environment, Known, inconsistent)| Truths])),
		\+ matches_verified_state(State, verified(Snapshot, Next, Absent, [truth(Environment, Known, consistent)| Truths])).

	test(atms_verified_evidence_gaps_and_clear_01, deterministic) :-
		history_fixture(State0, Model0),
		apply_model_edit(remove_node(node(1)), State0, Model0, State1, Model1),
		apply_model_edit(remove_node(node(3)), State1, Model1, State2, Model2),
		verify_model(State2, Model2, Evidence),
		Evidence = verified(_, 4, [node(1), node(3)], _),
		apply_model_edit(clear, State2, Model2, State3, Model3),
		matches_verified_state(State2, Evidence),
		verify_model(State3, Model3, Cleared),
		Cleared = verified(_, 0, [], _),
		apply_model_edit(create(ordinary), State3, Model3, State, Model),
		verify_model(State, Model, _),
		matches_verified_state(State3, Cleared).

	test(atms_history_evidence_noops_01, deterministic) :-
		history_fixture(State0, Model0),
		verify_model(State0, Model0, Evidence0),
		apply_model_edit(remove_batch([], []), State0, Model0, State, Model),
		State == State0,
		Model == Model0,
		verify_transition(State0, Model0, Evidence0, State, Model, Evidence),
		Evidence == Evidence0,
		verify_edit_history([
			remove_rules([]), remove_nodes([]), remove_batch([], []),
			remove_node(node(999)), remove_nodes([node(999), node(999)]),
			remove_rule(justification(node(999), [], missing)),
			remove_batch([node(999)], []),
			add(justification(node(2), [node(0)], first)),
			add(justification(node(2), [node(0)], first)),
			clear, clear, create(ordinary), remove_node(node(0)), remove_node(node(0))
		]).

	test(atms_history_evidence_both_guards_01, deterministic) :-
		history_fixture(State0, Model0),
		apply_model_edit(add(justification(node(2), [node(0)], extra)), State0, Model0, State, Model),
		label(node(2), State0, Label),
		label(node(2), State, Label),
		verify_transition(State0, Model, unused, State, Model, Evidence),
		verify_transition(State, Model0, unused, State, Model, Expected),
		Evidence == Expected,
		matches_verified_state(State, Evidence).

	test(atms_unchanged_label_calls_01, deterministic((Small < 40, Large < 40))) :-
		unchanged_label_calls(40, Small),
		unchanged_label_calls(80, Large).

	test(atms_interpretations_free_assumptions_01, deterministic((Interpretations == [Free], Calls < 40))) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_assumption(forbidden, State0, Forbidden, State1),
		add_free_assumptions(20, State1, Free, State2),
		create_contradiction(bottom, State2, Bottom, State3),
		justify(Bottom, [Forbidden], forbidden, State3, State),
		Counter::reset,
		interpretations(State, Interpretations),
		Counter::counts(Calls, _).

	test(atms_interpretations_overlapping_bounds_01, deterministic) :-
		verify_edit_history([
			create(assumption),
			add(justification(node(3), [node(1), node(4)], overlap)),
			remove_rule(justification(node(3), [node(0), node(1)], clash)),
			remove_node(node(1))
		]).

	test(atms_semantic_oracle_cycles_01, deterministic) :-
		verify_edit_history([
			create(ordinary),
			add(justification(node(4), [node(4)], self)),
			add(justification(node(4), [node(2)], forward)),
			add(justification(node(2), [node(4)], backward)),
			add(justification(node(0), [node(4)], assumption_consequent)),
			remove_rules([justification(node(2), [node(0)], first), justification(node(2), [node(1)], second)]),
			add(justification(node(4), [], unconditional)),
			add(justification(node(3), [], inconsistent)),
			remove_node(node(3)),
			clear
		]).

	test(atms_semantic_oracle_nogoods_01, deterministic) :-
		verify_edit_history([
			add(justification(node(3), [node(0)], smaller)),
			add(justification(node(3), [node(0)], shared)),
			remove_rule(justification(node(3), [node(0)], smaller)),
			remove_rules([justification(node(3), [node(0)], shared), justification(node(3), [node(0), node(1)], clash)]),
			add(justification(node(2), [node(0), node(1)], larger)),
			remove_rule(justification(node(2), [node(0)], first)),
			remove_batch([node(1)], [justification(node(2), [node(1)], second)]),
			create(assumption),
			add(justification(node(2), [node(4)], restored))
		]).

	% auxiliary predicates

	reset_sequence_seed(Saved) :-
		fast_random::get_seed(Saved),
		fast_random::reset_seed.

	edit_sequence_labels(Sequence, Labels) :-
		findall(Class, (member(Byte, Sequence), Opcode is Byte mod 16, opcode_class(Opcode, Class)), Classes),
		sort(Classes, Labels).

	opcode_class(0, creation).
	opcode_class(1, creation).
	opcode_class(2, creation).
	opcode_class(3, addition).
	opcode_class(4, unconditional).
	opcode_class(5, single_rule).
	opcode_class(6, batch_rules).
	opcode_class(7, single_node).
	opcode_class(8, batch_nodes).
	opcode_class(9, mixed_batch).
	opcode_class(10, empty_batch).
	opcode_class(11, missing_targets).
	opcode_class(12, repeated_addition).
	opcode_class(13, clear).
	opcode_class(14, multiple_antecedents).
	opcode_class(15, self_support).

	edit_sequence(Sequence) :-
		history_fixture(State, Model),
		verify_model(State, Model, Evidence),
		run_byte_history(Sequence, State, Model, Evidence).

	verify_edit_history(Edits) :-
		history_fixture(State, Model),
		verify_model(State, Model, Evidence),
		run_edit_history(Edits, State, Model, Evidence).

	history_fixture(State, model(4, Nodes, Rules)) :-
		new(_Representation_, State0),
		create_assumption(data(assumption, 0), State0, Assumption, State1),
		create_assumption(data(assumption, 1), State1, Backup, State2),
		create_node(data(ordinary, 2), State2, Head, State3),
		create_contradiction(data(contradiction, 3), State3, Bottom, State4),
		Nodes = [record(Assumption, assumption, data(assumption, 0)), record(Backup, assumption, data(assumption, 1)), record(Head, ordinary, data(ordinary, 2)), record(Bottom, contradiction, data(contradiction, 3))],
		Rules = [justification(Head, [Assumption], first), justification(Head, [Backup], second), justification(Bottom, [Assumption, Backup], clash)],
		reconstruct_rules(Rules, State4, State).

	run_byte_history([], _, _, _).
	run_byte_history([Byte| Bytes], State0, Model0, Evidence0) :-
		Opcode is Byte mod 16,
		Selector is Byte // 16,
		decode_edit(Opcode, Selector, Model0, Edit),
		apply_model_edit(Edit, State0, Model0, State, Model),
		verify_transition(State0, Model0, Evidence0, State, Model, Evidence),
		check_verified_state(State0, Evidence0),
		run_byte_history(Bytes, State, Model, Evidence).

	run_edit_history([], _, _, _).
	run_edit_history([Edit| Edits], State0, Model0, Evidence0) :-
		apply_model_edit(Edit, State0, Model0, State, Model),
		verify_transition(State0, Model0, Evidence0, State, Model, Evidence),
		check_verified_state(State0, Evidence0),
		run_edit_history(Edits, State, Model, Evidence).

	verify_transition(State0, Model0, Evidence0, State, Model, Evidence) :-
		( State == State0, Model == Model0 ->
			check_verified_state(State, Evidence0),
			Evidence = Evidence0
		; 	verify_model(State, Model, Evidence)
		).

	decode_edit(0, _, _, create(ordinary)).
	decode_edit(1, _, _, create(assumption)).
	decode_edit(2, _, _, create(contradiction)).
	decode_edit(3, Selector, Model, add(justification(Head, [Antecedent], generated(Info)))) :-
		selected_node(Selector, Model, Head),
		Other is Selector + 1,
		selected_node(Other, Model, Antecedent),
		Info is Selector mod 3.
	decode_edit(4, Selector, Model, add(justification(Head, [], generated(Info)))) :-
		selected_node(Selector, Model, Head),
		Info is Selector mod 3.
	decode_edit(5, Selector, Model, remove_rule(Rule)) :-
		selected_rule(Selector, Model, Rule).
	decode_edit(6, Selector, Model, remove_rules([Rule, Rule, justification(node(999), [], missing)])) :-
		selected_rule(Selector, Model, Rule).
	decode_edit(7, Selector, Model, remove_node(Node)) :-
		selected_node(Selector, Model, Node).
	decode_edit(8, Selector, Model, remove_nodes([Node, OtherNode, Node])) :-
		selected_node(Selector, Model, Node),
		Other is Selector + 1,
		selected_node(Other, Model, OtherNode).
	decode_edit(9, Selector, Model, remove_batch([Node, Node], [Rule, Rule])) :-
		selected_node(Selector, Model, Node),
		selected_rule(Selector, Model, Rule).
	decode_edit(10, _, _, remove_batch([], [])).
	decode_edit(11, _, _, remove_batch([node(999)], [justification(node(999), [], missing)])).
	decode_edit(12, Selector, Model, add(Rule)) :-
		selected_rule(Selector, Model, Rule).
	decode_edit(13, _, _, clear).
	decode_edit(14, Selector, Model, add(justification(Head, [Antecedent1, Antecedent2, Antecedent1], generated(Info)))) :-
		selected_node(Selector, Model, Head),
		Other1 is Selector + 1,
		Other2 is Selector + 2,
		selected_node(Other1, Model, Antecedent1),
		selected_node(Other2, Model, Antecedent2),
		Info is Selector mod 3.
	decode_edit(15, Selector, Model, add(justification(Head, [Head], generated(Info)))) :-
		selected_node(Selector, Model, Head),
		Info is Selector mod 3.

	selected_node(Selector, model(Next, _, _), node(Id)) :-
		Id is Selector mod (Next + 1).

	selected_rule(Selector, model(_, _, Rules), Rule) :-
		length(Rules, Count),
		Offset is Selector mod (Count + 1),
		(	Offset == Count ->
			Rule = justification(node(999), [], missing)
		;	Index is Offset + 1,
			nth1(Index, Rules, Rule)
		).

	apply_model_edit(create(Role), State0, model(Next0, Nodes0, Rules), State, model(Next, Nodes, Rules)) :-
		(	can_create_model_node(Role, Nodes0) ->
			Datum = data(Role, Next0),
			create_role(Role, Datum, State0, Node, State),
			Node == node(Next0),
			Next is Next0 + 1,
			append(Nodes0, [record(Node, Role, Datum)], Nodes)
		;	State = State0,
			Next = Next0,
			Nodes = Nodes0
		).
	apply_model_edit(add(justification(Head, Antecedents0, Info)), State0, model(Next, Nodes, Rules0), State, model(Next, Nodes, Rules)) :-
		sort(Antecedents0, Antecedents),
		(	model_has_node(Head, Nodes), model_has_nodes(Antecedents, Nodes) ->
			justify(Head, Antecedents0, Info, State0, State),
			Rule = justification(Head, Antecedents, Info),
			(	member(Rule, Rules0) ->
				Rules = Rules0,
				State == State0
			;	append(Rules0, [Rule], Rules)
			)
		;	\+ justify(Head, Antecedents0, Info, State0, _),
			State = State0,
			Rules = Rules0
		).
	apply_model_edit(remove_rule(justification(Head, Antecedents0, Info)), State0, model(Next, Nodes, Rules0), State, model(Next, Nodes, Rules)) :-
		sort(Antecedents0, Antecedents),
		model_remove_single(justification(Head, Antecedents, Info), Rules0, Rules),
		retract_justification(Head, Antecedents0, Info, State0, State).
	apply_model_edit(remove_rules(Targets0), State0, model(Next, Nodes, Rules0), State, model(Next, Nodes, Rules)) :-
		canonical_rule_targets(Targets0, Targets),
		linear_rule_survivors(Rules0, Targets, Rules),
		retract_justifications(Targets0, State0, State).
	apply_model_edit(remove_node(Node), State0, Model0, State, Model) :-
		model_remove_batch([Node], [], Model0, Model),
		retract_node(Node, State0, State).
	apply_model_edit(remove_nodes(Nodes), State0, Model0, State, Model) :-
		model_remove_batch(Nodes, [], Model0, Model),
		retract_nodes(Nodes, State0, State).
	apply_model_edit(remove_batch(Nodes, Targets0), State0, Model0, State, Model) :-
		canonical_rule_targets(Targets0, Targets),
		model_remove_batch(Nodes, Targets, Model0, Model),
		retract_batch(Nodes, Targets0, State0, State).
	apply_model_edit(clear, State0, _, State, model(0, [], [])) :-
		clear(State0, State).

	can_create_model_node(Role, Nodes) :-
		length(Nodes, Count),
		Count < 8,
		(	Role == assumption ->
			findall(Node, member(record(Node, assumption, _), Nodes), Assumptions),
			length(Assumptions, AssumptionCount),
			AssumptionCount < 4
		;	true
		).

	create_role(ordinary, Datum, State0, Node, State) :-
		create_node(Datum, State0, Node, State).
	create_role(assumption, Datum, State0, Node, State) :-
		create_assumption(Datum, State0, Node, State).
	create_role(contradiction, Datum, State0, Node, State) :-
		create_contradiction(Datum, State0, Node, State).

	model_has_node(Node, Nodes) :-
		member(record(Node, _, _), Nodes),
		!.

	model_has_nodes([], _).
	model_has_nodes([Node| Nodes], Records) :-
		model_has_node(Node, Records),
		model_has_nodes(Nodes, Records).

	model_remove_single(_, [], []) :-
		!.
	model_remove_single(Target, [Rule| Rules], Remaining) :-
		(	Target == Rule ->
			Remaining = Rules
		;	Remaining = [Rule| Tail],
			model_remove_single(Target, Rules, Tail)
		).

	canonical_rule_targets([], []).
	canonical_rule_targets([justification(Head, Antecedents0, Info)| Targets0], [justification(Head, Antecedents, Info)| Targets]) :-
		sort(Antecedents0, Antecedents),
		canonical_rule_targets(Targets0, Targets).

	model_remove_batch(NodeTargets, RuleTargets, model(Next, Nodes0, Rules0), model(Next, Nodes, Rules)) :-
		model_node_survivors(Nodes0, NodeTargets, Nodes),
		model_rule_survivors(Rules0, NodeTargets, RuleTargets, Rules).

	model_node_survivors([], _, []).
	model_node_survivors([Record| Records], Targets, Remaining) :-
		Record = record(Node, _, _),
		(	member(Node, Targets) ->
			Remaining = Tail
		;	Remaining = [Record| Tail]
		),
		model_node_survivors(Records, Targets, Tail).

	model_rule_survivors([], _, _, []).
	model_rule_survivors([Rule| Rules], Nodes, Targets, Remaining) :-
		Rule = justification(Head, Antecedents, _),
		(	(member(Head, Nodes); model_incident_antecedent(Antecedents, Nodes); linear_identity_member(Rule, Targets)) ->
			Remaining = Tail
		;	Remaining = [Rule| Tail]
		),
		model_rule_survivors(Rules, Nodes, Targets, Tail).

	model_incident_antecedent([Antecedent| Antecedents], Nodes) :-
		(	member(Antecedent, Nodes) ->
			true
		;	model_incident_antecedent(Antecedents, Nodes)
		).

	verify_model(State, model(Next, Nodes, Rules), verified(Expected, Next, Padding, Truths)) :-
		check_semantics(State, Nodes, Rules, Truths),
		new(_Representation_, Oracle0),
		reconstruct_nodes(0, Next, Nodes, Oracle0, Oracle1, Padding),
		reconstruct_rules(Rules, Oracle1, Oracle),
		state_snapshot(State, [], Actual),
		state_snapshot(Oracle, Padding, Expected),
		^^assertion(Actual == Expected),
		absent_padding_nodes(Padding, State),
		create_node(probe, State, NextNode, _),
		^^assertion(NextNode == node(Next)).

	check_verified_state(State, Evidence) :-
		( matches_verified_state(State, Evidence) ->
			Matches = true
		; 	Matches = false
		),
		^^assertion(Matches == true).

	matches_verified_state(State, verified(Expected, Next, Padding, Truths)) :-
		state_snapshot(State, [], Actual),
		Actual == Expected,
		matches_consistency(State, Truths),
		absent_padding_nodes(Padding, State),
		create_node(probe, State, NextNode, _),
		NextNode == node(Next).

	matches_consistency(_, []) :-
		!.
	matches_consistency(State, [truth(Environment, _, Expected)| Truths]) :-
		( consistent(Environment, State) ->
			Actual = consistent
		; 	Actual = inconsistent
		),
		Actual == Expected,
		matches_consistency(State, Truths).

	check_semantics(State, Nodes, Rules, Truths) :-
		findall(Node, member(record(Node, assumption, _), Nodes), Assumptions),
		findall(Environment, oracle_subset_choice(Assumptions, Environment), Environments),
		oracle_truths(Environments, Nodes, Rules, State, Truths),
		oracle_labels(Nodes, Truths, State),
		findall(Environment, member(truth(Environment, _, inconsistent), Truths), Inconsistent),
		oracle_extremal(Inconsistent, Inconsistent, minimal, ExpectedNogoods),
		findall(Environment, nogood(Environment, State), Nogoods0),
		sort(Nogoods0, Nogoods),
		sort(ExpectedNogoods, SortedNogoods),
		^^assertion(Nogoods == SortedNogoods),
		findall(Environment, member(truth(Environment, _, consistent), Truths), Consistent),
		oracle_extremal(Consistent, Consistent, maximal, ExpectedInterpretations),
		sort(ExpectedInterpretations, SortedInterpretations),
		interpretations(State, Interpretations),
		^^assertion(Interpretations == SortedInterpretations).

	oracle_subset_choice([], []).
	oracle_subset_choice([Node| Nodes], [Node| Chosen]) :-
		oracle_subset_choice(Nodes, Chosen).
	oracle_subset_choice([_| Nodes], Chosen) :-
		oracle_subset_choice(Nodes, Chosen).

	oracle_truths([], _, _, _, []).
	oracle_truths([Environment| Environments], Nodes, Rules, State, [truth(Environment, Known, Status)| Truths]) :-
		oracle_closure(Rules, Environment, Known),
		(	oracle_contradiction(Nodes, Known) ->
			Status = inconsistent
		;	Status = consistent
		),
		(	consistent(Environment, State) ->
			Actual = consistent
		;	Actual = inconsistent
		),
		^^assertion(Actual == Status),
		oracle_truths(Environments, Nodes, Rules, State, Truths).

	oracle_closure(Rules, Known0, Known) :-
		(	oracle_next(Rules, Known0, Head) ->
			oracle_closure(Rules, [Head| Known0], Known)
		;	Known = Known0
		).

	oracle_next([justification(Head, Antecedents, _)| Rules], Known, Next) :-
		(	\+ member(Head, Known), oracle_subset(Antecedents, Known) ->
			Next = Head
		;	oracle_next(Rules, Known, Next)
		).

	oracle_subset([], _).
	oracle_subset([Node| Nodes], Known) :-
		memberchk(Node, Known),
		oracle_subset(Nodes, Known).

	oracle_contradiction([record(Node, Role, _)| Nodes], Known) :-
		(	Role == contradiction, member(Node, Known) ->
			true
		;	oracle_contradiction(Nodes, Known)
		).

	oracle_labels([], _, _).
	oracle_labels([record(Node, _, _)| Nodes], Truths, State) :-
		findall(Environment, (member(truth(Environment, Known, consistent), Truths), member(Node, Known)), Supports),
		oracle_extremal(Supports, Supports, minimal, Minimal),
		sort(Minimal, Expected),
		label(Node, State, Actual),
		^^assertion(Actual == Expected),
		oracle_labels(Nodes, Truths, State).

	oracle_extremal([], _, _, []).
	oracle_extremal([Environment| Environments], All, Mode, Extremal) :-
		(	member(Other, All), Other \== Environment, oracle_dominated(Mode, Environment, Other) ->
			Extremal = Tail
		;	Extremal = [Environment| Tail]
		),
		oracle_extremal(Environments, All, Mode, Tail).

	oracle_dominated(minimal, Environment, Other) :-
		oracle_subset(Other, Environment).
	oracle_dominated(maximal, Environment, Other) :-
		oracle_subset(Environment, Other).

	reconstruct_nodes(Next, Next, _, State, State, []) :-
		!.
	reconstruct_nodes(Id, Next, Nodes, State0, State, Padding) :-
		(	member(record(node(Id), Role, Datum), Nodes) ->
			create_role(Role, Datum, State0, node(Id), State1),
			Padding = Tail
		;	create_node(padding, State0, Node, State1),
			Node == node(Id),
			Padding = [Node| Tail]
		),
		NextId is Id + 1,
		reconstruct_nodes(NextId, Next, Nodes, State1, State, Tail).

	reconstruct_rules([], State, State).
	reconstruct_rules([justification(Head, Antecedents, Info)| Rules], State0, State) :-
		justify(Head, Antecedents, Info, State0, State1),
		reconstruct_rules(Rules, State1, State).

	absent_padding_nodes([], _).
	absent_padding_nodes([Node| Nodes], State) :-
		\+ node(Node, State),
		\+ label(Node, State, _),
		absent_padding_nodes(Nodes, State).

	numbered_fixture(Count, State, Assumption, Head) :-
		new(_Representation_, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, State2),
		add_numbered_rules(Count, Assumption, Head, State2, State).

	add_numbered_rules(0, _, _, State, State) :-
		!.
	add_numbered_rules(Count, Assumption, Head, State0, State) :-
		justify(Head, [Assumption], index(Count), State0, State1),
		Next is Count - 1,
		add_numbered_rules(Next, Assumption, Head, State1, State).

	numbered_targets(0, _, _, []) :-
		!.
	numbered_targets(Count, Assumption, Head, [justification(Head, [Assumption], index(Id))| Targets]) :-
		Id is Count * 2,
		Next is Count - 1,
		numbered_targets(Next, Assumption, Head, Targets).

	linear_rule_survivors([], _, []).
	linear_rule_survivors([Rule| Rules], Targets, Remaining) :-
		(	linear_identity_member(Rule, Targets) ->
			Remaining = Tail
		;	Remaining = [Rule| Tail]
		),
		linear_rule_survivors(Rules, Targets, Tail).

	linear_identity_member(Rule, [Target| Targets]) :-
		(	Rule == Target ->
			true
		;	linear_identity_member(Rule, Targets)
		).

	add_unrelated_rules(0, State, State) :-
		!.
	add_unrelated_rules(Count, State0, State) :-
		create_node(unrelated, State0, Node, State1),
		justify(Node, [Node], unrelated, State1, State2),
		Next is Count - 1,
		add_unrelated_rules(Next, State2, State).

	singleton_environments([], _, []).
	singleton_environments([Node| Nodes], Representation, [Environment| Environments]) :-
		Representation::singleton(Node, Environment),
		singleton_environments(Nodes, Representation, Environments).

	add_free_assumptions(0, State, [], State) :-
		!.
	add_free_assumptions(Count, State0, [Node| Nodes], State) :-
		create_assumption(free, State0, Node, State1),
		Next is Count - 1,
		add_free_assumptions(Next, State1, Nodes, State).

	unchanged_label_calls(Count, Calls) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_node(h, State0, Head, State1),
		add_label_alternatives(Count, Head, State1, State2),
		Counter::reset,
		justify(Head, [node(Count)], extra, State2, State),
		Counter::counts(Calls, _),
		label(Head, State, Label),
		length(Label, Count),
		why(Head, State, Why),
		memberchk(justification(Head, [node(Count)], extra), Why).

	add_label_alternatives(0, _, State, State) :-
		!.
	add_label_alternatives(Count, Head, State0, State) :-
		create_assumption(alternative, State0, Assumption, State1),
		justify(Head, [Assumption], Count, State1, State2),
		Next is Count - 1,
		add_label_alternatives(Next, Head, State2, State).

	counted_fixture(State, Assumption, Head, Info) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, State2),
		justify(Head, [Assumption], info(Info), State2, State).

	state_snapshot(State, Padding, snapshot(Nodes, Assumptions, Contradictions, Labels, Nogoods, Interpretations, Rules, Why, Representation)) :-
		nodes(State, AllNodes),
		remove_padding_nodes(AllNodes, Padding, Nodes),
		assumptions(State, Assumptions),
		findall(Node, contradiction(Node, State), Contradictions),
		node_snapshots(Nodes, State, Labels, Why),
		findall(Environment, nogood(Environment, State), UnsortedNogoods),
		sort(UnsortedNogoods, Nogoods),
		interpretations(State, Interpretations),
		justifications(State, Rules),
		representation(State, Representation).

	remove_padding_nodes([], _, []).
	remove_padding_nodes([Node-Datum| Nodes], Padding, Remaining) :-
		(	member(Node, Padding) ->
			Remaining = Tail
		;	Remaining = [Node-Datum| Tail]
		),
		remove_padding_nodes(Nodes, Padding, Tail).

	node_snapshots([], _, [], []).
	node_snapshots([Node-_| Nodes], State, [Node-Label| Labels], [Node-Explanation| Why]) :-
		label(Node, State, Label),
		why(Node, State, Explanation),
		node_snapshots(Nodes, State, Labels, Why).

	create_padding_nodes(0, State, State) :-
		!.
	create_padding_nodes(Count, State0, State) :-
		create_node(padding, State0, _, State1),
		Remaining is Count - 1,
		create_padding_nodes(Remaining, State1, State).

:- end_object.

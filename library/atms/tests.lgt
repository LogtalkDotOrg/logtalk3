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
		date is 2026-09-26,
		comment is 'Unit tests for the portable ATMS library.',
		parameters is [
			'Representation' - 'ATMS environment representation under test.'
		]
	]).

	:- uses(atms, [
		new/2, create_node/4, create_assumption/4, create_contradiction/4, justify/5, node/2, nodes/2,
		label/3, consistent/2, nogood/2, interpretations/2, assumptions/2, justifications/2, why/3
	]).

	cover(atms).
	cover(atms_bitset_environment).
	cover(atms_ordered_list_environment).
	cover(atms_segmented_bitset_environment).

	test(environment_union_01, deterministic(Union == [node(0), node(1), node(2), node(3)])) :-
		_Representation_::from_list([node(0), node(2), node(3)], Environment1),
		_Representation_::from_list([node(1), node(2)], Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(environment_union_empty_01, deterministic(Union == [node(0), node(2)])) :-
		_Representation_::from_list([node(0), node(2)], Environment1),
		_Representation_::empty(Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(environment_subset_01, deterministic(IsSubset == true)) :-
		_Representation_::from_list([node(0), node(2)], Environment1),
		_Representation_::from_list([node(0), node(1), node(2), node(3)], Environment2),
		(	_Representation_::subset(Environment1, Environment2) ->
			IsSubset = true
		;	IsSubset = false
		).

	test(environment_not_subset_01, deterministic(IsSubset == false)) :-
		_Representation_::from_list([node(0), node(2)], Environment1),
		_Representation_::from_list([node(0), node(1), node(3)], Environment2),
		(	_Representation_::subset(Environment1, Environment2) ->
			IsSubset = true
		;	IsSubset = false
		).

	test(environment_canonicalization_01, deterministic(Nodes == [node(0), node(15), node(16), node(31), node(32)])) :-
		_Representation_::from_list([node(32), node(15), node(16), node(15), node(31), node(0)], Environment),
		_Representation_::to_list(Environment, Nodes).

	test(environment_equal_01, deterministic(Equal == true)) :-
		_Representation_::from_list([node(31), node(15), node(16), node(15)], Environment1),
		_Representation_::from_list([node(15), node(16), node(31)], Environment2),
		( 	_Representation_::equal(Environment1, Environment2) ->
			Equal = true
		; 	Equal = false
		).

	test(environment_empty_and_exact_subset_01, deterministic(Subset == true)) :-
		_Representation_::empty(Empty),
		_Representation_::from_list([node(15), node(16), node(31), node(32)], Environment),
		( 	_Representation_::subset(Empty, Environment),
			_Representation_::subset(Environment, Environment) ->
			Subset = true
		; 	Subset = false
		).

	test(environment_cross_block_subset_01, deterministic(Subset == true)) :-
		_Representation_::from_list([node(15), node(31)], Environment1),
		_Representation_::from_list([node(0), node(15), node(16), node(31), node(32)], Environment2),
		( 	_Representation_::subset(Environment1, Environment2) ->
			Subset = true
		; 	Subset = false
		).

	test(environment_cross_block_union_01, deterministic(Union == [node(15), node(16), node(31), node(32)])) :-
		_Representation_::from_list([node(15), node(16), node(31)], Environment1),
		_Representation_::from_list([node(16), node(32)], Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(environment_sparse_blocks_01, deterministic(Union == [node(0), node(16), node(32), node(48)])) :-
		_Representation_::from_list([node(0), node(48)], Environment1),
		_Representation_::from_list([node(16), node(32)], Environment2),
		_Representation_::union(Environment1, Environment2, Environment),
		_Representation_::to_list(Environment, Union).

	test(empty_system_01, deterministic(Assumptions == [])) :-
		new(_Representation_, State),
		assumptions(State, Assumptions).

	test(assumption_label_01, deterministic(Label == [[node(0)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, _, State),
		label(node(0), State, Label).

	test(state_collection_order_01, deterministic([EnumeratedNodes, Nodes, Assumptions, Justifications, Why] == [
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

	test(unconditional_justification_01, deterministic(Label == [[]])) :-
		new(_Representation_, State0),
		create_node(fact, State0, Fact, State1),
		justify(Fact, [], axiom, State1, State),
		label(Fact, State, Label).

	test(incremental_delta_01, deterministic(Label == [[node(0)], [node(1)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(c, State1, C, State2),
		create_node(h, State2, H, State3),
		create_node(k, State3, K, State4),
		justify(H, [A], first, State4, State5),
		justify(K, [H], inherited, State5, State6),
		justify(H, [C], alternative, State6, State),
		label(K, State, Label).

	test(subsumed_label_update_01, deterministic([LabelH, LabelK] == [[[node(0)]], [[node(0)]]])) :-
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

	test(cartesian_products_01, deterministic(Label == [
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

	test(cartesian_product_minimality_01, deterministic(Label == [[node(0)], [node(1),node(2)]])) :-
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

	test(cartesian_product_nogood_pruning_01, deterministic(Label == [
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

	test(empty_antecedent_product_01, deterministic([LabelX, LabelTarget] == [
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

	test(cyclic_justifications_01, deterministic([LabelB, LabelC] == [[[node(0)]], [[node(0)]]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(b, State1, B, State2),
		create_node(c, State2, C, State3),
		justify(C, [B], b_implies_c, State3, State4),
		justify(B, [C], c_implies_b, State4, State5),
		justify(B, [A], a_implies_b, State5, State),
		label(B, State, LabelB),
		label(C, State, LabelC).

	test(minimality_01, deterministic(Label == [[node(0)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(c, State1, C, State2),
		create_node(h, State2, H, State3),
		justify(H, [A, C], conjunction, State3, State4),
		justify(H, [A], simpler, State4, State),
		label(H, State, Label).

	test(nogood_pruning_01, deterministic([Label, IsConsistent, Nogood] == [[], false, [node(0),node(1)]])) :-
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

	test(sequential_nogood_pruning_01, deterministic([Label1, Label2] == [
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

	test(interpretations_01, deterministic(Interpretations == [[node(0)], [node(1)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(c, State1, C, State2),
		create_contradiction(false, State2, Bottom, State3),
		justify(Bottom, [A, C], clash, State3, State),
		interpretations(State, Interpretations).

	test(interpretations_without_nogoods_01, deterministic(Interpretations == [[node(0), node(1), node(2)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, _, State1),
		create_assumption(b, State1, _, State2),
		create_assumption(c, State2, _, State),
		interpretations(State, Interpretations).

	test(interpretations_empty_system_01, deterministic(Interpretations == [[]])) :-
		new(_Representation_, State),
		interpretations(State, Interpretations).

	test(interpretations_inconsistent_system_01, deterministic(Interpretations == [])) :-
		new(_Representation_, State0),
		create_contradiction(false, State0, Bottom, State1),
		justify(Bottom, [], contradiction, State1, State),
		interpretations(State, Interpretations).

	test(interpretations_multiple_nogoods_01, deterministic(Interpretations == [[node(0),node(2)], [node(1)]])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_assumption(b, State1, B, State2),
		create_assumption(c, State2, C, State3),
		create_contradiction(false_ab, State3, BottomAB, State4),
		create_contradiction(false_bc, State4, BottomBC, State5),
		justify(BottomAB, [A, B], clash_ab, State5, State6),
		justify(BottomBC, [B, C], clash_bc, State6, State),
		interpretations(State, Interpretations).

	test(why_01, deterministic(Why == [justification(node(1),[node(0)],rule)])) :-
		new(_Representation_, State0),
		create_assumption(a, State0, A, State1),
		create_node(h, State1, H, State2),
		justify(H, [A], rule, State2, State),
		why(H, State, Why).

:- end_object.

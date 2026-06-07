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


:- object(interval_constraint_network,
	implements(interval_constraint_network_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Allen interval constraint-network operations built on canonical relation sets, intended for small-to-medium symbolic networks.'
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	network(network(Nodes, Constraints)) :-
		unique_nodes(Nodes),
		valid_constraints(Nodes, Constraints).

	new(Nodes, network(Nodes, Constraints)) :-
		unique_nodes(Nodes),
		interval_relation_set::universal(UniversalRelationSet),
		new_constraints(Nodes, UniversalRelationSet, Constraints).

	nodes(Network, Nodes) :-
		network(Network),
		Network = network(Nodes, _Constraints).

	relation(Network, Node, Node, [equal]) :-
		!,
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node, Node, [equal]).
	relation(Network, Node1, Node2, RelationSet) :-
		Node1 \== Node2,
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, RelationSet).

	entails(Network, Node1, Node2, QueryRelationSet) :-
		interval_relation_set::relation_set(QueryRelationSet),
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, CurrentRelationSet),
		interval_relation_set::subset(CurrentRelationSet, QueryRelationSet).

	entails(Network, Node1, Node2, QueryRelationSet, Explanation) :-
		interval_relation_set::relation_set(QueryRelationSet),
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, CurrentRelationSet),
		interval_relation_set::subset(CurrentRelationSet, QueryRelationSet),
		compiled_entailment_explanation(CompiledNetwork, Node1, Node2, CurrentRelationSet, Explanation).

	possible(Network, Node1, Node2, QueryRelationSet) :-
		interval_relation_set::relation_set(QueryRelationSet),
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, CurrentRelationSet),
		interval_relation_set::intersection(CurrentRelationSet, QueryRelationSet, Intersection),
		Intersection \== [].

	excluded(Network, Node1, Node2, QueryRelationSet) :-
		interval_relation_set::relation_set(QueryRelationSet),
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, CurrentRelationSet),
		interval_relation_set::intersection(CurrentRelationSet, QueryRelationSet, []).

	contradiction(Network, contradiction(Node1, Node2, Cause)) :-
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_informative_contradiction(CompiledNetwork, Node1, Node2, Cause),
		!.
	contradiction(Network, contradiction(Node1, Node2, Cause)) :-
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_contradictory_constraint(CompiledNetwork, Node1, Node2, Cause).

	entailment_explanations(Network, Node1, Node2, QueryRelationSet, Explanations) :-
		interval_relation_set::relation_set(QueryRelationSet),
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, CurrentRelationSet),
		interval_relation_set::subset(CurrentRelationSet, QueryRelationSet),
		compiled_all_entailment_explanations(CompiledNetwork, Node1, Node2, CurrentRelationSet, Explanations).

	contradiction_explanations(Network, Explanations) :-
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_contradiction_explanations(CompiledNetwork, Explanations).

	refine(Network, Node1, Node2, RelationSet, RefinedNetwork) :-
		Node1 \== Node2,
		network(Network),
		interval_relation_set::relation_set(RelationSet),
		compile_network(Network, CompiledNetwork),
		compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, CurrentRelationSet),
		interval_relation_set::intersection(CurrentRelationSet, RelationSet, RefinedRelationSet),
		(	RefinedRelationSet == CurrentRelationSet ->
			RefinedNetwork = Network
		;	refine_compiled(CompiledNetwork, Node1, Node2, RefinedRelationSet, direct(RelationSet), UpdatedNetwork),
			decompile_network(UpdatedNetwork, RefinedNetwork)
		).

	consistent(Network) :-
		network(Network),
		Network = network(_Nodes, Constraints),
		\+ inconsistent_constraints(Constraints).

	propagate(Network, Closure) :-
		propagate(Network, Closure, _Changes).

	propagate(Network, Closure, Changes) :-
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_seed_pairs(CompiledNetwork, SeedPairs),
		propagate_compiled_seeded(SeedPairs, CompiledNetwork, CompiledClosure, Changes),
		decompile_network(CompiledClosure, Closure),
		consistent(Closure).

	refine_propagate(Network, Node1, Node2, RelationSet, Closure) :-
		refine_propagate(Network, Node1, Node2, RelationSet, Closure, _Changes).

	refine_propagate(Network, Constraints, Closure) :-
		refine_propagate(Network, Constraints, Closure, _Changes).

	refine_propagate(Network, Constraints, Closure, Changes) :-
		network(Network),
		compile_network(Network, CompiledNetwork),
		batch_refine_seed_compiled(Constraints, CompiledNetwork, RefinedNetwork, SeedPairs0, RefinementChanges),
		(	SeedPairs0 == [] ->
			compiled_seed_pairs(RefinedNetwork, SeedPairs)
		;	SeedPairs = SeedPairs0
		),
		propagate_compiled_seeded(SeedPairs, RefinedNetwork, CompiledClosure, PropagationChanges),
		decompile_network(CompiledClosure, Closure),
		append(RefinementChanges, PropagationChanges, Changes),
		consistent(Closure).

	refine_propagate(Network, Node1, Node2, RelationSet, Closure, Changes) :-
		network(Network),
		compile_network(Network, CompiledNetwork),
		refine_seed_compiled(CompiledNetwork, Node1, Node2, RelationSet, RefinedNetwork, SeedPairs0, RefinementChanges),
		(	SeedPairs0 == [] ->
			compiled_seed_pairs(RefinedNetwork, SeedPairs)
		;	SeedPairs = SeedPairs0
		),
		propagate_compiled_seeded(SeedPairs, RefinedNetwork, CompiledClosure, PropagationChanges),
		decompile_network(CompiledClosure, Closure),
		append(RefinementChanges, PropagationChanges, Changes),
		consistent(Closure).

	propagation_triple(change(Node1, Node2, _OldRelationSet, _NewRelationSet, propagated(Node3, _LeftRelationSet, _RightRelationSet, _ComposedRelationSet)), triple(Node1, Node3, Node2)).

	propagation_triples(Changes, Triples) :-
		propagation_triples(Changes, [], ReversedTriples),
		reverse(ReversedTriples, Triples).

	path_consistency(Network, Closure) :-
		network(Network),
		compile_network(Network, CompiledNetwork),
		compiled_seed_pairs(CompiledNetwork, SeedPairs),
		propagate_compiled_seeded(SeedPairs, CompiledNetwork, CompiledClosure, _Changes),
		decompile_network(CompiledClosure, Closure).

	compile_network(network(Nodes, Constraints), compiled_network(Nodes, NodeVector, NodeIndexVector, Matrix)) :-
		build_node_vector(Nodes, NodeVector),
		build_node_index_vector(Nodes, NodeIndexVector),
		build_constraint_matrix(Nodes, Constraints, Matrix).

	decompile_network(compiled_network(Nodes, _NodeVector, _NodeIndexVector, Matrix), network(Nodes, Constraints)) :-
		decompile_constraint_matrix(Nodes, Matrix, Constraints).

	build_node_vector(Nodes, NodeVector) :-
		length(Nodes, Size),
		functor(NodeVector, nodes, Size),
		fill_node_vector(Nodes, 1, NodeVector).

	fill_node_vector([], _Index, _NodeVector).
	fill_node_vector([Node| Nodes], Index, NodeVector) :-
		arg(Index, NodeVector, Node),
		NextIndex is Index + 1,
		fill_node_vector(Nodes, NextIndex, NodeVector).

	build_node_index_vector(Nodes, NodeIndexVector) :-
		collect_node_indices(Nodes, 1, NodeIndices),
		sort(NodeIndices, SortedNodeIndices),
		length(SortedNodeIndices, Size),
		functor(NodeIndexVector, node_indexes, Size),
		fill_node_index_vector(SortedNodeIndices, 1, NodeIndexVector).

	collect_node_indices([], _Index, []).
	collect_node_indices([Node| Nodes], Index, [entry(Node, Index)| NodeIndices]) :-
		NextIndex is Index + 1,
		collect_node_indices(Nodes, NextIndex, NodeIndices).

	fill_node_index_vector([], _Index, _NodeIndexVector).
	fill_node_index_vector([Entry| Entries], Index, NodeIndexVector) :-
		arg(Index, NodeIndexVector, Entry),
		NextIndex is Index + 1,
		fill_node_index_vector(Entries, NextIndex, NodeIndexVector).

	build_constraint_matrix(Nodes, Constraints, Matrix) :-
		length(Nodes, Size),
		(	Size =< 1 ->
			functor(Matrix, rows, 0),
			Constraints == []
		;	RowCount is Size - 1,
			functor(Matrix, rows, RowCount),
			build_constraint_rows(1, RowCount, Size, Constraints, [], Matrix)
		).

	build_constraint_rows(RowIndex, RowCount, _Size, Constraints, Constraints, _Matrix) :-
		RowIndex > RowCount,
		!.
	build_constraint_rows(RowIndex, RowCount, Size, Constraints0, Constraints, Matrix) :-
		RowLength is Size - RowIndex,
		functor(Row, row, RowLength),
		build_constraint_row(1, RowLength, Constraints0, Constraints1, Row),
		arg(RowIndex, Matrix, Row),
		NextRowIndex is RowIndex + 1,
		build_constraint_rows(NextRowIndex, RowCount, Size, Constraints1, Constraints, Matrix).

	build_constraint_row(ColumnIndex, RowLength, Constraints, Constraints, _Row) :-
		ColumnIndex > RowLength,
		!.
	build_constraint_row(ColumnIndex, RowLength, [Constraint| Constraints0], Constraints, Row) :-
		constraint_cell(Constraint, Cell),
		arg(ColumnIndex, Row, Cell),
		NextColumnIndex is ColumnIndex + 1,
		build_constraint_row(NextColumnIndex, RowLength, Constraints0, Constraints, Row).

	constraint_cell(constraint(_Node1, _Node2, RelationSet), cell(RelationSet)) :-
		!.
	constraint_cell(constraint(_Node1, _Node2, RelationSet, Cause), cell(RelationSet, Cause)).

	decompile_constraint_matrix(Nodes, Matrix, Constraints) :-
		decompile_constraint_rows(Nodes, 1, Matrix, Constraints, []).

	decompile_constraint_rows([_Node], _RowIndex, _Matrix, Constraints, Constraints) :-
		!.
	decompile_constraint_rows([Node| Nodes], RowIndex, Matrix, Constraints0, Constraints) :-
		arg(RowIndex, Matrix, Row),
		decompile_constraint_row(Nodes, Node, 1, Row, Constraints0, Constraints1),
		NextRowIndex is RowIndex + 1,
		decompile_constraint_rows(Nodes, NextRowIndex, Matrix, Constraints1, Constraints).
	decompile_constraint_rows([], _RowIndex, _Matrix, Constraints, Constraints).

	decompile_constraint_row([], _Node, _ColumnIndex, _Row, Constraints, Constraints).
	decompile_constraint_row([OtherNode| OtherNodes], Node, ColumnIndex, Row, [Constraint| Constraints0], Constraints) :-
		arg(ColumnIndex, Row, Cell),
		cell_constraint(Node, OtherNode, Cell, Constraint),
		NextColumnIndex is ColumnIndex + 1,
		decompile_constraint_row(OtherNodes, Node, NextColumnIndex, Row, Constraints0, Constraints).

	cell_constraint(Node1, Node2, cell(RelationSet), constraint(Node1, Node2, RelationSet)) :-
		!.
	cell_constraint(Node1, Node2, cell(RelationSet, Cause), constraint(Node1, Node2, RelationSet, Cause)).

	node_index(compiled_network(_Nodes, _NodeVector, NodeIndexVector, _Matrix), Node, Index) :-
		functor(NodeIndexVector, node_indexes, Size),
		binary_search_node_index(NodeIndexVector, Node, 1, Size, Index).

	binary_search_node_index(_NodeIndexVector, _Node, Low, High, _Index) :-
		Low > High,
		!,
		fail.
	binary_search_node_index(NodeIndexVector, Node, Low, High, Index) :-
		Middle is (Low + High) // 2,
		arg(Middle, NodeIndexVector, entry(MiddleNode, MiddleIndex)),
		(	Node == MiddleNode ->
			Index = MiddleIndex
		;	Node @< MiddleNode ->
			NextHigh is Middle - 1,
			binary_search_node_index(NodeIndexVector, Node, Low, NextHigh, Index)
		;	NextLow is Middle + 1,
			binary_search_node_index(NodeIndexVector, Node, NextLow, High, Index)
		).

	node_name(compiled_network(_Nodes, NodeVector, _NodeIndexVector, _Matrix), Index, Node) :-
		arg(Index, NodeVector, Node).

	compiled_size(compiled_network(_Nodes, NodeVector, _NodeIndexVector, _Matrix), Size) :-
		functor(NodeVector, nodes, Size).

	compiled_relation_by_nodes(CompiledNetwork, Node, Node, [equal]) :-
		!,
		node_index(CompiledNetwork, Node, _Index).
	compiled_relation_by_nodes(CompiledNetwork, Node1, Node2, RelationSet) :-
		node_index(CompiledNetwork, Node1, Index1),
		node_index(CompiledNetwork, Node2, Index2),
		compiled_relation(CompiledNetwork, Index1, Index2, RelationSet).

	compiled_relation(_CompiledNetwork, Index, Index, [equal]) :-
		!.
	compiled_relation(CompiledNetwork, Index1, Index2, RelationSet) :-
		(	Index1 < Index2 ->
			compiled_stored_relation(CompiledNetwork, Index1, Index2, RelationSet)
		;	compiled_stored_relation(CompiledNetwork, Index2, Index1, StoredRelationSet),
			interval_relation_set::converse(StoredRelationSet, RelationSet)
		).

	compiled_stored_relation(compiled_network(_Nodes, _NodeVector, _NodeIndexVector, Matrix), LeftIndex, RightIndex, RelationSet) :-
		arg(LeftIndex, Matrix, Row),
		ColumnIndex is RightIndex - LeftIndex,
		arg(ColumnIndex, Row, Cell),
		cell_relation(Cell, RelationSet).

	cell_relation(cell(RelationSet), RelationSet) :-
		!.
	cell_relation(cell(RelationSet, _Cause), RelationSet).

	cell_cause(cell(_RelationSet), no_cause).
	cell_cause(cell(_RelationSet, Cause), Cause).

	update_compiled_relation(CompiledNetwork, Index1, Index2, RelationSet, Cause, UpdatedNetwork) :-
		(	Index1 < Index2 ->
			LeftIndex = Index1,
			RightIndex = Index2,
			StoredRelationSet = RelationSet
		;	LeftIndex = Index2,
			RightIndex = Index1,
			interval_relation_set::converse(RelationSet, StoredRelationSet)
		),
		CompiledNetwork = compiled_network(Nodes, NodeVector, NodeIndexVector, Matrix),
		update_compiled_cell(Matrix, LeftIndex, RightIndex, cell(StoredRelationSet, Cause), UpdatedMatrix),
		UpdatedNetwork = compiled_network(Nodes, NodeVector, NodeIndexVector, UpdatedMatrix).

	update_compiled_cell(Matrix, LeftIndex, RightIndex, Cell, UpdatedMatrix) :-
		arg(LeftIndex, Matrix, Row),
		ColumnIndex is RightIndex - LeftIndex,
		replace_arg(Row, ColumnIndex, Cell, UpdatedRow),
		replace_arg(Matrix, LeftIndex, UpdatedRow, UpdatedMatrix).

	replace_arg(Term, Index, Replacement, UpdatedTerm) :-
		functor(Term, Functor, Arity),
		functor(UpdatedTerm, Functor, Arity),
		copy_args(1, Arity, Term, Index, Replacement, UpdatedTerm).

	copy_args(Position, Arity, _Term, _Index, _Replacement, _UpdatedTerm) :-
		Position > Arity,
		!.
	copy_args(Position, Arity, Term, Index, Replacement, UpdatedTerm) :-
		(	Position =:= Index ->
			Argument = Replacement
		;	arg(Position, Term, Argument)
		),
		arg(Position, UpdatedTerm, Argument),
		NextPosition is Position + 1,
		copy_args(NextPosition, Arity, Term, Index, Replacement, UpdatedTerm).

	compiled_seed_pairs(compiled_network(_Nodes, NodeVector, _NodeIndexVector, _Matrix), SeedPairs) :-
		functor(NodeVector, nodes, Size),
		compiled_seed_pairs(1, Size, SeedPairs).

	compiled_seed_pairs(Index, Size, []) :-
		Index >= Size,
		!.
	compiled_seed_pairs(Index, Size, SeedPairs) :-
		NextIndex is Index + 1,
		compiled_seed_pairs_row(Index, NextIndex, Size, SeedPairs, RestPairs),
		compiled_seed_pairs(NextIndex, Size, RestPairs).

	compiled_seed_pairs_row(_LeftIndex, RightIndex, Size, SeedPairs, SeedPairs) :-
		RightIndex > Size,
		!.
	compiled_seed_pairs_row(LeftIndex, RightIndex, Size, [pair(LeftIndex, RightIndex)| SeedPairs], RestPairs) :-
		NextRightIndex is RightIndex + 1,
		compiled_seed_pairs_row(LeftIndex, NextRightIndex, Size, SeedPairs, RestPairs).

	constraints(Network, Constraints) :-
		network(Network),
		Network = network(_Nodes, StoredConstraints),
		explicit_constraints(StoredConstraints, Constraints).

	subsumes(Network1, Network2) :-
		network(Network1),
		network(Network2),
		nodes(Network1, Nodes1),
		nodes(Network2, Nodes2),
		same_node_set(Nodes1, Nodes2),
		compile_network(Network1, CompiledNetwork1),
		compile_network(Network2, CompiledNetwork2),
		compiled_subsumes_nodes(Nodes1, CompiledNetwork1, CompiledNetwork2).

	equivalent(Network1, Network2) :-
		subsumes(Network1, Network2),
		subsumes(Network2, Network1).

	unique_nodes(Nodes) :-
		unique_nodes(Nodes, []).

	unique_nodes([], _Seen).
	unique_nodes([Node| Nodes], Seen) :-
		\+ member(Node, Seen),
		unique_nodes(Nodes, [Node| Seen]).

	valid_constraints([], []).
	valid_constraints([_Node], []) :-
		!.
	valid_constraints([Node| Nodes], Constraints) :-
		valid_constraints_from_node(Nodes, Node, Constraints, RestConstraints),
		valid_constraints(Nodes, RestConstraints).

	valid_constraints_from_node([], _Node, Constraints, Constraints).
	valid_constraints_from_node([OtherNode| OtherNodes], Node, [constraint(Node, OtherNode, RelationSet)| Constraints], RestConstraints) :-
		!,
		interval_relation_set::relation_set(RelationSet),
		valid_constraints_from_node(OtherNodes, Node, Constraints, RestConstraints).
	valid_constraints_from_node([OtherNode| OtherNodes], Node, [constraint(Node, OtherNode, RelationSet, _Cause)| Constraints], RestConstraints) :-
		interval_relation_set::relation_set(RelationSet),
		valid_constraints_from_node(OtherNodes, Node, Constraints, RestConstraints).

	new_constraints([], _UniversalRelationSet, []).
	new_constraints([_Node], _UniversalRelationSet, []) :-
		!.
	new_constraints([Node| Nodes], UniversalRelationSet, Constraints) :-
		new_constraints_from_node(Nodes, Node, UniversalRelationSet, Constraints, RestConstraints),
		new_constraints(Nodes, UniversalRelationSet, RestConstraints).

	new_constraints_from_node([], _Node, _UniversalRelationSet, Constraints, Constraints).
	new_constraints_from_node([OtherNode| OtherNodes], Node, UniversalRelationSet, [constraint(Node, OtherNode, UniversalRelationSet)| Constraints], RestConstraints) :-
		new_constraints_from_node(OtherNodes, Node, UniversalRelationSet, Constraints, RestConstraints).

	batch_refine_seed_compiled(Constraints, CompiledNetwork0, CompiledNetwork, SeedPairs, Changes) :-
		compiled_size(CompiledNetwork0, Size),
		empty_scheduled_pairs(Size, EmptyScheduledPairs),
		batch_refine_seed_compiled(Constraints, CompiledNetwork0, CompiledNetwork, EmptyScheduledPairs, _ScheduledPairs, SeedPairs, Changes).

	batch_refine_seed_compiled([], CompiledNetwork, CompiledNetwork, ScheduledPairs, ScheduledPairs, [], []).
	batch_refine_seed_compiled([Constraint| Constraints], CompiledNetwork0, CompiledNetwork, ScheduledPairs0, ScheduledPairs, SeedPairs, Changes) :-
		refine_seed_constraint_compiled(CompiledNetwork0, Constraint, CompiledNetwork1, SeedPairs1, Changes1),
		batch_refine_seed_compiled(Constraints, CompiledNetwork1, CompiledNetwork, ScheduledPairs0, ScheduledPairs1, SeedPairs2, Changes2),
		enqueue_unique_pairs(SeedPairs1, ScheduledPairs1, ScheduledPairs, SeedPairs2, SeedPairs),
		append(Changes1, Changes2, Changes).

	refine_seed_constraint_compiled(CompiledNetwork, constraint(Node1, Node2, RelationSet), RefinedNetwork, SeedPairs, Changes) :-
		!,
		refine_seed_compiled(CompiledNetwork, Node1, Node2, RelationSet, RefinedNetwork, SeedPairs, Changes).
	refine_seed_constraint_compiled(CompiledNetwork, constraint(Node1, Node2, RelationSet, _Cause), RefinedNetwork, SeedPairs, Changes) :-
		refine_seed_compiled(CompiledNetwork, Node1, Node2, RelationSet, RefinedNetwork, SeedPairs, Changes).

	refine_compiled(CompiledNetwork, Node1, Node2, RelationSet, Cause, RefinedNetwork) :-
		node_index(CompiledNetwork, Node1, Index1),
		node_index(CompiledNetwork, Node2, Index2),
		update_compiled_relation(CompiledNetwork, Index1, Index2, RelationSet, Cause, RefinedNetwork).

	propagate_compiled_seeded(SeedPairs, CompiledNetwork, Closure, Changes) :-
		compiled_size(CompiledNetwork, Size),
		empty_scheduled_pairs(Size, EmptyScheduledPairs),
		mark_scheduled_pairs(SeedPairs, EmptyScheduledPairs, ScheduledPairs),
		propagate_compiled_worklist(SeedPairs, ScheduledPairs, Size, CompiledNetwork, Closure, [], Changes).

	propagate_compiled_worklist([], _ScheduledPairs, _Size, CompiledNetwork, CompiledNetwork, Changes, Changes).
	propagate_compiled_worklist([pair(LeftIndex, RightIndex)| Worklist], ScheduledPairs0, Size, CompiledNetwork, Closure, Changes0, Changes) :-
		update_scheduled_pair(ScheduledPairs0, LeftIndex, RightIndex, no, ScheduledPairs1),
		propagate_compiled_pair(1, Size, LeftIndex, RightIndex, CompiledNetwork, UpdatedNetwork, NewPairs, Changes0, Changes1),
		enqueue_unique_pairs(NewPairs, ScheduledPairs1, ScheduledPairs2, Worklist, UpdatedWorklist),
		propagate_compiled_worklist(UpdatedWorklist, ScheduledPairs2, Size, UpdatedNetwork, Closure, Changes1, Changes).

	empty_scheduled_pairs(Size, scheduled(Rows)) :-
		(	Size =< 1 ->
			functor(Rows, rows, 0)
		;	RowCount is Size - 1,
			functor(Rows, rows, RowCount),
			fill_scheduled_rows(1, RowCount, Size, Rows)
		).

	fill_scheduled_rows(RowIndex, RowCount, _Size, _Rows) :-
		RowIndex > RowCount,
		!.
	fill_scheduled_rows(RowIndex, RowCount, Size, Rows) :-
		RowLength is Size - RowIndex,
		functor(Row, row, RowLength),
		fill_scheduled_row(1, RowLength, Row),
		arg(RowIndex, Rows, Row),
		NextRowIndex is RowIndex + 1,
		fill_scheduled_rows(NextRowIndex, RowCount, Size, Rows).

	fill_scheduled_row(ColumnIndex, RowLength, _Row) :-
		ColumnIndex > RowLength,
		!.
	fill_scheduled_row(ColumnIndex, RowLength, Row) :-
		arg(ColumnIndex, Row, no),
		NextColumnIndex is ColumnIndex + 1,
		fill_scheduled_row(NextColumnIndex, RowLength, Row).

	scheduled_pair(scheduled(Rows), LeftIndex, RightIndex, Scheduled) :-
		arg(LeftIndex, Rows, Row),
		ColumnIndex is RightIndex - LeftIndex,
		arg(ColumnIndex, Row, Scheduled).

	update_scheduled_pair(scheduled(Rows), LeftIndex, RightIndex, Scheduled, scheduled(UpdatedRows)) :-
		arg(LeftIndex, Rows, Row),
		ColumnIndex is RightIndex - LeftIndex,
		replace_arg(Row, ColumnIndex, Scheduled, UpdatedRow),
		replace_arg(Rows, LeftIndex, UpdatedRow, UpdatedRows).

	mark_scheduled_pairs([], ScheduledPairs, ScheduledPairs).
	mark_scheduled_pairs([pair(LeftIndex, RightIndex)| Pairs], ScheduledPairs0, ScheduledPairs) :-
		update_scheduled_pair(ScheduledPairs0, LeftIndex, RightIndex, yes, ScheduledPairs1),
		mark_scheduled_pairs(Pairs, ScheduledPairs1, ScheduledPairs).

	enqueue_unique_pairs([], ScheduledPairs, ScheduledPairs, Worklist, Worklist).
	enqueue_unique_pairs([pair(LeftIndex, RightIndex)| Pairs], ScheduledPairs0, ScheduledPairs, Worklist0, Worklist) :-
		(	scheduled_pair(ScheduledPairs0, LeftIndex, RightIndex, yes) ->
			ScheduledPairs1 = ScheduledPairs0,
			Worklist1 = Worklist0
		;	update_scheduled_pair(ScheduledPairs0, LeftIndex, RightIndex, yes, ScheduledPairs1),
			Worklist1 = [pair(LeftIndex, RightIndex)| Worklist0]
		),
		enqueue_unique_pairs(Pairs, ScheduledPairs1, ScheduledPairs, Worklist1, Worklist).

	propagate_compiled_pair(Index, Size, _LeftIndex, _RightIndex, CompiledNetwork, CompiledNetwork, [], Changes, Changes) :-
		Index > Size,
		!.
	propagate_compiled_pair(Index, Size, LeftIndex, RightIndex, CompiledNetwork0, CompiledNetwork, NewPairs, Changes0, Changes) :-
		(	distinct_indices(LeftIndex, RightIndex, Index) ->
			revise_via_index(CompiledNetwork0, LeftIndex, Index, RightIndex, CompiledNetwork1, NewPairs1, Changes0, Changes1),
			revise_via_index(CompiledNetwork1, Index, RightIndex, LeftIndex, CompiledNetwork2, NewPairs2, Changes1, Changes2),
			append(NewPairs1, NewPairs2, PairPrefix)
		;	CompiledNetwork2 = CompiledNetwork0,
			PairPrefix = [],
			Changes2 = Changes0
		),
		NextIndex is Index + 1,
		propagate_compiled_pair(NextIndex, Size, LeftIndex, RightIndex, CompiledNetwork2, CompiledNetwork, PairSuffix, Changes2, Changes),
		append(PairPrefix, PairSuffix, NewPairs).

	distinct_indices(Index1, Index2, Index3) :-
		Index1 =\= Index2,
		Index1 =\= Index3,
		Index2 =\= Index3.

	revise_via_index(CompiledNetwork, LeftIndex, RightIndex, ViaIndex, UpdatedNetwork, NewPairs, Changes0, Changes) :-
		compiled_relation(CompiledNetwork, LeftIndex, RightIndex, CurrentRelationSet),
		compiled_relation(CompiledNetwork, LeftIndex, ViaIndex, LeftRelationSet),
		compiled_relation(CompiledNetwork, ViaIndex, RightIndex, RightRelationSet),
		interval_relation_set::compose(LeftRelationSet, RightRelationSet, ComposedRelationSet),
		interval_relation_set::intersection(CurrentRelationSet, ComposedRelationSet, RefinedRelationSet),
		(	RefinedRelationSet == CurrentRelationSet ->
			UpdatedNetwork = CompiledNetwork,
			NewPairs = [],
			Changes = Changes0
		;	node_name(CompiledNetwork, LeftIndex, LeftNode),
			node_name(CompiledNetwork, RightIndex, RightNode),
			node_name(CompiledNetwork, ViaIndex, ViaNode),
			update_compiled_relation(CompiledNetwork, LeftIndex, RightIndex, RefinedRelationSet, propagated(ViaNode, LeftRelationSet, RightRelationSet, ComposedRelationSet), UpdatedNetwork),
			canonical_pair(LeftIndex, RightIndex, ChangedPair),
			NewPairs = [ChangedPair],
			Changes = [change(LeftNode, RightNode, CurrentRelationSet, RefinedRelationSet, propagated(ViaNode, LeftRelationSet, RightRelationSet, ComposedRelationSet))| Changes0]
		).

	refine_seed_compiled(CompiledNetwork, Node1, Node2, RelationSet, RefinedNetwork, SeedPairs, Changes) :-
		Node1 \== Node2,
		interval_relation_set::relation_set(RelationSet),
		node_index(CompiledNetwork, Node1, Index1),
		node_index(CompiledNetwork, Node2, Index2),
		compiled_relation(CompiledNetwork, Index1, Index2, CurrentRelationSet),
		interval_relation_set::intersection(CurrentRelationSet, RelationSet, RefinedRelationSet),
		(	RefinedRelationSet == CurrentRelationSet ->
			RefinedNetwork = CompiledNetwork,
			SeedPairs = [],
			Changes = []
		;	update_compiled_relation(CompiledNetwork, Index1, Index2, RefinedRelationSet, refined(RelationSet), RefinedNetwork),
			canonical_pair(Index1, Index2, ChangedPair),
			SeedPairs = [ChangedPair],
			Changes = [change(Node1, Node2, CurrentRelationSet, RefinedRelationSet, refined(RelationSet))]
		).

	canonical_pair(Index1, Index2, pair(Index1, Index2)) :-
		Index1 < Index2,
		!.
	canonical_pair(Index1, Index2, pair(Index2, Index1)).

	inconsistent_constraints([constraint(_Node1, _Node2, [])| _Constraints]) :-
		!.
	inconsistent_constraints([constraint(_Node1, _Node2, [], _Cause)| _Constraints]) :-
		!.
	inconsistent_constraints([_Constraint| Constraints]) :-
		inconsistent_constraints(Constraints).

	compiled_all_entailment_explanations(CompiledNetwork, Node, Node, [equal], [identity]) :-
		!,
		node_index(CompiledNetwork, Node, _Index).
	compiled_all_entailment_explanations(CompiledNetwork, Node1, Node2, CurrentRelationSet, Explanations) :-
		node_index(CompiledNetwork, Node1, Index1),
		node_index(CompiledNetwork, Node2, Index2),
		interval_relation_set::universal(UniversalRelationSet),
		compiled_size(CompiledNetwork, Size),
		compiled_supporting_explanations(1, Size, CompiledNetwork, Index1, Index2, CurrentRelationSet, UniversalRelationSet, [], ReversedExplanations),
		( ReversedExplanations == [] ->
			Explanations = [direct(CurrentRelationSet)]
		;	reverse(ReversedExplanations, Explanations)
		).

	compiled_entailment_explanation(CompiledNetwork, Node, Node, [equal], identity) :-
		!,
		node_index(CompiledNetwork, Node, _Index).
	compiled_entailment_explanation(CompiledNetwork, Node1, Node2, CurrentRelationSet, Explanation) :-
		node_index(CompiledNetwork, Node1, Index1),
		node_index(CompiledNetwork, Node2, Index2),
		interval_relation_set::universal(UniversalRelationSet),
		compiled_size(CompiledNetwork, Size),
		compiled_supporting_explanation(1, Size, CompiledNetwork, Index1, Index2, CurrentRelationSet, UniversalRelationSet, Explanation),
		!.
	compiled_entailment_explanation(_CompiledNetwork, _Node1, _Node2, CurrentRelationSet, direct(CurrentRelationSet)).

	compiled_supporting_explanation(Index, Size, _CompiledNetwork, _Index1, _Index2, _CurrentRelationSet, _UniversalRelationSet, _Explanation) :-
		Index > Size,
		!,
		fail.
	compiled_supporting_explanation(Index, Size, CompiledNetwork, Index1, Index2, CurrentRelationSet, UniversalRelationSet, Explanation) :-
		(	distinct_indices(Index1, Index2, Index),
			compiled_relation(CompiledNetwork, Index1, Index, LeftRelationSet),
			compiled_relation(CompiledNetwork, Index, Index2, RightRelationSet),
			interval_relation_set::compose(LeftRelationSet, RightRelationSet, ComposedRelationSet),
			ComposedRelationSet \== UniversalRelationSet,
			interval_relation_set::subset(CurrentRelationSet, ComposedRelationSet) ->
			node_name(CompiledNetwork, Index, Node3),
			Explanation = propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)
		;	NextIndex is Index + 1,
			compiled_supporting_explanation(NextIndex, Size, CompiledNetwork, Index1, Index2, CurrentRelationSet, UniversalRelationSet, Explanation)
		).

	compiled_supporting_explanations(Index, Size, _CompiledNetwork, _Index1, _Index2, _CurrentRelationSet, _UniversalRelationSet, Explanations, Explanations) :-
		Index > Size,
		!.
	compiled_supporting_explanations(Index, Size, CompiledNetwork, Index1, Index2, CurrentRelationSet, UniversalRelationSet, Explanations0, Explanations) :-
		(	distinct_indices(Index1, Index2, Index),
			compiled_relation(CompiledNetwork, Index1, Index, LeftRelationSet),
			compiled_relation(CompiledNetwork, Index, Index2, RightRelationSet),
			interval_relation_set::compose(LeftRelationSet, RightRelationSet, ComposedRelationSet),
			ComposedRelationSet \== UniversalRelationSet,
			interval_relation_set::subset(CurrentRelationSet, ComposedRelationSet) ->
			node_name(CompiledNetwork, Index, Node3),
			add_unique_explanation(propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet), Explanations0, Explanations1)
		;	Explanations1 = Explanations0
		),
		NextIndex is Index + 1,
		compiled_supporting_explanations(NextIndex, Size, CompiledNetwork, Index1, Index2, CurrentRelationSet, UniversalRelationSet, Explanations1, Explanations).

	compiled_contradiction_cause_indices(CompiledNetwork, Index1, Index2, Cause) :-
		compiled_size(CompiledNetwork, Size),
		compiled_contradictory_support(1, Size, CompiledNetwork, Index1, Index2, Cause),
		!.
	compiled_contradiction_cause_indices(_CompiledNetwork, _Index1, _Index2, direct([])).

	compiled_contradictory_support(Index, Size, _CompiledNetwork, _Index1, _Index2, _Cause) :-
		Index > Size,
		!,
		fail.
	compiled_contradictory_support(Index, Size, CompiledNetwork, Index1, Index2, Cause) :-
		(	distinct_indices(Index1, Index2, Index),
			compiled_relation(CompiledNetwork, Index1, Index, LeftRelationSet),
			compiled_relation(CompiledNetwork, Index, Index2, RightRelationSet),
			interval_relation_set::compose(LeftRelationSet, RightRelationSet, ComposedRelationSet),
			ComposedRelationSet \== [] ->
			node_name(CompiledNetwork, Index, Node3),
			Cause = propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)
		;	NextIndex is Index + 1,
			compiled_contradictory_support(NextIndex, Size, CompiledNetwork, Index1, Index2, Cause)
		).

	compiled_informative_contradiction(CompiledNetwork, Node1, Node2, Cause) :-
		compiled_size(CompiledNetwork, Size),
		compiled_informative_contradiction(1, Size, CompiledNetwork, Node1, Node2, Cause).

	compiled_informative_contradiction(RowIndex, Size, _CompiledNetwork, _Node1, _Node2, _Cause) :-
		RowIndex >= Size,
		!,
		fail.
	compiled_informative_contradiction(RowIndex, _Size, CompiledNetwork, Node1, Node2, Cause) :-
		CompiledNetwork = compiled_network(_Nodes, _NodeVector, _NodeIndexVector, Matrix),
		arg(RowIndex, Matrix, Row),
		compiled_informative_contradiction_row(Row, 1, RowIndex, CompiledNetwork, Node1, Node2, Cause),
		!.
	compiled_informative_contradiction(RowIndex, Size, CompiledNetwork, Node1, Node2, Cause) :-
		NextRowIndex is RowIndex + 1,
		compiled_informative_contradiction(NextRowIndex, Size, CompiledNetwork, Node1, Node2, Cause).

	compiled_informative_contradiction_row(Row, ColumnIndex, _RowIndex, _CompiledNetwork, _Node1, _Node2, _Cause) :-
		functor(Row, row, RowLength),
		ColumnIndex > RowLength,
		!,
		fail.
	compiled_informative_contradiction_row(Row, ColumnIndex, RowIndex, CompiledNetwork, Node1, Node2, Cause) :-
		functor(Row, row, RowLength),
		ColumnIndex =< RowLength,
		RightIndex is RowIndex + ColumnIndex,
		arg(ColumnIndex, Row, Cell),
		cell_relation(Cell, []),
		compiled_informative_contradiction_cell(CompiledNetwork, RowIndex, RightIndex, Cell, Cause),
		node_name(CompiledNetwork, RowIndex, Node1),
		node_name(CompiledNetwork, RightIndex, Node2).
	compiled_informative_contradiction_row(Row, ColumnIndex, RowIndex, CompiledNetwork, Node1, Node2, Cause) :-
		functor(Row, row, RowLength),
		ColumnIndex < RowLength,
		NextColumnIndex is ColumnIndex + 1,
		compiled_informative_contradiction_row(Row, NextColumnIndex, RowIndex, CompiledNetwork, Node1, Node2, Cause).

	compiled_informative_contradiction_cell(_CompiledNetwork, _LeftIndex, _RightIndex, Cell, propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)) :-
		cell_cause(Cell, propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)),
		ComposedRelationSet \== [],
		!.
	compiled_informative_contradiction_cell(CompiledNetwork, LeftIndex, RightIndex, cell([]), Cause) :-
		compiled_contradiction_cause_indices(CompiledNetwork, LeftIndex, RightIndex, Cause),
		Cause \== direct([]).

	compiled_contradictory_constraint(CompiledNetwork, Node1, Node2, Cause) :-
		compiled_size(CompiledNetwork, Size),
		compiled_contradictory_constraint(1, Size, CompiledNetwork, Node1, Node2, Cause).

	compiled_contradictory_constraint(RowIndex, Size, _CompiledNetwork, _Node1, _Node2, _Cause) :-
		RowIndex >= Size,
		!,
		fail.
	compiled_contradictory_constraint(RowIndex, _Size, CompiledNetwork, Node1, Node2, Cause) :-
		CompiledNetwork = compiled_network(_Nodes, _NodeVector, _NodeIndexVector, Matrix),
		arg(RowIndex, Matrix, Row),
		compiled_contradictory_constraint_row(Row, 1, RowIndex, CompiledNetwork, Node1, Node2, Cause),
		!.
	compiled_contradictory_constraint(RowIndex, Size, CompiledNetwork, Node1, Node2, Cause) :-
		NextRowIndex is RowIndex + 1,
		compiled_contradictory_constraint(NextRowIndex, Size, CompiledNetwork, Node1, Node2, Cause).

	compiled_contradictory_constraint_row(Row, ColumnIndex, _RowIndex, _CompiledNetwork, _Node1, _Node2, _Cause) :-
		functor(Row, row, RowLength),
		ColumnIndex > RowLength,
		!,
		fail.
	compiled_contradictory_constraint_row(Row, ColumnIndex, RowIndex, CompiledNetwork, Node1, Node2, Cause) :-
		functor(Row, row, RowLength),
		ColumnIndex =< RowLength,
		RightIndex is RowIndex + ColumnIndex,
		arg(ColumnIndex, Row, Cell),
		cell_relation(Cell, []),
		compiled_contradictory_constraint_cell(CompiledNetwork, RowIndex, RightIndex, Cell, Cause),
		node_name(CompiledNetwork, RowIndex, Node1),
		node_name(CompiledNetwork, RightIndex, Node2).
	compiled_contradictory_constraint_row(Row, ColumnIndex, RowIndex, CompiledNetwork, Node1, Node2, Cause) :-
		functor(Row, row, RowLength),
		ColumnIndex < RowLength,
		NextColumnIndex is ColumnIndex + 1,
		compiled_contradictory_constraint_row(Row, NextColumnIndex, RowIndex, CompiledNetwork, Node1, Node2, Cause).

	compiled_contradictory_constraint_cell(CompiledNetwork, LeftIndex, RightIndex, cell([]), Cause) :-
		compiled_contradiction_cause_indices(CompiledNetwork, LeftIndex, RightIndex, Cause).
	compiled_contradictory_constraint_cell(_CompiledNetwork, _LeftIndex, _RightIndex, cell([], propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)), propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)) :-
		ComposedRelationSet \== [],
		!.
	compiled_contradictory_constraint_cell(CompiledNetwork, LeftIndex, RightIndex, cell([], _Cause), Cause) :-
		compiled_contradiction_cause_indices(CompiledNetwork, LeftIndex, RightIndex, Cause).

	compiled_contradiction_explanations(CompiledNetwork, Explanations) :-
		compiled_contradiction_explanations(CompiledNetwork, 1, [], ReversedExplanations),
		reverse(ReversedExplanations, Explanations).

	compiled_contradiction_explanations(CompiledNetwork, RowIndex, Explanations, Explanations) :-
		compiled_size(CompiledNetwork, Size),
		RowIndex >= Size,
		!.
	compiled_contradiction_explanations(CompiledNetwork, RowIndex, Explanations0, Explanations) :-
		CompiledNetwork = compiled_network(_Nodes, _NodeVector, _NodeIndexVector, Matrix),
		arg(RowIndex, Matrix, Row),
		compiled_contradiction_row_explanations(Row, 1, RowIndex, CompiledNetwork, Explanations0, Explanations1),
		NextRowIndex is RowIndex + 1,
		compiled_contradiction_explanations(CompiledNetwork, NextRowIndex, Explanations1, Explanations).

	compiled_contradiction_row_explanations(Row, ColumnIndex, _RowIndex, _CompiledNetwork, Explanations, Explanations) :-
		functor(Row, row, RowLength),
		ColumnIndex > RowLength,
		!.
	compiled_contradiction_row_explanations(Row, ColumnIndex, RowIndex, CompiledNetwork, Explanations0, Explanations) :-
		functor(Row, row, RowLength),
		ColumnIndex =< RowLength,
		RightIndex is RowIndex + ColumnIndex,
		arg(ColumnIndex, Row, Cell),
		(	cell_relation(Cell, []) ->
			node_name(CompiledNetwork, RowIndex, Node1),
			node_name(CompiledNetwork, RightIndex, Node2),
			compiled_contradiction_explanation_cell(CompiledNetwork, RowIndex, RightIndex, Cell, Cause),
			Explanations1 = [contradiction(Node1, Node2, Cause)| Explanations0]
		;	Explanations1 = Explanations0
		),
		NextColumnIndex is ColumnIndex + 1,
		compiled_contradiction_row_explanations(Row, NextColumnIndex, RowIndex, CompiledNetwork, Explanations1, Explanations).

	compiled_contradiction_explanation_cell(CompiledNetwork, LeftIndex, RightIndex, cell([]), Cause) :-
		compiled_contradiction_cause_indices(CompiledNetwork, LeftIndex, RightIndex, Cause).
	compiled_contradiction_explanation_cell(_CompiledNetwork, _LeftIndex, _RightIndex, cell([], propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)), propagated(Node3, LeftRelationSet, RightRelationSet, ComposedRelationSet)) :-
		ComposedRelationSet \== [],
		!.
	compiled_contradiction_explanation_cell(CompiledNetwork, LeftIndex, RightIndex, cell([], _Cause), Cause) :-
		compiled_contradiction_cause_indices(CompiledNetwork, LeftIndex, RightIndex, Cause).

	compiled_subsumes_nodes([], _CompiledNetwork1, _CompiledNetwork2).
	compiled_subsumes_nodes([Node| Nodes], CompiledNetwork1, CompiledNetwork2) :-
		compiled_subsumes_from_node(Nodes, Node, CompiledNetwork1, CompiledNetwork2),
		compiled_subsumes_nodes(Nodes, CompiledNetwork1, CompiledNetwork2).

	compiled_subsumes_from_node([], _Node, _CompiledNetwork1, _CompiledNetwork2).
	compiled_subsumes_from_node([OtherNode| OtherNodes], Node, CompiledNetwork1, CompiledNetwork2) :-
		compiled_relation_by_nodes(CompiledNetwork1, Node, OtherNode, RelationSet1),
		compiled_relation_by_nodes(CompiledNetwork2, Node, OtherNode, RelationSet2),
		interval_relation_set::subset(RelationSet2, RelationSet1),
		compiled_subsumes_from_node(OtherNodes, Node, CompiledNetwork1, CompiledNetwork2).

	propagation_triples([], Triples, Triples).
	propagation_triples([Change| Changes], Triples0, Triples) :-
		(	propagation_triple(Change, Triple) ->
			add_unique_triple(Triple, Triples0, Triples1)
		;	Triples1 = Triples0
		),
		propagation_triples(Changes, Triples1, Triples).

	add_unique_triple(Triple, Triples0, Triples) :-
		(	member(Triple, Triples0) ->
			Triples = Triples0
		;	Triples = [Triple| Triples0]
		).

	add_unique_explanation(Explanation, Explanations0, Explanations) :-
		(	memberchk(Explanation, Explanations0) ->
			Explanations = Explanations0
		;	Explanations = [Explanation| Explanations0]
		).

	explicit_constraints([], []).
	explicit_constraints([constraint(Node1, Node2, RelationSet)| Constraints], [constraint(Node1, Node2, RelationSet)| ExplicitConstraints]) :-
		!,
		explicit_constraints(Constraints, ExplicitConstraints).
	explicit_constraints([constraint(Node1, Node2, RelationSet, _Cause)| Constraints], [constraint(Node1, Node2, RelationSet)| ExplicitConstraints]) :-
		explicit_constraints(Constraints, ExplicitConstraints).

	same_node_set(Nodes1, Nodes2) :-
		same_length_lists(Nodes1, Nodes2),
		all_nodes_member(Nodes1, Nodes2),
		all_nodes_member(Nodes2, Nodes1).

	same_length_lists([], []).
	same_length_lists([_Node1| Nodes1], [_Node2| Nodes2]) :-
		same_length_lists(Nodes1, Nodes2).

	all_nodes_member([], _Nodes).
	all_nodes_member([Node| Nodes], Others) :-
		memberchk(Node, Others),
		all_nodes_member(Nodes, Others).

:- end_object.

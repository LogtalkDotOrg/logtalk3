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


:- object(atms,
	implements(atms_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'Portable incremental Assumption-based Truth Maintenance System.',
		remarks is [
			'Algorithm' - 'A label is an antichain of minimal environments. New label environments are propagated as deltas through a reverse justification index.',
			'Nogoods' - 'Nogoods are also maintained as an antichain and immediately prune every affected label.',
			'Portability' - 'The implementation is purely functional and uses the portable AVL tree dictionary for its state indexes.'
		],
		see_also is [atms_environment_protocol, atms_ordered_list_environment, atms_bitset_environment]
	]).

	:- uses(avltree, [
		as_list/2 as dictionary_as_list/2, insert/4 as dictionary_insert/4, keys/2 as dictionary_keys/2,
		lookup/3 as dictionary_lookup/3, map/3 as dictionary_map/3, new/1 as dictionary_new/1,
		update/4 as dictionary_update/4
	]).

	:- uses(list, [
		member/2, reverse/2
	]).

	% State is deliberately opaque to clients. It contains the representation,
	% next node identifier, node, assumption, and contradiction dictionaries,
	% justifications, a label dictionary, nogoods, and a reverse justification
	% index dictionary.

	new(State) :-
		new(atms_ordered_list_environment, State).

	new(Representation, atms_state(Representation, 0, Empty, Empty, Empty, [], Empty, [], Empty)) :-
		conforms_to_protocol(Representation, atms_environment_protocol),
		dictionary_new(Empty).

	clear(State, Cleared) :-
		representation(State, Representation),
		new(Representation, Cleared).

	representation(atms_state(Representation, _, _, _, _, _, _, _, _), Representation).

	create_node(Datum, atms_state(Representation, Id, Nodes, Assumptions, Contradictions, Justifications, Labels, Nogoods, Dependents), Node, State) :-
		Node = node(Id),
		NextId is Id + 1,
		dictionary_insert(Nodes, Node, Datum, NewNodes),
		dictionary_insert(Labels, Node, [], NewLabels),
		State = atms_state(Representation, NextId, NewNodes, Assumptions, Contradictions, Justifications, NewLabels, Nogoods, Dependents).

	create_assumption(Datum, State0, Assumption, State) :-
		create_node(Datum, State0, Assumption, State1),
		State1 = atms_state(Representation, NextId, Nodes, Assumptions0, Contradictions, Justifications, Labels, Nogoods, Dependents),
		Representation::singleton(Assumption, Environment),
		dictionary_insert(Assumptions0, Assumption, true, Assumptions),
		State2 = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels, Nogoods, Dependents),
		update_label(Assumption, [Environment], State2, State3, Delta),
		propagate([Assumption-Delta], State3, State).

	create_contradiction(Datum, State0, Contradiction, State) :-
		create_node(Datum, State0, Contradiction, State1),
		State1 = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions0, Justifications, Labels, Nogoods, Dependents),
		dictionary_insert(Contradictions0, Contradiction, true, Contradictions),
		State = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels, Nogoods, Dependents).

	justify(Consequent, Antecedents0, Info, State0, State) :-
		known_node(Consequent, State0),
		sort(Antecedents0, Antecedents),
		all_nodes(Antecedents, State0),
		Justification = justification(Consequent, Antecedents, Info),
		add_justification(Justification, State0, State1, Added),
		(	Added == false ->
			State = State1
		;	candidates(Antecedents, State1, CandidateEnvironments),
			update_label(Consequent, CandidateEnvironments, State1, State2, Delta),
			(	contradiction(Consequent, State2) ->
				State = State2
			;	once(propagate([Consequent-Delta], State2, State))
			)
		).

	node(Node, atms_state(_, _, Nodes, _, _, _, _, _, _)) :-
		dictionary_key(Node, Nodes).

	assumption(Assumption, atms_state(_, _, _, Assumptions, _, _, _, _, _)) :-
		dictionary_key(Assumption, Assumptions).

	contradiction(Contradiction, atms_state(_, _, _, _, Contradictions, _, _, _, _)) :-
		dictionary_key(Contradiction, Contradictions).

	nodes(atms_state(_, _, Nodes, _, _, _, _, _, _), Pairs) :-
		dictionary_as_list(Nodes, Pairs).

	assumptions(atms_state(_, _, _, Assumptions, _, _, _, _, _), AssumptionNodes) :-
		dictionary_keys(Assumptions, AssumptionNodes).

	justifications(atms_state(_, _, _, _, _, StoredJustifications, _, _, _), Justifications) :-
		reverse(StoredJustifications, Justifications).

	label(Node, atms_state(Representation, _, _, _, _, _, Labels, _, _), Environments) :-
		dictionary_lookup(Node, InternalEnvironments, Labels),
		external_environments(InternalEnvironments, Representation, ExternalEnvironments),
		sort(ExternalEnvironments, Environments).

	in_label(Node, Environment, atms_state(Representation, _, _, _, _, _, Labels, _, _)) :-
		Representation::from_list(Environment, InternalEnvironment),
		dictionary_lookup(Node, InternalEnvironments, Labels),
		member(StoredEnvironment, InternalEnvironments),
		Representation::equal(InternalEnvironment, StoredEnvironment).

	consistent(Environment, atms_state(Representation, _, _, _, _, _, _, Nogoods, _)) :-
		Representation::from_list(Environment, InternalEnvironment),
		consistent_environment(InternalEnvironment, Nogoods, Representation).

	nogood(Environment, atms_state(Representation, _, _, _, _, _, _, Nogoods, _)) :-
		reverse(Nogoods, OrderedNogoods),
		member(InternalEnvironment, OrderedNogoods),
		Representation::to_list(InternalEnvironment, Environment).

	why(Node, atms_state(_, _, _, _, _, Justifications, _, _, _), Why) :-
		why_justifications(Justifications, Node, Why).

	interpretations(atms_state(Representation, _, _, Assumptions, _, _, _, Nogoods, _), Interpretations) :-
		dictionary_keys(Assumptions, AssumptionNodes),
		( 	Nogoods == [] ->
			Interpretations = [AssumptionNodes]
		; 	Representation::empty(Empty),
			( 	consistent_environment(Empty, Nogoods, Representation) ->
				maximal_consistent_environments(AssumptionNodes, Empty, Nogoods, Representation, [], MaximalEnvironments)
			; 	MaximalEnvironments = []
			),
			external_environments(MaximalEnvironments, Representation, ExternalInterpretations),
			sort(ExternalInterpretations, Interpretations)
		).

	% Adding a justification evaluates it once using all current labels. The
	% reverse index lets later label changes evaluate only the affected product.

	add_justification(Justification, State, State, false) :-
		State = atms_state(_, _, _, _, _, Justifications, _, _, _),
		member_term(Justification, Justifications),
		!.
	add_justification(Justification, atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications0, Labels, Nogoods, Dependents0), State, true) :-
		Justification = justification(_, Antecedents, _),
		Justifications = [Justification| Justifications0],
		index_justification(Antecedents, Justification, Dependents0, Dependents),
		State = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels, Nogoods, Dependents).

	index_justification([], _, Dependents, Dependents).
	index_justification([Node| Nodes], Justification, Dependents0, Dependents) :-
		add_dependent(Dependents0, Node, Justification, Dependents1),
		index_justification(Nodes, Justification, Dependents1, Dependents).

	add_dependent(Dependents0, Node, Justification, Dependents) :-
		(	dictionary_lookup(Node, Justifications, Dependents0) ->
			dictionary_update(Dependents0, Node, [Justification| Justifications], Dependents)
		;	dictionary_insert(Dependents0, Node, [Justification], Dependents)
		).

	% A label update first removes inconsistent candidates, then produces the
	% minimal antichain. Delta contains precisely its environments not present
	% in the former label. A contradiction delta becomes new nogoods at once.

	update_label(Node, Candidates, State0, State, Delta) :-
		State0 = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels0, Nogoods0, Dependents),
		dictionary_lookup(Node, OldLabel, Labels0),
		filter_consistent(Candidates, Nogoods0, Representation, ConsistentCandidates),
		minimal(ConsistentCandidates, Representation, OldLabel, NewLabel),
		new_environments(NewLabel, OldLabel, Representation, Delta),
		(	Delta == [] ->
			State = State0
		;	dictionary_update(Labels0, Node, NewLabel, Labels1),
			State1 = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels1, Nogoods0, Dependents),
			(	dictionary_lookup(Node, _, Contradictions) ->
				record_nogoods(Delta, State1, State)
			;	State = State1
			)
		).

	record_nogoods([], State, State).
	record_nogoods([Environment| Environments], State0, State) :-
		add_nogood(Environment, State0, State1, Added),
		(	Added == true ->
			prune_labels_for_nogood(Environment, State1, State2)
		;	State2 = State1
		),
		record_nogoods(Environments, State2, State).

	add_nogood(Environment, State, State, false) :-
		State = atms_state(Representation, _, _, _, _, _, _, Nogoods, _),
		member(Nogood, Nogoods),
		Representation::subset(Nogood, Environment),
		!.
	add_nogood(Environment, atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels, Nogoods0, Dependents), State, true) :-
		remove_strict_supersets(Nogoods0, Environment, Representation, RemainingNogoods),
		Nogoods = [Environment| RemainingNogoods],
		State = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels, Nogoods, Dependents).

	prune_labels_for_nogood(Nogood, atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels0, Nogoods, Dependents), State) :-
		dictionary_map(prune_label(Nogood, Representation), Labels0, Labels),
		State = atms_state(Representation, NextId, Nodes, Assumptions, Contradictions, Justifications, Labels, Nogoods, Dependents).

	prune_label(Nogood, Representation, Node-Label0, Node-Label) :-
		filter_consistent(Label0, [Nogood], Representation, Label).

	% Delta propagation uses a worklist. Cycles terminate because every queued
	% delta adds a previously absent member of a finite label antichain.

	propagate([], State, State).
	propagate([Node-Delta| Queue0], State0, State) :-
		(	Delta == [] ->
			State1 = State0,
			Worklist = Queue0
		;	dependents(Node, State0, Justifications),
			propagate_justifications(Justifications, Node, Delta, State0, State1, Worklist, Queue0)
		),
		propagate(Worklist, State1, State).

	dependents(Node, atms_state(_, _, _, _, _, _, _, _, Dependents), Justifications) :-
		( dictionary_lookup(Node, Justifications, Dependents) ->
			true
		;	Justifications = []
		).

	propagate_justifications([], _, _, State, State, Worklist, Worklist).
	propagate_justifications([Justification| Justifications], Node, Delta, State0, State, Worklist0, Worklist) :-
		Justification = justification(Consequent, Antecedents, _),
		candidates_with_delta(Antecedents, Node, Delta, State0, Candidates),
		update_label(Consequent, Candidates, State0, State1, NewDelta),
		(	(contradiction(Consequent, State1); NewDelta == []) ->
			Worklist0 = Worklist1
		;	Worklist0 = [Consequent-NewDelta| Worklist1]
		),
		propagate_justifications(Justifications, Node, Delta, State1, State, Worklist1, Worklist).

	% Cartesian products are limited to the newly added part of the triggering
	% antecedent label. Other antecedents use their complete current labels.

	candidates(Antecedents, State, Candidates) :-
		environment_lists(Antecedents, State, Lists),
		combine_state_environment_lists(Lists, State, Candidates).

	candidates_with_delta([], _, _, State, Candidates) :-
		State = atms_state(Representation, _, _, _, _, _, _, Nogoods, _),
		Representation::empty(Empty),
		(	consistent_environment(Empty, Nogoods, Representation) ->
			Candidates = [Empty]
		;	Candidates = []
		).
	candidates_with_delta([Antecedent| Antecedents], Node, Delta, State, Candidates) :-
		(	Antecedent == Node ->
			First = Delta
		;	raw_label(Antecedent, State, First)
		),
		( 	First == [] ->
			Candidates = []
		; 	candidates_with_delta(Antecedents, Node, Delta, State, Rest),
			State = atms_state(Representation, _, _, _, _, _, _, Nogoods, _),
			combine_environment_sets(First, Rest, Nogoods, Representation, Candidates)
		).

	environment_lists([], _, []).
	environment_lists([Node| Nodes], State, [Label| Labels]) :-
		raw_label(Node, State, Label),
		environment_lists(Nodes, State, Labels).

	combine_state_environment_lists(Lists, State, Environments) :-
		State = atms_state(Representation, _, _, _, _, _, _, Nogoods, _),
		Representation::empty(Empty),
		combine_environment_lists(Lists, [Empty], Nogoods, Representation, Environments).

	combine_environment_lists([], Environments, _, _, Environments).
	combine_environment_lists([First| Rest], Environments0, Nogoods, Representation, Environments) :-
		combine_environment_sets(Environments0, First, Nogoods, Representation, Environments1),
		combine_environment_lists(Rest, Environments1, Nogoods, Representation, Environments).

	combine_environment_sets(First, Second, Nogoods, Representation, Environments) :-
		accumulate_environment_products(First, Second, Nogoods, Representation, [], Environments).

	accumulate_environment_products([], _, _, _, Environments, Environments).
	accumulate_environment_products([Environment| Environments], Second, Nogoods, Representation, Accumulator0, Accumulator) :-
		accumulate_environment_unions(Second, Environment, Nogoods, Representation, Accumulator0, Accumulator1),
		accumulate_environment_products(Environments, Second, Nogoods, Representation, Accumulator1, Accumulator).

	accumulate_environment_unions([], _, _, _, Environments, Environments).
	accumulate_environment_unions([Environment2| Environments], Environment1, Nogoods, Representation, Accumulator0, Accumulator) :-
		Representation::union(Environment1, Environment2, Union),
		(	consistent_environment(Union, Nogoods, Representation) ->
			add_minimal_environment(Union, Accumulator0, Representation, Accumulator1)
		;	Accumulator1 = Accumulator0
		),
		accumulate_environment_unions(Environments, Environment1, Nogoods, Representation, Accumulator1, Accumulator).

	% Environment antichains and conversions.

	consistent_environment(Environment, Nogoods, Representation) :-
		\+ (
			member(Nogood, Nogoods),
			Representation::subset(Nogood, Environment)
		).

	filter_consistent([], _, _, []).
	filter_consistent([Environment| Environments], Nogoods, Representation, Filtered) :-
		(	consistent_environment(Environment, Nogoods, Representation) ->
			Filtered = [Environment| Tail]
		;	Filtered = Tail
		),
		filter_consistent(Environments, Nogoods, Representation, Tail).

	minimal(Environments, Representation, Minimal) :-
		minimal(Environments, Representation, [], Minimal).

	minimal([], _, Minimal, Minimal).
	minimal([Environment| Environments], Representation, Minimal0, Minimal) :-
		add_minimal_environment(Environment, Minimal0, Representation, Minimal1),
		minimal(Environments, Representation, Minimal1, Minimal).

	add_minimal_environment(Environment, Minimal, Representation, Minimal) :-
		subsumed_by(Environment, Minimal, Representation),
		!.
	add_minimal_environment(Environment, Minimal0, Representation, [Environment| Remaining]) :-
		remove_strict_supersets(Minimal0, Environment, Representation, Remaining).

	subsumed_by(Environment, Environments, Representation) :-
		member(Other, Environments),
		Representation::subset(Other, Environment).

	remove_strict_supersets([], _, _, []).
	remove_strict_supersets([Other| Environments], Environment, Representation, Remaining) :-
		(	Representation::subset(Environment, Other),
			\+ Representation::equal(Environment, Other) ->
			Remaining = Tail
		;	Remaining = [Other| Tail]
		),
		remove_strict_supersets(Environments, Environment, Representation, Tail).

	new_environments([], _, _, []).
	new_environments([Environment| Environments], OldLabel, Representation, Delta) :-
		(	member_equal(Environment, OldLabel, Representation) ->
			Delta = Tail
		;	Delta = [Environment| Tail]
		),
		new_environments(Environments, OldLabel, Representation, Tail).

	member_equal(Environment, Environments, Representation) :-
		member(Other, Environments),
		Representation::equal(Environment, Other).

	external_environments([], _, []).
	external_environments([Environment| Environments], Representation, [List| Lists]) :-
		Representation::to_list(Environment, List),
		external_environments(Environments, Representation, Lists).

	maximal_consistent_environments([], Environment, _, Representation, Maximal0, Maximal) :-
		add_maximal_environment(Environment, Maximal0, Representation, Maximal).
	maximal_consistent_environments([Assumption| Assumptions], Environment0, Nogoods, Representation, Maximal0, Maximal) :-
		Representation::singleton(Assumption, Singleton),
		Representation::union(Environment0, Singleton, Environment1),
		(	consistent_environment(Environment1, Nogoods, Representation) ->
			maximal_consistent_environments(Assumptions, Environment1, Nogoods, Representation, Maximal0, Maximal1)
		;	Maximal1 = Maximal0
		),
		maximal_consistent_environments(Assumptions, Environment0, Nogoods, Representation, Maximal1, Maximal).

	add_maximal_environment(Environment, Maximal, Representation, Maximal) :-
		member(Other, Maximal),
		Representation::subset(Environment, Other),
		!.
	add_maximal_environment(Environment, Maximal0, Representation, [Environment| Maximal]) :-
		remove_strict_subsets(Maximal0, Environment, Representation, Maximal).

	remove_strict_subsets([], _, _, []).
	remove_strict_subsets([Other| Environments], Environment, Representation, Remaining) :-
		(	Representation::subset(Other, Environment),
			\+ Representation::equal(Other, Environment) ->
			Remaining = Tail
		;	Remaining = [Other| Tail]
		),
		remove_strict_subsets(Environments, Environment, Representation, Tail).

	% State indexes and small list utilities.

	raw_label(Node, atms_state(_, _, _, _, _, _, Labels, _, _), Label) :-
		dictionary_lookup(Node, Label, Labels).

	all_nodes([], _).
	all_nodes([Node| Nodes], State) :-
		known_node(Node, State),
		all_nodes(Nodes, State).

	known_node(Node, atms_state(_, _, Nodes, _, _, _, _, _, _)) :-
		dictionary_lookup(Node, _, Nodes).

	% Preserve logarithmic lookup for ground keys while supporting enumeration
	% and unification with partially instantiated keys.
	dictionary_key(Key, Dictionary) :-
		(	ground(Key) ->
			dictionary_lookup(Key, _, Dictionary)
		;	dictionary_lookup(StoredKey, _, Dictionary),
			Key = StoredKey
		).

	member_term(Term, [Head| _]) :-
		Term == Head,
		!.
	member_term(Term, [_| Tail]) :-
		member_term(Term, Tail).

	why_justifications(Justifications, Node, Why) :-
		why_justifications(Justifications, Node, [], Why).

	why_justifications([], _, Why, Why).
	why_justifications([Justification| Justifications], Node, Why0, Why) :-
		Justification = justification(Consequent, _, _),
		(	Consequent == Node ->
			Why1 = [Justification| Why0]
		;	Why1 = Why0
		),
		why_justifications(Justifications, Node, Why1, Why).

:- end_object.

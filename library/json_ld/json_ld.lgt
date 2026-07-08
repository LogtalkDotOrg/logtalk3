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


:- object(json_ld(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_),
	implements(json_ld_protocol)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'JSON-LD 1.1 parser, generator, and processor. Builds on top of the ``json`` library for JSON parsing and generation.',
		parameters is [
			'ObjectRepresentation' - 'Object representation to be used when decoding JSON objects. Possible values are ``curly`` (default) and ``list``.',
			'PairRepresentation' - 'Pair representation to be used when decoding JSON objects. Possible values are ``dash`` (default), ``equal``, and ``colon``.',
			'StringRepresentation' - 'Text representation to be used when decoding JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- uses(json(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_), [
		parse/2 as json_parse/2,
		generate/2 as json_generate/2
	]).

	:- uses(list, [
		append/3, member/2, reverse/2, valid/1 as is_list/1
	]).

	% ==================== parse/2 ====================

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), Term) :-
		json_parse(file(File), Term),
		!.
	parse(stream(Stream), Term) :-
		json_parse(stream(Stream), Term),
		!.
	parse(codes(Codes), Term) :-
		json_parse(codes(Codes), Term),
		!.
	parse(chars(Chars), Term) :-
		json_parse(chars(Chars), Term),
		!.
	parse(atom(Atom), Term) :-
		json_parse(atom(Atom), Term),
		!.
	parse(Source, _) :-
		domain_error(json_ld_source, Source).

	% ==================== generate/2 ====================

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), Term) :-
		json_generate(file(File), Term),
		!.
	generate(stream(Stream), Term) :-
		json_generate(stream(Stream), Term),
		!.
	generate(codes(Codes), Term) :-
		json_generate(codes(Codes), Term),
		!.
	generate(chars(Chars), Term) :-
		json_generate(chars(Chars), Term),
		!.
	generate(atom(Atom), Term) :-
		json_generate(atom(Atom), Term),
		!.
	generate(Sink, _) :-
		domain_error(json_ld_sink, Sink).

	% ==================== expand/2 ====================

	expand(Document, _) :-
		var(Document),
		instantiation_error.
	expand(Document, Expanded) :-
		expand_document(Document, [], Expanded).

	% ==================== compact/3 ====================

	compact(Document, _, _) :-
		var(Document),
		instantiation_error.
	compact(_, Context, _) :-
		var(Context),
		instantiation_error.
	compact(Document, Context, Compacted) :-
		build_context(Context, ActiveContext),
		compact_document(Document, ActiveContext, Compacted).

	% ==================== frame/3 ====================

	frame(Document, _, _) :-
		var(Document),
		instantiation_error.
	frame(_, Frame, _) :-
		var(Frame),
		instantiation_error.
	frame(Document, Frame, Framed) :-
		build_context(Frame, ActiveContext),
		normalize_frame(Frame, ActiveContext, ExpandedFrame),
		expand(Document, ExpandedDocument),
		flatten(ExpandedDocument, Nodes),
		frame_nodes(Nodes, ExpandedFrame, Nodes, [], FramedExpanded),
		frame_output(Frame, ActiveContext, FramedExpanded, Framed).

	% ==================== flatten/2 ====================

	flatten(Document, _) :-
		var(Document),
		instantiation_error.
	flatten(Document, Flattened) :-
		node_map_generate(Document, '@default', [], _, 0, _, [], NodeMap),
		flatten_node_map(NodeMap, Flattened).

	node_map_generate(List, GraphName, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		is_list(List),
		!,
		node_map_generate_list(List, GraphName, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap).
	node_map_generate(Object, GraphName, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		is_object(Object),
		!,
		object_to_pairs(Object, Pairs),
		(	get_pair_value('@value', Pairs, _) ->
			IdentifierMap = IdentifierMap0,
			Counter = Counter0,
			NodeMap = NodeMap0
		;	node_map_subject_id(Pairs, IdentifierMap0, IdentifierMap1, Counter0, Counter1, Subject),
			ensure_node(GraphName, Subject, NodeMap0, NodeMap1),
			node_map_properties(Pairs, GraphName, Subject, IdentifierMap1, IdentifierMap, Counter1, Counter, NodeMap1, NodeMap)
		).

	node_map_generate_list([], _GraphName, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap).
	node_map_generate_list([Element| Elements], GraphName, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		node_map_generate(Element, GraphName, IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1),
		node_map_generate_list(Elements, GraphName, IdentifierMap1, IdentifierMap, Counter1, Counter, NodeMap1, NodeMap).

	node_map_subject_id(Pairs, IdentifierMap0, IdentifierMap, Counter0, Counter, Subject) :-
		(	get_pair_value('@id', Pairs, Id0) ->
			node_map_identifier(Id0, IdentifierMap0, IdentifierMap, Counter0, Counter, Subject)
		;	new_blank_node_identifier(Counter0, Counter, Subject),
			IdentifierMap = IdentifierMap0
		).

	node_map_identifier(Id0, IdentifierMap0, IdentifierMap, Counter0, Counter, Id) :-
		json_ld_string_atom(Id0, Id1),
		!,
		(	sub_atom(Id1, 0, 2, _, '_:') ->
			blank_node_identifier(Id1, IdentifierMap0, IdentifierMap, Counter0, Counter, Id)
		;	Id = Id1,
			IdentifierMap = IdentifierMap0,
			Counter = Counter0
		).
	node_map_identifier(Id, IdentifierMap, IdentifierMap, Counter, Counter, Id).

	blank_node_identifier(Id, [Id-Mapped| _], [Id-Mapped| _], Counter, Counter, Mapped) :-
		!.
	blank_node_identifier(Id, [Entry| Entries0], [Entry| Entries], Counter0, Counter, Mapped) :-
		blank_node_identifier(Id, Entries0, Entries, Counter0, Counter, Mapped).
	blank_node_identifier(Id, [], [Id-Mapped], Counter0, Counter, Mapped) :-
		new_blank_node_identifier(Counter0, Counter, Mapped).

	new_blank_node_identifier(Counter0, Counter, Identifier) :-
		number_codes(Counter0, CounterCodes),
		atom_codes(CounterAtom, CounterCodes),
		atom_concat('_:b', CounterAtom, Identifier),
		Counter is Counter0 + 1.

	node_map_properties([], _GraphName, _Subject, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap).
	node_map_properties([Pair| Pairs], GraphName, Subject, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		pair_key_value(Pair, Key0, Value),
		normalize_key(Key0, Key),
		(	Key == '@id' ->
			NodeMap1 = NodeMap0,
			IdentifierMap1 = IdentifierMap0,
			Counter1 = Counter0
		;	Key == '@type' ->
			node_map_types(Value, IdentifierMap0, IdentifierMap1, Counter0, Counter1, Types),
			add_node_property(GraphName, Subject, '@type', Types, NodeMap0, NodeMap1)
		;	Key == '@index' ->
			add_node_single_property(GraphName, Subject, '@index', Value, NodeMap0, NodeMap1),
			IdentifierMap1 = IdentifierMap0,
			Counter1 = Counter0
		;	Key == '@graph' ->
			node_map_generate(Value, Subject, IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1)
		;	Key == '@included' ->
			node_map_generate(Value, GraphName, IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1)
		;	Key == '@reverse' ->
			node_map_reverse(Value, GraphName, Subject, IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1)
		;	node_map_property_values(Value, GraphName, Subject, Key, IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1)
		),
		node_map_properties(Pairs, GraphName, Subject, IdentifierMap1, IdentifierMap, Counter1, Counter, NodeMap1, NodeMap).

	node_map_types([], IdentifierMap, IdentifierMap, Counter, Counter, []) :-
		!.
	node_map_types([Type0| Types0], IdentifierMap0, IdentifierMap, Counter0, Counter, [Type| Types]) :-
		!,
		node_map_identifier(Type0, IdentifierMap0, IdentifierMap1, Counter0, Counter1, Type),
		node_map_types(Types0, IdentifierMap1, IdentifierMap, Counter1, Counter, Types).
	node_map_types(Type0, IdentifierMap0, IdentifierMap, Counter0, Counter, [Type]) :-
		node_map_identifier(Type0, IdentifierMap0, IdentifierMap, Counter0, Counter, Type).

	node_map_reverse(Value, GraphName, Subject, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		is_object(Value),
		!,
		object_to_pairs(Value, Pairs),
		node_map_reverse_properties(Pairs, GraphName, Subject, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap).
	node_map_reverse(_Value, _GraphName, _Subject, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap).

	node_map_reverse_properties([], _GraphName, _Subject, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap).
	node_map_reverse_properties([Pair| Pairs], GraphName, Subject, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		pair_key_value(Pair, Key0, Values),
		normalize_key(Key0, Key),
		node_reference_object(Subject, Reference),
		node_map_reverse_values(Values, GraphName, Key, Reference, IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1),
		node_map_reverse_properties(Pairs, GraphName, Subject, IdentifierMap1, IdentifierMap, Counter1, Counter, NodeMap1, NodeMap).

	node_map_reverse_values([], _GraphName, _Key, _Reference, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap).
	node_map_reverse_values([Value| Values], GraphName, Key, Reference, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		is_object(Value),
		!,
		object_to_pairs(Value, Pairs),
		node_map_subject_id(Pairs, IdentifierMap0, IdentifierMap1, Counter0, Counter1, Id),
		ensure_node(GraphName, Id, NodeMap0, NodeMap1),
		add_node_property(GraphName, Id, Key, [Reference], NodeMap1, NodeMap2),
		node_map_properties(Pairs, GraphName, Id, IdentifierMap1, IdentifierMap2, Counter1, Counter2, NodeMap2, NodeMap3),
		node_map_reverse_values(Values, GraphName, Key, Reference, IdentifierMap2, IdentifierMap, Counter2, Counter, NodeMap3, NodeMap).
	node_map_reverse_values([_| Values], GraphName, Key, Reference, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		node_map_reverse_values(Values, GraphName, Key, Reference, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap).

	node_map_property_values([], _GraphName, _Subject, _Key, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap) :-
		!.
	node_map_property_values([Value| Values], GraphName, Subject, Key, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		!,
		node_map_property_value(Value, GraphName, Subject, Key, IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1, MappedValue),
		add_node_property(GraphName, Subject, Key, [MappedValue], NodeMap1, NodeMap2),
		node_map_property_values(Values, GraphName, Subject, Key, IdentifierMap1, IdentifierMap, Counter1, Counter, NodeMap2, NodeMap).
	node_map_property_values(Value, GraphName, Subject, Key, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap) :-
		node_map_property_value(Value, GraphName, Subject, Key, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap1, MappedValue),
		add_node_property(GraphName, Subject, Key, [MappedValue], NodeMap1, NodeMap).

	node_map_property_value(Value, GraphName, _Subject, _Key, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap, MappedValue) :-
		is_object(Value),
		object_to_pairs(Value, Pairs),
		\+ get_pair_value('@value', Pairs, _),
		!,
		(	get_pair_value('@list', Pairs, ListValues) ->
			node_map_list(ListValues, GraphName, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap, MappedListValues),
			make_pair('@list', MappedListValues, ListPair),
			pairs_to_object([ListPair], MappedValue)
		;	node_map_subject_id(Pairs, IdentifierMap0, IdentifierMap1, Counter0, Counter1, Id),
			ensure_node(GraphName, Id, NodeMap0, NodeMap1),
			node_map_properties(Pairs, GraphName, Id, IdentifierMap1, IdentifierMap, Counter1, Counter, NodeMap1, NodeMap),
			node_reference_object(Id, MappedValue)
		).
	node_map_property_value(Value, _GraphName, _Subject, _Key, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap, Value).

	node_map_list([], _GraphName, IdentifierMap, IdentifierMap, Counter, Counter, NodeMap, NodeMap, []).
	node_map_list([Value| Values], GraphName, IdentifierMap0, IdentifierMap, Counter0, Counter, NodeMap0, NodeMap, [MappedValue| MappedValues]) :-
		node_map_property_value(Value, GraphName, list, '@list', IdentifierMap0, IdentifierMap1, Counter0, Counter1, NodeMap0, NodeMap1, MappedValue),
		node_map_list(Values, GraphName, IdentifierMap1, IdentifierMap, Counter1, Counter, NodeMap1, NodeMap, MappedValues).

	node_reference_object(Id, Reference) :-
		make_pair('@id', Id, IdPair),
		pairs_to_object([IdPair], Reference).

	ensure_node(GraphName, Id, NodeMap0, NodeMap) :-
		(	get_graph_nodes(GraphName, NodeMap0, Nodes),
			get_node_pairs(Id, Nodes, _) ->
			NodeMap = NodeMap0
		;	make_pair('@id', Id, IdPair),
			set_graph_node(GraphName, Id, [IdPair], NodeMap0, NodeMap)
		).

	add_node_single_property(GraphName, Id, Key, Value, NodeMap0, NodeMap) :-
		make_pair(Key, Value, Pair),
		get_graph_nodes(GraphName, NodeMap0, Nodes),
		get_node_pairs(Id, Nodes, Pairs0),
		(	get_pair_value(Key, Pairs0, CurrentValue) ->
			(	CurrentValue == Value ->
				Pairs = Pairs0
			;	Key == '@index' ->
				domain_error(json_ld_conflicting_indexes, Id)
			;		replace_pair(Pairs0, Key, Pair, Pairs)
			)
		;	Pairs = [Pair| Pairs0]
		),
		set_graph_node(GraphName, Id, Pairs, NodeMap0, NodeMap).

	add_node_property(_GraphName, _Id, _Key, [], NodeMap, NodeMap) :-
		!.
	add_node_property(GraphName, Id, Key, Values, NodeMap0, NodeMap) :-
		get_graph_nodes(GraphName, NodeMap0, Nodes),
		get_node_pairs(Id, Nodes, Pairs0),
		(	get_pair_value(Key, Pairs0, CurrentValues) ->
			append_unique_values(Values, CurrentValues, MergedValues),
			make_pair(Key, MergedValues, Pair),
			replace_pair(Pairs0, Key, Pair, Pairs)
		;	make_pair(Key, Values, Pair),
			Pairs = [Pair| Pairs0]
		),
		set_graph_node(GraphName, Id, Pairs, NodeMap0, NodeMap).

	append_unique_values([], Values, Values).
	append_unique_values([Value| Values], Values0, Values2) :-
		(	contains_value(Values0, Value) ->
			Values1 = Values0
		;	append(Values0, [Value], Values1)
		),
		append_unique_values(Values, Values1, Values2).

	contains_value([Value0| _], Value) :-
		Value0 == Value,
		!.
	contains_value([_| Values], Value) :-
		contains_value(Values, Value).

	replace_pair([], _Key, _Pair, []).
	replace_pair([OldPair| Pairs], Key, Pair, [Pair| Pairs]) :-
		pair_key_value(OldPair, OldKey, _),
		same_key(OldKey, Key),
		!.
	replace_pair([OldPair| Pairs], Key, Pair, [OldPair| ReplacedPairs]) :-
		replace_pair(Pairs, Key, Pair, ReplacedPairs).

	set_graph_node(GraphName, Id, Pairs, NodeMap0, NodeMap) :-
		get_graph_nodes(GraphName, NodeMap0, Nodes),
		(	select_node(Id, Nodes, _OldPairs, OtherNodes) ->
			NewNodes = [node(Id, Pairs)| OtherNodes]
		;	NewNodes = [node(Id, Pairs)| Nodes]
		),
		set_graph_nodes(GraphName, NewNodes, NodeMap0, NodeMap).

	get_graph_nodes(GraphName, [graph(GraphName0, Nodes)| _], Nodes) :-
		GraphName0 == GraphName,
		!.
	get_graph_nodes(GraphName, [_| Graphs], Nodes) :-
		get_graph_nodes(GraphName, Graphs, Nodes),
		!.
	get_graph_nodes(_GraphName, [], []).

	set_graph_nodes(GraphName, Nodes, [], [graph(GraphName, Nodes)]) :-
		!.
	set_graph_nodes(GraphName, Nodes, [graph(GraphName0, _)| Graphs], [graph(GraphName, Nodes)| Graphs]) :-
		GraphName0 == GraphName,
		!.
	set_graph_nodes(GraphName, Nodes, [Graph| Graphs], [Graph| NewGraphs]) :-
		set_graph_nodes(GraphName, Nodes, Graphs, NewGraphs).

	get_node_pairs(Id, [node(Id0, Pairs)| _], Pairs) :-
		Id0 == Id,
		!.
	get_node_pairs(Id, [_| Nodes], Pairs) :-
		get_node_pairs(Id, Nodes, Pairs).

	select_node(Id, [node(Id0, Pairs)| Nodes], Pairs, Nodes) :-
		Id0 == Id,
		!.
	select_node(Id, [Node| Nodes], Pairs, [Node| OtherNodes]) :-
		select_node(Id, Nodes, Pairs, OtherNodes).

	flatten_node_map(NodeMap, Flattened) :-
		get_graph_nodes('@default', NodeMap, DefaultNodes),
		named_graph_nodes(NodeMap, NamedGraphNodes),
		merge_named_graph_nodes(NamedGraphNodes, DefaultNodes, Nodes),
		nodes_to_objects(Nodes, Flattened).

	merge_named_graph_nodes([], Nodes, Nodes).
	merge_named_graph_nodes([node(Id, GraphPairs)| GraphNodes], Nodes0, Nodes) :-
		(	select_node(Id, Nodes0, Pairs0, OtherNodes) ->
			merge_flatten_node_pairs(GraphPairs, Pairs0, Pairs),
			Nodes1 = [node(Id, Pairs)| OtherNodes]
		;	Nodes1 = [node(Id, GraphPairs)| Nodes0]
		),
		merge_named_graph_nodes(GraphNodes, Nodes1, Nodes).

	merge_flatten_node_pairs([], Pairs, Pairs).
	merge_flatten_node_pairs([Pair| Pairs], Pairs0, MergedPairs) :-
		pair_key_value(Pair, Key, Value),
		(	get_pair_value(Key, Pairs0, CurrentValue) ->
			(	CurrentValue == Value ->
				Pairs1 = Pairs0
			;	replace_pair(Pairs0, Key, Pair, Pairs1)
			)
		;	Pairs1 = [Pair| Pairs0]
		),
		merge_flatten_node_pairs(Pairs, Pairs1, MergedPairs).

	named_graph_nodes([], []).
	named_graph_nodes([graph(GraphName, _)| Graphs], Nodes) :-
		GraphName == '@default',
		!,
		named_graph_nodes(Graphs, Nodes).
	named_graph_nodes([graph(GraphName, GraphNodes)| Graphs], [node(GraphName, Pairs)| Nodes]) :-
		make_pair('@id', GraphName, IdPair),
		nodes_to_objects(GraphNodes, GraphObjects),
		make_pair('@graph', GraphObjects, GraphPair),
		Pairs = [IdPair, GraphPair],
		named_graph_nodes(Graphs, Nodes).

	nodes_to_objects([], []).
	nodes_to_objects([node(_Id, Pairs)| Nodes], [Object| Objects]) :-
		reverse(Pairs, OrderedPairs),
		pairs_to_object(OrderedPairs, Object),
		nodes_to_objects(Nodes, Objects).

	% ==================== Framing ====================

	normalize_frame(Frame, ActiveContext, ExpandedFrame) :-
		(	is_object(Frame) ->
			object_to_pairs(Frame, Pairs),
			normalize_frame_pairs(Pairs, ActiveContext, ExpandedPairs),
			pairs_to_object(ExpandedPairs, ExpandedFrame)
		;	pairs_to_object([], ExpandedFrame)
		).

	normalize_frame_pairs([], _ActiveContext, []).
	normalize_frame_pairs([Pair| Pairs], ActiveContext, ExpandedPairs) :-
		pair_key_value(Pair, Key0, Value),
		normalize_key(Key0, Key),
		(	Key == '@context' ->
			ExpandedPairs = RestPairs
		;	Key == '@id' ->
			normalize_frame_id(Value, ActiveContext, ExpandedValue),
			make_pair('@id', ExpandedValue, ExpandedPair),
			ExpandedPairs = [ExpandedPair| RestPairs]
		;	Key == '@type' ->
			normalize_frame_types(Value, ActiveContext, ExpandedValue),
			make_pair('@type', ExpandedValue, ExpandedPair),
			ExpandedPairs = [ExpandedPair| RestPairs]
		;	Key == '@reverse' ->
			normalize_frame_reverse(Value, ActiveContext, ExpandedValue),
			make_pair('@reverse', ExpandedValue, ExpandedPair),
			ExpandedPairs = [ExpandedPair| RestPairs]
		;	is_framing_keyword(Key) ->
			make_pair(Key, Value, ExpandedPair),
			ExpandedPairs = [ExpandedPair| RestPairs]
		;	expand_iri(Key, ActiveContext, vocab_relative, ExpandedKey),
			normalize_frame_value(Value, ActiveContext, ExpandedValue),
			make_pair(ExpandedKey, ExpandedValue, ExpandedPair),
			ExpandedPairs = [ExpandedPair| RestPairs]
		),
		normalize_frame_pairs(Pairs, ActiveContext, RestPairs).

	normalize_frame_id([], _ActiveContext, []) :-
		!.
	normalize_frame_id([Id| Ids], ActiveContext, [ExpandedId| ExpandedIds]) :-
		!,
		expand_iri(Id, ActiveContext, document_relative, ExpandedId),
		normalize_frame_id(Ids, ActiveContext, ExpandedIds).
	normalize_frame_id(Id, ActiveContext, ExpandedId) :-
		expand_iri(Id, ActiveContext, document_relative, ExpandedId).

	normalize_frame_types([], _ActiveContext, []) :-
		!.
	normalize_frame_types([Type| Types], ActiveContext, [ExpandedType| ExpandedTypes]) :-
		!,
		expand_iri(Type, ActiveContext, vocab_relative, ExpandedType),
		normalize_frame_types(Types, ActiveContext, ExpandedTypes).
	normalize_frame_types(Type, ActiveContext, [ExpandedType]) :-
		expand_iri(Type, ActiveContext, vocab_relative, ExpandedType).

	normalize_frame_reverse(Value, ActiveContext, ExpandedReverse) :-
		is_object(Value),
		!,
		object_to_pairs(Value, Pairs),
		normalize_frame_reverse_pairs(Pairs, ActiveContext, ExpandedPairs),
		pairs_to_object(ExpandedPairs, ExpandedReverse).
	normalize_frame_reverse(Value, _ActiveContext, Value).

	normalize_frame_reverse_pairs([], _ActiveContext, []).
	normalize_frame_reverse_pairs([Pair| Pairs], ActiveContext, [ExpandedPair| ExpandedPairs]) :-
		pair_key_value(Pair, Key0, Value),
		normalize_key(Key0, Key),
		expand_iri(Key, ActiveContext, vocab_relative, ExpandedKey),
		normalize_frame_value(Value, ActiveContext, ExpandedValue),
		make_pair(ExpandedKey, ExpandedValue, ExpandedPair),
		normalize_frame_reverse_pairs(Pairs, ActiveContext, ExpandedPairs).

	normalize_frame_value([], _ActiveContext, []) :-
		!.
	normalize_frame_value([Value| Values], ActiveContext, [ExpandedValue| ExpandedValues]) :-
		!,
		normalize_frame_value(Value, ActiveContext, ExpandedValue),
		normalize_frame_value(Values, ActiveContext, ExpandedValues).
	normalize_frame_value(Value, ActiveContext, ExpandedValue) :-
		is_object(Value),
		!,
		normalize_frame(Value, ActiveContext, ExpandedValue).
	normalize_frame_value(Value, _ActiveContext, Value).

	frame_nodes([], _Frame, _AllNodes, _Embedded, []).
	frame_nodes([Node| Nodes], Frame, AllNodes, Embedded, FramedNodes) :-
		(	frame_matches(Node, Frame) ->
			frame_node(Node, Frame, AllNodes, frame_options(@true, @false, @false, @false), Embedded, FramedNode),
			FramedNodes = [FramedNode| RestFramedNodes]
		;	FramedNodes = RestFramedNodes
		),
		frame_nodes(Nodes, Frame, AllNodes, Embedded, RestFramedNodes).

	frame_matches(Node, Frame) :-
		object_to_pairs(Node, NodePairs),
		object_to_pairs(Frame, FramePairs),
		framing_options(FramePairs, frame_options(@true, @false, @false, @false), frame_options(_Embed, _Explicit, RequireAll, _OmitDefault)),
		frame_id_matches(NodePairs, FramePairs),
		frame_type_matches(NodePairs, FramePairs),
		frame_required_properties_match(RequireAll, NodePairs, FramePairs).

	frame_id_matches(_NodePairs, FramePairs) :-
		\+ get_pair_value('@id', FramePairs, _),
		!.
	frame_id_matches(NodePairs, FramePairs) :-
		get_pair_value('@id', NodePairs, NodeId),
		get_pair_value('@id', FramePairs, FrameId),
		frame_id_value_matches(FrameId, NodeId).

	frame_id_value_matches([], _NodeId) :-
		!,
		fail.
	frame_id_value_matches([FrameId| _], NodeId) :-
		FrameId == NodeId,
		!.
	frame_id_value_matches([_| FrameIds], NodeId) :-
		!,
		frame_id_value_matches(FrameIds, NodeId).
	frame_id_value_matches(FrameId, NodeId) :-
		FrameId == NodeId.

	frame_type_matches(_NodePairs, FramePairs) :-
		(	\+ get_pair_value('@type', FramePairs, _)
		;	get_pair_value('@type', FramePairs, [])
		),
		!.
	frame_type_matches(NodePairs, FramePairs) :-
		get_pair_value('@type', NodePairs, NodeTypes),
		get_pair_value('@type', FramePairs, FrameTypes),
		frame_type_value_matches(FrameTypes, NodeTypes).

	frame_type_value_matches([FrameType| _], NodeTypes) :-
		contains_value(NodeTypes, FrameType),
		!.
	frame_type_value_matches([_| FrameTypes], NodeTypes) :-
		frame_type_value_matches(FrameTypes, NodeTypes).

	frame_required_properties_match(@false, _NodePairs, _FramePairs) :-
		!.
	frame_required_properties_match(_RequireAll, NodePairs, FramePairs) :-
		frame_required_properties_match(FramePairs, NodePairs).

	frame_required_properties_match([], _NodePairs).
	frame_required_properties_match([FramePair| FramePairs], NodePairs) :-
		(	frame_data_property_pair(FramePair) ->
			pair_key_value(FramePair, Key, _),
			get_pair_value(Key, NodePairs, _)
		;	true
		),
		frame_required_properties_match(FramePairs, NodePairs).

	frame_node(Node, Frame, AllNodes, ParentOptions, Embedded0, FramedNode) :-
		object_to_pairs(Node, NodePairs),
		object_to_pairs(Frame, FramePairs),
		framing_options(FramePairs, ParentOptions, Options),
		Options = frame_options(_Embed, Explicit, _RequireAll, OmitDefault),
		frame_node_id(NodePairs, IdPairs, Embedded0, Embedded),
		frame_node_type(NodePairs, FramePairs, Explicit, TypePairs),
		frame_node_properties(NodePairs, FramePairs, AllNodes, Options, Embedded, PropertyPairs),
		frame_missing_defaults(FramePairs, NodePairs, OmitDefault, DefaultPairs),
		frame_reverse_properties(FramePairs, NodePairs, AllNodes, Options, Embedded, ReversePairs),
		append(IdPairs, TypePairs, Pairs0),
		append(Pairs0, PropertyPairs, Pairs1),
		append(Pairs1, DefaultPairs, Pairs2),
		append(Pairs2, ReversePairs, Pairs),
		pairs_to_object(Pairs, FramedNode).

	frame_node_id(NodePairs, [IdPair], Embedded0, Embedded) :-
		get_pair_value('@id', NodePairs, Id),
		!,
		make_pair('@id', Id, IdPair),
		(	contains_value(Embedded0, Id) ->
			Embedded = Embedded0
		;	Embedded = [Id| Embedded0]
		).
	frame_node_id(_NodePairs, [], Embedded, Embedded).

	frame_node_type(NodePairs, FramePairs, Explicit, [TypePair]) :-
		get_pair_value('@type', NodePairs, Types),
		(	Explicit == @false
		;	get_pair_value('@type', FramePairs, _)
		),
		!,
		make_pair('@type', Types, TypePair).
	frame_node_type(_NodePairs, _FramePairs, _Explicit, []).

	frame_node_properties([], _FramePairs, _AllNodes, _Options, _Embedded, []).
	frame_node_properties([Pair| Pairs], FramePairs, AllNodes, Options, Embedded, FramedPairs) :-
		pair_key_value(Pair, Key, Values),
		(	frame_include_node_pair(Key, FramePairs, Options) ->
			frame_property_frame(FramePairs, Key, PropertyFrame),
			frame_property_values(Values, PropertyFrame, AllNodes, Options, Embedded, FramedValues),
			make_pair(Key, FramedValues, FramedPair),
			FramedPairs = [FramedPair| RestFramedPairs]
		;	FramedPairs = RestFramedPairs
		),
		frame_node_properties(Pairs, FramePairs, AllNodes, Options, Embedded, RestFramedPairs).

	frame_include_node_pair('@id', _FramePairs, _Options) :-
		!,
		fail.
	frame_include_node_pair('@type', _FramePairs, _Options) :-
		!,
		fail.
	frame_include_node_pair('@reverse', _FramePairs, _Options) :-
		!,
		fail.
	frame_include_node_pair(Key, _FramePairs, frame_options(_Embed, Explicit, _RequireAll, _OmitDefault)) :-
		(	Explicit == @false
		),
		!,
		\+ is_framing_keyword(Key).
	frame_include_node_pair(Key, FramePairs, frame_options(_Embed, _Explicit, _RequireAll, _OmitDefault)) :-
		get_pair_value(Key, FramePairs, _).

	frame_property_values([], _PropertyFrame, _AllNodes, _Options, _Embedded, []) :-
		!.
	frame_property_values([Value| Values], PropertyFrame, AllNodes, Options, Embedded, [FramedValue| FramedValues]) :-
		!,
		frame_property_value(Value, PropertyFrame, AllNodes, Options, Embedded, FramedValue),
		frame_property_values(Values, PropertyFrame, AllNodes, Options, Embedded, FramedValues).
	frame_property_values(Value, PropertyFrame, AllNodes, Options, Embedded, FramedValue) :-
		frame_property_value(Value, PropertyFrame, AllNodes, Options, Embedded, FramedValue).

	frame_property_value(Value, PropertyFrame, AllNodes, Options, Embedded, FramedValue) :-
		is_object(Value),
		object_to_pairs(Value, ValuePairs),
		get_pair_value('@list', ValuePairs, ListValues),
		!,
		frame_list_frame(PropertyFrame, ListFrame),
		frame_effective_options(ListFrame, Options, ListOptions),
		frame_property_values(ListValues, ListFrame, AllNodes, ListOptions, Embedded, FramedListValues),
		make_pair('@list', FramedListValues, ListPair),
		pairs_to_object([ListPair], FramedValue).
	frame_property_value(Value, PropertyFrame, AllNodes, Options, Embedded, FramedValue) :-
		is_object(Value),
		object_to_pairs(Value, ValuePairs),
		get_pair_value('@id', ValuePairs, Id),
		\+ get_pair_value('@value', ValuePairs, _),
		!,
		frame_effective_options(PropertyFrame, Options, EffectiveOptions),
		EffectiveOptions = frame_options(Embed, _Explicit, _RequireAll, _OmitDefault),
		( (Embed == @false ; contains_value(Embedded, Id) ; \+ node_by_id(AllNodes, Id, _)) ->
			node_reference_object(Id, FramedValue)
		;	node_by_id(AllNodes, Id, Node),
			frame_node(Node, PropertyFrame, AllNodes, EffectiveOptions, Embedded, FramedValue)
		).
	frame_property_value(Value, _PropertyFrame, _AllNodes, _Options, _Embedded, Value).

	frame_effective_options(Frame, ParentOptions, Options) :-
		is_object(Frame),
		!,
		object_to_pairs(Frame, FramePairs),
		framing_options(FramePairs, ParentOptions, Options).
	frame_effective_options(_Frame, Options, Options).

	frame_property_frame(FramePairs, Key, PropertyFrame) :-
		get_pair_value(Key, FramePairs, Value),
		!,
		frame_value_frame(Value, PropertyFrame).
	frame_property_frame(_FramePairs, _Key, PropertyFrame) :-
		pairs_to_object([], PropertyFrame).

	frame_value_frame([Value| _], Frame) :-
		!,
		frame_value_frame(Value, Frame).
	frame_value_frame(Value, Value) :-
		is_object(Value),
		!.
	frame_value_frame(_Value, Frame) :-
		pairs_to_object([], Frame).

	frame_list_frame(PropertyFrame, ListFrame) :-
		is_object(PropertyFrame),
		object_to_pairs(PropertyFrame, Pairs),
		get_pair_value('@list', Pairs, Value),
		!,
		frame_value_frame(Value, ListFrame).
	frame_list_frame(PropertyFrame, PropertyFrame).

	frame_missing_defaults(_FramePairs, _NodePairs, @true, []) :-
		!.
	frame_missing_defaults(FramePairs, NodePairs, _OmitDefault, DefaultPairs) :-
		frame_missing_defaults_acc(FramePairs, NodePairs, [], DefaultPairs).

	frame_missing_defaults_acc([], _NodePairs, Defaults, Defaults).
	frame_missing_defaults_acc([FramePair| FramePairs], NodePairs, Defaults0, Defaults) :-
		(	frame_data_property_pair(FramePair),
			pair_key_value(FramePair, Key, Value),
			\+ get_pair_value(Key, NodePairs, _),
			frame_value_frame(Value, PropertyFrame),
			is_object(PropertyFrame),
			object_to_pairs(PropertyFrame, PropertyFramePairs),
			get_pair_value('@default', PropertyFramePairs, DefaultValue) ->
			make_pair(Key, [DefaultValue], DefaultPair),
			Defaults1 = [DefaultPair| Defaults0]
		;	Defaults1 = Defaults0
		),
		frame_missing_defaults_acc(FramePairs, NodePairs, Defaults1, Defaults).

	frame_reverse_properties(FramePairs, NodePairs, AllNodes, Options, Embedded, ReversePairs) :-
		get_pair_value('@reverse', FramePairs, ReverseFrame),
		get_pair_value('@id', NodePairs, Id),
		is_object(ReverseFrame),
		!,
		object_to_pairs(ReverseFrame, ReverseFramePairs),
		frame_reverse_property_pairs(ReverseFramePairs, Id, AllNodes, Options, Embedded, ReverseObjectPairs),
		(	ReverseObjectPairs == [] ->
			ReversePairs = []
		;	pairs_to_object(ReverseObjectPairs, ReverseObject),
			make_pair('@reverse', ReverseObject, ReversePair),
			ReversePairs = [ReversePair]
		).
	frame_reverse_properties(_FramePairs, _NodePairs, _AllNodes, _Options, _Embedded, []).

	frame_reverse_property_pairs([], _Id, _AllNodes, _Options, _Embedded, []).
	frame_reverse_property_pairs([ReverseFramePair| ReverseFramePairs], Id, AllNodes, Options, Embedded, ReverseObjectPairs) :-
		pair_key_value(ReverseFramePair, Key, Value),
		frame_value_frame(Value, PropertyFrame),
		frame_reverse_property_values(AllNodes, AllNodes, Id, Key, PropertyFrame, Options, Embedded, Values),
		(	Values == [] ->
			ReverseObjectPairs = RestReverseObjectPairs
		;	make_pair(Key, Values, ReverseObjectPair),
			ReverseObjectPairs = [ReverseObjectPair| RestReverseObjectPairs]
		),
		frame_reverse_property_pairs(ReverseFramePairs, Id, AllNodes, Options, Embedded, RestReverseObjectPairs).

	frame_reverse_property_values([], _AllNodes, _Id, _Key, _PropertyFrame, _Options, _Embedded, []).
	frame_reverse_property_values([Node| Nodes], AllNodes, Id, Key, PropertyFrame, Options, Embedded, Values) :-
		object_to_pairs(Node, NodePairs),
		(	get_pair_value('@id', NodePairs, NodeId),
			NodeId \== Id,
			get_pair_value(Key, NodePairs, PropertyValues),
			frame_values_reference_id(PropertyValues, Id) ->
			frame_node(Node, PropertyFrame, AllNodes, Options, Embedded, FramedNode),
			Values = [FramedNode| RestValues]
		;	Values = RestValues
		),
		frame_reverse_property_values(Nodes, AllNodes, Id, Key, PropertyFrame, Options, Embedded, RestValues).

	frame_values_reference_id([Value| _], Id) :-
		frame_value_references_id(Value, Id),
		!.
	frame_values_reference_id([_| Values], Id) :-
		!,
		frame_values_reference_id(Values, Id).
	frame_values_reference_id(Value, Id) :-
		frame_value_references_id(Value, Id).

	frame_value_references_id(Value, Id) :-
		is_object(Value),
		object_to_pairs(Value, Pairs),
		get_pair_value('@id', Pairs, RefId),
		RefId == Id.

	node_by_id([Node| _], Id, Node) :-
		object_to_pairs(Node, Pairs),
		get_pair_value('@id', Pairs, NodeId),
		NodeId == Id,
		!.
	node_by_id([_| Nodes], Id, Node) :-
		node_by_id(Nodes, Id, Node).

	framing_options(FramePairs, frame_options(ParentEmbed, ParentExplicit, ParentRequireAll, ParentOmitDefault), frame_options(Embed, Explicit, RequireAll, OmitDefault)) :-
		framing_boolean_option('@embed', FramePairs, ParentEmbed, Embed),
		framing_boolean_option('@explicit', FramePairs, ParentExplicit, Explicit),
		framing_boolean_option('@requireAll', FramePairs, ParentRequireAll, RequireAll),
		framing_boolean_option('@omitDefault', FramePairs, ParentOmitDefault, OmitDefault).

	framing_boolean_option(Key, FramePairs, _Default, Value) :-
		get_pair_value(Key, FramePairs, Option),
		!,
		( 	Option == @true ->
			Value = @true
		; 	Option == @false ->
			Value = @false
		; 	domain_error(json_ld_framing_boolean, Option)
		),
		!.
	framing_boolean_option(_Key, _FramePairs, Default, Default).

	frame_data_property_pair(Pair) :-
		pair_key_value(Pair, Key, _Value),
		\+ is_keyword(Key),
		\+ is_framing_keyword(Key).

	is_framing_keyword('@embed').
	is_framing_keyword('@explicit').
	is_framing_keyword('@requireAll').
	is_framing_keyword('@omitDefault').
	is_framing_keyword('@default').

	frame_output(Frame, ActiveContext, FramedExpanded, Framed) :-
		frame_context(Frame, Context),
		!,
		compact_document(FramedExpanded, ActiveContext, Compacted),
		add_frame_context(Context, Compacted, Framed).
	frame_output(_Frame, _ActiveContext, Framed, Framed).

	frame_context(Frame, Context) :-
		is_object(Frame),
		object_to_pairs(Frame, Pairs),
		get_pair_value('@context', Pairs, Context).

	add_frame_context(Context, [Object], Framed) :-
		is_object(Object),
		!,
		object_to_pairs(Object, Pairs),
		make_pair('@context', Context, ContextPair),
		pairs_to_object([ContextPair| Pairs], Framed).
	add_frame_context(Context, Graph, Framed) :-
		make_pair('@context', Context, ContextPair),
		make_pair('@graph', Graph, GraphPair),
		pairs_to_object([ContextPair, GraphPair], Framed).

	% ==================== Expansion ====================

	% Top-level expansion: a JSON-LD document can be an array or an object
	expand_document([], _ActiveContext, []) :- !.
	expand_document(List, ActiveContext, Expanded) :-
		is_list(List),
		!,
		expand_array_elements(List, ActiveContext, Expanded).
	expand_document(Object, ActiveContext, Expanded) :-
		is_object(Object),
		!,
		object_to_pairs(Object, Pairs),
		% Extract @context if present
		(	get_pair_value('@context', Pairs, ContextValue) ->
			process_local_context(ContextValue, ActiveContext, NewContext)
		;	NewContext = ActiveContext
		),
		expand_node_object(Pairs, NewContext, ExpandedNode),
		(	ExpandedNode == [] ->
			Expanded = []
		;	Expanded = [ExpandedNode]
		).
	expand_document(Value, _ActiveContext, [Value]) :- !.

	expand_array_elements([], _ActiveContext, []).
	expand_array_elements([Element| Elements], ActiveContext, Expanded) :-
		expand_document(Element, ActiveContext, ExpandedElement),
		append(ExpandedElement, ExpandedRest, Expanded),
		expand_array_elements(Elements, ActiveContext, ExpandedRest).

	% Expand a node object (list of pairs)
	expand_node_object([], _ActiveContext, []) :- !.
	expand_node_object(Pairs, ActiveContext, ExpandedObject) :-
		expand_pairs(Pairs, ActiveContext, ExpandedPairs0),
		remove_empty_pairs(ExpandedPairs0, ExpandedPairs),
		(	ExpandedPairs == [] ->
			ExpandedObject = []
		;	pairs_to_object(ExpandedPairs, ExpandedObject)
		).

	expand_pairs([], _ActiveContext, []).
	expand_pairs([Pair| Pairs], ActiveContext, ExpandedPairs) :-
		pair_key_value(Pair, Key0, Value),
		normalize_key(Key0, Key),
		expand_pair(Key, Value, ActiveContext, ExpandedPair),
		append(ExpandedPair, ExpandedRest, ExpandedPairs),
		expand_pairs(Pairs, ActiveContext, ExpandedRest).

	% Skip @context during expansion (already processed)
	expand_pair('@context', _Value, _ActiveContext, []) :- !.

	% Expand @id
	expand_pair('@id', Value, ActiveContext, [ExpandedPair]) :-
		!,
		expand_iri(Value, ActiveContext, document_relative, ExpandedIRI),
		make_pair('@id', ExpandedIRI, ExpandedPair).

	% Expand @type
	expand_pair('@type', Value, ActiveContext, [ExpandedPair]) :-
		!,
		(	is_list(Value) ->
			expand_type_list(Value, ActiveContext, ExpandedTypes),
			make_pair('@type', ExpandedTypes, ExpandedPair)
		;	expand_iri(Value, ActiveContext, vocab_relative, ExpandedType),
			make_pair('@type', [ExpandedType], ExpandedPair)
		).

	% Expand @value
	expand_pair('@value', Value, _ActiveContext, [ExpandedPair]) :-
		!,
		make_pair('@value', Value, ExpandedPair).

	% Expand @language
	expand_pair('@language', Value, _ActiveContext, [ExpandedPair]) :-
		!,
		downcase_atom_if_possible(Value, LoweredValue),
		make_pair('@language', LoweredValue, ExpandedPair).

	% Expand @direction
	expand_pair('@direction', Value, _ActiveContext, [ExpandedPair]) :-
		!,
		make_pair('@direction', Value, ExpandedPair).

	% Expand @graph
	expand_pair('@graph', Value, ActiveContext, [ExpandedPair]) :-
		!,
		(	is_list(Value) ->
			expand_array_elements(Value, ActiveContext, ExpandedValue)
		;	expand_document(Value, ActiveContext, ExpandedValue)
		),
		make_pair('@graph', ExpandedValue, ExpandedPair).

	% Expand @reverse
	expand_pair('@reverse', Value, ActiveContext, [ExpandedPair]) :-
		!,
		(	is_object(Value) ->
			object_to_pairs(Value, ReversePairs),
			expand_node_object(ReversePairs, ActiveContext, ExpandedReverse),
			make_pair('@reverse', ExpandedReverse, ExpandedPair)
		;	make_pair('@reverse', Value, ExpandedPair)
		).

	% Expand @list
	expand_pair('@list', Value, ActiveContext, [ExpandedPair]) :-
		!,
		(	is_list(Value) ->
			expand_list_elements_strict(Value, ActiveContext, ExpandedElements)
		;	expand_value(Value, ActiveContext, none, ExpandedValue),
			(	ExpandedValue == [] ->
				ExpandedElements = []
			;	ExpandedElements = [ExpandedValue]
			)
		),
		make_pair('@list', ExpandedElements, ExpandedPair).

	% Expand @set
	expand_pair('@set', Value, ActiveContext, [ExpandedPair]) :-
		!,
		(	is_list(Value) ->
			expand_list_elements(Value, ActiveContext, ExpandedValues)
		;	expand_value(Value, ActiveContext, none, ExpandedValue),
			(	ExpandedValue == [] ->
				ExpandedValues = []
			;	ExpandedValues = [ExpandedValue]
			)
		),
		make_pair('@set', ExpandedValues, ExpandedPair).

	% Expand @included
	expand_pair('@included', Value, ActiveContext, [ExpandedPair]) :-
		!,
		(	is_list(Value) ->
			expand_array_elements(Value, ActiveContext, ExpandedInc)
		;	expand_document(Value, ActiveContext, ExpandedInc)
		),
		make_pair('@included', ExpandedInc, ExpandedPair).

	% Expand @index (preserve as-is)
	expand_pair('@index', Value, _ActiveContext, [ExpandedPair]) :-
		!,
		make_pair('@index', Value, ExpandedPair).

	% Expand @nest
	expand_pair('@nest', Value, ActiveContext, ExpandedPairs) :-
		!,
		expand_nest_value(Value, ActiveContext, ExpandedPairs).

	% Expand regular properties
	expand_pair(Key, Value, ActiveContext, ExpandedPairs) :-
		expand_iri(Key, ActiveContext, vocab_relative, ExpandedKey),
		(	ExpandedKey == Key,
			\+ is_absolute_iri(ExpandedKey),
			\+ is_keyword(ExpandedKey) ->
			% Property not expanded and not an IRI - skip
			ExpandedPairs = []
		;	% Pass original Key for term definition lookup
			expand_property_value(Value, ActiveContext, Key, ExpandedKey, ExpandedValue),
			make_pair(ExpandedKey, ExpandedValue, ExpandedPair),
			ExpandedPairs = [ExpandedPair]
		).

	% expand_property_value/5: Term is original term name for definition lookup
	expand_property_value(Value, ActiveContext, Term, _ExpandedKey, ExpandedValue) :-
		is_list(Value),
		!,
		expand_list_elements(Value, ActiveContext, Term, ExpandedValue).
	expand_property_value(Value, ActiveContext, Term, _ExpandedKey, ExpandedValue) :-
		is_object(Value),
		!,
		expand_document(Value, ActiveContext, ExpandedValue0),
		check_type_coercion(Term, ActiveContext, ExpandedValue0, ExpandedValue).
	expand_property_value(Value, ActiveContext, Term, _ExpandedKey, ExpandedValue) :-
		expand_value(Value, ActiveContext, Term, ExpandedValue0),
		(	is_list(ExpandedValue0) ->
			ExpandedValue = ExpandedValue0
		;	ExpandedValue = [ExpandedValue0]
		).

	check_type_coercion(Term, ActiveContext, ExpandedValue0, ExpandedValue) :-
		(	get_term_definition(ActiveContext, Term, TermDef),
			get_pair_value('@type', TermDef, TypeCoercion),
			same_key(TypeCoercion, '@id') ->
			% Values should be treated as IRIs
			ExpandedValue = ExpandedValue0
		;	ExpandedValue = ExpandedValue0
		).

	% expand_list_elements/3: no term context (standalone lists)
	expand_list_elements([], _ActiveContext, []).
	expand_list_elements([Element| Elements], ActiveContext, Expanded) :-
		(	is_object(Element) ->
			expand_document(Element, ActiveContext, ExpandedElement),
			expand_list_elements(Elements, ActiveContext, ExpandedRest),
			append(ExpandedElement, ExpandedRest, Expanded)
		;	is_list(Element) ->
			expand_list_elements(Element, ActiveContext, ExpandedElement),
			expand_list_elements(Elements, ActiveContext, ExpandedRest),
			Expanded = [ExpandedElement| ExpandedRest]
		;	expand_value(Element, ActiveContext, none, ExpandedElement),
			expand_list_elements(Elements, ActiveContext, ExpandedRest),
			(	ExpandedElement == [] ->
				Expanded = ExpandedRest
			;	Expanded = [ExpandedElement| ExpandedRest]
			)
		).

	expand_list_elements_strict([], _ActiveContext, []).
	expand_list_elements_strict([Element| Elements], ActiveContext, Expanded) :-
		(	is_list(Element) ->
			domain_error(json_ld_list_of_lists, Element)
		;	is_object(Element) ->
			expand_document(Element, ActiveContext, ExpandedElement),
			expand_list_elements_strict(Elements, ActiveContext, ExpandedRest),
			append(ExpandedElement, ExpandedRest, Expanded)
		;	expand_value(Element, ActiveContext, none, ExpandedElement),
			expand_list_elements_strict(Elements, ActiveContext, ExpandedRest),
			(	ExpandedElement == [] ->
				Expanded = ExpandedRest
			;	Expanded = [ExpandedElement| ExpandedRest]
			)
		).

	expand_nest_value(Value, ActiveContext, ExpandedPairs) :-
		is_object(Value),
		!,
		object_to_pairs(Value, Pairs),
		expand_pairs(Pairs, ActiveContext, ExpandedPairs0),
		remove_empty_pairs(ExpandedPairs0, ExpandedPairs).
	expand_nest_value(Value, ActiveContext, ExpandedPairs) :-
		is_list(Value),
		!,
		expand_nest_list(Value, ActiveContext, ExpandedPairs).
	expand_nest_value(Value, _ActiveContext, _ExpandedPairs) :-
		domain_error(json_ld_nest_value, Value).

	expand_nest_list([], _ActiveContext, []).
	expand_nest_list([Value| Values], ActiveContext, ExpandedPairs) :-
		expand_nest_value(Value, ActiveContext, ExpandedPairs0),
		append(ExpandedPairs0, ExpandedPairs1, ExpandedPairs),
		expand_nest_list(Values, ActiveContext, ExpandedPairs1).

	% expand_list_elements/4: with term context for type coercion
	expand_list_elements([], _ActiveContext, _Term, []).
	expand_list_elements([Element| Elements], ActiveContext, Term, Expanded) :-
		(	is_object(Element) ->
			expand_document(Element, ActiveContext, ExpandedElement),
			expand_list_elements(Elements, ActiveContext, Term, ExpandedRest),
			append(ExpandedElement, ExpandedRest, Expanded)
		;	is_list(Element) ->
			expand_list_elements(Element, ActiveContext, Term, ExpandedElement),
			expand_list_elements(Elements, ActiveContext, Term, ExpandedRest),
			Expanded = [ExpandedElement| ExpandedRest]
		;	expand_value(Element, ActiveContext, Term, ExpandedElement),
			expand_list_elements(Elements, ActiveContext, Term, ExpandedRest),
			(	ExpandedElement == [] ->
				Expanded = ExpandedRest
			;	Expanded = [ExpandedElement| ExpandedRest]
			)
		).

	expand_type_list([], _ActiveContext, []).
	expand_type_list([Type| Types], ActiveContext, [ExpandedType| ExpandedTypes]) :-
		expand_iri(Type, ActiveContext, vocab_relative, ExpandedType),
		expand_type_list(Types, ActiveContext, ExpandedTypes).

	% Expand a scalar value
	expand_value(@null, _ActiveContext, _Key, []) :- !.
	expand_value(@true, _ActiveContext, _Key, ExpandedValue) :-
		!,
		make_value_object(@true, ExpandedValue).
	expand_value(@false, _ActiveContext, _Key, ExpandedValue) :-
		!,
		make_value_object(@false, ExpandedValue).
	expand_value(Value, _ActiveContext, _Key, ExpandedValue) :-
		number(Value),
		!,
		make_value_object(Value, ExpandedValue).
	expand_value(Value, ActiveContext, Key, ExpandedValue) :-
		json_ld_string_atom(Value, ValueAtom),
		!,
		(	Key \== none,
			get_term_definition(ActiveContext, Key, TermDef),
			get_pair_value('@type', TermDef, TypeCoercion0),
			json_ld_string_atom(TypeCoercion0, TypeCoercion) ->
			(	TypeCoercion == '@id' ->
				expand_iri(ValueAtom, ActiveContext, document_relative, ExpandedIRI),
				make_value_object_id(ExpandedIRI, ExpandedValue)
			;	TypeCoercion == '@vocab' ->
				expand_iri(ValueAtom, ActiveContext, vocab_relative, ExpandedIRI),
				make_value_object_id(ExpandedIRI, ExpandedValue)
			;	make_typed_value_object(Value, TypeCoercion, ExpandedValue)
			)
		;	% Check for default @language
			(	get_context_value(ActiveContext, '@language', DefaultLang) ->
				make_lang_value_object(Value, DefaultLang, ExpandedValue)
			;	make_value_object(Value, ExpandedValue)
			)
		).
	expand_value(Value, _ActiveContext, _Key, Value).

	make_value_object(Value, Object) :-
		make_pair('@value', Value, Pair),
		pairs_to_object([Pair], Object).

	make_value_object_id(IRI, Object) :-
		make_pair('@id', IRI, Pair),
		pairs_to_object([Pair], Object).

	make_typed_value_object(Value, Type, Object) :-
		make_pair('@value', Value, Pair1),
		make_pair('@type', Type, Pair2),
		pairs_to_object([Pair1, Pair2], Object).

	make_lang_value_object(Value, Lang, Object) :-
		make_pair('@value', Value, Pair1),
		make_pair('@language', Lang, Pair2),
		pairs_to_object([Pair1, Pair2], Object).

	% ==================== Context Processing ====================

	process_local_context(ContextValue, ActiveContext, NewContext) :-
		(	is_list(ContextValue) ->
			process_context_array(ContextValue, ActiveContext, NewContext)
		;	is_object(ContextValue) ->
			process_context_definition(ContextValue, ActiveContext, NewContext)
		;	atom(ContextValue) ->
			% Remote context reference - store as IRI
			% (fetching remote contexts is not supported in this implementation)
			NewContext = ActiveContext
		;	ContextValue == @null ->
			NewContext = []
		;	NewContext = ActiveContext
		).

	process_context_array([], ActiveContext, ActiveContext).
	process_context_array([Context| Contexts], ActiveContext, NewContext) :-
		process_local_context(Context, ActiveContext, Context1),
		process_context_array(Contexts, Context1, NewContext).

	process_context_definition(ContextObject, ActiveContext, NewContext) :-
		object_to_pairs(ContextObject, Pairs),
		process_context_entries(Pairs, ActiveContext, NewContext).

	process_context_entries([], ActiveContext, ActiveContext).
	process_context_entries([Pair| Pairs], ActiveContext, NewContext) :-
		pair_key_value(Pair, Key0, Value),
		normalize_key(Key0, Key),
		process_context_entry(Key, Value, ActiveContext, Context1),
		process_context_entries(Pairs, Context1, NewContext).

	process_context_entry('@base', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@base', NewContext)
		;	json_ld_string_atom(Value, Atom) ->
			set_context_value(ActiveContext, '@base', Atom, NewContext)
		;	domain_error(json_ld_context_base, Value)
		).
	process_context_entry('@vocab', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@vocab', NewContext)
		;	json_ld_string_atom(Value, Atom) ->
			set_context_value(ActiveContext, '@vocab', Atom, NewContext)
		;	domain_error(json_ld_context_vocab, Value)
		).
	process_context_entry('@language', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@language', NewContext)
		;	json_ld_string_atom(Value, Atom) ->
			downcase_atom(Atom, LoweredValue),
			set_context_value(ActiveContext, '@language', LoweredValue, NewContext)
		;	domain_error(json_ld_context_language, Value)
		).
	process_context_entry('@direction', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@direction', NewContext)
		;	(	json_ld_string_atom(Value, Direction),
				(	Direction == ltr
				;	Direction == rtl
				)
			) ->
			set_context_value(ActiveContext, '@direction', Direction, NewContext)
		;	domain_error(json_ld_context_direction, Value)
		).
	process_context_entry('@version', 1.1, ActiveContext, ActiveContext) :-
		!.
	process_context_entry('@version', Value, _ActiveContext, _NewContext) :-
		!,
		domain_error(json_ld_context_version, Value).
	process_context_entry('@import', Value, _ActiveContext, _NewContext) :-
		!,
		domain_error(json_ld_context_import, Value).
	process_context_entry('@propagate', Value, ActiveContext, ActiveContext) :-
		(	Value == @true
		;	Value == @false
		),
		!.
	process_context_entry('@propagate', Value, _ActiveContext, _NewContext) :-
		!,
		domain_error(json_ld_context_propagate, Value).
	process_context_entry('@protected', Value, ActiveContext, ActiveContext) :-
		(	Value == @true
		;	Value == @false
		),
		!.
	process_context_entry('@protected', Value, _ActiveContext, _NewContext) :-
		!,
		domain_error(json_ld_context_protected, Value).
	process_context_entry(Term0, Value, ActiveContext, NewContext) :-
		normalize_key(Term0, Term),
		% Term definition
		(	is_object(Value) ->
			% Expanded term definition
			object_to_pairs(Value, DefPairs),
			set_context_value(ActiveContext, term(Term), DefPairs, Context1),
			(	get_pair_value('@id', DefPairs, IRI0) ->
				(	json_ld_string_atom(IRI0, IRI) ->
					true
				;	IRI = IRI0
				),
				% Term maps to an IRI
				(	is_keyword(IRI) ->
					set_context_value(Context1, Term, IRI, NewContext)
				;	% Check if it's a compact IRI needing expansion
					expand_term_iri(IRI, ActiveContext, ExpandedIRI),
					set_context_value(Context1, Term, ExpandedIRI, NewContext)
				)
			;	% No @id - use @vocab + term or term as IRI prefix
				(	get_context_value(ActiveContext, '@vocab', Vocab) ->
					atom_concat(Vocab, Term, ExpandedIRI),
					set_context_value(Context1, Term, ExpandedIRI, NewContext)
				;	NewContext = Context1
				)
			)
		;	json_ld_string_atom(Value, ValueAtom) ->
			% Simple term definition: term -> IRI or keyword
			(	is_keyword(ValueAtom) ->
				set_context_value(ActiveContext, Term, ValueAtom, NewContext)
			;	expand_term_iri(ValueAtom, ActiveContext, ExpandedIRI),
				set_context_value(ActiveContext, Term, ExpandedIRI, NewContext)
			)
		;	Value == @null ->
			remove_context_value(ActiveContext, Term, NewContext)
		;	NewContext = ActiveContext
		).

	expand_term_iri(IRI0, ActiveContext, ExpandedIRI) :-
		json_ld_string_atom(IRI0, IRI),
		!,
		(	is_compact_iri(IRI) ->
			expand_compact_iri(IRI, ActiveContext, ExpandedIRI)
		;	is_absolute_iri(IRI) ->
			ExpandedIRI = IRI
		;	% Try to resolve against @vocab
			(	get_context_value(ActiveContext, '@vocab', Vocab) ->
				atom_concat(Vocab, IRI, ExpandedIRI)
			;	ExpandedIRI = IRI
			)
		).
	expand_term_iri(IRI, _ActiveContext, IRI).

	% ==================== IRI Expansion ====================

	expand_iri(Value, _ActiveContext, _Mode, Value) :-
		var(Value),
		!.
	expand_iri(Value, ActiveContext, Mode, ExpandedIRI) :-
		json_ld_string_atom(Value, Atom),
		!,
		expand_iri_atom(Atom, ActiveContext, Mode, ExpandedIRI).
	expand_iri(Value, _ActiveContext, _Mode, Value).

	expand_iri_atom(Value, _ActiveContext, _Mode, Value) :-
		is_keyword(Value),
		!.
	expand_iri_atom(Value, ActiveContext, _Mode, ExpandedIRI) :-
		get_context_value(ActiveContext, Value, Mapping),
		atom(Mapping),
		!,
		ExpandedIRI = Mapping.
	expand_iri_atom(Value, ActiveContext, _Mode, ExpandedIRI) :-
		is_compact_iri(Value),
		!,
		expand_compact_iri(Value, ActiveContext, ExpandedIRI).
	expand_iri_atom(Value, _ActiveContext, _Mode, Value) :-
		is_absolute_iri(Value),
		!.
	expand_iri_atom(Value, ActiveContext, vocab_relative, ExpandedIRI) :-
		get_context_value(ActiveContext, '@vocab', Vocab),
		!,
		atom_concat(Vocab, Value, ExpandedIRI).
	expand_iri_atom(Value, ActiveContext, document_relative, ExpandedIRI) :-
		get_context_value(ActiveContext, '@base', Base),
		!,
		resolve_relative_iri(Value, Base, ExpandedIRI).
	expand_iri_atom(Value, _ActiveContext, _Mode, Value).

	% Expand compact IRI (prefix:suffix)
	expand_compact_iri(CompactIRI, ActiveContext, ExpandedIRI) :-
		split_compact_iri(CompactIRI, Prefix, Suffix),
		(	Prefix == '_' ->
			% Blank node identifier
			ExpandedIRI = CompactIRI
		;	get_context_value(ActiveContext, Prefix, PrefixIRI),
			atom(PrefixIRI) ->
			atom_concat(PrefixIRI, Suffix, ExpandedIRI)
		;	ExpandedIRI = CompactIRI
		).

	% ==================== Compaction ====================

	build_context(ContextTerm, ActiveContext) :-
		(	is_object(ContextTerm) ->
			object_to_pairs(ContextTerm, Pairs),
			(	get_pair_value('@context', Pairs, ContextDef) ->
				process_local_context(ContextDef, [], ActiveContext)
			;	process_local_context(ContextTerm, [], ActiveContext)
			)
		;	ActiveContext = []
		).

	compact_document([], _ActiveContext, []) :- !.
	compact_document(List, ActiveContext, Compacted) :-
		is_list(List),
		!,
		compact_array(List, ActiveContext, Compacted).
	compact_document(Object, ActiveContext, Compacted) :-
		is_object(Object),
		!,
		object_to_pairs(Object, Pairs),
		compact_pairs(Pairs, ActiveContext, CompactedPairs0),
		remove_empty_pairs(CompactedPairs0, CompactedPairs),
		pairs_to_object(CompactedPairs, Compacted).
	compact_document(Value, _ActiveContext, Value).

	compact_array([], _ActiveContext, []).
	compact_array([Element| Elements], ActiveContext, [Compacted| CompactedRest]) :-
		compact_document(Element, ActiveContext, Compacted),
		compact_array(Elements, ActiveContext, CompactedRest).

	compact_pairs([], _ActiveContext, []).
	compact_pairs([Pair| Pairs], ActiveContext, [CompactedPair| CompactedRest]) :-
		pair_key_value(Pair, Key, Value),
		compact_key(Key, ActiveContext, CompactedKey),
		compact_value(Value, ActiveContext, CompactedValue),
		make_pair(CompactedKey, CompactedValue, CompactedPair),
		compact_pairs(Pairs, ActiveContext, CompactedRest).

	compact_key(Key, _ActiveContext, Key) :-
		is_keyword(Key),
		!.
	compact_key(Key, ActiveContext, CompactedKey) :-
		% Try to find a term that maps to this IRI
		(	find_term_for_iri(ActiveContext, Key, Term) ->
			CompactedKey = Term
		;	% Try to compact using vocab
			(	get_context_value(ActiveContext, '@vocab', Vocab),
				atom_concat(Vocab, LocalName, Key),
				LocalName \== '' ->
				CompactedKey = LocalName
			;	% Try to compact as prefix:suffix
				(	find_prefix_for_iri(ActiveContext, Key, Prefix, Suffix) ->
					atom_concat(Prefix, ':', Prefix0),
					atom_concat(Prefix0, Suffix, CompactedKey)
				;	CompactedKey = Key
				)
			)
		).

	compact_value(Value, ActiveContext, CompactedValue) :-
		is_list(Value),
		!,
		compact_value_array(Value, ActiveContext, CompactedValue).
	compact_value(Value, ActiveContext, CompactedValue) :-
		is_object(Value),
		!,
		compact_document(Value, ActiveContext, CompactedValue).
	compact_value(Value, _ActiveContext, Value).

	compact_value_array([], _ActiveContext, []).
	compact_value_array([Value], ActiveContext, CompactedValue) :-
		!,
		compact_value(Value, ActiveContext, CompactedValue0),
		% Unwrap single @value objects
		(	is_object(CompactedValue0),
			object_to_pairs(CompactedValue0, CVPairs),
			CVPairs = [SinglePair],
			pair_key_value(SinglePair, '@value', SimpleValue),
			\+ is_object(SimpleValue) ->
			CompactedValue = SimpleValue
		;	CompactedValue = CompactedValue0
		).
	compact_value_array(Values, ActiveContext, CompactedValues) :-
		compact_array(Values, ActiveContext, CompactedValues).

	find_term_for_iri([], _IRI, _Term) :- fail.
	find_term_for_iri([Entry| _], IRI, Term) :-
		pair_key_value(Entry, Key, Value),
		atom(Value),
		Value == IRI,
		atom(Key),
		\+ is_keyword(Key),
		Key \== '@base',
		Key \== '@vocab',
		Key \== '@language',
		Key \== '@direction',
		!,
		Term = Key.
	find_term_for_iri([_| Entries], IRI, Term) :-
		find_term_for_iri(Entries, IRI, Term).

	find_prefix_for_iri([], _IRI, _Prefix, _Suffix) :- fail.
	find_prefix_for_iri([Entry| _], IRI, Prefix, Suffix) :-
		pair_key_value(Entry, Key, Value),
		atom(Value),
		atom(Key),
		\+ is_keyword(Key),
		Key \== '@base',
		Key \== '@vocab',
		Key \== '@language',
		Key \== '@direction',
		atom_concat(Value, Suffix, IRI),
		Suffix \== '',
		Suffix \== IRI,
		!,
		Prefix = Key.
	find_prefix_for_iri([_| Entries], IRI, Prefix, Suffix) :-
		find_prefix_for_iri(Entries, IRI, Prefix, Suffix).

	% ==================== Utility predicates ====================

	% Object representation handling
	is_object({}) :- _ObjectRepresentation_ == curly, !.
	is_object({_}) :- _ObjectRepresentation_ == curly, !.
	is_object(json(_)) :- _ObjectRepresentation_ == list, !.

	object_to_pairs({}, []) :- _ObjectRepresentation_ == curly, !.
	object_to_pairs({Pairs}, PairsList) :-
		_ObjectRepresentation_ == curly,
		!,
		curly_pairs_to_list(Pairs, PairsList).
	object_to_pairs(json([]), []) :- _ObjectRepresentation_ == list, !.
	object_to_pairs(json(Pairs), Pairs) :- _ObjectRepresentation_ == list, !.

	pairs_to_object([], Object) :-
		(	_ObjectRepresentation_ == curly ->
			Object = {}
		;	Object = json([])
		),
		!.
	pairs_to_object(Pairs, Object) :-
		(	_ObjectRepresentation_ == curly ->
			list_to_curly_pairs(Pairs, CurlyPairs),
			Object = {CurlyPairs}
		;	Object = json(Pairs)
		),
		!.

	curly_pairs_to_list((Pair, Rest), [Pair| RestList]) :-
		!,
		curly_pairs_to_list(Rest, RestList).
	curly_pairs_to_list(Pair, [Pair]).

	list_to_curly_pairs([Pair], Pair) :- !.
	list_to_curly_pairs([Pair| Pairs], (Pair, Rest)) :-
		list_to_curly_pairs(Pairs, Rest).

	% Pair handling based on representation
	make_pair(Key, Value, Pair) :-
		(	_PairRepresentation_ == dash ->
			Pair = (Key-Value)
		;	_PairRepresentation_ == equal ->
			Pair = (Key=Value)
		;	Pair = ':'(Key, Value)
		).

	pair_key_value(Key-Value, Key, Value) :- _PairRepresentation_ == dash, !.
	pair_key_value(Key=Value, Key, Value) :- _PairRepresentation_ == equal, !.
	pair_key_value(':'(Key, Value), Key, Value) :- !.
	% Fallback for mixed representations (e.g. reading from JSON library)
	pair_key_value(Key-Value, Key, Value) :- !.
	pair_key_value(Key=Value, Key, Value) :- !.
	pair_key_value(':'(Key, Value), Key, Value) :- !.

	json_ld_string_atom(Value, Atom) :-
		atom(Value),
		!,
		Atom = Value.
	json_ld_string_atom(chars(Chars), Atom) :-
		!,
		atom_chars(Atom, Chars).
	json_ld_string_atom(codes(Codes), Atom) :-
		!,
		atom_codes(Atom, Codes).

	normalize_key(Key0, Key) :-
		(	json_ld_string_atom(Key0, Atom) ->
			Key = Atom
		;	Key = Key0
		).

	same_key(Key1, Key2) :-
		normalize_key(Key1, Key),
		normalize_key(Key2, Key).

	get_pair_value(Key, Pairs, Value) :-
		member(Pair, Pairs),
		pair_key_value(Pair, PairKey, Value),
		same_key(PairKey, Key),
		!.

	get_term_definition(ActiveContext, Key, TermDef) :-
		json_ld_string_atom(Key, Term),
		get_context_value(ActiveContext, term(Term), TermDef).

	% Context as association list
	get_context_value([Entry| _], Key, Value) :-
		pair_key_value(Entry, EntryKey, Value),
		same_key(EntryKey, Key),
		!.
	get_context_value([_| Entries], Key, Value) :-
		get_context_value(Entries, Key, Value).

	set_context_value([], Key, Value, [Pair]) :-
		normalize_key(Key, NormalizedKey),
		make_pair(NormalizedKey, Value, Pair).
	set_context_value([Entry| Rest], Key, Value, [NewPair| Rest]) :-
		pair_key_value(Entry, EntryKey, _),
		same_key(EntryKey, Key),
		!,
		normalize_key(Key, NormalizedKey),
		make_pair(NormalizedKey, Value, NewPair).
	set_context_value([Entry| Rest], Key, Value, [Entry| NewRest]) :-
		set_context_value(Rest, Key, Value, NewRest).

	remove_context_value([], _Key, []).
	remove_context_value([Entry| Rest], Key, NewContext) :-
		pair_key_value(Entry, EntryKey, _),
		(	same_key(EntryKey, Key) ->
			NewContext = Rest
		;	NewContext = [Entry| NewRest],
			remove_context_value(Rest, Key, NewRest)
		).

	remove_empty_pairs([], []).
	remove_empty_pairs([Pair| Pairs], Result) :-
		pair_key_value(Pair, Key, Value),
		(	Value == [], Key \== '@list' ->
			remove_empty_pairs(Pairs, Result)
		;	Result = [Pair| Rest],
			remove_empty_pairs(Pairs, Rest)
		).

	% JSON-LD keywords
	is_keyword(Value) :-
		json_ld_string_atom(Value, Atom),
		is_keyword_atom(Atom).

	is_keyword_atom('@base').
	is_keyword_atom('@container').
	is_keyword_atom('@context').
	is_keyword_atom('@direction').
	is_keyword_atom('@graph').
	is_keyword_atom('@id').
	is_keyword_atom('@import').
	is_keyword_atom('@included').
	is_keyword_atom('@index').
	is_keyword_atom('@json').
	is_keyword_atom('@language').
	is_keyword_atom('@list').
	is_keyword_atom('@nest').
	is_keyword_atom('@none').
	is_keyword_atom('@prefix').
	is_keyword_atom('@propagate').
	is_keyword_atom('@protected').
	is_keyword_atom('@reverse').
	is_keyword_atom('@set').
	is_keyword_atom('@type').
	is_keyword_atom('@value').
	is_keyword_atom('@version').
	is_keyword_atom('@vocab').

	% IRI detection
	is_absolute_iri(IRI0) :-
		json_ld_string_atom(IRI0, IRI),
		(	sub_atom(IRI, _, _, _, '://') ->
			true
		;	sub_atom(IRI, 0, 4, _, 'urn:')
		).

	is_compact_iri(IRI0) :-
		json_ld_string_atom(IRI0, IRI),
		\+ is_keyword(IRI),
		\+ is_absolute_iri(IRI),
		sub_atom(IRI, Before, 1, _, ':'),
		Before > 0,
		% Only the first colon matters
		\+ (sub_atom(IRI, Before2, 1, _, ':'), Before2 < Before),
		!.

	split_compact_iri(CompactIRI0, Prefix, Suffix) :-
		json_ld_string_atom(CompactIRI0, CompactIRI),
		sub_atom(CompactIRI, Before, 1, After, ':'),
		\+ (sub_atom(CompactIRI, Before2, 1, _, ':'), Before2 < Before),
		!,
		sub_atom(CompactIRI, 0, Before, _, Prefix),
		sub_atom(CompactIRI, _, After, 0, Suffix).

	resolve_relative_iri(Relative0, Base0, Resolved) :-
		json_ld_string_atom(Relative0, Relative),
		!,
		(	is_absolute_iri(Relative) ->
			Resolved = Relative
		;	json_ld_string_atom(Base0, Base) ->
			resolve_relative_iri_atom(Relative, Base, Resolved)
		;	Resolved = Relative
		).
	resolve_relative_iri(Relative, _Base, Relative).

	resolve_relative_iri_atom('', Base, Resolved) :-
		!,
		remove_fragment(Base, Resolved).
	resolve_relative_iri_atom(Relative, Base, Resolved) :-
		sub_atom(Relative, 0, 2, _, '//'),
		!,
		base_scheme_prefix(Base, SchemePrefix),
		atom_concat(SchemePrefix, Relative, Resolved).
	resolve_relative_iri_atom(Relative, Base, Resolved) :-
		sub_atom(Relative, 0, 1, _, '/'),
		!,
		base_authority_prefix(Base, Prefix),
		normalize_path(Relative, NormalizedPath),
		atom_concat(Prefix, NormalizedPath, Resolved).
	resolve_relative_iri_atom(Relative, Base, Resolved) :-
		sub_atom(Relative, 0, 1, _, '?'),
		!,
		remove_query_fragment(Base, BasePath),
		atom_concat(BasePath, Relative, Resolved).
	resolve_relative_iri_atom(Relative, Base, Resolved) :-
		sub_atom(Relative, 0, 1, _, '#'),
		!,
		remove_fragment(Base, BasePath),
		atom_concat(BasePath, Relative, Resolved).
	resolve_relative_iri_atom(Relative, Base, Resolved) :-
		split_reference_suffix(Relative, RelativePath, Suffix),
		base_prefix_and_directory_path(Base, Prefix, DirectoryPath),
		atom_concat(DirectoryPath, RelativePath, MergedPath),
		normalize_path(MergedPath, NormalizedPath),
		atom_concat(Prefix, NormalizedPath, ResolvedPath),
		atom_concat(ResolvedPath, Suffix, Resolved).

	base_scheme_prefix(Base, Prefix) :-
		first_separator_position(Base, ':', Pos),
		!,
		Len is Pos + 1,
		sub_atom(Base, 0, Len, _, Prefix).
	base_scheme_prefix(_Base, '').

	base_authority_prefix(Base, Prefix) :-
		remove_query_fragment(Base, BasePath),
		(	sub_atom(BasePath, SchemeEnd, 3, AfterScheme, '://') ->
			AuthorityStart is SchemeEnd + 3,
			sub_atom(BasePath, AuthorityStart, AfterScheme, 0, AuthorityAndPath),
			(	first_separator_position(AuthorityAndPath, '/', SlashPos) ->
				Len is AuthorityStart + SlashPos,
				sub_atom(BasePath, 0, Len, _, Prefix)
			;	Prefix = BasePath
			)
		;	Prefix = ''
		).

	base_prefix_and_directory_path(Base, Prefix, DirectoryPath) :-
		remove_query_fragment(Base, BasePath),
		base_authority_prefix(BasePath, Prefix),
		atom_concat(Prefix, Path, BasePath),
		directory_path(Path, DirectoryPath).

	directory_path(Path, DirectoryPath) :-
		atom_codes(Path, Codes),
		(	last_slash_position(Codes, 0, Pos) ->
			Len is Pos + 1,
			sub_atom(Path, 0, Len, _, DirectoryPath)
		;	DirectoryPath = ''
		).

	remove_fragment(IRI, WithoutFragment) :-
		(	first_separator_position(IRI, '#', Pos) ->
			sub_atom(IRI, 0, Pos, _, WithoutFragment)
		;	WithoutFragment = IRI
		).

	remove_query_fragment(IRI, WithoutQueryFragment) :-
		(	first_query_fragment_position(IRI, Pos) ->
			sub_atom(IRI, 0, Pos, _, WithoutQueryFragment)
		;	WithoutQueryFragment = IRI
		).

	split_reference_suffix(Reference, Path, Suffix) :-
		(	first_query_fragment_position(Reference, Pos) ->
			sub_atom(Reference, 0, Pos, _, Path),
			sub_atom(Reference, Pos, _, 0, Suffix)
		;	Path = Reference,
			Suffix = ''
		).

	normalize_path(Path, NormalizedPath) :-
		sub_atom(Path, 0, 1, _, '/'),
		!,
		split_path_segments(Path, Segments),
		normalize_segments(Segments, [], NormalizedSegments),
		join_segments(NormalizedSegments, JoinedPath),
		(	JoinedPath == '' ->
			NormalizedPath = ('/')
		;	atom_concat('/', JoinedPath, NormalizedPath)
		).
	normalize_path(Path, NormalizedPath) :-
		split_path_segments(Path, Segments),
		normalize_segments(Segments, [], NormalizedSegments),
		join_segments(NormalizedSegments, NormalizedPath).

	split_path_segments(Path, [Segment| Segments]) :-
		first_separator_position(Path, '/', Pos),
		!,
		sub_atom(Path, 0, Pos, _, Segment),
		RestStart is Pos + 1,
		sub_atom(Path, RestStart, _, 0, Rest),
		split_path_segments(Rest, Segments).
	split_path_segments(Path, [Path]).

	normalize_segments([], Segments, Segments).
	normalize_segments([Segment| Segments], Segments0, NormalizedSegments) :-
		(	Segment == '' ->
			Segments1 = Segments0
		;	Segment == '.' ->
			Segments1 = Segments0
		;	Segment == '..' ->
			remove_last_segment(Segments0, Segments1)
		;	append(Segments0, [Segment], Segments1)
		),
		normalize_segments(Segments, Segments1, NormalizedSegments).

	remove_last_segment([], []).
	remove_last_segment([_Segment], []) :-
		!.
	remove_last_segment([Segment| Segments], [Segment| RemainingSegments]) :-
		remove_last_segment(Segments, RemainingSegments).

	join_segments([], '').
	join_segments([Segment], Segment) :-
		!.
	join_segments([Segment| Segments], Joined) :-
		join_segments(Segments, JoinedRest),
		atom_concat(Segment, '/', Prefix),
		atom_concat(Prefix, JoinedRest, Joined).

	first_query_fragment_position(Atom, Pos) :-
		(	first_separator_position(Atom, '?', QueryPos) ->
			(	first_separator_position(Atom, '#', FragmentPos), FragmentPos < QueryPos ->
				Pos = FragmentPos
			;	Pos = QueryPos
			)
		;	first_separator_position(Atom, '#', Pos)
		).

	first_separator_position(Atom, Separator, Pos) :-
		sub_atom(Atom, Pos, 1, _, Separator),
		\+ (sub_atom(Atom, EarlierPos, 1, _, Separator), EarlierPos < Pos),
		!.

	last_slash_position([], _Pos, _) :- fail.
	last_slash_position([Code| Codes], CurrentPos, LastPos) :-
		NextPos is CurrentPos + 1,
		(	Code =:= 0'/ ->
			(	last_slash_position(Codes, NextPos, Later) ->
				LastPos = Later
			;	LastPos = CurrentPos
			)
		;	last_slash_position(Codes, NextPos, LastPos)
		).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		downcase_atom(Atom, LoweredAtom) :-
			{downcase_atom(Atom, LoweredAtom)}.

	:- else.

		downcase_atom(AnyAtom, Lower) :-
			atom_chars(AnyAtom, AnyChars),
			downcase_chars(AnyChars, LowerChars),
			atom_chars(Lower, LowerChars).

		% ASCII only and avoiding 0'Char notation that would break some backends!
		downcase_chars([], []).
		downcase_chars([AnyChar| AnyChars], [LowerChar| LowerChars]) :-
			(	'A' @=< AnyChar, AnyChar @=< 'Z' ->
				char_code(AnyChar, AnyCode),
				LowerCode is AnyCode + 32,
				char_code(LowerChar, LowerCode)
			;	LowerChar = AnyChar
			),
			downcase_chars(AnyChars, LowerChars).

	:- endif.

	downcase_atom_if_possible(Value, LoweredAtom) :-
		(	json_ld_string_atom(Value, Atom) ->
			downcase_atom(Atom, LoweredAtom)
		;	LoweredAtom = Value
		).

:- end_object.


:- object(json_ld(StringRepresentation),
	extends(json_ld(curly, dash, StringRepresentation))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
		comment is 'JSON-LD 1.1 parser, generator, and processor. Uses curly terms for parsed JSON objects and dashes for parsed JSON pairs.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used when decoding JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

:- end_object.


:- object(json_ld,
	extends(json_ld(curly, dash, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
		comment is 'JSON-LD 1.1 parser, generator, and processor. Uses curly terms for parsed JSON objects, dashes for parsed JSON pairs, and atoms for parsed JSON strings.'
	]).

:- end_object.

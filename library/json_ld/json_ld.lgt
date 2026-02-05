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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
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
		append/3, member/2, msort/2, valid/1 as is_list/1
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

	% ==================== flatten/2 ====================

	flatten(Document, _) :-
		var(Document),
		instantiation_error.
	flatten(Document, Flattened) :-
		% Collect all nodes and generate blank node IDs for nodes without @id
		flatten_collect_nodes(Document, 0, _, [], Nodes),
		% Merge nodes with the same @id
		merge_nodes(Nodes, MergedNodes),
		% Build result - if single node without @graph, return as-is; otherwise wrap in @graph
		(	MergedNodes = [SingleNode],
			\+ has_pair(SingleNode, '@graph') ->
			Flattened = SingleNode
		;	make_pair('@graph', MergedNodes, GraphPair),
			pairs_to_object([GraphPair], Flattened)
		).

	% Collect nodes from document, replacing nested objects with references
	flatten_collect_nodes([], Counter, Counter, Nodes, Nodes) :- !.
	flatten_collect_nodes(List, Counter0, Counter, Nodes0, Nodes) :-
		is_list(List),
		!,
		flatten_collect_list(List, Counter0, Counter, Nodes0, Nodes).
	flatten_collect_nodes(Object, Counter0, Counter, Nodes0, Nodes) :-
		is_object(Object),
		!,
		object_to_pairs(Object, Pairs),
		flatten_node(Pairs, Counter0, Counter, Nodes0, Nodes).
	flatten_collect_nodes(_, Counter, Counter, Nodes, Nodes).

	flatten_collect_list([], Counter, Counter, Nodes, Nodes).
	flatten_collect_list([Element| Elements], Counter0, Counter, Nodes0, Nodes) :-
		flatten_collect_nodes(Element, Counter0, Counter1, Nodes0, Nodes1),
		flatten_collect_list(Elements, Counter1, Counter, Nodes1, Nodes).

	% Process a node object
	flatten_node(Pairs, Counter0, Counter, Nodes0, Nodes) :-
		% Get or generate @id for this node
		(	get_pair_value('@id', Pairs, NodeId) ->
			Counter1 = Counter0
		;	% Generate blank node ID
			number_codes(Counter0, CounterCodes),
			atom_codes(CounterAtom, CounterCodes),
			atom_concat('_:b', CounterAtom, NodeId),
			Counter1 is Counter0 + 1
		),
		% Process each property, collecting nested nodes and creating references
		flatten_pairs(Pairs, NodeId, Counter1, Counter, Nodes0, Nodes1, FlattenedPairs0),
		% Ensure @id is in the flattened pairs
		(	get_pair_value('@id', FlattenedPairs0, _) ->
			FlattenedPairs = FlattenedPairs0
		;	make_pair('@id', NodeId, IdPair),
			FlattenedPairs = [IdPair| FlattenedPairs0]
		),
		pairs_to_object(FlattenedPairs, FlattenedNode),
		Nodes = [FlattenedNode| Nodes1].

	flatten_pairs([], _NodeId, Counter, Counter, Nodes, Nodes, []).
	flatten_pairs([Pair| Pairs], NodeId, Counter0, Counter, Nodes0, Nodes, [FlatPair| FlatPairs]) :-
		pair_key_value(Pair, Key, Value),
		flatten_value(Key, Value, Counter0, Counter1, Nodes0, Nodes1, FlatValue),
		make_pair(Key, FlatValue, FlatPair),
		flatten_pairs(Pairs, NodeId, Counter1, Counter, Nodes1, Nodes, FlatPairs).

	% Flatten a property value
	flatten_value('@id', Value, Counter, Counter, Nodes, Nodes, Value) :- !.
	flatten_value('@type', Value, Counter, Counter, Nodes, Nodes, Value) :- !.
	flatten_value('@value', Value, Counter, Counter, Nodes, Nodes, Value) :- !.
	flatten_value('@language', Value, Counter, Counter, Nodes, Nodes, Value) :- !.
	flatten_value('@direction', Value, Counter, Counter, Nodes, Nodes, Value) :- !.
	flatten_value('@index', Value, Counter, Counter, Nodes, Nodes, Value) :- !.
	flatten_value(_Key, Value, Counter0, Counter, Nodes0, Nodes, FlatValue) :-
		is_list(Value),
		!,
		flatten_value_list(Value, Counter0, Counter, Nodes0, Nodes, FlatValue).
	flatten_value(_Key, Value, Counter0, Counter, Nodes0, Nodes, FlatValue) :-
		is_object(Value),
		is_node_object(Value),
		!,
		% This is a nested node - extract it and replace with reference
		object_to_pairs(Value, ValuePairs),
		flatten_node(ValuePairs, Counter0, Counter, Nodes0, Nodes),
		% Create reference
		(	get_pair_value('@id', ValuePairs, RefId) ->
			true
		;	% ID was generated - need to get it from the just-added node
			Nodes = [AddedNode| _],
			object_to_pairs(AddedNode, AddedPairs),
			get_pair_value('@id', AddedPairs, RefId)
		),
		make_pair('@id', RefId, RefPair),
		pairs_to_object([RefPair], FlatValue).
	flatten_value(_Key, Value, Counter, Counter, Nodes, Nodes, Value).

	flatten_value_list([], Counter, Counter, Nodes, Nodes, []).
	flatten_value_list([Value| Values], Counter0, Counter, Nodes0, Nodes, [FlatValue| FlatValues]) :-
		flatten_value(property, Value, Counter0, Counter1, Nodes0, Nodes1, FlatValue),
		flatten_value_list(Values, Counter1, Counter, Nodes1, Nodes, FlatValues).

	% Check if an object is a node object (has properties other than @value)
	is_node_object(Object) :-
		object_to_pairs(Object, Pairs),
		\+ get_pair_value('@value', Pairs, _).

	% Check if object has a specific pair
	has_pair(Object, Key) :-
		object_to_pairs(Object, Pairs),
		get_pair_value(Key, Pairs, _).

	% Merge nodes with the same @id
	merge_nodes([], []).
	merge_nodes([Node| Nodes], MergedNodes) :-
		object_to_pairs(Node, Pairs),
		(	get_pair_value('@id', Pairs, NodeId) ->
			% Find and remove all nodes with same @id
			partition_by_id(Nodes, NodeId, SameId, DifferentId),
			% Merge properties from all nodes with same @id
			merge_node_list([Node| SameId], MergedNode),
			merge_nodes(DifferentId, RestMerged),
			MergedNodes = [MergedNode| RestMerged]
		;	merge_nodes(Nodes, RestMerged),
			MergedNodes = [Node| RestMerged]
		).

	partition_by_id([], _Id, [], []).
	partition_by_id([Node| Nodes], Id, SameId, DifferentId) :-
		object_to_pairs(Node, Pairs),
		(	get_pair_value('@id', Pairs, NodeId),
			NodeId == Id ->
			SameId = [Node| RestSameId],
			partition_by_id(Nodes, Id, RestSameId, DifferentId)
		;	DifferentId = [Node| RestDifferentId],
			partition_by_id(Nodes, Id, SameId, RestDifferentId)
		).

	merge_node_list([Node], Node) :- !.
	merge_node_list([Node1, Node2| Rest], MergedNode) :-
		object_to_pairs(Node1, Pairs1),
		object_to_pairs(Node2, Pairs2),
		merge_pairs(Pairs1, Pairs2, MergedPairs),
		pairs_to_object(MergedPairs, Merged),
		merge_node_list([Merged| Rest], MergedNode).

	merge_pairs([], Pairs, Pairs).
	merge_pairs([Pair| Pairs1], Pairs2, MergedPairs) :-
		pair_key_value(Pair, Key, Value1),
		(	get_pair_value(Key, Pairs2, Value2) ->
			% Key exists in both - merge values
			merge_values(Value1, Value2, MergedValue),
			make_pair(Key, MergedValue, MergedPair),
			remove_pair(Key, Pairs2, Pairs2Rest),
			merge_pairs(Pairs1, Pairs2Rest, RestMerged),
			MergedPairs = [MergedPair| RestMerged]
		;	merge_pairs(Pairs1, Pairs2, RestMerged),
			MergedPairs = [Pair| RestMerged]
		).

	merge_values(Value1, Value2, MergedValue) :-
		(	is_list(Value1), is_list(Value2) ->
			append(Value1, Value2, MergedValue)
		;	is_list(Value1) ->
			append(Value1, [Value2], MergedValue)
		;	is_list(Value2) ->
			MergedValue = [Value1| Value2]
		;	Value1 == Value2 ->
			MergedValue = Value1
		;	MergedValue = [Value1, Value2]
		).

	remove_pair(_Key, [], []).
	remove_pair(Key, [Pair| Pairs], Result) :-
		pair_key_value(Pair, PairKey, _),
		(	PairKey == Key ->
			Result = Pairs
		;	remove_pair(Key, Pairs, RestResult),
			Result = [Pair| RestResult]
		).

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
		expand_array_elements(Elements, ActiveContext, ExpandedRest),
		append(ExpandedElement, ExpandedRest, Expanded).

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
		pair_key_value(Pair, Key, Value),
		expand_pair(Key, Value, ActiveContext, ExpandedPair),
		expand_pairs(Pairs, ActiveContext, ExpandedRest),
		append(ExpandedPair, ExpandedRest, ExpandedPairs).

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
			expand_list_elements(Value, ActiveContext, ExpandedElements),
			make_pair('@list', ExpandedElements, ExpandedPair)
		;	expand_value(Value, ActiveContext, none, ExpandedValue),
			make_pair('@list', [ExpandedValue], ExpandedPair)
		).

	% Expand @set
	expand_pair('@set', Value, ActiveContext, ExpandedValues) :-
		!,
		(	is_list(Value) ->
			expand_list_elements(Value, ActiveContext, ExpandedValues0),
			values_to_pairs('@set', ExpandedValues0, ExpandedValues)
		;	expand_value(Value, ActiveContext, none, ExpandedValue),
			values_to_pairs('@set', [ExpandedValue], ExpandedValues)
		).

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
	expand_pair('@nest', _Value, _ActiveContext, []) :- !.

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
			get_pair_value('@type', TermDef, '@id') ->
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
			Expanded = [ExpandedElement| ExpandedRest]
		).

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
			Expanded = [ExpandedElement| ExpandedRest]
		).

	expand_type_list([], _ActiveContext, []).
	expand_type_list([Type| Types], ActiveContext, [ExpandedType| ExpandedTypes]) :-
		expand_iri(Type, ActiveContext, vocab_relative, ExpandedType),
		expand_type_list(Types, ActiveContext, ExpandedTypes).

	% Expand a scalar value
	expand_value(@true, _ActiveContext, _Key, @true) :- !.
	expand_value(@false, _ActiveContext, _Key, @false) :- !.
	expand_value(@null, _ActiveContext, _Key, @null) :- !.
	expand_value(Value, _ActiveContext, _Key, Value) :-
		number(Value),
		!.
	expand_value(Value, ActiveContext, Key, ExpandedValue) :-
		atom(Value),
		!,
		(	Key \== none,
			get_term_definition(ActiveContext, Key, TermDef),
			get_pair_value('@type', TermDef, TypeCoercion) ->
			(	TypeCoercion == '@id' ->
				expand_iri(Value, ActiveContext, document_relative, ExpandedIRI),
				make_value_object_id(ExpandedIRI, ExpandedValue)
			;	TypeCoercion == '@vocab' ->
				expand_iri(Value, ActiveContext, vocab_relative, ExpandedIRI),
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

	values_to_pairs(_, [], []).
	values_to_pairs(Key, [Value| Values], [Pair| Pairs]) :-
		make_pair(Key, Value, Pair),
		values_to_pairs(Key, Values, Pairs).

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
		pair_key_value(Pair, Key, Value),
		process_context_entry(Key, Value, ActiveContext, Context1),
		process_context_entries(Pairs, Context1, NewContext).

	process_context_entry('@base', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@base', NewContext)
		;	set_context_value(ActiveContext, '@base', Value, NewContext)
		).
	process_context_entry('@vocab', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@vocab', NewContext)
		;	set_context_value(ActiveContext, '@vocab', Value, NewContext)
		).
	process_context_entry('@language', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@language', NewContext)
		;	downcase_atom_if_possible(Value, LoweredValue),
			set_context_value(ActiveContext, '@language', LoweredValue, NewContext)
		).
	process_context_entry('@direction', Value, ActiveContext, NewContext) :-
		!,
		(	Value == @null ->
			remove_context_value(ActiveContext, '@direction', NewContext)
		;	set_context_value(ActiveContext, '@direction', Value, NewContext)
		).
	process_context_entry('@version', _Value, ActiveContext, ActiveContext) :- !.
	process_context_entry('@import', _Value, ActiveContext, ActiveContext) :- !.
	process_context_entry('@propagate', _Value, ActiveContext, ActiveContext) :- !.
	process_context_entry('@protected', _Value, ActiveContext, ActiveContext) :- !.
	process_context_entry(Term, Value, ActiveContext, NewContext) :-
		% Term definition
		(	is_object(Value) ->
			% Expanded term definition
			object_to_pairs(Value, DefPairs),
			set_context_value(ActiveContext, term(Term), DefPairs, Context1),
			(	get_pair_value('@id', DefPairs, IRI) ->
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
		;	atom(Value) ->
			% Simple term definition: term -> IRI or keyword
			(	is_keyword(Value) ->
				set_context_value(ActiveContext, Term, Value, NewContext)
			;	expand_term_iri(Value, ActiveContext, ExpandedIRI),
				set_context_value(ActiveContext, Term, ExpandedIRI, NewContext)
			)
		;	Value == @null ->
			remove_context_value(ActiveContext, Term, NewContext)
		;	NewContext = ActiveContext
		).

	expand_term_iri(IRI, ActiveContext, ExpandedIRI) :-
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

	% ==================== IRI Expansion ====================

	expand_iri(Value, _ActiveContext, _Mode, Value) :-
		var(Value),
		!.
	expand_iri(Value, _ActiveContext, _Mode, Value) :-
		\+ atom(Value),
		!.
	expand_iri(Value, _ActiveContext, _Mode, Value) :-
		is_keyword(Value),
		!.
	expand_iri(Value, ActiveContext, _Mode, ExpandedIRI) :-
		get_context_value(ActiveContext, Value, Mapping),
		atom(Mapping),
		!,
		ExpandedIRI = Mapping.
	expand_iri(Value, ActiveContext, _Mode, ExpandedIRI) :-
		is_compact_iri(Value),
		!,
		expand_compact_iri(Value, ActiveContext, ExpandedIRI).
	expand_iri(Value, _ActiveContext, _Mode, Value) :-
		is_absolute_iri(Value),
		!.
	expand_iri(Value, ActiveContext, vocab_relative, ExpandedIRI) :-
		get_context_value(ActiveContext, '@vocab', Vocab),
		!,
		atom_concat(Vocab, Value, ExpandedIRI).
	expand_iri(Value, ActiveContext, document_relative, ExpandedIRI) :-
		get_context_value(ActiveContext, '@base', Base),
		!,
		resolve_relative_iri(Value, Base, ExpandedIRI).
	expand_iri(Value, _ActiveContext, _Mode, Value).

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

	get_pair_value(Key, Pairs, Value) :-
		member(Pair, Pairs),
		pair_key_value(Pair, PairKey, Value),
		PairKey == Key,
		!.

	get_term_definition(ActiveContext, Key, TermDef) :-
		atom(Key),
		get_context_value(ActiveContext, term(Key), TermDef).

	% Context as association list
	get_context_value([Entry| _], Key, Value) :-
		pair_key_value(Entry, EntryKey, Value),
		EntryKey == Key,
		!.
	get_context_value([_| Entries], Key, Value) :-
		get_context_value(Entries, Key, Value).

	set_context_value([], Key, Value, [Pair]) :-
		make_pair(Key, Value, Pair).
	set_context_value([Entry| Rest], Key, Value, [NewPair| Rest]) :-
		pair_key_value(Entry, EntryKey, _),
		EntryKey == Key,
		!,
		make_pair(Key, Value, NewPair).
	set_context_value([Entry| Rest], Key, Value, [Entry| NewRest]) :-
		set_context_value(Rest, Key, Value, NewRest).

	remove_context_value([], _Key, []).
	remove_context_value([Entry| Rest], Key, NewContext) :-
		pair_key_value(Entry, EntryKey, _),
		(	EntryKey == Key ->
			NewContext = Rest
		;	NewContext = [Entry| NewRest],
			remove_context_value(Rest, Key, NewRest)
		).

	remove_empty_pairs([], []).
	remove_empty_pairs([Pair| Pairs], Result) :-
		pair_key_value(Pair, _Key, Value),
		(	Value == [] ->
			remove_empty_pairs(Pairs, Result)
		;	Result = [Pair| Rest],
			remove_empty_pairs(Pairs, Rest)
		).

	% JSON-LD keywords
	is_keyword('@base').
	is_keyword('@container').
	is_keyword('@context').
	is_keyword('@direction').
	is_keyword('@graph').
	is_keyword('@id').
	is_keyword('@import').
	is_keyword('@included').
	is_keyword('@index').
	is_keyword('@json').
	is_keyword('@language').
	is_keyword('@list').
	is_keyword('@nest').
	is_keyword('@none').
	is_keyword('@prefix').
	is_keyword('@propagate').
	is_keyword('@protected').
	is_keyword('@reverse').
	is_keyword('@set').
	is_keyword('@type').
	is_keyword('@value').
	is_keyword('@version').
	is_keyword('@vocab').

	% IRI detection
	is_absolute_iri(IRI) :-
		atom(IRI),
		(	sub_atom(IRI, _, _, _, '://') ->
			true
		;	sub_atom(IRI, 0, 4, _, 'urn:')
		).

	is_compact_iri(IRI) :-
		atom(IRI),
		\+ is_keyword(IRI),
		\+ is_absolute_iri(IRI),
		sub_atom(IRI, Before, 1, _, ':'),
		Before > 0,
		% Only the first colon matters
		\+ (sub_atom(IRI, Before2, 1, _, ':'), Before2 < Before),
		!.

	split_compact_iri(CompactIRI, Prefix, Suffix) :-
		atom(CompactIRI),
		sub_atom(CompactIRI, Before, 1, After, ':'),
		\+ (sub_atom(CompactIRI, Before2, 1, _, ':'), Before2 < Before),
		!,
		sub_atom(CompactIRI, 0, Before, _, Prefix),
		sub_atom(CompactIRI, _, After, 0, Suffix).

	resolve_relative_iri(Relative, Base, Resolved) :-
		(	is_absolute_iri(Relative) ->
			Resolved = Relative
		;	atom(Base) ->
			(	sub_atom(Relative, 0, 1, _, '/') ->
				% Relative to authority
				Resolved = Relative
			;	Relative == '' ->
				Resolved = Base
			;	% Remove fragment/query and last path segment from base
				remove_last_path_segment(Base, BaseDir),
				atom_concat(BaseDir, Relative, Resolved)
			)
		;	Resolved = Relative
		).

	remove_last_path_segment(IRI, BaseDir) :-
		atom_codes(IRI, Codes),
		(	last_slash_position(Codes, 0, Pos) ->
			Len is Pos + 1,
			sub_atom(IRI, 0, Len, _, BaseDir)
		;	atom_concat(IRI, '/', BaseDir)
		).

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

	% downcase_atom/2: conditional definition for backend compatibility

:- if(current_logtalk_flag(prolog_dialect, swi)).

	downcase_atom(Atom, LoweredAtom) :-
		{downcase_atom(Atom, LoweredAtom)}.

:- else.

	downcase_atom(AnyAtom, Lower) :-
		atom_chars(AnyAtom, AnyChars),
		downcase_atom_(AnyChars, LowerChars),
		atom_chars(Lower, LowerChars).

	% ASCII only and avoiding 0'Char notation that would break some backends!
	downcase_atom_([], []).
	downcase_atom_([AnyChar| AnyChars], [LowerChar| LowerChars]) :-
		(	'A' @=< AnyChar, AnyChar @=< 'Z' ->
			char_code(AnyChar, AnyCode),
			LowerCode is AnyCode + 32,
			char_code(LowerChar, LowerCode)
		;	LowerChar = AnyChar
		),
		downcase_atom_(AnyChars, LowerChars).

:- endif.

	downcase_atom_if_possible(Atom, LoweredAtom) :-
		(	atom(Atom) ->
			downcase_atom(Atom, LoweredAtom)
		;	LoweredAtom = Atom
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

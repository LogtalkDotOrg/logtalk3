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


:- object(trie(_Representation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-05,
		comment is 'Persistent trie predicates supporting different string representations.',
		parameters is [
			'Representation' - 'String representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		]
	]).

	:- public(new/1).
	:- mode(new(-trie), one).
	:- info(new/1, [
		comment is 'Creates an empty trie.',
		argnames is ['Trie']
	]).

	:- public(as_trie/2).
	:- mode(as_trie(@list(pairs), -trie), one).
	:- info(as_trie/2, [
		comment is 'Converts a list of string-value pairs to a trie. When a string occurs multiple times, the last value is retained.',
		argnames is ['Pairs', 'Trie']
	]).

	:- public(as_list/2).
	:- mode(as_list(@trie, -list(pairs)), one).
	:- info(as_list/2, [
		comment is 'Converts a trie to a lexicographically ordered list of string-value pairs.',
		argnames is ['Trie', 'Pairs']
	]).

	:- public(as_list/3).
	:- mode(as_list(@trie, +text, -list(pairs)), zero_or_one).
	:- info(as_list/3, [
		comment is 'Returns a lexicographically ordered list of all string-value pairs whose strings start with the given prefix. Fails if no string starts with the prefix.',
		argnames is ['Trie', 'Prefix', 'Pairs']
	]).

	:- public(strings/2).
	:- mode(strings(@trie, -list(text)), one).
	:- info(strings/2, [
		comment is 'Returns all strings in a trie in lexicographic order.',
		argnames is ['Trie', 'Strings']
	]).

	:- public(strings/3).
	:- mode(strings(@trie, +text, -list(text)), zero_or_one).
	:- info(strings/3, [
		comment is 'Returns all strings that start with the given prefix in lexicographic order. Fails if no string starts with the prefix.',
		argnames is ['Trie', 'Prefix', 'Strings']
	]).

	:- public(values/2).
	:- mode(values(@trie, -list), one).
	:- info(values/2, [
		comment is 'Returns all values in lexicographic order of their associated strings.',
		argnames is ['Trie', 'Values']
	]).

	:- public(values/3).
	:- mode(values(@trie, +text, -list), zero_or_one).
	:- info(values/3, [
		comment is 'Returns all values associated with strings that start with the given prefix, in lexicographic order of the strings. Fails if no string starts with the prefix.',
		argnames is ['Trie', 'Prefix', 'Values']
	]).

	:- public(empty/1).
	:- mode(empty(@trie), zero_or_one).
	:- info(empty/1, [
		comment is 'True iff the trie is empty.',
		argnames is ['Trie']
	]).

	:- public(insert/4).
	:- mode(insert(+trie, +text, @term, -trie), one).
	:- info(insert/4, [
		comment is 'Inserts a string-value pair into a trie, returning the updated trie. When the string already exists, its associated value is updated.',
		argnames is ['OldTrie', 'String', 'Value', 'NewTrie']
	]).

	:- public(lookup/3).
	:- mode(lookup(+text, ?term, @trie), zero_or_one).
	:- mode(lookup(-text, ?term, @trie), zero_or_more).
	:- info(lookup/3, [
		comment is 'Looks up a matching string-value pair. With an unbound string, enumerates all pairs in lexicographic order.',
		argnames is ['String', 'Value', 'Trie']
	]).

	:- public(update/4).
	:- mode(update(+trie, +text, +term, -trie), zero_or_one).
	:- info(update/4, [
		comment is 'Updates the value associated with a string, returning the updated trie. Fails if the string is not stored.',
		argnames is ['OldTrie', 'String', 'NewValue', 'NewTrie']
	]).

	:- public(update/5).
	:- mode(update(+trie, +text, ?term, +term, -trie), zero_or_one).
	:- info(update/5, [
		comment is 'Updates the value associated with a string, returning the updated trie. Fails if the string is not stored or its value does not unify with the expected old value.',
		argnames is ['OldTrie', 'String', 'OldValue', 'NewValue', 'NewTrie']
	]).

	:- public(delete/4).
	:- mode(delete(+trie, +text, ?term, -trie), zero_or_one).
	:- info(delete/4, [
		comment is 'Deletes a matching string-value pair, returning the updated trie. Descendant strings are preserved.',
		argnames is ['OldTrie', 'String', 'Value', 'NewTrie']
	]).

	:- public(lookup_prefix/4).
	:- mode(lookup_prefix(+text, -text, ?term, @trie), zero_or_more).
	:- info(lookup_prefix/4, [
		comment is 'Enumerates all string-value pairs whose strings start with the given prefix, in lexicographic order.',
		argnames is ['Prefix', 'String', 'Value', 'Trie']
	]).

	:- public(prefixes/3).
	:- mode(prefixes(@trie, +text, -list(pairs)), one).
	:- info(prefixes/3, [
		comment is 'Returns all stored string-value pairs whose strings are prefixes of the query, ordered from shortest to longest.',
		argnames is ['Trie', 'Query', 'Pairs']
	]).

	:- public(longest_prefix/4).
	:- mode(longest_prefix(@trie, +text, -text, -term), zero_or_one).
	:- info(longest_prefix/4, [
		comment is 'Returns the longest stored string that is a prefix of the query and its associated value. Fails if no stored string is a prefix.',
		argnames is ['Trie', 'Query', 'Prefix', 'Value']
	]).

	:- public(delete_prefix/3).
	:- mode(delete_prefix(+trie, +text, -trie), zero_or_one).
	:- info(delete_prefix/3, [
		comment is 'Deletes all string-value pairs whose strings start with the given prefix, returning the updated trie. Fails if no string starts with the prefix.',
		argnames is ['OldTrie', 'Prefix', 'NewTrie']
	]).

	:- public(size/2).
	:- mode(size(@trie, ?integer), one).
	:- info(size/2, [
		comment is 'Number of string-value pairs stored in a trie.',
		argnames is ['Trie', 'Size']
	]).

	:- uses(string(_Representation_), [
		string_codes/2
	]).

	:- uses(list, [
		append/3, last/2
	]).

	:- uses(pairs, [
		keys/2 as pairs_strings/2,
		values/2 as pairs_values/2
	]).

	new(trie(0, node(none, []))).

	as_trie(Pairs, Trie) :-
		new(Empty),
		insert_pairs(Pairs, Empty, Trie).

	as_list(trie(_, Node), Pairs) :-
		node_pairs(Node, [], Pairs, []).

	as_list(trie(_, Node), Prefix, Pairs) :-
		string_codes(Prefix, PrefixCodes),
		prefix_node(PrefixCodes, Node, PrefixNode),
		node_pairs(PrefixNode, PrefixCodes, Pairs, []),
		Pairs = [_| _].

	strings(Trie, Strings) :-
		as_list(Trie, Pairs),
		pairs_strings(Pairs, Strings).

	strings(Trie, Prefix, Strings) :-
		as_list(Trie, Prefix, Pairs),
		pairs_strings(Pairs, Strings).

	values(Trie, Values) :-
		as_list(Trie, Pairs),
		pairs_values(Pairs, Values).

	values(Trie, Prefix, Values) :-
		as_list(Trie, Prefix, Pairs),
		pairs_values(Pairs, Values).

	empty(trie(0, _)).

	insert(trie(Size0, Node0), String, Value, trie(Size, Node)) :-
		string_codes(String, Codes),
		insert_codes(Codes, Value, Node0, Node, Added),
		Size is Size0 + Added.

	lookup(String, Value, trie(_, Node)) :-
		(	nonvar(String) ->
			string_codes(String, Codes),
			lookup_codes(Codes, Value, Node)
		;	node_pair(Node, [], Codes, Value),
			string_codes(String, Codes)
		).

	update(trie(Size, Node0), String, NewValue, trie(Size, Node)) :-
		string_codes(String, Codes),
		update_codes(Codes, NewValue, Node0, Node).

	update(trie(Size, Node0), String, OldValue, NewValue, trie(Size, Node)) :-
		string_codes(String, Codes),
		lookup_codes(Codes, OldValue, Node0),
		update_codes(Codes, NewValue, Node0, Node).

	delete(trie(Size0, Node0), String, Value, trie(Size, Node)) :-
		string_codes(String, Codes),
		delete_codes(Codes, Value, Node0, Node),
		Size is Size0 - 1.

	lookup_prefix(Prefix, String, Value, trie(_, Node)) :-
		string_codes(Prefix, PrefixCodes),
		prefix_node(PrefixCodes, Node, PrefixNode),
		node_pair(PrefixNode, PrefixCodes, Codes, Value),
		string_codes(String, Codes).

	prefixes(trie(_, Node), Query, Pairs) :-
		string_codes(Query, Codes),
		prefix_pairs(Codes, Node, [], Pairs).

	longest_prefix(Trie, Query, Prefix, Value) :-
		prefixes(Trie, Query, Pairs),
		last(Pairs, Prefix-Value).

	delete_prefix(trie(Size0, Node0), Prefix, trie(Size, Node)) :-
		string_codes(Prefix, Codes),
		delete_prefix_codes(Codes, Node0, Node, Removed),
		Removed > 0,
		Size is Size0 - Removed.

	size(trie(Size, _), Size).

	insert_pairs([], Trie, Trie).
	insert_pairs([String-Value| Pairs], Trie0, Trie) :-
		insert(Trie0, String, Value, Trie1),
		insert_pairs(Pairs, Trie1, Trie).

	insert_codes([], Value, node(none, Edges), node(some(Value), Edges), 1) :-
		!.
	insert_codes([], Value, node(some(_), Edges), node(some(Value), Edges), 0) :-
		!.
	insert_codes([Code| Codes], Value, node(Maybe, Edges0), node(Maybe, Edges), Added) :-
		insert_edge(Edges0, Code, Codes, Value, Edges, Added).

	insert_edge([], Code, Codes, Value, [edge(Code, Node)], 1) :-
		insert_codes(Codes, Value, node(none, []), Node, _).
	insert_edge([edge(Code0, Node0)| Edges0], Code, Codes, Value, Edges, Added) :-
		compare(Order, Code, Code0),
		insert_edge(Order, Code0, Node0, Edges0, Code, Codes, Value, Edges, Added).

	insert_edge(=, Code, Node0, Edges, _, Codes, Value, [edge(Code, Node)| Edges], Added) :-
		insert_codes(Codes, Value, Node0, Node, Added).
	insert_edge(<, Code0, Node0, Edges, Code, Codes, Value, [edge(Code, Node), edge(Code0, Node0)| Edges], 1) :-
		insert_codes(Codes, Value, node(none, []), Node, _).
	insert_edge(>, Code0, Node0, Edges0, Code, Codes, Value, [edge(Code0, Node0)| Edges], Added) :-
		insert_edge(Edges0, Code, Codes, Value, Edges, Added).

	lookup_codes([], Value, node(some(Value), _)).
	lookup_codes([Code| Codes], Value, node(_, Edges)) :-
		lookup_edge(Edges, Code, Node),
		lookup_codes(Codes, Value, Node).

	lookup_edge([edge(Code0, Node)| Edges], Code, Found) :-
		compare(Order, Code, Code0),
		lookup_edge(Order, Node, Edges, Code, Found).

	lookup_edge(=, Node, _, _, Node).
	lookup_edge(>, _, Edges, Code, Node) :-
		lookup_edge(Edges, Code, Node).

	prefix_node([], Node, Node).
	prefix_node([Code| Codes], node(_, Edges), Node) :-
		lookup_edge(Edges, Code, Child),
		prefix_node(Codes, Child, Node).

	update_codes([], Value, node(some(_), Edges), node(some(Value), Edges)).
	update_codes([Code| Codes], Value, node(Maybe, Edges0), node(Maybe, Edges)) :-
		update_edge(Edges0, Code, Codes, Value, Edges).

	update_edge([edge(Code0, Node0)| Edges0], Code, Codes, Value, [edge(Code0, Node)| Edges0]) :-
		Code == Code0,
		!,
		update_codes(Codes, Value, Node0, Node).
	update_edge([Edge| Edges0], Code, Codes, Value, [Edge| Edges]) :-
		update_edge(Edges0, Code, Codes, Value, Edges).

	delete_codes([], Value, node(some(Value), Edges), node(none, Edges)).
	delete_codes([Code| Codes], Value, node(Maybe, Edges0), node(Maybe, Edges)) :-
		delete_edge(Edges0, Code, Codes, Value, Edges).

	delete_edge([edge(Code0, Node0)| Edges0], Code, Codes, Value, Edges) :-
		compare(Order, Code, Code0),
		delete_edge(Order, Code0, Node0, Edges0, Code, Codes, Value, Edges).

	delete_edge(=, Code, Node0, Edges, _, Codes, Value, NewEdges) :-
		delete_codes(Codes, Value, Node0, Node),
		(	Node == node(none, []) ->
			NewEdges = Edges
		;	NewEdges = [edge(Code, Node)| Edges]
		).
	delete_edge(>, Code0, Node0, Edges0, Code, Codes, Value, [edge(Code0, Node0)| Edges]) :-
		delete_edge(Edges0, Code, Codes, Value, Edges).

	delete_prefix_codes([], Node, node(none, []), Removed) :-
		node_size(Node, Removed).
	delete_prefix_codes([Code| Codes], node(Maybe, Edges0), node(Maybe, Edges), Removed) :-
		delete_prefix_edge(Edges0, Code, Codes, Edges, Removed).

	delete_prefix_edge([edge(Code0, Node0)| Edges0], Code, Codes, Edges, Removed) :-
		compare(Order, Code, Code0),
		delete_prefix_edge(Order, Code0, Node0, Edges0, Code, Codes, Edges, Removed).

	delete_prefix_edge(=, Code, Node0, Edges, _, Codes, NewEdges, Removed) :-
		delete_prefix_codes(Codes, Node0, Node, Removed),
		(	Node == node(none, []) ->
			NewEdges = Edges
		;	NewEdges = [edge(Code, Node)| Edges]
		).
	delete_prefix_edge(>, Code0, Node0, Edges0, Code, Codes, [edge(Code0, Node0)| Edges], Removed) :-
		delete_prefix_edge(Edges0, Code, Codes, Edges, Removed).

	node_size(node(Maybe, Edges), Size) :-
		maybe_size(Maybe, Size0),
		edges_size(Edges, Size0, Size).

	maybe_size(none, 0).
	maybe_size(some(_), 1).

	edges_size([], Size, Size).
	edges_size([edge(_, Node)| Edges], Size0, Size) :-
		node_size(Node, NodeSize),
		Size1 is Size0 + NodeSize,
		edges_size(Edges, Size1, Size).

	prefix_pairs(Codes, Node, PrefixCodes, Pairs) :-
		node_prefix_pair(Node, PrefixCodes, Pairs, Pairs0),
		prefix_pairs_codes(Codes, Node, PrefixCodes, Pairs0).

	node_prefix_pair(node(none, _), _, Pairs, Pairs) :-
		!.
	node_prefix_pair(node(some(Value), _), Codes, [String-Value| Pairs], Pairs) :-
		string_codes(String, Codes),
		!.

	prefix_pairs_codes([], _, _, []).
	prefix_pairs_codes([Code| Codes], node(_, Edges), Prefix, Pairs) :-
		(	lookup_edge(Edges, Code, Child) ->
			append(Prefix, [Code], ChildPrefix),
			prefix_pairs(Codes, Child, ChildPrefix, Pairs)
		;	Pairs = []
		).

	node_pairs(Node, Prefix, Pairs0, Pairs) :-
		node_pair_prefix(Node, Prefix, Pairs0, Pairs1),
		node_edges_pairs(Node, Prefix, Pairs1, Pairs).

	node_pair_prefix(node(none, _), _, Pairs, Pairs) :-
		!.
	node_pair_prefix(node(some(Value), _), Codes, [String-Value| Pairs], Pairs) :-
		string_codes(String, Codes),
		!.

	node_edges_pairs(node(_, Edges), Prefix, Pairs0, Pairs) :-
		edges_pairs(Edges, Prefix, Pairs0, Pairs).

	edges_pairs([], _, Pairs, Pairs).
	edges_pairs([edge(Code, Node)| Edges], Prefix, Pairs0, Pairs) :-
		append(Prefix, [Code], ChildPrefix),
		node_pairs(Node, ChildPrefix, Pairs0, Pairs1),
		edges_pairs(Edges, Prefix, Pairs1, Pairs).

	node_pair(node(some(Value), _), Codes, Codes, Value).
	node_pair(node(_, Edges), Prefix, Codes, Value) :-
		edge_pair(Edges, Prefix, Codes, Value).

	edge_pair([edge(Code, Node)| _], Prefix, Codes, Value) :-
		append(Prefix, [Code], ChildPrefix),
		node_pair(Node, ChildPrefix, Codes, Value).
	edge_pair([_| Edges], Prefix, Codes, Value) :-
		edge_pair(Edges, Prefix, Codes, Value).

:- end_object.

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


:- object(c45).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-14,
		comment is 'C4.5 decision tree learning algorithm. Builds a decision tree from a dataset object implementing the ``dataset_protocol`` protocol and provides predicates for exporting the learned tree as a list of predicate clauses or to a file.',
		remarks is [
			'Algorithm' - 'C4.5 is an extension of the ID3 algorithm that uses information gain ratio instead of information gain for attribute selection, which avoids bias towards attributes with many values.',
			'Tree representation' - 'The learned decision tree is represented as ``leaf(Class)`` for leaf nodes and ``tree(Attribute, Subtrees)`` for internal nodes, where ``Subtrees`` is a list of ``Value-Subtree`` pairs.',
			'Export format' - 'The tree can be exported as a list of Prolog/Logtalk clauses of the form ``Class(AttributeValue1, AttributeValue2, ...)``.'
		],
		see_also is [dataset_protocol]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -tree), one).
	:- info(learn/2, [
		comment is 'Learns a decision tree from the given dataset object.',
		argnames is ['Dataset', 'Tree']
	]).

	:- public(tree_to_clauses/4).
	:- mode(tree_to_clauses(+object_identifier, +tree, +callable, -list(clause)), one).
	:- info(tree_to_clauses/4, [
		comment is 'Converts a decision tree into a list of predicate clauses. ``Functor`` is the functor for the generated clauses. Each clause has the form ``Functor(AttributeValue1, ..., AttributeValueN, Class)``.',
		argnames is ['Dataset', 'Tree', 'Functor', 'Clauses']
	]).

	:- public(tree_to_file/4).
	:- mode(tree_to_file(+object_identifier, +tree, +callable, +atom), one).
	:- info(tree_to_file/4, [
		comment is 'Exports a decision tree as predicate clauses to a file. ``Functor`` is the functor for the generated clauses.',
		argnames is ['Dataset', 'Tree', 'Functor', 'File']
	]).

	:- public(print_tree/1).
	:- mode(print_tree(+tree), one).
	:- info(print_tree/1, [
		comment is 'Prints a decision tree to the current output stream in a human-readable format.',
		argnames is ['Tree']
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, msort/2, nth1/3,
		sort/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(numberlist, [
		sum/2, max/2
	]).

	:- uses(format, [
		format/2, format/3
	]).

	% learn/2 - main entry point
	learn(Dataset, Tree) :-
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		findall(
			Id-Class-AVs,
			Dataset::example(Id, Class, AVs),
			Examples
		),
		build_tree(Examples, AttributeNames, Attributes, Tree).

	% build_tree/4 - recursive tree construction
	build_tree([], _, _, leaf(unknown)) :-
		!.
	build_tree(Examples, _, _, leaf(Class)) :-
		all_same_class(Examples, Class),
		!.
	build_tree(Examples, [], _, leaf(Class)) :-
		!,
		majority_class(Examples, Class).
	build_tree(Examples, AttributeNames, Attributes, Tree) :-
		best_attribute(Examples, AttributeNames, Attributes, BestAttribute),
		(	BestAttribute == none ->
			majority_class(Examples, Class),
			Tree = leaf(Class)
		;	memberchk(BestAttribute-Values, Attributes),
			build_subtrees(Values, BestAttribute, Examples, AttributeNames, Attributes, Subtrees),
			Tree = tree(BestAttribute, Subtrees)
		).


	% build_subtrees/6 - build subtree for each value of the best attribute
	build_subtrees([], _, _, _, _, []).
	build_subtrees([Value| Values], Attribute, Examples, AttributeNames, Attributes, [Value-Subtree| Subtrees]) :-
		filter_examples(Examples, Attribute, Value, Filtered),
		select_attribute(AttributeNames, Attribute, RemainingNames),
		(	Filtered == [] ->
			majority_class(Examples, Class),
			Subtree = leaf(Class)
		;	build_tree(Filtered, RemainingNames, Attributes, Subtree)
		),
		build_subtrees(Values, Attribute, Examples, AttributeNames, Attributes, Subtrees).

	% filter_examples/4 - filter examples by attribute value
	filter_examples([], _, _, []).
	filter_examples([Id-Class-AVs| Examples], Attribute, Value, Filtered) :-
		(	member(Attribute-Value, AVs) ->
			Filtered = [Id-Class-AVs| Rest]
		;	Filtered = Rest
		),
		filter_examples(Examples, Attribute, Value, Rest).

	% select_attribute/3 - remove an attribute from the list
	select_attribute([], _, []).
	select_attribute([Attr| Attrs], Attr, Rest) :-
		!,
		select_attribute(Attrs, Attr, Rest).
	select_attribute([Attr| Attrs], Target, [Attr| Rest]) :-
		select_attribute(Attrs, Target, Rest).

	% all_same_class/2 - check if all examples have the same class
	all_same_class([_-Class-_], Class).
	all_same_class([_-Class-_| Rest], Class) :-
		all_same_class(Rest, Class).

	% majority_class/2 - find the most common class
	majority_class(Examples, Class) :-
		findall(C, member(_-C-_, Examples), Classes),
		msort(Classes, Sorted),
		count_occurrences(Sorted, Counts),
		max_count(Counts, Class).

	count_occurrences([], []).
	count_occurrences([X| Xs], [X-Count| Counts]) :-
		count_same(Xs, X, 1, Count, Rest),
		count_occurrences(Rest, Counts).

	count_same([], _, Count, Count, []).
	count_same([X| Xs], X, Count0, Count, Rest) :-
		!,
		Count1 is Count0 + 1,
		count_same(Xs, X, Count1, Count, Rest).
	count_same([Y| Xs], _, Count, Count, [Y| Xs]).

	max_count([Class-Count], Class) :-
		Count > 0.
	max_count([C1-N1, C2-N2| Rest], Class) :-
		(	N1 >= N2 ->
			max_count([C1-N1| Rest], Class)
		;	max_count([C2-N2| Rest], Class)
		).


	% best_attribute/4 - select the attribute with the highest gain ratio
	best_attribute(Examples, AttributeNames, Attributes, BestAttribute) :-
		length(Examples, Total),
		(	Total =:= 0 ->
			BestAttribute = none
		;	entropy(Examples, Total, BaseEntropy),
			compute_gain_ratios(AttributeNames, Attributes, Examples, Total, BaseEntropy, GainRatios),
			select_best(GainRatios, BestAttribute)
		).

	% entropy/3 - compute entropy of a set of examples
	entropy(Examples, Total, Entropy) :-
		findall(C, member(_-C-_, Examples), Classes),
		msort(Classes, Sorted),
		count_occurrences(Sorted, Counts),
		compute_entropy(Counts, Total, 0.0, Entropy).

	compute_entropy([], _, Entropy, Entropy).
	compute_entropy([_-Count| Counts], Total, Acc, Entropy) :-
		(	Count =:= 0 ->
			compute_entropy(Counts, Total, Acc, Entropy)
		;	P is Count / Total,
			LogP is log(P) / log(2),
			Acc1 is Acc - P * LogP,
			compute_entropy(Counts, Total, Acc1, Entropy)
		).

	% compute_gain_ratios/6 - compute gain ratio for each attribute
	compute_gain_ratios([], _, _, _, _, []).
	compute_gain_ratios([Attr| Attrs], Attributes, Examples, Total, BaseEntropy, [Attr-GainRatio| GainRatios]) :-
		member(Attr-Values, Attributes),
		!,
		information_gain(Values, Attr, Examples, Total, BaseEntropy, Gain),
		split_info(Values, Attr, Examples, Total, SplitInfo),
		(	SplitInfo > 0.0 ->
			GainRatio is Gain / SplitInfo
		;	GainRatio is 0.0
		),
		compute_gain_ratios(Attrs, Attributes, Examples, Total, BaseEntropy, GainRatios).
	compute_gain_ratios([_| Attrs], Attributes, Examples, Total, BaseEntropy, GainRatios) :-
		compute_gain_ratios(Attrs, Attributes, Examples, Total, BaseEntropy, GainRatios).

	% information_gain/6 - compute information gain for an attribute
	information_gain(Values, Attr, Examples, Total, BaseEntropy, Gain) :-
		weighted_entropy(Values, Attr, Examples, Total, 0.0, WeightedEntropy),
		Gain is BaseEntropy - WeightedEntropy.

	weighted_entropy([], _, _, _, WE, WE).
	weighted_entropy([Value| Values], Attr, Examples, Total, Acc, WE) :-
		filter_examples(Examples, Attr, Value, Filtered),
		length(Filtered, SubTotal),
		(	SubTotal =:= 0 ->
			weighted_entropy(Values, Attr, Examples, Total, Acc, WE)
		;	entropy(Filtered, SubTotal, SubEntropy),
			Acc1 is Acc + (SubTotal / Total) * SubEntropy,
			weighted_entropy(Values, Attr, Examples, Total, Acc1, WE)
		).

	% split_info/5 - compute split information for gain ratio
	split_info(Values, Attr, Examples, Total, SplitInfo) :-
		split_info_acc(Values, Attr, Examples, Total, 0.0, SplitInfo).

	split_info_acc([], _, _, _, SI, SI).
	split_info_acc([Value| Values], Attr, Examples, Total, Acc, SI) :-
		filter_examples(Examples, Attr, Value, Filtered),
		length(Filtered, SubTotal),
		(	SubTotal =:= 0 ->
			split_info_acc(Values, Attr, Examples, Total, Acc, SI)
		;	P is SubTotal / Total,
			LogP is log(P) / log(2),
			Acc1 is Acc - P * LogP,
			split_info_acc(Values, Attr, Examples, Total, Acc1, SI)
		).

	% select_best/2 - select attribute with highest gain ratio
	select_best([], none).
	select_best([Attr-GR| Rest], Best) :-
		select_best(Rest, Attr, GR, Best).

	select_best([], Best, _, Best).
	select_best([Attr-GR| Rest], _CurrentBest, CurrentGR, Best) :-
		GR > CurrentGR,
		!,
		select_best(Rest, Attr, GR, Best).
	select_best([_| Rest], CurrentBest, CurrentGR, Best) :-
		select_best(Rest, CurrentBest, CurrentGR, Best).


	% tree_to_clauses/4 - convert tree to list of clauses
	tree_to_clauses(Dataset, Tree, Functor, Clauses) :-
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		tree_to_clauses_(Tree, Functor, AttributeNames, [], Clauses).

	tree_to_clauses_(leaf(Class), Functor, AttributeNames, Bindings, [Clause]) :-
		build_clause(Functor, AttributeNames, Bindings, Class, Clause).
	tree_to_clauses_(tree(Attribute, Subtrees), Functor, AttributeNames, Bindings, Clauses) :-
		subtrees_to_clauses(Subtrees, Attribute, Functor, AttributeNames, Bindings, Clauses).

	subtrees_to_clauses([], _, _, _, _, []).
	subtrees_to_clauses([Value-Subtree| Subtrees], Attribute, Functor, AttributeNames, Bindings, Clauses) :-
		tree_to_clauses_(Subtree, Functor, AttributeNames, [Attribute-Value| Bindings], Clauses1),
		subtrees_to_clauses(Subtrees, Attribute, Functor, AttributeNames, Bindings, Clauses2),
		append(Clauses1, Clauses2, Clauses).

	build_clause(Functor, AttributeNames, Bindings, Class, Clause) :-
		build_args(AttributeNames, Bindings, Args),
		append(Args, [Class], AllArgs),
		Clause =.. [Functor| AllArgs].

	build_args([], _, []).
	build_args([Attr| Attrs], Bindings, [Value| Values]) :-
		(	member(Attr-Value, Bindings) ->
			true
		;	Value = _
		),
		build_args(Attrs, Bindings, Values).

	% tree_to_file/4 - export tree as clauses to a file
	tree_to_file(Dataset, Tree, Functor, File) :-
		tree_to_clauses(Dataset, Tree, Functor, Clauses),
		open(File, write, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	% print_tree/1 - pretty print the tree
	print_tree(Tree) :-
		print_tree(Tree, 0).

	print_tree(leaf(Class), Indent) :-
		Spaces is Indent * 2,
		format('~*c=> ~w~n', [Spaces, 32, Class]).
	print_tree(tree(Attribute, Subtrees), Indent) :-
		print_subtrees(Subtrees, Attribute, Indent).

	print_subtrees([], _, _).
	print_subtrees([Value-Subtree| Subtrees], Attribute, Indent) :-
		Spaces is Indent * 2,
		format('~*c~w = ~w:~n', [Spaces, 32, Attribute, Value]),
		Indent1 is Indent + 1,
		print_tree(Subtree, Indent1),
		print_subtrees(Subtrees, Attribute, Indent).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

:- end_object.

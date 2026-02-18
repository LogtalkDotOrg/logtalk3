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


:- object(c45,
	implements(classifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-16,
		comment is 'C4.5 decision tree learning algorithm. Builds a decision tree from a dataset object implementing the ``dataset_protocol`` protocol and provides predicates for exporting the learned tree as a list of predicate clauses or to a file. Supports both discrete and continuous attributes, handles missing values, and supports tree pruning.',
		remarks is [
			'Algorithm' - 'C4.5 is an extension of the ID3 algorithm that uses information gain ratio instead of information gain for attribute selection, which avoids bias towards attributes with many values.',
			'Discrete attributes' - 'The learned decision tree is represented as ``leaf(Class)`` for leaf nodes and ``tree(Attribute, Subtrees)`` for internal nodes with discrete attributes, where ``Subtrees`` is a list of ``Value-Subtree`` pairs.',
			'Continuous attributes' - 'For continuous (numeric) attributes, the tree uses binary threshold splits represented as ``tree(Attribute, threshold(Threshold), LeftSubtree, RightSubtree)`` where ``LeftSubtree`` corresponds to values ``=< Threshold`` and ``RightSubtree`` to values ``> Threshold``.',
			'Missing values' - 'Missing attribute values are represented using anonymous variables. During tree construction, examples with missing values for the split attribute are distributed to all branches. Entropy and gain calculations use only examples with known values for the attribute being evaluated.',
			'Tree pruning' - 'The ``prune/3`` and ``prune/5`` predicates implement pessimistic error pruning (PEP), which estimates error rates using the upper confidence bound of the binomial distribution (Wilson score interval) with a configurable confidence factor (default 0.25, range ``(0.0, 1.0)``) and minimum instances per leaf (default 2). Subtrees are replaced with leaf nodes when doing so would not increase the estimated error.'
		],
		see_also is [dataset_protocol, knn, naive_bayes, nearest_centroid, random_forest]
	]).

	:- public(prune/5).
	:- mode(prune(+object_identifier, +tree, +float, +positive_integer, -tree), one).
	:- info(prune/5, [
		comment is 'Prunes a decision tree using pessimistic error pruning (PEP). This post-pruning method estimates error rates using the upper confidence bound of the binomial distribution with the given confidence factor and replaces subtrees with leaf nodes when doing so would not increase the estimated error. Pruning helps reduce overfitting and can improve generalization to unseen data.',
		argnames is ['Dataset', 'Tree', 'ConfidenceFactor', 'MinInstances', 'PrunedTree'],
		remarks is [
			'Confidence factor' - 'The confidence factor controls the aggressiveness of pruning. It must be in the range ``(0.0, 1.0)``. Lower values result in more aggressive pruning (smaller, simpler trees), while higher values result in less pruning (larger, more complex trees). The default value is ``0.25``.',
			'Minimum instances per leaf' - 'The minimum number of instances required at a leaf node. When a node has fewer instances than this value, the node may be pruned. It must be a positive integer. The default value is ``2``.',
			'Statistical basis' - 'The pruning uses the upper confidence bound of the binomial distribution to estimate the true error rate.'
		]
	]).

	:- public(prune/3).
	:- mode(prune(+object_identifier, +tree, -tree), one).
	:- info(prune/3, [
		comment is 'Prunes a decision tree using pessimistic error pruning (PEP) with default parameter values. Calls ``prune/5`` with ``ConfidenceFactor = 0.25`` and ``MinInstances = 2``.',
		argnames is ['Dataset', 'Tree', 'PrunedTree'],
		remarks is [
			'Default parameters' - 'Uses the standard C4.5 default values: confidence factor of ``0.25`` (the confidence level for computing the upper bound of the error estimate) and minimum instances per leaf of ``2``.'
		]
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, msort/2
	]).

	:- uses(numberlist, [
		min/2, max/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	learn(Dataset, Tree) :-
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		findall(
			Id-Class-AVs,
			Dataset::example(Id, Class, AVs),
			Examples
		),
		build_tree(Examples, AttributeNames, Attributes, Tree).

	predict(Tree, Instance, Class) :-
		predict_tree(Tree, Instance, Class).

	predict_tree(leaf(Class), _, Class).
	predict_tree(tree(Attribute, threshold(Threshold), LeftTree, RightTree), Instance, Class) :-
		!,
		memberchk(Attribute-Value, Instance),
		(	var(Value) ->
			% Missing value: use majority voting from both branches
			predict_tree(LeftTree, Instance, LeftClass),
			predict_tree(RightTree, Instance, RightClass),
			majority_vote([LeftClass, RightClass], Class)
		;	Value =< Threshold ->
			predict_tree(LeftTree, Instance, Class)
		;	predict_tree(RightTree, Instance, Class)
		).
	predict_tree(tree(Attribute, Subtrees), Instance, Class) :-
		memberchk(Attribute-Value, Instance),
		(	var(Value) ->
			% Missing value: use majority voting from all subtrees
			findall(C, (member(_-Subtree, Subtrees), predict_tree(Subtree, Instance, C)), Classes),
			majority_vote(Classes, Class)
		;	memberchk(Value-Subtree, Subtrees),
			predict_tree(Subtree, Instance, Class)
		).

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
		;	BestAttribute = threshold(Attr, Threshold),
			Threshold == none ->
			% Continuous attribute with no valid threshold (all values identical)
			majority_class(Examples, Class),
			Tree = leaf(Class)
		;	BestAttribute = threshold(Attr, Threshold) ->
			% Continuous attribute with binary threshold split
			filter_examples_threshold(Examples, Attr, Threshold, Left, Right),
			Tree = tree(Attr, threshold(Threshold), LeftTree, RightTree),
			(	Left == [] ->
				majority_class(Examples, ClassL),
				LeftTree = leaf(ClassL)
			;	build_tree(Left, AttributeNames, Attributes, LeftTree)
			),
			(	Right == [] ->
				majority_class(Examples, ClassR),
				RightTree = leaf(ClassR)
			;	build_tree(Right, AttributeNames, Attributes, RightTree)
			)
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
	% Examples with missing values (uninstantiated) for the attribute
	% are included in all branches
	filter_examples([], _, _, []).
	filter_examples([Id-Class-AVs| Examples], Attribute, Value, Filtered) :-
		(	member(Attribute-AV, AVs),
			var(AV) ->
			% Missing value: include in all branches
			Filtered = [Id-Class-AVs| Rest]
		;	member(Attribute-Value, AVs) ->
			Filtered = [Id-Class-AVs| Rest]
		;	Filtered = Rest
		),
		filter_examples(Examples, Attribute, Value, Rest).

	% filter_examples_threshold/5 - split examples by threshold on a continuous attribute
	% Examples with missing values for the attribute are included in both branches
	filter_examples_threshold([], _, _, [], []).
	filter_examples_threshold([Id-Class-AVs| Examples], Attr, Threshold, Left, Right) :-
		memberchk(Attr-Value, AVs),
		(	var(Value) ->
			% Missing value: include in both branches
			Left = [Id-Class-AVs| LeftRest],
			Right = [Id-Class-AVs| RightRest]
		;	Value =< Threshold ->
			Left = [Id-Class-AVs| LeftRest],
			Right = RightRest
		;	Left = LeftRest,
			Right = [Id-Class-AVs| RightRest]
		),
		filter_examples_threshold(Examples, Attr, Threshold, LeftRest, RightRest).

	% select_attribute/3 - remove an attribute from the list
	select_attribute([], _, []).
	select_attribute([Attr| Attrs], Attr, Rest) :-
		!,
		select_attribute(Attrs, Attr, Rest).
	select_attribute([Attr| Attrs], Target, [Attr| Rest]) :-
		select_attribute(Attrs, Target, Rest).

	% all_same_class/2 - check if all examples have the same class
	all_same_class([_-Class-_], Class) :-
		!.
	all_same_class([_-Class-_| Rest], Class) :-
		all_same_class(Rest, Class).

	% majority_vote/2 - find the most common class in a list
	majority_vote([Class], Class) :-
		!.
	majority_vote(Classes, MajorityClass) :-
		msort(Classes, Sorted),
		count_occurrences(Sorted, Counts),
		max_count(Counts, MajorityClass).

	% majority_class/2 - find the most common class in the examples
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
		!,
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
	% Uses only examples with known values for the attribute being evaluated
	compute_gain_ratios([], _, _, _, _, []).
	compute_gain_ratios([Attr| Attrs], Attributes, Examples, Total, BaseEntropy, [BestKey-GainRatio| GainRatios]) :-
		member(Attr-Values, Attributes),
		!,
		(	Values == continuous ->
			% Continuous attribute: find best threshold
			best_threshold(Attr, Examples, Total, BaseEntropy, GainRatio, Threshold),
			BestKey = threshold(Attr, Threshold)
		;	% Discrete attribute
			known_examples(Examples, Attr, KnownExamples),
			length(KnownExamples, KnownTotal),
			(	KnownTotal =:= 0 ->
				GainRatio is 0.0
			;	entropy(KnownExamples, KnownTotal, KnownEntropy),
				information_gain(Values, Attr, KnownExamples, KnownTotal, KnownEntropy, Gain),
				split_info(Values, Attr, KnownExamples, KnownTotal, SplitInfo),
				(	SplitInfo > 0.0 ->
					GainRatio is Gain / SplitInfo
				;	GainRatio is 0.0
				)
			),
			BestKey = Attr
		),
		compute_gain_ratios(Attrs, Attributes, Examples, Total, BaseEntropy, GainRatios).
	compute_gain_ratios([_| Attrs], Attributes, Examples, Total, BaseEntropy, GainRatios) :-
		compute_gain_ratios(Attrs, Attributes, Examples, Total, BaseEntropy, GainRatios).

	% known_examples/3 - filter examples to only those with known (non-missing) values for an attribute
	known_examples([], _, []).
	known_examples([Id-Class-AVs| Examples], Attr, Known) :-
		(	member(Attr-Value, AVs),
			nonvar(Value) ->
			Known = [Id-Class-AVs| Rest]
		;	Known = Rest
		),
		known_examples(Examples, Attr, Rest).

	% best_threshold/6 - find the best threshold for a continuous attribute
	% Only considers examples with known (non-missing) values for the attribute
	best_threshold(Attr, Examples, Total, BaseEntropy, BestGainRatio, BestThreshold) :-
		findall(V, (member(_-_-AVs, Examples), member(Attr-V, AVs), nonvar(V)), AllValues),
		sort(AllValues, Sorted),
		candidate_thresholds(Sorted, Candidates),
		evaluate_thresholds(Candidates, Attr, Examples, Total, BaseEntropy, 0.0, none, BestGainRatio, BestThreshold).

	% candidate_thresholds/2 - generate midpoints between consecutive sorted values
	candidate_thresholds([], []).
	candidate_thresholds([_], []) :-
		!.
	candidate_thresholds([V1, V2| Vs], [Mid| Mids]) :-
		Mid is (V1 + V2) / 2,
		candidate_thresholds([V2| Vs], Mids).

	% evaluate_thresholds/9 - find threshold with highest gain ratio
	evaluate_thresholds([], _, _, _, _, BestGR, BestT, BestGR, BestT).
	evaluate_thresholds([T| Ts], Attr, Examples, Total, BaseEntropy, CurrentGR, CurrentT, BestGR, BestT) :-
		threshold_gain_ratio(T, Attr, Examples, Total, BaseEntropy, GR),
		(	GR > CurrentGR ->
			evaluate_thresholds(Ts, Attr, Examples, Total, BaseEntropy, GR, T, BestGR, BestT)
		;	evaluate_thresholds(Ts, Attr, Examples, Total, BaseEntropy, CurrentGR, CurrentT, BestGR, BestT)
		).

	% threshold_gain_ratio/6 - compute gain ratio for a binary split at threshold
	threshold_gain_ratio(Threshold, Attr, Examples, Total, BaseEntropy, GainRatio) :-
		filter_examples_threshold(Examples, Attr, Threshold, Left, Right),
		length(Left, NLeft),
		length(Right, NRight),
		(	NLeft =:= 0 ->
			GainRatio is 0.0
		;	NRight =:= 0 ->
			GainRatio is 0.0
		;	entropy(Left, NLeft, LeftEntropy),
			entropy(Right, NRight, RightEntropy),
			WeightedEntropy is (NLeft / Total) * LeftEntropy + (NRight / Total) * RightEntropy,
			Gain is BaseEntropy - WeightedEntropy,
			PLeft is NLeft / Total,
			PRight is NRight / Total,
			SplitInfo is -(PLeft * log(PLeft) / log(2)) - (PRight * log(PRight) / log(2)),
			(	SplitInfo > 0.0 ->
				GainRatio is Gain / SplitInfo
			;	GainRatio is 0.0
			)
		).

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

	% classifier_to_clauses/4 - convert tree to list of clauses
	classifier_to_clauses(Dataset, Tree, Functor, Clauses) :-
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		tree_to_clauses_(Tree, Functor, AttributeNames, [], Clauses).

	tree_to_clauses_(leaf(Class), Functor, AttributeNames, Bindings, [Clause]) :-
		build_clause(Functor, AttributeNames, Bindings, Class, Clause).
	tree_to_clauses_(tree(Attribute, threshold(Threshold), LeftTree, RightTree), Functor, AttributeNames, Bindings, Clauses) :-
		tree_to_clauses_(LeftTree, Functor, AttributeNames, [Attribute-(=<(Threshold))| Bindings], LeftClauses),
		tree_to_clauses_(RightTree, Functor, AttributeNames, [Attribute-(>(Threshold))| Bindings], RightClauses),
		append(LeftClauses, RightClauses, Clauses).
	tree_to_clauses_(tree(Attribute, Subtrees), Functor, AttributeNames, Bindings, Clauses) :-
		subtrees_to_clauses(Subtrees, Attribute, Functor, AttributeNames, Bindings, Clauses).

	subtrees_to_clauses([], _, _, _, _, []).
	subtrees_to_clauses([Value-Subtree| Subtrees], Attribute, Functor, AttributeNames, Bindings, Clauses) :-
		tree_to_clauses_(Subtree, Functor, AttributeNames, [Attribute-Value| Bindings], Clauses1),
		subtrees_to_clauses(Subtrees, Attribute, Functor, AttributeNames, Bindings, Clauses2),
		append(Clauses1, Clauses2, Clauses).

	build_clause(Functor, AttributeNames, Bindings, Class, Clause) :-
		build_args(AttributeNames, Bindings, Args, Goals),
		append(Args, [Class], AllArgs),
		Head =.. [Functor| AllArgs],
		(	Goals == [] ->
			Clause = Head
		;	list_to_conjunction(Goals, Body),
			Clause = (Head :- Body)
		).

	build_args([], _, [], []).
	build_args([Attr| Attrs], Bindings, [Arg| Args], Goals) :-
		findall(T, member(Attr-(=<(T)), Bindings), LEThresholds),
		findall(T, member(Attr-(>(T)), Bindings), GTThresholds),
		(	(LEThresholds \== [] ; GTThresholds \== []) ->
			make_simplified_goals(LEThresholds, GTThresholds, Arg, AttrGoals),
			append(AttrGoals, RestGoals, Goals)
		;	memberchk(Attr-Arg, Bindings) ->
			Goals = RestGoals
		;	Goals = RestGoals
		),
		build_args(Attrs, Bindings, Args, RestGoals).

	% make_simplified_goals/4 - keep only the most restrictive threshold for each direction
	% For =< constraints: keep the minimum (most restrictive upper bound)
	% For > constraints: keep the maximum (most restrictive lower bound)
	make_simplified_goals(LEThresholds, GTThresholds, Var, Goals) :-
		(	LEThresholds == [] ->
			LEGoals = []
		;	min(LEThresholds, MinLE),
			LEGoals = [Var =< MinLE]
		),
		(	GTThresholds == [] ->
			GTGoals = []
		;	max(GTThresholds, MaxGT),
			GTGoals = [Var > MaxGT]
		),
		append(LEGoals, GTGoals, Goals).

	list_to_conjunction([Goal], Goal) :-
		!.
	list_to_conjunction([Goal| Goals], (Goal, Rest)) :-
		list_to_conjunction(Goals, Rest).

	% tree_to_file/4 - export tree as clauses to a file
	classifier_to_file(Dataset, Tree, Functor, File) :-
		classifier_to_clauses(Dataset, Tree, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Dataset, Functor, Stream) :-
		Dataset::class(Class),
		findall(
			Attribute,
			Dataset::attribute_values(Attribute, _),
			Arguments,
			[Class]
		),
		title_case(Arguments, TitleCaseArguments),
		Template =.. [Functor| TitleCaseArguments],
		format(Stream, '% ~q~n', [Template]).

	% assumes ASCII attribute names
	title_case([], []).
	title_case([Attribute| Attributes], [TitleCaseAttribute| TitleCaseAttributes]) :-
		atom_codes(Attribute, [Letter| Letters]),
		(	0'a @=< Letter, Letter @=< 0'z ->
			UpperCaseLetter is Letter - 32,
			atom_codes(TitleCaseAttribute, [UpperCaseLetter| Letters])
		;	TitleCaseAttribute = Attribute
		),
		title_case(Attributes, TitleCaseAttributes).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	% print_classifier/1 - pretty print the tree
	print_classifier(Tree) :-
		print_classifier(Tree, 0).

	print_classifier(leaf(Class), Indent) :-
		Spaces is Indent * 2,
		format('~*c=> ~w~n', [Spaces, 32, Class]).
	print_classifier(tree(Attribute, threshold(Threshold), LeftTree, RightTree), Indent) :-
		Spaces is Indent * 2,
		Indent1 is Indent + 1,
		format('~*c~w =< ~w:~n', [Spaces, 32, Attribute, Threshold]),
		print_classifier(LeftTree, Indent1),
		format('~*c~w > ~w:~n', [Spaces, 32, Attribute, Threshold]),
		print_classifier(RightTree, Indent1).
	print_classifier(tree(Attribute, Subtrees), Indent) :-
		print_subtrees(Subtrees, Attribute, Indent).

	print_subtrees([], _, _).
	print_subtrees([Value-Subtree| Subtrees], Attribute, Indent) :-
		Spaces is Indent * 2,
		format('~*c~w = ~w:~n', [Spaces, 32, Attribute, Value]),
		Indent1 is Indent + 1,
		print_classifier(Subtree, Indent1),
		print_subtrees(Subtrees, Attribute, Indent).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	% prune/5 - pessimistic error pruning with configurable parameters
	% Prunes the tree by comparing error estimates of subtrees vs. leaf replacement
	prune(Dataset, Tree, ConfidenceFactor, MinInstances, PrunedTree) :-
		% Convert confidence factor to z-score using inverse normal approximation
		confidence_to_zscore(ConfidenceFactor, ZScore),
		findall(
			Id-Class-AVs,
			Dataset::example(Id, Class, AVs),
			Examples
		),
		prune_tree(Tree, ZScore, MinInstances, Examples, PrunedTree).

	% prune/3 - pessimistic error pruning with default parameters
	prune(Dataset, Tree, PrunedTree) :-
		prune(Dataset, Tree, 0.25, 2, PrunedTree).

	% confidence_to_zscore/2 - converts confidence factor to z-score
	% Uses the Abramowitz and Stegun approximation for the inverse normal distribution
	% For confidence factor CF, we want the z-score such that P(Z <= z) = 1 - CF
	confidence_to_zscore(ConfidenceFactor, ZScore) :-
		P is 1.0 - ConfidenceFactor,
		% Handle edge cases
		(	P =< 0.0 ->
			ZScore = -10.0
		;	P >= 1.0 ->
			ZScore = 10.0
		;	P < 0.5 ->
			% For P < 0.5, we compute for 1-P and negate
			P1 is 1.0 - P,
			inverse_normal_approx(P1, Z1),
			ZScore is -Z1
		;	inverse_normal_approx(P, ZScore)
		).

	% inverse_normal_approx/2 - Abramowitz and Stegun rational approximation
	% Valid for P in (0.5, 1.0), returns positive z-score
	inverse_normal_approx(P, Z) :-
		% Constants for the approximation
		C0 = 2.515517,
		C1 = 0.802853,
		C2 = 0.010328,
		D1 = 1.432788,
		D2 = 0.189269,
		D3 = 0.001308,
		% Calculate t from P
		T is sqrt(-2.0 * log(1.0 - P)),
		% Rational approximation
		Z is T - (C0 + C1 * T + C2 * T * T) / (1.0 + D1 * T + D2 * T * T + D3 * T * T * T).

	% prune_tree/5 - recursive tree pruning (bottom-up)
	prune_tree(leaf(Class), _, _, _, leaf(Class)).
	prune_tree(tree(Attr, threshold(Threshold), Left, Right), ZScore, MinInstances, Examples, PrunedTree) :-
		% First, recursively prune subtrees
		filter_examples_threshold(Examples, Attr, Threshold, LeftExamples, RightExamples),
		prune_tree(Left, ZScore, MinInstances, LeftExamples, PrunedLeft),
		prune_tree(Right, ZScore, MinInstances, RightExamples, PrunedRight),
		% Calculate error estimate for the pruned subtree
		subtree_error(tree(Attr, threshold(Threshold), PrunedLeft, PrunedRight), ZScore, Examples, SubtreeError),
		% Calculate error estimate if we replace with a leaf
		leaf_error(Examples, ZScore, MinInstances, LeafError, MajorityClass),
		% Prune if leaf error is not worse than subtree error
		(	LeafError =< SubtreeError ->
			PrunedTree = leaf(MajorityClass)
		;	PrunedTree = tree(Attr, threshold(Threshold), PrunedLeft, PrunedRight)
		).
	prune_tree(tree(Attr, Subtrees), ZScore, MinInstances, Examples, PrunedTree) :-
		% First, recursively prune all subtrees
		prune_subtrees(Subtrees, ZScore, MinInstances, Attr, Examples, PrunedSubtrees),
		% Calculate error estimate for the pruned subtree
		subtree_error(tree(Attr, PrunedSubtrees), ZScore, Examples, SubtreeError),
		% Calculate error estimate if we replace with a leaf
		leaf_error(Examples, ZScore, MinInstances, LeafError, MajorityClass),
		% Prune if leaf error is not worse than subtree error
		(	LeafError =< SubtreeError ->
			PrunedTree = leaf(MajorityClass)
		;	PrunedTree = tree(Attr, PrunedSubtrees)
		).

	% prune_subtrees/6 - recursively prune each subtree
	prune_subtrees([], _, _, _, _, []).
	prune_subtrees([Value-Subtree| Subtrees], ZScore, MinInstances, Attr, Examples, [Value-PrunedSubtree| PrunedSubtrees]) :-
		filter_examples(Examples, Attr, Value, FilteredExamples),
		prune_tree(Subtree, ZScore, MinInstances, FilteredExamples, PrunedSubtree),
		prune_subtrees(Subtrees, ZScore, MinInstances, Attr, Examples, PrunedSubtrees).

	% leaf_error/5 - pessimistic error estimate for replacing with a leaf
	% Uses upper confidence bound of binomial distribution (C4.5 formula)
	leaf_error([], _, _, 1.0, unknown) :-
		!.
	leaf_error(Examples, ZScore, MinInstances, Error, MajorityClass) :-
		majority_class(Examples, MajorityClass),
		length(Examples, Total),
		count_errors(Examples, MajorityClass, Errors),
		% Apply minimum instances consideration
		(	Total < MinInstances ->
			% Small sample: use pessimistic estimate
			Error is 1.0
		;	upper_confidence_bound(Errors, Total, ZScore, Error)
		).

	% count_errors/3 - count examples not belonging to the given class
	count_errors([], _, 0).
	count_errors([_-Class-_| Examples], MajorityClass, Errors) :-
		count_errors(Examples, MajorityClass, RestErrors),
		(	Class == MajorityClass ->
			Errors = RestErrors
		;	Errors is RestErrors + 1
		).

	% upper_confidence_bound/4 - C4.5 upper confidence bound formula
	% Computes the upper bound of the binomial distribution confidence interval
	% Formula: UCF = (f + z²/(2N) + z*sqrt(f*(1-f)/N + z²/(4N²))) / (1 + z²/N)
	% where f = E/N is the observed error rate and z is the z-score
	upper_confidence_bound(Errors, Total, ZScore, UCB) :-
		(	Total =:= 0 ->
			UCB = 1.0
		;	Errors =:= 0 ->
			% No errors: use a minimal upper bound based on sample size
			Z2 is ZScore * ZScore,
			UCB is Z2 / (Total + Z2)
		;	F is Errors / Total,
			Z2 is ZScore * ZScore,
			% Upper confidence bound formula (Wilson score interval upper bound)
			Numerator is F + Z2 / (2 * Total) + ZScore * sqrt(F * (1 - F) / Total + Z2 / (4 * Total * Total)),
			Denominator is 1 + Z2 / Total,
			UCB is Numerator / Denominator
		).

	% subtree_error/4 - pessimistic error estimate for a subtree
	% Sums the error estimates of all leaves, weighted by proportion of examples
	subtree_error(leaf(Class), ZScore, Examples, Error) :-
		length(Examples, Total),
		(	Total =:= 0 ->
			Error is 1.0
		;	count_errors(Examples, Class, Errors),
			upper_confidence_bound(Errors, Total, ZScore, Error)
		).
	subtree_error(tree(Attr, threshold(Threshold), Left, Right), ZScore, Examples, Error) :-
		filter_examples_threshold(Examples, Attr, Threshold, LeftExamples, RightExamples),
		length(Examples, Total),
		length(LeftExamples, LeftTotal),
		length(RightExamples, RightTotal),
		(	Total =:= 0 ->
			Error is 1.0
		;	subtree_error(Left, ZScore, LeftExamples, LeftError),
			subtree_error(Right, ZScore, RightExamples, RightError),
			% Weighted sum of subtree errors
			Error is (LeftTotal * LeftError + RightTotal * RightError) / Total
		).
	subtree_error(tree(Attr, Subtrees), ZScore, Examples, Error) :-
		length(Examples, Total),
		(	Total =:= 0 ->
			Error is 1.0
		;	subtrees_error(Subtrees, Attr, ZScore, Examples, Total, 0.0, Error)
		).

	% subtrees_error/7 - accumulate weighted error from all subtrees
	subtrees_error([], _, _, _, _, Error, Error).
	subtrees_error([Value-Subtree| Subtrees], Attr, ZScore, Examples, Total, Acc, Error) :-
		filter_examples(Examples, Attr, Value, FilteredExamples),
		length(FilteredExamples, SubTotal),
		subtree_error(Subtree, ZScore, FilteredExamples, SubError),
		Acc1 is Acc + (SubTotal * SubError) / Total,
		subtrees_error(Subtrees, Attr, ZScore, Examples, Total, Acc1, Error).

:- end_object.

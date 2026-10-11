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


:- object(c45_classifier,
	imports(classifier_common)).

	:- info([
		version is 3:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'C4.5 decision tree learning algorithm. Builds a decision tree from a dataset object implementing the ``dataset_protocol`` protocol and provides predicates for exporting the learned tree as a list of predicate clauses or to a file. Supports both discrete and continuous attributes, handles missing values, and supports tree pruning.',
		see_also is [dataset_protocol, isolation_forest_anomaly_detector, knn_classifier, naive_bayes_classifier, nearest_centroid_classifier, random_forest_classifier, adaptive_boosting_classifier]
	]).

	:- public(prune/5).
	:- mode(prune(+object_identifier, +classifier, +float, +positive_integer, -classifier), one).
	:- info(prune/5, [
		comment is 'Prunes a decision tree using pessimistic error pruning (PEP). This post-pruning method estimates error rates using the upper confidence bound of the binomial distribution with the given confidence factor and replaces subtrees with leaf nodes when doing so would not increase the estimated error. Pruning helps reduce overfitting and can improve generalization to unseen data.',
		argnames is ['Dataset', 'Tree', 'ConfidenceFactor', 'MinInstances', 'PrunedTree'],
		exceptions is [
			'An input parameter or tree is a variable' - instantiation_error,
			'Confidence factor is not a float in (0.0, 0.5]' - domain_error(confidence_factor, 'ConfidenceFactor'),
			'Minimum instances is not a positive integer' - domain_error(positive_integer, 'MinInstances'),
			'Tree is invalid' - domain_error(classifier, 'Tree'),
			'Dataset is empty' - domain_error(non_empty_dataset, 'Dataset'),
			'Dataset class values are invalid' - domain_error(class_values, 'ClassValues'),
			'Dataset attribute declarations are invalid' - domain_error(attribute_declarations, 'Attribute'),
			'A required attribute binding is missing' - existence_error(attribute, 'Attribute'),
			'Other dataset validation error' - 'Exception'
		],
		remarks is [
			'Confidence factor' - 'The confidence factor must be a float in ``(0.0, 0.5]`` so that the error estimate is an upper confidence bound. Lower values increase the estimated uncertainty. The default value is ``0.25``.',
			'Minimum node population' - 'A non-empty node with weighted population below this positive integer is replaced by its majority leaf. This does not require every leaf to meet the minimum. The default value is ``2``.',
			'Statistical basis' - 'The pruning uses the upper confidence bound of the binomial distribution to estimate the true error rate.'
		]
	]).

	:- public(prune/3).
	:- mode(prune(+object_identifier, +classifier, -classifier), one).
	:- info(prune/3, [
		comment is 'Prunes a decision tree using pessimistic error pruning (PEP) with default parameter values. Calls ``prune/5`` with ``ConfidenceFactor = 0.25`` and ``MinInstances = 2``.',
		argnames is ['Dataset', 'Tree', 'PrunedTree'],
		exceptions is [
			'Tree is a variable' - instantiation_error,
			'Tree is invalid' - domain_error(classifier, 'Tree'),
			'Dataset is empty' - domain_error(non_empty_dataset, 'Dataset'),
			'Dataset class values are invalid' - domain_error(class_values, 'ClassValues'),
			'Dataset attribute declarations are invalid' - domain_error(attribute_declarations, 'Attribute'),
			'A required attribute binding is missing' - existence_error(attribute, 'Attribute'),
			'Other dataset validation error' - 'Exception'
		],
		remarks is [
			'Default parameters' - 'Uses a confidence factor of ``0.25`` and minimum weighted node population of ``2``. Existing leaves and empty branches are preserved.'
		]
	]).

	:- uses(format, [
		format/2
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

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, c45_model(Tree, Statistics), []) :-
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples(Dataset, Examples),
		weight_examples(Examples, Weighted),
		build_weighted_tree(Weighted, AttributeNames, Attributes, Tree, Statistics).

	predict(Tree, Instance, Class) :-
		predict_tree(Tree, Instance, Class).

	predict_tree(c45_model(Tree, Statistics), Instance, Class) :-
		predict_model(Tree, Statistics, Instance, Class).
	predict_tree(leaf(Class), _, Class).
	predict_tree(tree(Attribute, threshold(Threshold), LeftTree, RightTree), Instance, Class) :-
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
	build_tree(Examples, AttributeNames, Attributes, Tree) :-
		weight_examples(Examples, Weighted),
		build_weighted_tree(Weighted, AttributeNames, Attributes, Tree, _Statistics).

	build_weighted_tree([], _, _, leaf(unknown), leaf_stats([])) :-
		!.
	build_weighted_tree(Examples, _, _, leaf(Class), leaf_stats([Class-Mass])) :-
		all_same_class(Examples, Class),
		!,
		example_mass(Examples, Mass).
	build_weighted_tree(Examples, [], _, leaf(Class), leaf_stats(Counts)) :-
		!,
		weighted_class_counts(Examples, Counts),
		max_count(Counts, Class).
	build_weighted_tree(Examples, AttributeNames, Attributes, Tree, Statistics) :-
		weighted_class_counts(Examples, Counts),
		sum_counts(Counts, 0.0, Total),
		compute_entropy(Counts, Total, 0.0, BaseEntropy),
		choose_attribute(Examples, AttributeNames, Attributes, Total, BaseEntropy, BestAttribute),
		(	BestAttribute == none ->
			max_count(Counts, Class),
			Tree = leaf(Class),
			Statistics = leaf_stats(Counts)
		;	BestAttribute = threshold(Attr, Threshold) ->
			% Continuous attribute with binary threshold split
			filter_examples_threshold(Examples, Attr, Threshold, Left, Right),
			Tree = tree(Attr, threshold(Threshold), LeftTree, RightTree),
			example_mass(Left, LeftMass),
			example_mass(Right, RightMass),
			build_child_tree(Left, AttributeNames, Attributes, Counts, LeftTree, LeftStats),
			build_child_tree(Right, AttributeNames, Attributes, Counts, RightTree, RightStats),
			Statistics = threshold_stats(Counts, LeftMass, LeftStats, RightMass, RightStats)
		;	memberchk(BestAttribute-Values, Attributes),
			select_attribute(AttributeNames, BestAttribute, RemainingNames),
			build_subtrees(Values, BestAttribute, Examples, RemainingNames, Attributes, Counts, Subtrees, Branches),
			Tree = tree(BestAttribute, Subtrees),
			Statistics = discrete_stats(Counts, Branches)
		).

	build_child_tree([], _, _, Counts, leaf(Class), leaf_stats(Counts)) :-
		!,
		max_count(Counts, Class).
	build_child_tree(Examples, AttributeNames, Attributes, _, Tree, Statistics) :-
		build_weighted_tree(Examples, AttributeNames, Attributes, Tree, Statistics).

	% build_subtrees/8 - build subtree for each value of the best attribute
	build_subtrees([], _, _, _, _, _, [], []).
	build_subtrees([Value| Values], Attribute, Examples, AttributeNames, Attributes, Counts, [Value-Subtree| Subtrees], [branch(Value, Mass, Statistics)| Branches]) :-
		filter_examples(Examples, Attribute, Value, Filtered),
		example_mass(Filtered, Mass),
		build_child_tree(Filtered, AttributeNames, Attributes, Counts, Subtree, Statistics),
		build_subtrees(Values, Attribute, Examples, AttributeNames, Attributes, Counts, Subtrees, Branches).

	% filter_examples/4 - filter examples by attribute value
	filter_examples(Examples, Attribute, Value, Filtered) :-
		partition_weighted_value(Examples, Attribute, Value, 0.0, Filtered).

	% filter_examples_threshold/5 - split examples by threshold on a continuous attribute
	filter_examples_threshold(Examples, Attr, Threshold, Left, Right) :-
		partition_weighted_threshold(Examples, Attr, Threshold, Left, Right).

	% select_attribute/3 - remove an attribute from the list
	select_attribute([], _, []).
	select_attribute([Attr| Attrs], Attr, Rest) :-
		!,
		select_attribute(Attrs, Attr, Rest).
	select_attribute([Attr| Attrs], Target, [Attr| Rest]) :-
		select_attribute(Attrs, Target, Rest).

	% all_same_class/2 - check if all examples have the same class
	all_same_class([weighted_example(_, Class, _, _)], Class) :-
		!.
	all_same_class([weighted_example(_, Class, _, _)| Rest], Class) :-
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
		weighted_class_counts(Examples, Counts),
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
		example_mass(Examples, Total),
		(	Total =:= 0 ->
			BestAttribute = none
		;	entropy(Examples, Total, BaseEntropy),
			choose_attribute(Examples, AttributeNames, Attributes, Total, BaseEntropy, BestAttribute)
		).

	choose_attribute(Examples, AttributeNames, Attributes, Total, BaseEntropy, BestAttribute) :-
		compute_gain_ratios(AttributeNames, Attributes, Examples, Total, BaseEntropy, Candidates),
		candidate_gain_totals(Candidates, 0.0, 0, GainTotal, CandidateCount),
		(	CandidateCount > 0 ->
			AverageGain is GainTotal / CandidateCount
		;	AverageGain = 0.0
		),
		eligible_ratios(Candidates, AverageGain, GainRatios),
		select_best(GainRatios, BestAttribute).

	% entropy/3 - compute entropy of a set of examples
	entropy(Examples, Total, Entropy) :-
		weighted_class_counts(Examples, Counts),
		compute_entropy(Counts, Total, 0.0, Entropy).

	compute_entropy([], _, Entropy, Entropy).
	compute_entropy([_-Count| Counts], Total, Acc, Entropy) :-
		(	Count =:= 0 ->
			compute_entropy(Counts, Total, Acc, Entropy)
		;	P is Count / Total,
			(	P > 0.0,
				P < 1.0 ->
				LogP is log(P) / log(2),
				Acc1 is Acc - P * LogP
			;	Acc1 = Acc
			),
			compute_entropy(Counts, Total, Acc1, Entropy)
		).

	% compute_gain_ratios/6 - compute gain ratio for each attribute
	% Uses only examples with known values for the attribute being evaluated
	compute_gain_ratios([], _, _, _, _, []).
	compute_gain_ratios([Attr| Attrs], Attributes, Examples, Total, BaseEntropy, [candidate(BestKey, Gain, GainRatio)| GainRatios]) :-
		member(Attr-Values, Attributes),
		!,
		(	Values == continuous ->
			% Continuous attribute: find best threshold
			best_threshold(Attr, Examples, Total, BaseEntropy, Gain, GainRatio, Threshold),
			(	Threshold == none ->
				BestKey = none
			;	BestKey = threshold(Attr, Threshold)
			)
		;	discrete_candidate(Attr, Examples, Total, BestKey, Gain, GainRatio)
		),
		compute_gain_ratios(Attrs, Attributes, Examples, Total, BaseEntropy, GainRatios).
	compute_gain_ratios([_| Attrs], Attributes, Examples, Total, BaseEntropy, GainRatios) :-
		compute_gain_ratios(Attrs, Attributes, Examples, Total, BaseEntropy, GainRatios).

	% known_examples/3 - filter examples to only those with known (non-missing) values for an attribute
	known_examples([], _, []).
	known_examples([Example| Examples], Attr, Known) :-
		Example = weighted_example(_, _, AVs, _),
		(	member(Attr-Value, AVs),
			nonvar(Value) ->
			Known = [Example| Rest]
		;	Known = Rest
		),
		known_examples(Examples, Attr, Rest).

	% best_threshold/7 - find the best threshold for a continuous attribute
	% Only considers examples with known (non-missing) values for the attribute
	best_threshold(Attr, Examples, Total, _BaseEntropy, BestGain, BestGainRatio, BestThreshold) :-
		known_examples(Examples, Attr, Known),
		example_mass(Known, KnownTotal),
		weighted_class_counts(Known, RightCounts),
		compute_entropy(RightCounts, KnownTotal, 0.0, KnownEntropy),
		attribute_rows(Known, Attr, 1, Rows),
		sort(Rows, Sorted),
		group_attribute_rows(Sorted, continuous, Groups),
		zero_class_counts(RightCounts, LeftCounts),
		Context = split_context(KnownTotal, Total, KnownEntropy),
		sweep_thresholds(Groups, LeftCounts, RightCounts, 0.0, Context, best_split(-1.0, 0.0, none), Best),
		(	Best = best_split(_, _, none) ->
			BestGain = 0.0,
			BestGainRatio = 0.0,
			BestThreshold = none
		;	Best = best_split(BestGain, BestGainRatio, BestThreshold)
		).

	% candidate_thresholds/2 - generate midpoints between consecutive sorted values
	candidate_thresholds([], []).
	candidate_thresholds([_], []) :-
		!.
	candidate_thresholds([V1, V2| Vs], Thresholds) :-
		Midpoint is float(V1 / 2 + V2 / 2),
		(	Midpoint >= V1,
			Midpoint < V2 ->
			Mid = Midpoint
		;	Mid is float(V1)
		),
		(	Mid >= V1,
			Mid < V2 ->
			Thresholds = [Mid| Mids]
		;	Thresholds = Mids
		),
		candidate_thresholds([V2| Vs], Mids).

	% threshold_gain_ratio/6 - compute gain ratio for a binary split at threshold
	threshold_gain_ratio(Threshold, Attr, Examples, Total, _BaseEntropy, GainRatio) :-
		known_examples(Examples, Attr, KnownExamples),
		example_mass(KnownExamples, KnownTotal),
		filter_examples_threshold(KnownExamples, Attr, Threshold, Left, Right),
		example_mass(Left, NLeft),
		example_mass(Right, NRight),
		(	NLeft =:= 0 ->
			GainRatio is 0.0
		;	NRight =:= 0 ->
			GainRatio is 0.0
		;	entropy(KnownExamples, KnownTotal, KnownEntropy),
			entropy(Left, NLeft, LeftEntropy),
			entropy(Right, NRight, RightEntropy),
			WeightedEntropy is (NLeft / KnownTotal) * LeftEntropy + (NRight / KnownTotal) * RightEntropy,
			Gain is (KnownTotal / Total) * (KnownEntropy - WeightedEntropy),
			PLeft is NLeft / Total,
			PRight is NRight / Total,
			SplitInfo0 is -(PLeft * log(PLeft) / log(2)) - (PRight * log(PRight) / log(2)),
			missing_split_info(KnownTotal, Total, SplitInfo0, SplitInfo),
			(	SplitInfo > 0.0 ->
				GainRatio is Gain / SplitInfo
			;	GainRatio is 0.0
			)
		).

	missing_split_info(KnownTotal, Total, KnownInfo, SplitInfo) :-
		(	Total > KnownTotal ->
			Missing is Total - KnownTotal,
			compute_entropy([missing-Missing], Total, KnownInfo, SplitInfo)
		;	SplitInfo = KnownInfo
		).

	% information_gain/6 - compute information gain for an attribute
	information_gain(Values, Attr, Examples, Total, BaseEntropy, Gain) :-
		weighted_entropy(Values, Attr, Examples, Total, 0.0, WeightedEntropy),
		Gain is BaseEntropy - WeightedEntropy.

	weighted_entropy([], _, _, _, WE, WE).
	weighted_entropy([Value| Values], Attr, Examples, Total, Acc, WE) :-
		filter_examples(Examples, Attr, Value, Filtered),
		example_mass(Filtered, SubTotal),
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
		example_mass(Filtered, SubTotal),
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
		(	Attr == none ->
			select_best(Rest, Best)
		;	select_best(Rest, Attr, GR, Best)
		).

	select_best([], Best, _, Best).
	select_best([Attr-GR| Rest], _CurrentBest, CurrentGR, Best) :-
		Attr \== none,
		GR > CurrentGR,
		!,
		select_best(Rest, Attr, GR, Best).
	select_best([_| Rest], CurrentBest, CurrentGR, Best) :-
		select_best(Rest, CurrentBest, CurrentGR, Best).

	% export_to_clauses/4 - convert tree to list of clauses
	export_to_clauses(Dataset, Tree, Functor, Clauses) :-
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		(	Tree = c45_model(_, _) ->
			model_clause_arguments(AttributeNames, Arguments, Instance),
			append(Arguments, [Class], AllArguments),
			Head =.. [Functor| AllArguments],
			Clauses = [(Head :- c45_classifier::predict(Tree, Instance, Class))]
		;	tree_to_clauses(Tree, Functor, AttributeNames, [], Clauses)
		).

	check_classifier(Classifier) :-
		(	var(Classifier) ->
			instantiation_error
		;	valid_tree(Classifier) ->
			true
		;	domain_error(classifier, Classifier)
		).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		(	nonvar(Classifier),
			Classifier = c45_model(_, Statistics) ->
			statistics_counts(Statistics, Counts),
			keys(Counts, Classes),
			Diagnostics = [model(c45_classifier), classes(Classes)]
		;	Diagnostics = [model(c45_classifier)]
		).

	classifier_export_template(Dataset, _Tree, Functor, Template) :-
		Dataset::class(Class),
		findall(
			Attribute,
			Dataset::attribute_values(Attribute, _),
			Arguments,
			[Class]
		),
		title_case(Arguments, TitleCaseArguments),
		Template =.. [Functor| TitleCaseArguments].

	classifier_term_template(c45_model(_Tree, _Statistics), c45_model('Tree', 'Statistics')).
	classifier_term_template(leaf(_Class), leaf('Class')).
	classifier_term_template(tree(_Attribute, threshold(_Threshold), _LeftTree, _RightTree), tree('Attribute', threshold('Threshold'), 'LeftTree', 'RightTree')).
	classifier_term_template(tree(_Attribute, _Subtrees), tree('Attribute', 'Subtrees')).

	valid_tree(c45_model(Tree, Statistics)) :-
		valid_tree(Tree),
		statistics_counts(Statistics, Counts),
		valid(list(pair(atom, positive_float)), Counts),
		keys(Counts, Classes),
		^^valid_class_values(Classes),
		valid_statistics(Tree, Statistics, Classes).
	valid_tree(leaf(Class)) :-
		atom(Class).
	valid_tree(tree(Attribute, threshold(Threshold), LeftTree, RightTree)) :-
		atom(Attribute),
		valid(float, Threshold),
		valid_tree(LeftTree),
		valid_tree(RightTree).
	valid_tree(tree(Attribute, Subtrees)) :-
		atom(Attribute),
		valid(list(compound), Subtrees),
		Subtrees \== [],
		valid_subtrees(Subtrees, []).

	valid_subtrees([], _SeenValues).
	valid_subtrees([Value-Subtree| Subtrees], SeenValues) :-
		nonvar(Value),
		\+ member(Value, SeenValues),
		valid_tree(Subtree),
		valid_subtrees(Subtrees, [Value| SeenValues]).

	tree_to_clauses(Tree, Functor, AttributeNames, Bindings, Clauses) :-
		tree_to_clauses(Tree, Functor, AttributeNames, Bindings, Clauses, []).

	tree_to_clauses(leaf(Class), Functor, AttributeNames, Bindings, [Clause| Tail], Tail) :-
		build_clause(Functor, AttributeNames, Bindings, Class, Clause).
	tree_to_clauses(tree(Attribute, threshold(Threshold), LeftTree, RightTree), Functor, AttributeNames, Bindings, Clauses, Tail) :-
		tree_to_clauses(LeftTree, Functor, AttributeNames, [upper_bound(Attribute, Threshold)| Bindings], Clauses, Rest),
		tree_to_clauses(RightTree, Functor, AttributeNames, [lower_bound(Attribute, Threshold)| Bindings], Rest, Tail).
	tree_to_clauses(tree(Attribute, Subtrees), Functor, AttributeNames, Bindings, Clauses, Tail) :-
		subtrees_to_clauses(Subtrees, Attribute, Functor, AttributeNames, Bindings, Clauses, Tail).

	subtrees_to_clauses([], _, _, _, _, Tail, Tail).
	subtrees_to_clauses([Value-Subtree| Subtrees], Attribute, Functor, AttributeNames, Bindings, Clauses, Tail) :-
		tree_to_clauses(Subtree, Functor, AttributeNames, [categorical(Attribute, Value)| Bindings], Clauses, Rest),
		subtrees_to_clauses(Subtrees, Attribute, Functor, AttributeNames, Bindings, Rest, Tail).

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
		findall(T, member(upper_bound(Attr, T), Bindings), LEThresholds),
		findall(T, member(lower_bound(Attr, T), Bindings), GTThresholds),
		(	(LEThresholds \== [] ; GTThresholds \== []) ->
			make_simplified_goals(LEThresholds, GTThresholds, Arg, AttrGoals),
			append(AttrGoals, RestGoals, Goals)
		;	member(categorical(Attr, Arg), Bindings) ->
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

	% print_classifier/1 - pretty print the tree
	print_classifier(Tree) :-
		^^print_classifier_template(Tree),
		print_classifier(Tree, 0).

	print_classifier(c45_model(Tree, _Statistics), Indent) :-
		print_classifier(Tree, Indent).
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

	% assumes ASCII attribute and class names
	title_case([], []).
	title_case([Name| Names], [TitleCaseName| TitleCaseNames]) :-
		atom_codes(Name, [Letter| Letters]),
		(	0'a @=< Letter, Letter @=< 0'z ->
			UpperCaseLetter is Letter - 32,
			atom_codes(TitleCaseName, [UpperCaseLetter| Letters])
		;	TitleCaseName = Name
		),
		title_case(Names, TitleCaseNames).

	% prune/5 - pessimistic error pruning with configurable parameters
	% Prunes the tree by comparing error estimates of subtrees vs. leaf replacement
	prune(Dataset, Tree, ConfidenceFactor, MinInstances, PrunedTree) :-
		(	(var(ConfidenceFactor); var(MinInstances)) ->
			instantiation_error
		;	true
		),
		(	float(ConfidenceFactor),
			ConfidenceFactor > 0.0,
			ConfidenceFactor =< 0.5 ->
			true
		;	domain_error(confidence_factor, ConfidenceFactor)
		),
		(	valid(positive_integer, MinInstances) ->
			true
		;	domain_error(positive_integer, MinInstances)
		),
		::check_classifier(Tree),
		% Convert confidence factor to z-score using inverse normal approximation
		confidence_to_zscore(ConfidenceFactor, ZScore),
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples(Dataset, Examples),
		weight_examples(Examples, Weighted),
		prune_tree(Tree, ZScore, MinInstances, Weighted, PrunedTree).

	% prune/3 - pessimistic error pruning with default parameters
	prune(Dataset, Tree, PrunedTree) :-
		prune(Dataset, Tree, 0.25, 2, PrunedTree).

	% confidence_to_zscore/2 - converts confidence factor to z-score
	% Uses the Abramowitz and Stegun approximation for the inverse normal distribution
	% For confidence factor CF, we want the z-score such that P(Z <= z) = 1 - CF
	confidence_to_zscore(ConfidenceFactor, ZScore) :-
		(	ConfidenceFactor >= 0.5 ->
			ZScore = 0.0
		;	inverse_normal_tail_approx(ConfidenceFactor, ZScore)
		).

	% inverse_normal_approx/2 - Abramowitz and Stegun rational approximation
	% Valid for P in (0.5, 1.0), returns positive z-score
	inverse_normal_tail_approx(TailProbability, Z) :-
		% Constants for the approximation
		C0 = 2.515517,
		C1 = 0.802853,
		C2 = 0.010328,
		D1 = 1.432788,
		D2 = 0.189269,
		D3 = 0.001308,
		% Calculate t from P
		T is sqrt(-2.0 * log(TailProbability)),
		% Rational approximation
		Z is T - (C0 + C1 * T + C2 * T * T) / (1.0 + D1 * T + D2 * T * T + D3 * T * T * T).

	% prune_tree/5 - recursive tree pruning (bottom-up)
	prune_tree(c45_model(Tree, _Statistics), ZScore, MinInstances, Examples, c45_model(PrunedTree, Statistics)) :-
		prune_node(Tree, ZScore, MinInstances, Examples, [], PrunedTree, Statistics, _Error).
	prune_tree(leaf(Class), _, _, _, leaf(Class)).
	prune_tree(tree(Attribute, threshold(Threshold), Left, Right), _, _, [], tree(Attribute, threshold(Threshold), Left, Right)) :-
		!.
	prune_tree(tree(Attr, threshold(Threshold), Left, Right), ZScore, MinInstances, Examples, PrunedTree) :-
		prune_node(tree(Attr, threshold(Threshold), Left, Right), ZScore, MinInstances, Examples, [], PrunedTree, _Statistics, _Error).
	prune_tree(tree(Attribute, Subtrees), _, _, [], tree(Attribute, Subtrees)) :-
		!.
	prune_tree(tree(Attr, Subtrees), ZScore, MinInstances, Examples, PrunedTree) :-
		prune_node(tree(Attr, Subtrees), ZScore, MinInstances, Examples, [], PrunedTree, _Statistics, _Error).

	prune_node(Tree, _, _, [], Counts, Tree, Statistics, 1.0) :-
		!,
		tree_statistics(Tree, [], Counts, Statistics).
	prune_node(leaf(Class), ZScore, _, Examples, _, leaf(Class), leaf_stats(Counts), Error) :-
		weighted_class_counts(Examples, Counts),
		example_mass(Examples, Total),
		counts_errors(Counts, Class, 0.0, Errors),
		upper_confidence_bound(Errors, Total, ZScore, Error).
	prune_node(tree(Attribute, threshold(Threshold), Left, Right), ZScore, Minimum, Examples, _, Pruned, Statistics, Error) :-
		weighted_class_counts(Examples, Counts),
		example_mass(Examples, Total),
		partition_weighted_threshold(Examples, Attribute, Threshold, LeftExamples, RightExamples),
		example_mass(LeftExamples, LeftMass),
		example_mass(RightExamples, RightMass),
		prune_node(Left, ZScore, Minimum, LeftExamples, Counts, PrunedLeft, LeftStats, LeftError),
		prune_node(Right, ZScore, Minimum, RightExamples, Counts, PrunedRight, RightStats, RightError),
		SubtreeError is (LeftMass * LeftError + RightMass * RightError) / Total,
		Tree = tree(Attribute, threshold(Threshold), PrunedLeft, PrunedRight),
		NodeStats = threshold_stats(Counts, LeftMass, LeftStats, RightMass, RightStats),
		finish_pruning(Counts, Total, ZScore, Minimum, Tree, NodeStats, SubtreeError, Pruned, Statistics, Error).
	prune_node(tree(Attribute, Subtrees), ZScore, Minimum, Examples, _, Pruned, Statistics, Error) :-
		weighted_class_counts(Examples, Counts),
		example_mass(Examples, Total),
		length(Subtrees, BranchCount),
		DefaultShare is 1.0 / BranchCount,
		Context = pruning_context(Attribute, ZScore, Minimum, Examples, Counts, Total, DefaultShare),
		prune_branches(Subtrees, Context, PrunedSubtrees, Branches, 0.0, SubtreeError),
		finish_pruning(Counts, Total, ZScore, Minimum, tree(Attribute, PrunedSubtrees), discrete_stats(Counts, Branches), SubtreeError, Pruned, Statistics, Error).

	prune_branches([], _, [], [], Error, Error).
	prune_branches([Value-Tree| Subtrees], Context, [Value-Pruned| PrunedSubtrees], [branch(Value, Mass, Statistics)| Branches], Acc, Error) :-
		Context = pruning_context(Attribute, ZScore, Minimum, Examples, Counts, Total, DefaultShare),
		partition_weighted_value(Examples, Attribute, Value, DefaultShare, Filtered),
		example_mass(Filtered, Mass),
		prune_node(Tree, ZScore, Minimum, Filtered, Counts, Pruned, Statistics, SubError),
		Next is Acc + (Mass * SubError) / Total,
		prune_branches(Subtrees, Context, PrunedSubtrees, Branches, Next, Error).

	finish_pruning(Counts, Total, ZScore, Minimum, Tree, NodeStats, SubtreeError, Pruned, Statistics, Error) :-
		max_count(Counts, Class),
		counts_errors(Counts, Class, 0.0, Errors),
		upper_confidence_bound(Errors, Total, ZScore, LeafError),
		(	(Total < Minimum; LeafError =< SubtreeError) ->
			Pruned = leaf(Class),
			Statistics = leaf_stats(Counts),
			Error = LeafError
		;	Pruned = Tree,
			Statistics = NodeStats,
			Error = SubtreeError
		).

	counts_errors([], _, Errors, Errors).
	counts_errors([Class-Count| Counts], Majority, Acc, Errors) :-
		(	Class == Majority ->
			Next = Acc
		;	Next is Acc + Count
		),
		counts_errors(Counts, Majority, Next, Errors).

	% leaf_error/5 - pessimistic error estimate for replacing with a leaf
	% Uses upper confidence bound of binomial distribution (C4.5 formula)
	leaf_error([], _, _, 1.0, unknown) :-
		!.
	leaf_error(Examples, ZScore, _MinInstances, Error, MajorityClass) :-
		majority_class(Examples, MajorityClass),
		example_mass(Examples, Total),
		count_errors(Examples, MajorityClass, 0, Errors),
		upper_confidence_bound(Errors, Total, ZScore, Error).

	% count_errors/3 - count examples not belonging to the given class
	count_errors([], _, Errors, Errors).
	count_errors([weighted_example(_, Class, _, Weight)| Examples], MajorityClass, Errors0, Errors) :-
		(	Class == MajorityClass ->
			Errors1 is Errors0
		;	Errors1 is Errors0 + Weight
		),
		count_errors(Examples, MajorityClass, Errors1, Errors).

	% upper_confidence_bound/4 - C4.5 upper confidence bound formula
	% Computes the upper bound of the binomial distribution confidence interval
	% Formula: UCF = (f + z^2/(2N) + z*sqrt(f*(1-f)/N + z^2/(4N^2))) / (1 + z^2/N)
	% where f = E/N is the observed error rate and z is the z-score
	upper_confidence_bound(Errors, Total, ZScore, UCB) :-
		(	Total =< 0.0 ->
			UCB = 1.0
		;	Errors >= Total ->
			UCB = 1.0
		;	Errors =< 0.0 ->
			% No errors: use a minimal upper bound based on sample size
			Z2 is ZScore * ZScore,
			UCB is Z2 / (Total + Z2)
		;	F is Errors / Total,
			Z2 is ZScore * ZScore,
			% Upper confidence bound formula (Wilson score interval upper bound)
			Numerator is Errors + Z2 / 2 + ZScore * sqrt(Errors * (1 - F) + Z2 / 4),
			Denominator is Total + Z2,
			Bound is Numerator / Denominator,
			(	Bound > 1.0 ->
				UCB = 1.0
			;	UCB = Bound
			)
		).

	% subtree_error/4 - pessimistic error estimate for a subtree
	% Sums the error estimates of all leaves, weighted by proportion of examples
	subtree_error(leaf(Class), ZScore, Examples, Error) :-
		example_mass(Examples, Total),
		(	Total =:= 0 ->
			Error is 1.0
		;	count_errors(Examples, Class, 0, Errors),
			upper_confidence_bound(Errors, Total, ZScore, Error)
		).
	subtree_error(tree(Attr, threshold(Threshold), Left, Right), ZScore, Examples, Error) :-
		filter_examples_threshold(Examples, Attr, Threshold, LeftExamples, RightExamples),
		example_mass(Examples, Total),
		example_mass(LeftExamples, LeftTotal),
		example_mass(RightExamples, RightTotal),
		(	Total =:= 0 ->
			Error is 1.0
		;	subtree_error(Left, ZScore, LeftExamples, LeftError),
			subtree_error(Right, ZScore, RightExamples, RightError),
			% Weighted sum of subtree errors
			Error is (LeftTotal * LeftError + RightTotal * RightError) / Total
		).
	subtree_error(tree(Attr, Subtrees), ZScore, Examples, Error) :-
		example_mass(Examples, Total),
		(	Total =:= 0 ->
			Error is 1.0
		;	subtrees_error(Subtrees, Attr, ZScore, Examples, Total, 0.0, Error)
		).

	% subtrees_error/7 - accumulate weighted error from all subtrees
	subtrees_error(Subtrees, Attr, ZScore, Examples, Total, Acc, Error) :-
		length(Subtrees, Count),
		DefaultShare is 1.0 / Count,
		subtrees_error(Subtrees, Attr, ZScore, Examples, Total, DefaultShare, Acc, Error).

	subtrees_error([], _, _, _, _, _, Error, Error).
	subtrees_error([Value-Subtree| Subtrees], Attr, ZScore, Examples, Total, DefaultShare, Acc, Error) :-
		partition_weighted_value(Examples, Attr, Value, DefaultShare, FilteredExamples),
		example_mass(FilteredExamples, SubTotal),
		subtree_error(Subtree, ZScore, FilteredExamples, SubError),
		Acc1 is Acc + (SubTotal * SubError) / Total,
		subtrees_error(Subtrees, Attr, ZScore, Examples, Total, DefaultShare, Acc1, Error).

	weight_examples([], []).
	weight_examples([Id-Class-AVs| Examples], [weighted_example(Id, Class, AVs, 1.0)| Weighted]) :-
		weight_examples(Examples, Weighted).

	example_mass(Examples, Total) :-
		example_mass(Examples, 0.0, Total).

	example_mass([], Total, Total).
	example_mass([weighted_example(_, _, _, Weight)| Examples], Acc, Total) :-
		Next is Acc + Weight,
		example_mass(Examples, Next, Total).

	weighted_class_counts(Examples, Counts) :-
		class_weight_pairs(Examples, Pairs),
		keysort(Pairs, Sorted),
		combine_class_weights(Sorted, Counts).

	class_weight_pairs([], []).
	class_weight_pairs([weighted_example(_, Class, _, Weight)| Examples], [Class-Weight| Pairs]) :-
		class_weight_pairs(Examples, Pairs).

	combine_class_weights([], []).
	combine_class_weights([Class-Weight| Pairs], [Class-Total| Counts]) :-
		combine_same_class(Pairs, Class, Weight, Total, Rest),
		combine_class_weights(Rest, Counts).

	combine_same_class([], _, Total, Total, []).
	combine_same_class([Class-Weight| Pairs], Class, Acc, Total, Rest) :-
		!,
		Next is Acc + Weight,
		combine_same_class(Pairs, Class, Next, Total, Rest).
	combine_same_class([Pair| Pairs], _, Total, Total, [Pair| Pairs]).

	partition_weighted_value(Examples, Attribute, Value, DefaultShare, Filtered) :-
		known_value_masses(Examples, Attribute, Value, 0.0, 0.0, KnownMass, ValueMass),
		(	KnownMass > 0.0 ->
			Share is ValueMass / KnownMass
		;	Share = DefaultShare
		),
		filter_weighted_value(Examples, Attribute, Value, Share, Filtered).

	known_value_masses([], _, _, Known, Selected, Known, Selected).
	known_value_masses([weighted_example(_, _, AVs, Weight)| Examples], Attribute, Value, Known0, Selected0, Known, Selected) :-
		memberchk(Attribute-AV, AVs),
		(	var(AV) ->
			Known1 = Known0,
			Selected1 = Selected0
		;	Known1 is Known0 + Weight,
			(	AV == Value ->
				Selected1 is Selected0 + Weight
			;	Selected1 = Selected0
			)
		),
		known_value_masses(Examples, Attribute, Value, Known1, Selected1, Known, Selected).

	filter_weighted_value([], _, _, _, []).
	filter_weighted_value([Example| Examples], Attribute, Value, Share, Filtered) :-
		Example = weighted_example(_, _, AVs, _),
		memberchk(Attribute-AV, AVs),
		(	var(AV) ->
			fractional_example(Example, Share, Rest, Filtered)
		;	AV == Value ->
			Filtered = [Example| Rest]
		;	Filtered = Rest
		),
		filter_weighted_value(Examples, Attribute, Value, Share, Rest).

	partition_weighted_threshold(Examples, Attribute, Threshold, Left, Right) :-
		known_threshold_masses(Examples, Attribute, Threshold, 0.0, 0.0, LeftMass, RightMass),
		KnownMass is LeftMass + RightMass,
		(	KnownMass > 0.0 ->
			LeftShare is LeftMass / KnownMass,
			RightShare is RightMass / KnownMass
		;	LeftShare = 0.5,
			RightShare = 0.5
		),
		filter_weighted_threshold(Examples, Attribute, Threshold, LeftShare, RightShare, Left, Right).

	known_threshold_masses([], _, _, Left, Right, Left, Right).
	known_threshold_masses([weighted_example(_, _, AVs, Weight)| Examples], Attribute, Threshold, Left0, Right0, Left, Right) :-
		memberchk(Attribute-Value, AVs),
		(	var(Value) ->
			Left1 = Left0,
			Right1 = Right0
		;	Value =< Threshold ->
			Left1 is Left0 + Weight,
			Right1 = Right0
		;	Left1 = Left0,
			Right1 is Right0 + Weight
		),
		known_threshold_masses(Examples, Attribute, Threshold, Left1, Right1, Left, Right).

	filter_weighted_threshold([], _, _, _, _, [], []).
	filter_weighted_threshold([Example| Examples], Attribute, Threshold, LeftShare, RightShare, Left, Right) :-
		Example = weighted_example(_, _, AVs, _),
		memberchk(Attribute-Value, AVs),
		(	var(Value) ->
			fractional_example(Example, LeftShare, LeftRest, Left),
			fractional_example(Example, RightShare, RightRest, Right)
		;	Value =< Threshold ->
			Left = [Example| LeftRest],
			Right = RightRest
		;	Left = LeftRest,
			Right = [Example| RightRest]
		),
		filter_weighted_threshold(Examples, Attribute, Threshold, LeftShare, RightShare, LeftRest, RightRest).

	fractional_example(weighted_example(Id, Class, AVs, Weight), Share, Rest, Examples) :-
		Fraction is Weight * Share,
		(	Fraction > 0.0 ->
			Examples = [weighted_example(Id, Class, AVs, Fraction)| Rest]
		;	Examples = Rest
		).

	model_clause_arguments([], [], []).
	model_clause_arguments([Attribute| Attributes], [Value| Arguments], [Attribute-Value| Instance]) :-
		model_clause_arguments(Attributes, Arguments, Instance).

	statistics_counts(leaf_stats(Counts), Counts).
	statistics_counts(threshold_stats(Counts, _, _, _, _), Counts).
	statistics_counts(discrete_stats(Counts, _), Counts).

	tree_statistics(Tree, Examples, Statistics) :-
		weighted_class_counts(Examples, Counts),
		tree_statistics(Tree, Examples, Counts, Statistics).

	tree_statistics(leaf(_), Examples, Fallback, leaf_stats(Counts)) :-
		node_counts(Examples, Fallback, Counts).
	tree_statistics(tree(Attribute, threshold(Threshold), Left, Right), Examples, Fallback, threshold_stats(Counts, LeftMass, LeftStats, RightMass, RightStats)) :-
		node_counts(Examples, Fallback, Counts),
		partition_weighted_threshold(Examples, Attribute, Threshold, LeftExamples, RightExamples),
		example_mass(LeftExamples, LeftMass),
		example_mass(RightExamples, RightMass),
		tree_statistics(Left, LeftExamples, Counts, LeftStats),
		tree_statistics(Right, RightExamples, Counts, RightStats).
	tree_statistics(tree(Attribute, Subtrees), Examples, Fallback, discrete_stats(Counts, Branches)) :-
		node_counts(Examples, Fallback, Counts),
		length(Subtrees, Count),
		DefaultShare is 1.0 / Count,
		branch_statistics(Subtrees, Attribute, Examples, Counts, DefaultShare, Branches).

	node_counts([], Fallback, Fallback) :-
		!.
	node_counts(Examples, _, Counts) :-
		weighted_class_counts(Examples, Counts).

	branch_statistics([], _, _, _, _, []).
	branch_statistics([Value-Tree| Subtrees], Attribute, Examples, Counts, DefaultShare, [branch(Value, Mass, Statistics)| Branches]) :-
		partition_weighted_value(Examples, Attribute, Value, DefaultShare, Filtered),
		example_mass(Filtered, Mass),
		tree_statistics(Tree, Filtered, Counts, Statistics),
		branch_statistics(Subtrees, Attribute, Examples, Counts, DefaultShare, Branches).

	valid_statistics(leaf(Class), leaf_stats(Counts), Classes) :-
		valid_stat_counts(Counts, Classes),
		max_count(Counts, Class).
	valid_statistics(tree(_, threshold(_), Left, Right), threshold_stats(Counts, LeftMass, LeftStats, RightMass, RightStats), Classes) :-
		valid_stat_counts(Counts, Classes),
		valid(non_negative_float, LeftMass),
		valid(non_negative_float, RightMass),
		Total is LeftMass + RightMass,
		Total > 0.0,
		valid_statistics(Left, LeftStats, Classes),
		valid_statistics(Right, RightStats, Classes).
	valid_statistics(tree(_, Subtrees), discrete_stats(Counts, Branches), Classes) :-
		valid_stat_counts(Counts, Classes),
		valid(list(compound), Branches),
		valid_branch_statistics(Subtrees, Branches, Classes, 0.0, Total),
		Total > 0.0.

	valid_stat_counts(Counts, Classes) :-
		valid(list(pair(atom, positive_float)), Counts),
		keys(Counts, NodeClasses),
		^^valid_class_values(NodeClasses),
		forall(member(Class, NodeClasses), list::memberchk(Class, Classes)).

	valid_branch_statistics([], [], _, Total, Total).
	valid_branch_statistics([Value-Tree| Subtrees], [branch(BranchValue, Mass, Statistics)| Branches], Classes, Acc, Total) :-
		Value == BranchValue,
		valid(non_negative_float, Mass),
		valid_statistics(Tree, Statistics, Classes),
		Next is Acc + Mass,
		valid_branch_statistics(Subtrees, Branches, Classes, Next, Total).

	predict_model(leaf(Class), leaf_stats(_), _, Class).
	predict_model(tree(Attribute, threshold(Threshold), Left, Right), threshold_stats(Counts, LeftMass, LeftStats, RightMass, RightStats), Instance, Class) :-
		(	member(Attribute-Value, Instance),
			nonvar(Value) ->
			(	Value > Threshold ->
				predict_model(Right, RightStats, Instance, Class)
			;	predict_model(Left, LeftStats, Instance, Class)
			)
		;	model_vote(tree(Attribute, threshold(Threshold), Left, Right), threshold_stats(Counts, LeftMass, LeftStats, RightMass, RightStats), Instance, Class)
		).
	predict_model(tree(Attribute, Subtrees), discrete_stats(Counts, Branches), Instance, Class) :-
		(	member(Attribute-Value, Instance),
			nonvar(Value) ->
			(	model_branch(Value, Subtrees, Branches, Subtree, ChildStats) ->
				predict_model(Subtree, ChildStats, Instance, Class)
			;	max_count(Counts, Class)
			)
		;	model_vote(tree(Attribute, Subtrees), discrete_stats(Counts, Branches), Instance, Class)
		).

	model_branch(Value, [Key-Tree| Subtrees], [branch(_, _, Statistics)| Branches], Subtree, ChildStats) :-
		(	Value == Key ->
			Subtree = Tree,
			ChildStats = Statistics
		;	model_branch(Value, Subtrees, Branches, Subtree, ChildStats)
		).

	model_vote(Tree, Statistics, Instance, Class) :-
		model_scores(Tree, Statistics, Instance, 1.0, Scores, []),
		keysort(Scores, Sorted),
		combine_class_weights(Sorted, Counts),
		max_count(Counts, Class).

	model_scores(leaf(_), leaf_stats(Counts), _, Scale, Scores, Tail) :-
		sum_counts(Counts, 0.0, Total),
		scaled_counts(Counts, Scale, Total, Scores, Tail).
	model_scores(tree(Attribute, threshold(Threshold), Left, Right), threshold_stats(_, LeftMass, LeftStats, RightMass, RightStats), Instance, Scale, Scores, Tail) :-
		(	member(Attribute-Value, Instance),
			nonvar(Value) ->
			(	Value > Threshold ->
				model_scores(Right, RightStats, Instance, Scale, Scores, Tail)
			;	model_scores(Left, LeftStats, Instance, Scale, Scores, Tail)
			)
		;	Total is LeftMass + RightMass,
			score_branch(LeftMass, Total, Left, LeftStats, Instance, Scale, Scores, RightScores),
			score_branch(RightMass, Total, Right, RightStats, Instance, Scale, RightScores, Tail)
		).
	model_scores(tree(Attribute, Subtrees), discrete_stats(Counts, Branches), Instance, Scale, Scores, Tail) :-
		(	member(Attribute-Value, Instance),
			nonvar(Value) ->
			(	model_branch(Value, Subtrees, Branches, Subtree, ChildStats) ->
				model_scores(Subtree, ChildStats, Instance, Scale, Scores, Tail)
			;	sum_counts(Counts, 0.0, Total),
				scaled_counts(Counts, Scale, Total, Scores, Tail)
			)
		;	branch_mass(Branches, 0.0, Total),
			score_discrete_branches(Subtrees, Branches, Total, Instance, Scale, Scores, Tail)
		).

	branch_mass([], Total, Total).
	branch_mass([branch(_, Mass, _)| Branches], Acc, Total) :-
		Next is Acc + Mass,
		branch_mass(Branches, Next, Total).

	score_discrete_branches([], [], _, _, _, Tail, Tail).
	score_discrete_branches([_-Tree| Subtrees], [branch(_, Mass, Statistics)| Branches], Total, Instance, Scale, Scores, Tail) :-
		score_branch(Mass, Total, Tree, Statistics, Instance, Scale, Scores, Rest),
		score_discrete_branches(Subtrees, Branches, Total, Instance, Scale, Rest, Tail).

	score_branch(Mass, Total, Tree, Statistics, Instance, Scale, Scores, Tail) :-
		(	Mass > 0.0 ->
			ChildScale is Scale * (Mass / Total),
			model_scores(Tree, Statistics, Instance, ChildScale, Scores, Tail)
		;	Scores = Tail
		).

	sum_counts([], Total, Total).
	sum_counts([_-Count| Counts], Acc, Total) :-
		Next is Acc + Count,
		sum_counts(Counts, Next, Total).

	scaled_counts([], _, _, Tail, Tail).
	scaled_counts([Class-Count| Counts], Scale, Total, [Class-Score| Scores], Tail) :-
		Score is Scale * (Count / Total),
		scaled_counts(Counts, Scale, Total, Scores, Tail).

	attribute_rows([], _, _, []).
	attribute_rows([weighted_example(_, Class, AVs, Weight)| Examples], Attribute, Ordinal, [attribute_row(Value, Class, Weight, Ordinal)| Rows]) :-
		memberchk(Attribute-Value, AVs),
		Next is Ordinal + 1,
		attribute_rows(Examples, Attribute, Next, Rows).

	group_attribute_rows([], _, []).
	group_attribute_rows([attribute_row(Value, Class, Weight, _)| Rows], Kind, [value_group(Value, Counts, Mass)| Groups]) :-
		value_group_pairs(Rows, Kind, Value, Pairs, Remaining),
		keysort([Class-Weight| Pairs], Sorted),
		combine_class_weights(Sorted, Counts),
		sum_counts(Counts, 0.0, Mass),
		group_attribute_rows(Remaining, Kind, Groups).

	value_group_pairs([], _, _, [], []).
	value_group_pairs([attribute_row(NextValue, Class, Weight, Ordinal)| Rows], Kind, Value, Pairs, Remaining) :-
		(	same_attribute_value(Kind, Value, NextValue) ->
			Pairs = [Class-Weight| Rest],
			value_group_pairs(Rows, Kind, Value, Rest, Remaining)
		;	Pairs = [],
			Remaining = [attribute_row(NextValue, Class, Weight, Ordinal)| Rows]
		).

	same_attribute_value(continuous, Left, Right) :-
		Left =:= Right.
	same_attribute_value(discrete, Left, Right) :-
		Left == Right.

	zero_class_counts([], []).
	zero_class_counts([Class-_| Counts], [Class-0.0| Zero]) :-
		zero_class_counts(Counts, Zero).

	adjust_class_counts(Counts, [], _, Counts) :-
		!.
	adjust_class_counts([Class-Count| Counts], [GroupClass-Weight| GroupCounts], Direction, [Class-NewCount| Updated]) :-
		(	Class == GroupClass ->
			Value is Count + Direction * Weight,
			(	Value > 0.0 ->
				NewCount = Value
			;	NewCount = 0.0
			),
			adjust_class_counts(Counts, GroupCounts, Direction, Updated)
		;	NewCount = Count,
			adjust_class_counts(Counts, [GroupClass-Weight| GroupCounts], Direction, Updated)
		).

	sweep_thresholds([], _, _, _, _, Best, Best).
	sweep_thresholds([_], _, _, _, _, Best, Best) :-
		!.
	sweep_thresholds([value_group(Value, GroupCounts, GroupMass), NextGroup| Groups], Left0, Right0, LeftMass0, Context, Best0, Best) :-
		adjust_class_counts(Left0, GroupCounts, 1, Left),
		adjust_class_counts(Right0, GroupCounts, -1, Right),
		LeftMass is LeftMass0 + GroupMass,
		NextGroup = value_group(NextValue, _, _),
		candidate_thresholds([Value, NextValue], Thresholds),
		(	Thresholds = [Threshold],
			Context = split_context(KnownTotal, _, _),
			RightMass is KnownTotal - LeftMass,
			RightMass > 0.0 ->
			histogram_threshold_score(Left, LeftMass, Right, RightMass, Context, Gain, Ratio),
			Best0 = best_split(PreviousGain, _, _),
			(	Gain > PreviousGain ->
				Best1 = best_split(Gain, Ratio, Threshold)
			;	Best1 = Best0
			)
		;	Best1 = Best0
		),
		sweep_thresholds([NextGroup| Groups], Left, Right, LeftMass, Context, Best1, Best).

	histogram_threshold_score(Left, LeftMass, Right, RightMass, split_context(KnownTotal, Total, KnownEntropy), Gain, Ratio) :-
		compute_entropy(Left, LeftMass, 0.0, LeftEntropy),
		compute_entropy(Right, RightMass, 0.0, RightEntropy),
		WeightedEntropy is (LeftMass / KnownTotal) * LeftEntropy + (RightMass / KnownTotal) * RightEntropy,
		RawGain is (KnownTotal / Total) * (KnownEntropy - WeightedEntropy),
		compute_entropy([left-LeftMass, right-RightMass], Total, 0.0, SplitInfo0),
		missing_split_info(KnownTotal, Total, SplitInfo0, SplitInfo),
		(	SplitInfo > 0.0 ->
			(	RawGain > 0.0 ->
				Gain = RawGain
			;	Gain = 0.0
			),
			Ratio is Gain / SplitInfo
		;	Gain = 0.0,
			Ratio = 0.0
		).

	discrete_candidate(Attribute, Examples, Total, Key, Gain, Ratio) :-
		known_examples(Examples, Attribute, Known),
		example_mass(Known, KnownTotal),
		(	KnownTotal > 0.0 ->
			weighted_class_counts(Known, Counts),
			compute_entropy(Counts, KnownTotal, 0.0, KnownEntropy),
			attribute_rows(Known, Attribute, 1, Rows),
			sort(Rows, Sorted),
			group_attribute_rows(Sorted, discrete, Groups),
			group_split_statistics(Groups, KnownTotal, Total, 0.0, 0.0, 0.0, WeightedEntropy, KnownSplitInfo, SplitInfo0),
			(	KnownSplitInfo > 0.0 ->
				RawGain is (KnownTotal / Total) * (KnownEntropy - WeightedEntropy),
				missing_split_info(KnownTotal, Total, SplitInfo0, SplitInfo),
				(	SplitInfo > 0.0 ->
					(	RawGain > 0.0 ->
						Gain = RawGain
					;	Gain = 0.0
					),
					Ratio is Gain / SplitInfo,
					Key = Attribute
				;	Key = none,
					Gain = 0.0,
					Ratio = 0.0
				)
			;	Key = none,
				Gain = 0.0,
				Ratio = 0.0
			)
		;	Key = none,
			Gain = 0.0,
			Ratio = 0.0
		).

	group_split_statistics([], _, _, Entropy, KnownInfo, Info, Entropy, KnownInfo, Info).
	group_split_statistics([value_group(_, Counts, Mass)| Groups], KnownTotal, Total, Entropy0, KnownInfo0, Info0, Entropy, KnownInfo, Info) :-
		compute_entropy(Counts, Mass, 0.0, GroupEntropy),
		KnownP is Mass / KnownTotal,
		Entropy1 is Entropy0 + KnownP * GroupEntropy,
		compute_entropy([group-Mass], KnownTotal, KnownInfo0, KnownInfo1),
		compute_entropy([group-Mass], Total, Info0, Info1),
		group_split_statistics(Groups, KnownTotal, Total, Entropy1, KnownInfo1, Info1, Entropy, KnownInfo, Info).

	candidate_gain_totals([], Gain, Count, Gain, Count).
	candidate_gain_totals([candidate(Key, Gain, _)| Candidates], Gain0, Count0, TotalGain, Count) :-
		(	Key == none ->
			Gain1 = Gain0,
			Count1 = Count0
		;	Gain1 is Gain0 + Gain,
			Count1 is Count0 + 1
		),
		candidate_gain_totals(Candidates, Gain1, Count1, TotalGain, Count).

	eligible_ratios([], _, []).
	eligible_ratios([candidate(Key, Gain, Ratio)| Candidates], Average, Ratios) :-
		(	Key \== none,
			Gain >= Average ->
			Ratios = [Key-Ratio| Rest]
		;	Ratios = Rest
		),
		eligible_ratios(Candidates, Average, Rest).

:- end_object.

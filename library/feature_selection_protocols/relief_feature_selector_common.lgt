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


:- category(relief_feature_selector_common,
	extends(feature_selector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Shared joint nearest-neighbor feature selection for Relief, ReliefF, and RReliefF.'
	]).

	:- uses(list, [length/2, member/2, memberchk/2, nth1/3]).
	:- uses(type, [valid/2, check/3]).

	:- protected(relief_model/1).
	:- mode(relief_model(-atom), one).
	:- info(relief_model/1, [
		comment is 'Returns the receiving selector model functor.',
		argnames is ['Model']
	]).

	:- protected(relief_variant/1).
	:- mode(relief_variant(-atom), one).
	:- info(relief_variant/1, [
		comment is 'Returns ``binary``, ``multiclass``, or ``regression``.',
		argnames is ['Variant']
	]).

	:- protected(anchors/4).
	:- mode(anchors(+term, +positive_integer, +list(compound), -list(compound)), zero_or_one_or_error).
	:- info(anchors/4, [
		comment is 'Processes all rows or samples anchors while restoring RNG state on every exit path.',
		argnames is ['Size', 'Seed', 'Rows', 'Anchors'],
		exceptions is [
			'A sampling size is not evaluable' - type_error(evaluable, 'Size'),
			'Sampling arithmetic exceeds backend limits' - evaluation_error('Reason')
		]
	]).

	:- private(declarations/3).
	:- mode(declarations(+list(atomic), +object_identifier, -list(pair)), one_or_error).
	:- info(declarations/3, [
		comment is 'Resolves continuous and discrete feature declarations to numeric and categorical types.',
		argnames is ['Features', 'Dataset', 'Declarations'],
		exceptions is ['A declaration is unsupported' - domain_error(feature_type, 'Feature-Declaration')]
	]).

	:- private(check_target/2).
	:- mode(check_target(+atom, +nonvar), one_or_error).
	:- info(check_target/2, [
		comment is 'Checks the target type required by the algorithm variant.',
		argnames is ['Variant', 'Target'],
		exceptions is [
			'A regression target is not numeric' - type_error(number, 'Target'),
			'A classification target is not atomic' - type_error(atomic, 'Target')
		]
	]).

	:- private(row_values/4).
	:- mode(row_values(+list(pair), +compound, -list, -boolean), one_or_error).
	:- info(row_values/4, [
		comment is 'Collects typed row values from an indexed feature dictionary without binding missing values.',
		argnames is ['Declarations', 'Dictionary', 'Values', 'Complete'],
		exceptions is [
			'A known numeric feature is not numeric' - type_error(number, 'Value'),
			'A known categorical feature is not atomic' - type_error(atomic, 'Value'),
			'A known categorical value is outside its declared domain' - domain_error(feature_value, 'Feature-Value')
		]
	]).

	:- private(population/4).
	:- mode(population(+atom, +list(compound), -list(pair), -term), one_or_error).
	:- info(population/4, [
		comment is 'Checks eligible class counts or regression row count and prepares target normalization.',
		argnames is ['Variant', 'Rows', 'Classes', 'TargetScale'],
		exceptions is [
			'The eligible population does not satisfy the variant minimum' - domain_error(relief_population, 'Variant-Counts')
		]
	]).

	selector_term_template(Selector, Template) :-
		::relief_model(Model),
		Selector =.. [Model, _Scores, _Selected, _Diagnostics],
		Template =.. [Model, 'FeatureScores', 'SelectedFeatures', 'Diagnostics'].

	selector_export_template(_Dataset, _Selector, Functor, Template) :-
		Template =.. [Functor, 'Selector'].

	learn(Dataset, Selector, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_examples(Dataset, Features, Examples),
		::relief_variant(Variant),
		declarations(Features, Dataset, Declarations),
		^^option(missing_values(Missing), Options),
		prepare_rows(Examples, Declarations, Variant, Missing, 1, RawRows),
		length(RawRows, Eligible),
		population(Variant, RawRows, Classes, TargetScale),
		column_ranges(Declarations, RawRows, 1, Ranges),
		normalize_rows(RawRows, Ranges, Rows),
		column_distributions(Ranges, Rows, Classes, 1, Columns),
		^^option(sample_size(Size), Options),
		^^option(random_seed(Seed), Options),
		anchors(Size, Seed, Rows, Anchors),
		length(Anchors, SampleCount),
		^^option(neighbor_weighting(Weighting), Options),
		neighbor_count(Variant, Options, K),
		zero_vector(Features, Zeros),
		accumulate(Anchors, Rows, Columns, Variant, Classes, TargetScale, K, Weighting,
			Zeros, Zeros, 0.0, Totals, Products, TargetMass),
		final_scores(Variant, TargetScale, SampleCount, Totals, Products, TargetMass, Values, Degeneracy),
		pair_scores(Features, Values, Unsorted),
		^^sort_by_decreasing_score(Unsorted, Scores),
		^^option(selection_strategy(Strategy), Options),
		selection(Strategy, Scores, Selected),
		length(Examples, ExampleCount),
		length(Features, CandidateCount),
		length(Selected, SelectedCount),
		Excluded is ExampleCount - Eligible,
		row_positions(Rows, Positions),
		row_positions(Anchors, Samples),
		population_metadata(Variant, Classes, TargetScale, TargetMass, SampleCount, Population),
		findall(Feature-Type, member(range(Feature, Type, _), Ranges), FeatureTypes),
		::relief_model(Model),
		^^base_selector_diagnostics(Model, ExampleCount, Options, [
			variant(Variant), candidate_count(CandidateCount), selected_count(SelectedCount),
			features(FeatureTypes), eligible_count(Eligible), excluded_count(Excluded),
			eligible_positions(Positions), samples(Samples), population(Population), degeneracy(Degeneracy)
		], Diagnostics),
		Selector =.. [Model, Scores, Selected, Diagnostics],
		!.

	selected_features(Selector, Selected) :-
		::check_selector(Selector),
		Selector =.. [_Model, _Scores, Selected, _Diagnostics].

	feature_scores(Selector, Scores) :-
		::check_selector(Selector),
		Selector =.. [_Model, Scores, _Selected, _Diagnostics].

	check_selector(Selector) :-
		( 	\+ ground(Selector) ->
			instantiation_error
		;	valid_model(Selector) ->
			true
		;	domain_error(selector, Selector)
		).

	export_to_clauses(_Dataset, Selector, Functor, [Clause]) :-
		::check_selector(Selector),
		Clause =.. [Functor, Selector].

	print_selector(Selector) :-
		::check_selector(Selector),
		^^print_selector_template(Selector),
		writeq(Selector), nl.

	default_option(selection_strategy(top_k(10))).
	default_option(sample_size(all)).
	default_option(random_seed(1357911)).
	default_option(missing_values(complete_case)).
	default_option(number_of_neighbors(10)) :-
		::relief_variant(Variant),
		Variant \== binary.
	default_option(neighbor_weighting(Weighting)) :-
		::relief_variant(Variant),
		(	Variant == regression ->
			Weighting = rank(2)
		;	Weighting = uniform
		).

	valid_option(selection_strategy(Strategy)) :-
		(	Strategy == all ->
			true
		;	Strategy = top_k(K) ->
			integer(K),
			K > 0
		;	Strategy = threshold(T),
			number(T)
		).
	valid_option(sample_size(Size)) :-
		(	Size == all ->
			true
		;	integer(Size),
			Size > 0
		).
	valid_option(random_seed(Seed)) :-
		integer(Seed),
		Seed > 0.
	valid_option(missing_values(Mode)) :-
		once((Mode == complete_case; Mode == probabilistic)).
	valid_option(number_of_neighbors(K)) :-
		::relief_variant(Variant),
		Variant \== binary,
		integer(K),
		K > 0.
	valid_option(neighbor_weighting(Weighting)) :-
		(	Weighting == uniform ->
			true
		;	Weighting = rank(Sigma),
			integer(Sigma),
			Sigma > 0
		).

	neighbor_count(binary, _Options, 1).
	neighbor_count(multiclass, Options, K) :-
		^^option(number_of_neighbors(K), Options).
	neighbor_count(regression, Options, K) :-
		^^option(number_of_neighbors(K), Options).

	selection(all, Scores, Selected) :-
		score_names(Scores, Selected).
	selection(top_k(K), Scores, Selected) :-
		^^select_top_k(Scores, K, Selected).
	selection(threshold(T), Scores, Selected) :-
		^^select_above_threshold(Scores, T, Selected).

	declarations([], _Dataset, []).
	declarations([Feature| Features], Dataset, [Feature-Type| Declarations]) :-
		Dataset::attribute_values(Feature, Declaration),
		(	Declaration == continuous ->
			Type = numeric
		;	Declaration = [_| _], ground(Declaration), valid(list(atomic), Declaration) ->
			Type = categorical(Declaration)
		;	domain_error(feature_type, Feature-Declaration)
		),
		declarations(Features, Dataset, Declarations).

	prepare_rows([], _Declarations, _Variant, _Missing, _Position, []).
	prepare_rows([example(_Id, Features, Target)| Examples], Declarations, Variant, Missing, Position, Rows) :-
		(	var(Target) ->
			Rows = Rest
		;	check_target(Variant, Target),
			avltree::as_dictionary(Features, Dictionary),
			row_values(Declarations, Dictionary, Values, Complete),
			(	Missing == complete_case, Complete == false ->
				Rows = Rest
			;	Rows = [row(Position, Target, Values)| Rest]
			)
		),
		Next is Position + 1,
		prepare_rows(Examples, Declarations, Variant, Missing, Next, Rest).

	check_target(Variant, Target) :-
		context(Context),
		(	Variant == regression ->
			check(number, Target, Context)
		;	check(atomic, Target, Context)
		).

	row_values([], _Dictionary, [], true).
	row_values([Feature-Type| Declarations], Dictionary, [Value| Values], Complete) :-
		(	avltree::lookup(Feature, Found, Dictionary), nonvar(Found) ->
			context(Context),
			(	Type == numeric ->
				check(number, Found, Context)
			;	Type = categorical(Domain),
				check(atomic, Found, Context),
				(	member(Found, Domain) ->
					true
				;	domain_error(feature_value, Feature-Found)
				)
			),
			Value = known(Found),
			row_values(Declarations, Dictionary, Values, Complete)
		;	Value = missing,
			row_values(Declarations, Dictionary, Values, _),
			Complete = false
		).

	population(regression, Rows, [], Scale) :-
		!,
		length(Rows, Count),
		(	Count >= 2 ->
			true
		;	domain_error(relief_population, regression-Count)
		),
		findall(Target, member(row(_, Target, _), Rows), Targets),
		numeric_scale(Targets, Scale).
	population(Variant, Rows, Classes, none) :-
		findall(Target-1, member(row(_, Target, _), Rows), Pairs),
		keysort(Pairs, Sorted),
		histogram(Sorted, Counts),
		length(Counts, ClassCount),
		(	(Variant == binary, ClassCount =:= 2; Variant == multiclass, ClassCount >= 2),
			class_minimum(Counts) ->
			Classes = Counts
		;	domain_error(relief_population, Variant-Counts)
		).

	class_minimum([]).
	class_minimum([_-Count| Counts]) :-
		Count >= 2,
		class_minimum(Counts).

	histogram([], []).
	histogram([Value-Weight| Pairs], [Value-Count| Counts]) :-
		histogram_run(Pairs, Value, Weight, Count, Rest),
		histogram(Rest, Counts).

	histogram_run([Value-Weight| Pairs], Key, Count0, Count, Rest) :-
		Value == Key,
		!,
		Count1 is Count0 + Weight,
		histogram_run(Pairs, Key, Count1, Count, Rest).
	histogram_run(Rest, _Key, Count, Count, Rest).

	numeric_scale([], scale(1.0, 0.0, 0.0, 0.0, 0.0)).
	numeric_scale([Value| Values], scale(Scale, Low, Range, Minimum, Maximum)) :-
		extrema(Values, Value, Value, Minimum, Maximum),
		Magnitude is max(abs(Minimum), abs(Maximum)),
		(	Magnitude =:= 0 ->
			Scale = 1.0
		;	Scale = Magnitude
		),
		Low is Minimum / Scale,
		Range is Maximum / Scale - Low.

	extrema([], Minimum, Maximum, Minimum, Maximum).
	extrema([Value| Values], Min0, Max0, Minimum, Maximum) :-
		Min1 is min(Value, Min0),
		Max1 is max(Value, Max0),
		extrema(Values, Min1, Max1, Minimum, Maximum).

	normalized(Value, scale(Scale, Low, Range, _Min, _Max), Normalized) :-
		(	Range =:= 0 ->
			Normalized = 0.0
		;	Normalized is (Value / Scale - Low) / Range
		).

	column_ranges([], _Rows, _Index, []).
	column_ranges([Feature-Type| Declarations], Rows, Index, [range(Feature, Kind, Scale)| Ranges]) :-
		(	Type == numeric ->
			Kind = numeric,
			findall(Value, (member(row(_, _, Values), Rows), nth1(Index, Values, known(Value))), Observed),
			numeric_scale(Observed, Scale)
		;	Kind = categorical,
			Scale = none
		),
		Next is Index + 1,
		column_ranges(Declarations, Rows, Next, Ranges).

	normalize_rows([], _Ranges, []).
	normalize_rows([row(Position, Target, Values)| Rows], Ranges, [row(Position, Target, Normalized)| Rest]) :-
		normalize_values(Values, Ranges, Normalized),
		normalize_rows(Rows, Ranges, Rest).

	normalize_values([], [], []).
	normalize_values([Value| Values], [range(_, Type, Scale)| Ranges], [Result| Results]) :-
		(	Value = known(Number),
			Type == numeric ->
			normalized(Number, Scale, Normal),
			Result = known(Normal)
		;	Result = Value
		),
		normalize_values(Values, Ranges, Results).

	column_distributions([], _Rows, _Classes, _Index, []).
	column_distributions([range(_Feature, Type, _Scale)| Ranges], Rows, Classes, Index, [column(Type, Distributions, Cache)| Columns]) :-
		avltree::new(Empty),
		(	member(row(_, _, Values), Rows), nth1(Index, Values, missing) ->
			distribution(Rows, Index, pooled, Type, Pooled),
			findall(
				Class-Count,
				(	member(Class-Count, Classes),
					class_missing(Rows, Class, Index)
				),
				MissingClasses
			),
			class_distributions(MissingClasses, Rows, Index, Type, Pooled, ClassDistributions),
			Pairs = [pooled-Pooled| ClassDistributions],
			avltree::as_dictionary(Pairs, Distributions),
			cache_pairs(Pairs, Pairs, Type, Empty, Cache)
		;	Distributions = Empty,
			Cache = Empty
		),
		Next is Index + 1,
		column_distributions(Ranges, Rows, Classes, Next, Columns).

	class_missing(Rows, Class, Index) :-
		member(row(_, Target, Values), Rows),
		Target == Class,
		nth1(Index, Values, missing),
		!.

	class_distributions([], _Rows, _Index, _Type, _Pooled, []).
	class_distributions([Class-_| Classes], Rows, Index, Type, Pooled, [class(Class)-Effective| Distributions]) :-
		distribution(Rows, Index, class(Class), Type, Distribution),
		( Distribution = dist(empty, _, _) -> Effective = Pooled; Effective = Distribution ),
		class_distributions(Classes, Rows, Index, Type, Pooled, Distributions).

	distribution(Rows, Index, Group, Type, dist(Tree, Mean, Supports)) :-
		findall(Value-1, (member(row(_, Target, Values), Rows), group_target(Group, Target), nth1(Index, Values, known(Value))), Pairs),
		keysort(Pairs, Sorted),
		histogram(Sorted, Counts),
		length(Pairs, Total),
		probabilities(Counts, Total, Supports),
		length(Supports, Size),
		build_tree(Size, Supports, [], Type, 0.0, 0.0, _Probability, Mean, Tree).

	group_target(pooled, _Target).
	group_target(class(Class), Target) :-
		Class == Target.

	probabilities([], _Total, []).
	probabilities([Value-Count| Counts], Total, [Value-Probability| Supports]) :-
		Probability is Count / Total,
		probabilities(Counts, Total, Supports).

	build_tree(0, Rest, Rest, _Type, Probability, Mean, Probability, Mean, empty) :-
		!.
	build_tree(Size, Supports, Rest, Type, Prob0, Mean0, Prob, Mean, node(Value, Weight, PrefixProb, PrefixMean, Left, Right)) :-
		LeftSize is Size // 2,
		RightSize is Size - LeftSize - 1,
		build_tree(LeftSize, Supports, [Value-Weight| Tail], Type, Prob0, Mean0, Prob1, Mean1, Left),
		PrefixProb is Prob1 + Weight,
		(	Type == numeric ->
			PrefixMean is Mean1 + Value * Weight
		;	PrefixMean = Mean1
		),
		build_tree(RightSize, Tail, Rest, Type, PrefixProb, PrefixMean, Prob, Mean, Right).

	cache_pairs([], _All, _Type, Cache, Cache).
	cache_pairs([Key-Distribution| Distributions], All, Type, Cache0, Cache) :-
		cache_row(All, Key, Distribution, Type, Cache0, Cache1),
		cache_pairs(Distributions, All, Type, Cache1, Cache).

	cache_row([], _Key, _Distribution, _Type, Cache, Cache).
	cache_row([Other-Distribution| Rest], Key, First, Type, Cache0, Cache) :-
		unknown_pair(Type, First, Distribution, Difference),
		avltree::insert(Cache0, Key-Other, Difference, Cache1),
		cache_row(Rest, Key, First, Type, Cache1, Cache).

	unknown_pair(_Type, dist(empty, _, _), _Other, 0.0) :-
		!.
	unknown_pair(_Type, _First, dist(empty, _, _), 0.0) :-
		!.
	unknown_pair(Type, dist(_Tree, _Mean, Supports), Other, Difference) :-
		expected_supports(Supports, Type, Other, 0.0, Difference).

	expected_supports([], _Type, _Other, Sum, Sum).
	expected_supports([Value-Probability| Supports], Type, Other, Sum0, Sum) :-
		known_unknown(Type, Value, Other, Difference),
		Sum1 is Sum0 + Probability * Difference,
		expected_supports(Supports, Type, Other, Sum1, Sum).

	known_unknown(_Type, _Value, dist(empty, _, _), 0.0) :-
		!.
	known_unknown(categorical, Value, dist(Tree, _, _), Difference) :-
		category_probability(Tree, Value, Probability),
		Difference is 1.0 - Probability.
	known_unknown(numeric, Value, dist(Tree, Mean, _), Difference) :-
		prefix(Tree, Value, 0.0, 0.0, Probability, Sum),
		Difference is max(0.0, Value * (2.0 * Probability - 1.0) + Mean - 2.0 * Sum).

	category_probability(empty, _Value, 0.0).
	category_probability(node(Key, Weight, _, _, Left, Right), Value, Probability) :-
		compare(Order, Value, Key),
		(	Order == (=) ->
			Probability = Weight
		;	Order == (<) ->
			category_probability(Left, Value, Probability)
		;	category_probability(Right, Value, Probability)
		).

	prefix(empty, _Value, Probability, Sum, Probability, Sum).
	prefix(node(Key, _, PrefixProb, PrefixMean, Left, Right), Value, Prob0, Sum0, Probability, Sum) :-
		( Value < Key -> prefix(Left, Value, Prob0, Sum0, Probability, Sum)
		; prefix(Right, Value, PrefixProb, PrefixMean, Probability, Sum)
		).

	anchors(all, _Seed, Rows, Rows) :-
		!.
	anchors(Size, Seed, Rows, Anchors) :-
		fast_random(xoshiro128pp)::get_seed(Saved),
		(	catch(
				(fast_random(xoshiro128pp)::randomize(Seed), sample_rows(Size, Rows, Anchors)),
				Error,
				(fast_random(xoshiro128pp)::set_seed(Saved), throw(Error))
			) ->
			fast_random(xoshiro128pp)::set_seed(Saved)
		;	fast_random(xoshiro128pp)::set_seed(Saved),
			fail
		).

	sample_rows(Size, Rows, Anchors) :-
		length(Rows, Count),
		avltree::new(Empty),
		index_rows(Rows, 1, Empty, Indexed),
		sample_indexed(Size, Count, Indexed, Anchors).

	index_rows([], _Index, Dictionary, Dictionary).
	index_rows([Row| Rows], Index, Dictionary0, Dictionary) :-
		avltree::insert(Dictionary0, Index, Row, Dictionary1),
		Next is Index + 1,
		index_rows(Rows, Next, Dictionary1, Dictionary).

	sample_indexed(0, _Count, _Rows, []) :-
		!.
	sample_indexed(Size, Count, Rows, [Row| Anchors]) :-
		fast_random(xoshiro128pp)::between(1, Count, Index),
		avltree::lookup(Index, Row, Rows),
		Next is Size - 1,
		sample_indexed(Next, Count, Rows, Anchors).

	row_positions([], []).
	row_positions([row(Position, _, _)| Rows], [Position| Positions]) :-
		row_positions(Rows, Positions).

	zero_vector([], []).
	zero_vector([_| Features], [0.0| Zeros]) :-
		zero_vector(Features, Zeros).

	pair_scores([], [], []).
	pair_scores([Feature| Features], [Score| Values], [Feature-Score| Scores]) :-
		pair_scores(Features, Values, Scores).

	score_names([], []).
	score_names([Feature-_| Scores], [Feature| Names]) :-
		score_names(Scores, Names).

	accumulate([], _Rows, _Columns, _Variant, _Classes, _Scale, _K, _Weighting, Totals, Products, Mass, Totals, Products, Mass).
	accumulate([Anchor| Anchors], Rows, Columns, Variant, Classes, Scale, K, Weighting, Totals0, Products0, Mass0, Totals, Products, Mass) :-
		neighbors(Anchor, Rows, Columns, Variant, Neighbors),
		(	Variant == regression ->
			take_neighbors(K, Neighbors, Taken),
			weighted_neighbors(Taken, Weighting, Weighted),
			Anchor = row(_, Target, _),
			normalized(Target, Scale, NormalTarget),
			regression_neighbors(Weighted, NormalTarget, Scale, Totals0, Products0, Mass0, Totals1, Products1, Mass1)
		;	class_update(Classes, Anchor, Neighbors, K, Weighting, Classes, Totals0, Totals1),
			Products1 = Products0,
			Mass1 = Mass0
		),
		accumulate(Anchors, Rows, Columns, Variant, Classes, Scale, K, Weighting, Totals1, Products1, Mass1, Totals, Products, Mass).

	neighbors(row(Position, Target, Values), Rows, Columns, Variant, Neighbors) :-
		findall(
			(Distance-OtherPosition)-neighbor(OtherTarget, Differences),
			(	member(row(OtherPosition, OtherTarget, OtherValues), Rows),
				OtherPosition =\= Position,
				(	Variant == regression ->
					Group = pooled,
					OtherGroup = pooled
				;	Group = class(Target),
					OtherGroup = class(OtherTarget)
				),
				differences(Values, OtherValues, Columns, Group, OtherGroup, Differences, 0.0, Distance)
			),
			Keyed
		),
		keysort(Keyed, Sorted),
		neighbor_values(Sorted, Neighbors).

	neighbor_values([], []).
	neighbor_values([_-Neighbor| Keyed], [Neighbor| Neighbors]) :-
		neighbor_values(Keyed, Neighbors).

	differences([], [], [], _Group, _OtherGroup, [], Distance, Distance).
	differences([Value| Values], [Other| Others], [Column| Columns], Group, OtherGroup, [Diff| Diffs], Distance0, Distance) :-
		feature_difference(Value, Other, Column, Group, OtherGroup, Diff),
		Distance1 is Distance0 + Diff,
		differences(Values, Others, Columns, Group, OtherGroup, Diffs, Distance1, Distance).

	feature_difference(known(Value), known(Other), column(Type, _, _), _Group, _OtherGroup, Diff) :-
		(	Type == numeric ->
			Diff is abs(Value - Other)
		;	Value == Other ->
			Diff = 0.0
		;	Diff = 1.0
		).
	feature_difference(known(Value), missing, column(Type, Distributions, _), _Group, OtherGroup, Diff) :-
		avltree::lookup(OtherGroup, Distribution, Distributions),
		known_unknown(Type, Value, Distribution, Diff).
	feature_difference(missing, known(Value), column(Type, Distributions, _), Group, _OtherGroup, Diff) :-
		avltree::lookup(Group, Distribution, Distributions),
		known_unknown(Type, Value, Distribution, Diff).
	feature_difference(missing, missing, column(_, _, Cache), Group, OtherGroup, Diff) :-
		avltree::lookup(Group-OtherGroup, Diff, Cache).

	take_neighbors(0, _Neighbors, []) :-
		!.
	take_neighbors(_K, [], []) :-
		!.
	take_neighbors(K, [Neighbor| Neighbors], [Neighbor| Taken]) :-
		Next is K - 1,
		take_neighbors(Next, Neighbors, Taken).

	weighted_neighbors(Neighbors, Weighting, Weighted) :-
		rank_weights(Neighbors, Weighting, 0, Raw, 0.0, Total),
		normalize_weights(Raw, Total, Weighted).

	rank_weights([], _Weighting, _Rank, [], Total, Total).
	rank_weights([Neighbor| Neighbors], Weighting, Rank, [Weight-Neighbor| Weighted], Total0, Total) :-
		(	Weighting == uniform ->
			Weight = 1.0
		;	Weighting = rank(Sigma),
			Ratio is Rank / Sigma,
			Weight is exp(-(Ratio * Ratio))
		),
		Next is Rank + 1,
		Total1 is Total0 + Weight,
		rank_weights(Neighbors, Weighting, Next, Weighted, Total1, Total).

	normalize_weights([], _Total, []).
	normalize_weights([Weight-Neighbor| Raw], Total, [Normal-Neighbor| Weighted]) :-
		Normal is Weight / Total,
		normalize_weights(Raw, Total, Weighted).

	class_update([], _Anchor, _Neighbors, _K, _Weighting, _Classes, Totals, Totals).
	class_update([Class-Count| Rest], Anchor, Neighbors, K, Weighting, Classes, Totals0, Totals) :-
		Anchor = row(_, Target, _),
		class_neighbors(Neighbors, Class, ClassNeighbors),
		take_neighbors(K, ClassNeighbors, Taken),
		weighted_neighbors(Taken, Weighting, Weighted),
		(	Class == Target ->
			Factor = -1.0
		;	memberchk(Target-AnchorCount, Classes),
			count_total(Classes, 0, TotalCount),
			Factor is Count / (TotalCount - AnchorCount)
		),
		add_neighbors(Weighted, Factor, Totals0, Totals1),
		class_update(Rest, Anchor, Neighbors, K, Weighting, Classes, Totals1, Totals).

	class_neighbors([], _Class, []).
	class_neighbors([neighbor(Target, Diffs)| Neighbors], Class, Selected) :-
		(	Class == Target ->
			Selected = [neighbor(Target, Diffs)| Rest]
		;	Selected = Rest
		),
		class_neighbors(Neighbors, Class, Rest).

	count_total([], Total, Total).
	count_total([_-Count| Counts], Total0, Total) :-
		Total1 is Count + Total0,
		count_total(Counts, Total1, Total).

	add_neighbors([], _Factor, Totals, Totals).
	add_neighbors([Weight-neighbor(_, Diffs)| Neighbors], Factor, Totals0, Totals) :-
		Scale is Factor * Weight,
		add_vector(Diffs, Scale, Totals0, Totals1),
		add_neighbors(Neighbors, Factor, Totals1, Totals).

	add_vector([], _Scale, [], []).
	add_vector([Diff| Diffs], Scale, [Total| Totals], [New| News]) :-
		New is Total + Diff * Scale,
		add_vector(Diffs, Scale, Totals, News).

	regression_neighbors([], _Target, _Scale, Totals, Products, Mass, Totals, Products, Mass).
	regression_neighbors([Weight-neighbor(Target, Diffs)| Neighbors], NormalTarget, Scale, Totals0, Products0, Mass0, Totals, Products, Mass) :-
		normalized(Target, Scale, Normal),
		TargetDiff is abs(Normal - NormalTarget),
		ProductWeight is Weight * TargetDiff,
		Mass1 is Mass0 + ProductWeight,
		add_vector(Diffs, Weight, Totals0, Totals1),
		add_vector(Diffs, ProductWeight, Products0, Products1),
		regression_neighbors(Neighbors, NormalTarget, Scale, Totals1, Products1, Mass1, Totals, Products, Mass).

	final_scores(regression, scale(_, _, Range, _, _), M, Totals, Products, Mass, Scores, Degeneracy) :-
		!,
		Complement is M - Mass,
		(	Range =:= 0 ->
			zero_vector(Totals, Scores),
			Degeneracy = constant_target
		;	Mass =< 0.0 ->
			zero_vector(Totals, Scores),
			Degeneracy = zero_conditioning_mass
		;	Complement =< 0.0 ->
			zero_vector(Totals, Scores),
			Degeneracy = zero_conditioning_mass
		;	regression_scores(Totals, Products, Mass, Complement, Scores),
			Degeneracy = none
		).
	final_scores(_Variant, _Scale, M, Totals, _Products, _Mass, Scores, none) :-
		divide_vector(Totals, M, Scores).

	divide_vector([], _M, []).
	divide_vector([Total| Totals], M, [Score| Scores]) :-
		Score is Total / M,
		divide_vector(Totals, M, Scores).

	regression_scores([], [], _Mass, _Complement, []).
	regression_scores([Total| Totals], [Product| Products], Mass, Complement, [Score| Scores]) :-
		Score is Product / Mass - (Total - Product) / Complement,
		regression_scores(Totals, Products, Mass, Complement, Scores).

	population_metadata(regression, _Classes, scale(_, _, _, Min, Max), Mass, M,
		regression(target_range(Min, Max), conditioning_mass(Mass, Complement))) :-
		!,
		Complement is M - Mass.
	population_metadata(_Variant, Classes, _Scale, _Mass, _M, classes(Classes)).

	valid_model(Selector) :-
		::relief_model(Model),
		::relief_variant(Variant),
		Selector =.. [Model, Scores, Selected, Diagnostics],
		^^valid_selector_metadata(Model, Diagnostics),
		Diagnostics = [model(Model), example_count(ExampleCount), options(Options),
			variant(Variant), candidate_count(CandidateCount), selected_count(SelectedCount),
			features(Declarations), eligible_count(Eligible), excluded_count(Excluded),
			eligible_positions(Positions), samples(Samples), population(Population), degeneracy(Degeneracy)
		],
		^^valid_options(Options),
		^^option(missing_values(_), Options),
		^^option(random_seed(_), Options),
		^^option(neighbor_weighting(_), Options),
		neighbor_count(Variant, Options, _),
		valid(non_negative_integer, CandidateCount),
		valid(non_negative_integer, SelectedCount),
		valid(positive_integer, Eligible),
		valid(non_negative_integer, Excluded),
		ExampleCount =:= Eligible + Excluded,
		valid(list(pair), Declarations),
		valid_declarations(Declarations, Names),
		length(Names, CandidateCount),
		valid(list(pair), Scores),
		length(Scores, CandidateCount),
		ordered_scores(Names, Scores, Ordered),
		^^sort_by_decreasing_score(Ordered, ExpectedScores),
		Scores == ExpectedScores,
		^^option(selection_strategy(Strategy), Options),
		selection(Strategy, Scores, Expected),
		Selected == Expected,
		length(Selected, SelectedCount),
		valid(list(positive_integer), Positions),
		length(Positions, Eligible),
		increasing_positions(Positions, 0, ExampleCount),
		valid(list(positive_integer), Samples),
		^^option(sample_size(Size), Options),
		valid_samples(Size, Samples, Positions),
		length(Samples, M),
		valid_population(Variant, Population, Eligible, M, Degeneracy, Scores).

	valid_declarations([], []).
	valid_declarations([Feature-Type| Declarations], [Feature| Names]) :-
		atomic(Feature),
		once((Type == numeric; Type == categorical)),
		valid_declarations(Declarations, Names),
		\+ member(Feature, Names).

	ordered_scores([], _Scores, []).
	ordered_scores([Feature| Features], Scores, [Feature-Score| Ordered]) :-
		memberchk(Feature-Score, Scores),
		number(Score),
		ordered_scores(Features, Scores, Ordered).

	increasing_positions([], _Previous, _Limit).
	increasing_positions([Position| Positions], Previous, Limit) :-
		Position > Previous,
		Position =< Limit,
		increasing_positions(Positions, Position, Limit).

	valid_samples(all, Samples, Positions) :-
		Samples == Positions.
	valid_samples(Size, Samples, Positions) :-
		integer(Size),
		length(Samples, Size),
		samples_in_pool(Samples, Positions).

	samples_in_pool([], _Positions).
	samples_in_pool([Position| Samples], Positions) :-
		memberchk(Position, Positions),
		samples_in_pool(Samples, Positions).

	valid_population(regression, regression(target_range(Min, Max), conditioning_mass(Mass, Complement)), Eligible, M, Degeneracy, Scores) :-
		!,
		Eligible >= 2,
		number(Min),
		number(Max),
		Min =< Max,
		number(Mass),
		number(Complement),
		Mass >= 0,
		Complement >= 0,
		M =:= Mass + Complement,
		(	Min =:= Max ->
			Degeneracy == constant_target,
			Mass =:= 0,
			zero_scores(Scores)
		;	(Mass =:= 0; Complement =:= 0) ->
			Degeneracy == zero_conditioning_mass,
			zero_scores(Scores)
		;	Degeneracy == none
		).
	valid_population(Variant, classes(Counts), Eligible, _M, none, _Scores) :-
		valid(list(pair), Counts),
		valid_class_counts(Counts, Keys),
		length(Keys, Count),
		(	Variant == binary ->
			Count =:= 2
		;	Variant == multiclass,
			Count >= 2
		),
		count_total(Counts, 0, Eligible).

	valid_class_counts([], []).
	valid_class_counts([Class-Count| Counts], [Class| Keys]) :-
		atomic(Class),
		integer(Count),
		Count >= 2,
		valid_class_counts(Counts, Keys),
		\+ member(Class, Keys).

	zero_scores([]).
	zero_scores([_-Score| Scores]) :-
		Score =:= 0,
		zero_scores(Scores).

:- end_category.

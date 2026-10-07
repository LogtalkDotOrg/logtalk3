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


:- object(relief_test_dataset(_Declarations_, _Rows_),
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Parametric small-data fixtures for the Relief selector family.',
		parameters is [
			'Declarations' - 'Feature declarations.',
			'Rows' - 'Feature-list and target pairs.'
		]
	]).

	:- uses(list, [
		length/2, member/2, nth1/3
	]).

	attribute_values(Feature, Declaration) :-
		member(Feature-Declaration, _Declarations_).

	example_count(Count) :-
		length(_Rows_, Count).

	example(Position, Features, Target) :-
		nth1(Position, _Rows_, Features-Target).

:- end_object.


:- object(relief_sampling_probe,
	imports(relief_feature_selector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Inherited sampling cleanup probe for the Relief family.'
	]).

	:- public(sample/4).
	:- mode(sample(+term, +positive_integer, +list(compound), -list(compound)), zero_or_one_or_error).
	:- info(sample/4, [
		comment is 'Delegates to protected sampling to exercise failure and exception restoration.',
		argnames is ['Size', 'Seed', 'Rows', 'Anchors'],
		exceptions is [
			'An injected sampling size is not evaluable' - type_error(evaluable, 'Size')
		]
	]).

	relief_model(relief_sampling_probe).

	relief_variant(binary).

	sample(Size, Seed, Rows, Anchors) :-
		^^anchors(Size, Seed, Rows, Anchors).

:- end_object.


:- object(relief_test_reference).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Independent exhaustive small-data Relief oracle using direct empirical support scans.'
	]).

	:- public(scores/4).
	:- mode(scores(+object_identifier, +atom, -list(pair), +list(compound)), one).
	:- info(scores/4, [
		comment is 'Computes all-row reference scores for valid small fixtures without cached distributions.',
		argnames is ['Dataset', 'Variant', 'Scores', 'Options']
	]).

	:- uses(list, [
		last/2, length/2, member/2, memberchk/2, nth1/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	scores(Dataset, Variant, Scores, Options) :-
		findall(Feature-Type, Dataset::attribute_values(Feature, Type), Features),
		findall(row(Index, Values, Target), Dataset::example(Index, Values, Target), Rows),
		findall(Target, member(row(_, _, Target), Rows), Targets),
		sort(Targets, Classes),
		(	member(missing_values(probabilistic), Options) ->
			Missing = probabilistic
		;	Missing = complete_case
		),
		(	member(number_of_neighbors(K0), Options) ->
			K = K0
		;	K = 1
		),
		(	member(neighbor_weighting(Weighting0), Options) ->
			Weighting = Weighting0
		;	Weighting = uniform
		),
		length(Rows, M),
		findall(
			Feature-Score,
			(	member(Feature-Type, Features),
				findall(
					Weight-Diff-TargetDiff,
					(	member(Anchor, Rows),
						reference_neighbors(Anchor, Rows, Features, Variant, Missing, Neighbors),
						(	Variant == regression ->
							prefix_neighbors(K, Neighbors, Taken),
							weighted(Taken, Weighting, Weighted),
							member(Weight-Neighbor, Weighted)
						;	member(Class, Classes),
							findall(Neighbor, (member(Neighbor, Neighbors), Neighbor = row(_, _, Label), Label == Class), SameClass),
							prefix_neighbors(K, SameClass, Taken),
							weighted(Taken, Weighting, Weighted),
							member(LocalWeight-Neighbor, Weighted),
							Anchor = row(_, _, AnchorClass),
							(	Class == AnchorClass ->
								Weight is -LocalWeight
							;	count_class(Rows, Class, Count),
								count_class(Rows, AnchorClass, AnchorCount),
								Weight is LocalWeight * Count / (M - AnchorCount)
							)
						),
						difference(Feature, Type, Anchor, Neighbor, Rows, Variant, Missing, Diff),
						Anchor = row(_, _, Target1),
						Neighbor = row(_, _, Target2),
						(	Variant == regression ->
							normalized_difference(Target1, Target2, Targets, TargetDiff)
						;	TargetDiff = 0
						)
					),
					Contributions
				),
				sums(Contributions, 0.0, 0.0, 0.0, Total, Product, Mass),
				(	Variant \== regression ->
					Score is Total / M
				;	(Mass =:= 0; M - Mass =:= 0) ->
					Score = 0.0
				;	Score is Product / Mass - (Total - Product) / (M - Mass)
				)
			),
			Scores
		).

	reference_neighbors(Anchor, Rows, Features, Variant, Missing, Neighbors) :-
		Anchor = row(Position, _, _),
		findall(
			(Distance-OtherPosition)-Other,
			(	member(Other, Rows),
				Other = row(OtherPosition, _, _),
				Position =\= OtherPosition,
				findall(Diff, (member(Feature-Type, Features), difference(Feature, Type, Anchor, Other, Rows, Variant, Missing, Diff)), Diffs),
				sum(Diffs, Distance)
			),
			Keyed
		),
		keysort(Keyed, Sorted),
		findall(Row, member(_-Row, Sorted), Neighbors).

	difference(Feature, Type, row(_, First, Class1), row(_, Second, Class2), Rows, Variant, _Missing, Diff) :-
		value(Feature, First, Value1),
		value(Feature, Second, Value2),
		support(Feature, Rows, Variant, Class1, Support1),
		support(Feature, Rows, Variant, Class2, Support2),
		findall(Value, (member(row(_, Values, _), Rows), member(Feature-Value, Values), nonvar(Value)), Pooled),
		(	nonvar(Value1) ->
			Values1 = [Value1]
		;	Values1 = Support1
		),
		(	nonvar(Value2) ->
			Values2 = [Value2]
		;	Values2 = Support2
		),
		findall(
			Difference,
			(	member(Left, Values1),
				member(Right, Values2),
				( Type == continuous ->
					normalized_difference(Left, Right, Pooled, Difference)
				;	Left == Right ->
					Difference = 0.0
				;	Difference = 1.0
				)
			),
			Differences
		),
		(	Differences == [] ->
			Diff = 0.0
		;	sum(Differences, Total),
			length(Differences, Count),
			Diff is Total / Count
		).

	value(Feature, Values, Value) :-
		(	member(Feature-Found, Values) ->
			Value = Found
		;	true
		).

	support(Feature, Rows, Variant, Class, Support) :-
		findall(
			Value,
			(	member(row(_, Values, Target), Rows),
				(	Variant == regression ->
					true
				;	Target == Class
				),
				member(Feature-Value, Values), nonvar(Value)
			),
			ClassValues
		),
		(	ClassValues == [] ->
			findall(Value, (member(row(_, Values, _), Rows), member(Feature-Value, Values), nonvar(Value)), Support)
		;	Support = ClassValues
		).

	normalized_difference(Left, Right, Values, Diff) :-
		sort(Values, Sorted),
		Sorted = [Minimum| _],
		last(Sorted, Maximum),
		(	Minimum =:= Maximum ->
			Diff = 0.0
		;	Diff is abs(Left - Right) / (Maximum - Minimum)
		).

	count_class(Rows, Class, Count) :-
		findall(1, (member(row(_, _, Target), Rows), Target == Class), Matches),
		length(Matches, Count).

	prefix_neighbors(0, _Rows, []) :-
		!.
	prefix_neighbors(_K, [], []) :-
		!.
	prefix_neighbors(K, [Row| Rows], [Row| Taken]) :-
		Next is K - 1,
		prefix_neighbors(Next, Rows, Taken).

	weighted(Rows, Scheme, Weighted) :-
		findall(
			Weight-Row,
			(	nth1(Position, Rows, Row),
				(	Scheme == uniform -> Weight = 1.0
				;	Scheme = rank(Sigma),
					Ratio is (Position - 1) / Sigma,
					Weight is exp(-(Ratio * Ratio))
				)
			),
			Raw
		),
		findall(Weight, member(Weight-_, Raw), Weights),
		sum(Weights, Total),
		findall(Normal-Row, (member(Weight-Row, Raw), Normal is Weight / Total), Weighted).

	sums([], Total, Product, Mass, Total, Product, Mass).
	sums([Weight-Diff-TargetDiff| Rest], Total0, Product0, Mass0, Total, Product, Mass) :-
		Total1 is Total0 + Weight * Diff,
		Product1 is Product0 + Weight * Diff * TargetDiff,
		Mass1 is Mass0 + Weight * TargetDiff,
		sums(Rest, Total1, Product1, Mass1, Total, Product, Mass).

:- end_object.

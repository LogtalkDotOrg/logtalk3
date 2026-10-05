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


:- category(item_content_dataset_validation).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Shared collection and validation of item catalogs and content descriptors.',
		see_also is [item_content_dataset_protocol, recommender_common]
	]).

	:- protected(collect_contents/3).
	:- mode(collect_contents(+object_identifier, -list(pair), -atom), one_or_error).
	:- info(collect_contents/3, [
		comment is 'Collects and validates a nonempty catalog and its content declarations.',
		argnames is ['Dataset', 'Contents', 'Kind'],
		exceptions is [
			'The catalog is empty' - domain_error(non_empty_catalog, 'Dataset'),
			'An item identifier is a variable' - instantiation_error,
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'A catalog item is declared more than once' - domain_error(duplicate_item, 'Item'),
			'Content declarations do not cover the catalog exactly once' - domain_error(item_content_coverage, 'Dataset'),
			'A content descriptor is invalid' - domain_error(item_content, 'Content'),
			'Content representations are mixed' - domain_error(content_representation, 'Content'),
			'A content list is not a proper list' - type_error(list, 'List'),
			'A feature or content list is not ground' - instantiation_error,
			'A sparse vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A sparse vector entry is not a pair' - type_error(pair, 'Entry'),
			'A sparse weight is not numeric' - type_error(number, 'Weight'),
			'A sparse weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight')
		]
	]).

	:- protected(canonical_contents/3).
	:- mode(canonical_contents(+list(pair), -list(pair), -atom), zero_or_one_or_error).
	:- info(canonical_contents/3, [
		comment is 'Canonicalizes content declarations, preserving feature occurrences and removing zero vector weights. Fails for an empty list.',
		argnames is ['Contents', 'Canonical', 'Kind'],
		exceptions is [
			'An item identifier or feature is a variable' - instantiation_error,
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'An item is declared more than once' - domain_error(duplicate_item, 'Item'),
			'A content descriptor is invalid' - domain_error(item_content, 'Content'),
			'Content representations are mixed' - domain_error(content_representation, 'Content'),
			'A content list is not a proper list' - type_error(list, 'List'),
			'A sparse vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A sparse vector entry is not a pair' - type_error(pair, 'Entry'),
			'A sparse weight is not numeric' - type_error(number, 'Weight'),
			'A sparse weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight')
		]
	]).

	:- protected(check_rated_catalog/2).
	:- mode(check_rated_catalog(+list(compound), +list(pair)), one_or_error).
	:- info(check_rated_catalog/2, [
		comment is 'Checks that every rated item belongs to the content catalog.',
		argnames is ['Ratings', 'Contents'],
		exceptions is [
			'A rated item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		check/3
	]).

	collect_contents(Dataset, Contents, Kind) :-
		findall(Item, Dataset::item(Item), Items0),
		(	Items0 == [] ->
			domain_error(non_empty_catalog, Dataset)
		;	true
		),
		check_catalog_items(Items0, []),
		sort(Items0, Items),
		findall(Item-Content, Dataset::item_content(Item, Content), Contents0),
		(	Contents0 == [] ->
			domain_error(item_content_coverage, Dataset)
		;	true
		),
		canonical_contents(Contents0, Contents, Kind),
		keys(Contents, ContentItems),
		(	Items == ContentItems ->
			true
		;	domain_error(item_content_coverage, Dataset)
		).

	check_catalog_items([], _Seen).
	check_catalog_items([Item| Items], Seen) :-
		context(Context), check(atomic, Item, Context),
		(	member(Item, Seen) ->
			domain_error(duplicate_item, Item)
		;	true
		),
		check_catalog_items(Items, [Item| Seen]).

	canonical_contents(Contents0, Contents, Kind) :-
		context(Context), check(list, Contents0, Context),
		keys(Contents0, Items),
		check_catalog_items(Items, []),
		keysort(Contents0, Sorted),
		Sorted = [_-First| _],
		content_kind(First, Kind),
		canonical_descriptors(Sorted, Kind, Contents).

	content_kind(Content, Kind) :-
		(	var(Content) ->
			instantiation_error
		;	Content = features(_) ->
			Kind = features
		;	Content = vector(_) ->
			Kind = vectors
		;	domain_error(item_content, Content)
		).

	canonical_descriptors([], _Kind, []).
	canonical_descriptors([Item-Content| Contents], Kind, [Item-Canonical| Canonicals]) :-
		content_kind(Content, ContentKind),
		(	ContentKind == Kind ->
			true
		;	domain_error(content_representation, Content)
		),
		canonical_descriptor(Content, Canonical),
		canonical_descriptors(Contents, Kind, Canonicals).

	canonical_descriptor(features(Features), features(Sorted)) :-
		context(Context),
		check(list, Features, Context),
		(	ground(Features) ->
			true
		;	instantiation_error
		),
		occurrence_pairs(Features, Pairs), keysort(Pairs, Ordered), keys(Ordered, Sorted).
	canonical_descriptor(vector(Pairs), vector(Vector)) :-
		context(Context),
		check(list, Pairs, Context),
		check_vector_entries(Pairs, []),
		keysort(Pairs, Sorted),
		remove_zero_weights(Sorted, Vector).

	occurrence_pairs([], []).
	occurrence_pairs([Feature| Features], [Feature-1| Pairs]) :-
		occurrence_pairs(Features, Pairs).

	check_vector_entries([], _Seen).
	check_vector_entries([Entry| Entries], Seen) :-
		context(Context),
		(	var(Entry) ->
			instantiation_error
		;	Entry = Feature-Weight ->
			(	ground(Feature) ->
				true
			;	instantiation_error
			),
			(	member(Feature, Seen) ->
				domain_error(duplicate_feature, Feature)
			;	true
			),
			check(number, Weight, Context),
			(	finite_weight(Weight),
				Weight >= 0 ->
				true
			;	domain_error(non_negative_finite_weight, Weight)
			),
			check_vector_entries(Entries, [Feature| Seen])
		;	type_error(pair, Entry)
		).

	finite_weight(Value) :-
		number(Value),
		catch((Zero is Value - Value, Zero =:= 0), _, fail).

	remove_zero_weights([], []).
	remove_zero_weights([Feature-Weight| Pairs], Vector) :-
		(	Weight =:= 0 ->
			Vector = Rest
		;	Vector = [Feature-Weight| Rest]
		),
		remove_zero_weights(Pairs, Rest).

	check_rated_catalog([], _Contents).
	check_rated_catalog([rating(_, Item, _)| Ratings], Contents) :-
		(	member(Item-_, Contents) ->
			true
		;	domain_error(catalog_item, Item)
		),
		check_rated_catalog(Ratings, Contents).

:- end_category.

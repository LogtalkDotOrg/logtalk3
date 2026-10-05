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


:- object(bm25_recommender,
	imports([recommender_common, item_content_dataset_validation])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-05,
		comment is 'Content-based recommender using raw-count query profiles and Okapi BM25 relevance scores.',
		see_also is [recommender_protocol, item_content_dataset_protocol, tfidf_recommender]
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- public(score_all/4).
	:- mode(score_all(+compound, +atomic, +list(atomic), -list(pair)), one_or_error).
	:- info(score_all/4, [
		comment is 'Scores catalog items in input order, preserving duplicates and validating the model once. An empty batch validates the model and user.',
		argnames is ['Recommender', 'User', 'Items', 'Scores'],
		exceptions is [
			'A required argument or identifier is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'Items is not a proper list' - type_error(list, 'Items'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	:- public(score_content/4).
	:- mode(score_content(+compound, +atomic, +compound, -float), one_or_error).
	:- info(score_content/4, [
		comment is 'Scores supplied content of the trained representation using frozen corpus statistics. Unseen terms contribute to document length but not matching relevance.',
		argnames is ['Recommender', 'User', 'Content', 'Score'],
		exceptions is [
			'A required argument, feature, entry, or count is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'User is not atomic' - type_error(atomic, 'User'),
			'A descriptor is invalid' - domain_error(item_content, 'Content'),
			'The descriptor differs from the trained representation' - domain_error(content_representation, 'Content'),
			'A content list is not a proper list' - type_error(list, 'List'),
			'A vector entry is not a pair' - type_error(pair, 'Entry'),
			'A vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A vector count is not numeric' - type_error(number, 'Count'),
			'A vector count is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Count'),
			'A nonzero vector count is not an integer' - type_error(integer, 'Count')
		]
	]).

	:- public(update_ratings/3).
	:- mode(update_ratings(+compound, +list(compound), -compound), one_or_error).
	:- info(update_ratings/3, [
		comment is 'Returns a model with inserted or replaced ratings and rebuilt query profiles, retaining fitted corpus weights. New users are accepted but items must belong to the catalog.',
		argnames is ['Recommender', 'Ratings', 'UpdatedRecommender'],
		exceptions is [
			'A required argument, record, identifier, or value is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'Ratings is not a proper list' - type_error(list, 'Ratings'),
			'An entry is not a rating record' - type_error(rating, 'Entry'),
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'A rating value is not numeric' - type_error(number, 'Rating'),
			'A rating value is nonfinite' - domain_error(finite_rating, 'Rating'),
			'A rating value is outside the stored scale' - domain_error(rating_scale('Min', 'Max'), 'Rating'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item'),
			'A user-item pair occurs more than once in the updates' - domain_error(duplicate_rating, 'User'-'Item'),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	:- public(remove_ratings/3).
	:- mode(remove_ratings(+compound, +list(pair), -compound), one_or_error).
	:- info(remove_ratings/3, [
		comment is 'Returns a model with requested ratings removed and query profiles rebuilt. Missing and repeated pairs are accepted; no matching ratings returns the validated original model.',
		argnames is ['Recommender', 'UserItemPairs', 'UpdatedRecommender'],
		exceptions is [
			'A required argument, entry, or identifier is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'UserItemPairs is not a proper list' - type_error(list, 'UserItemPairs'),
			'An entry is not a pair' - type_error(pair, 'Entry'),
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'No ratings would remain' - domain_error(non_empty_ratings, []),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	:- public(extend_catalog/3).
	:- mode(extend_catalog(+compound, +list(pair), -compound), one_or_error).
	:- info(extend_catalog/3, [
		comment is 'Returns a model with new catalog items, refitting corpus statistics and all item weights. Descriptors must match the trained representation.',
		argnames is ['Recommender', 'ItemContents', 'UpdatedRecommender'],
		exceptions is [
			'A required argument, entry, identifier, feature, or count is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'A supplied list is not proper' - type_error(list, 'List'),
			'A catalog or vector entry is not a pair' - type_error(pair, 'Entry'),
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'A batch repeats an item identifier' - domain_error(duplicate_item, 'Item'),
			'An item already belongs to the catalog' - domain_error(new_catalog_item, 'Item'),
			'A descriptor is invalid' - domain_error(item_content, 'Content'),
			'A descriptor differs from the trained representation' - domain_error(content_representation, 'Content'),
			'A vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A vector count is not numeric' - type_error(number, 'Count'),
			'A vector count is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Count'),
			'A nonzero vector count is not an integer' - type_error(integer, 'Count')
		]
	]).

	:- public(replace_content/3).
	:- mode(replace_content(+compound, +list(pair), -compound), one_or_error).
	:- info(replace_content/3, [
		comment is 'Returns a model with replaced catalog descriptors, refitting corpus statistics, all item weights, and raw query profiles. Identifiers must already belong to the catalog.',
		argnames is ['Recommender', 'ItemContents', 'UpdatedRecommender'],
		exceptions is [
			'A required argument, entry, identifier, feature, or count is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'A supplied list is not proper' - type_error(list, 'List'),
			'A catalog or vector entry is not a pair' - type_error(pair, 'Entry'),
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'A batch repeats an item identifier' - domain_error(duplicate_item, 'Item'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item'),
			'A descriptor is invalid' - domain_error(item_content, 'Content'),
			'A descriptor differs from the trained representation' - domain_error(content_representation, 'Content'),
			'A vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A vector count is not numeric' - type_error(number, 'Count'),
			'A vector count is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Count'),
			'A nonzero vector count is not an integer' - type_error(integer, 'Count')
		]
	]).

	:- public(remove_catalog/3).
	:- mode(remove_catalog(+compound, +list(atomic), -compound), one_or_error).
	:- info(remove_catalog/3, [
		comment is 'Removes catalog items and their ratings, refitting remaining corpus weights and affected profiles. Missing and repeated identifiers are accepted.',
		argnames is ['Recommender', 'Items', 'UpdatedRecommender'],
		exceptions is [
			'A required argument or identifier is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'Items is not a proper list' - type_error(list, 'Items'),
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'No catalog items would remain' - domain_error(non_empty_catalog, []),
			'No ratings would remain' - domain_error(non_empty_ratings, []),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	:- public(set_rating_scale/3).
	:- mode(set_rating_scale(+compound, +term, -compound), one_or_error).
	:- info(set_rating_scale/3, [
		comment is 'Changes only the stored rating scale to ``none`` or ``scale(Min,Max)``, requiring finite ordered bounds containing all ratings. Ratings and relevance scores are not rescaled.',
		argnames is ['Recommender', 'Scale', 'UpdatedRecommender'],
		exceptions is [
			'A required argument or scale bound is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'A scale bound is not numeric' - type_error(number, 'Bound'),
			'The scale descriptor is invalid' - domain_error(rating_scale, 'Scale'),
			'Scale bounds are nonfinite or reversed' - domain_error(rating_scale, 'Min'-'Max'),
			'A stored rating is outside the proposed scale' - domain_error(rating_scale('Min','Max'), 'Rating')
		]
	]).

	learn(Dataset, bm25_model(Ratings, Contents, Weights, Profiles, Corpus, Scale, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_ratings(Dataset, Ratings0),
		^^check_ratings(Dataset, Ratings0),
		sort(Ratings0, Ratings),
		check_finite_ratings(Ratings),
		^^dataset_rating_scale(Dataset, Scale),
		^^collect_contents(Dataset, Contents, Kind),
		^^check_rated_catalog(Ratings, Contents),
		content_documents(Contents, Documents),
		fit_corpus(Documents, Corpus),
		build_item_weights(Documents, Corpus, Options, Weights),
		build_profiles(Ratings, Documents, Options, Profiles),
		model_diagnostics(Ratings, Profiles, Corpus, Kind, Options, Diagnostics).

	score(Model, User, Item, Score) :-
		^^check_recommender(Model),
		^^check_query_identifiers(User, Item),
		Model = bm25_model(_, _, Weights, Profiles, _, _, Diagnostics),
		memberchk(options(Options), Diagnostics),
		catalog_score(Weights, Profiles, User, Item, Score, Options).

	score_all(Model, User, Items, Scores) :-
		^^check_recommender(Model),
		context(Context),
		check(atomic, User, Context),
		check(list, Items, Context),
		Model = bm25_model(_, _, Weights, Profiles, _, _, Diagnostics),
		memberchk(options(Options), Diagnostics),
		score_catalog_items(Weights, Profiles, User, Items, Scores, Options).

	score_content(Model, User, Content, Score) :-
		^^check_recommender(Model),
		context(Context),
		check(atomic, User, Context),
		^^canonical_contents([content-Content], [content-Canonical], Kind),
		Model = bm25_model(_, _, _, Profiles, Corpus, _, Diagnostics),
		check_content_kind(Kind, Content, Diagnostics),
		descriptor_document(Canonical, Document),
		memberchk(options(Options), Diagnostics),
		document_weights(Document, Corpus, Options, Weights),
		profile_score(Profiles, User, Weights, Score, Options).

	update_ratings(Model, Updates, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, Updates, Context),
		(	Updates == [] ->
			UpdatedModel = Model
		;	Model = bm25_model(Ratings, Contents, Weights, Profiles, Corpus, Scale, Diagnostics),
			check_rating_updates(Updates, Contents, Scale),
			^^check_no_duplicate_ratings(Updates),
			retain_unchanged_ratings(Ratings, Updates, Retained),
			append(Retained, Updates, Merged),
			sort(Merged, UpdatedRatings),
			(	UpdatedRatings == Ratings ->
				UpdatedModel = Model
			;	changed_rating_records(Updates, Ratings, Changed),
				^^users(Changed, Affected),
				rebuild_feedback_model(UpdatedRatings, Contents, Weights, Profiles, Corpus, Scale, Diagnostics, Affected, UpdatedModel)
			)
		).

	remove_ratings(Model, UserItemPairs, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, UserItemPairs, Context),
		Model = bm25_model(Ratings, Contents, Weights, Profiles, Corpus, Scale, Diagnostics),
		rating_removal_records(UserItemPairs, Ratings, Removals),
		retain_unchanged_ratings(Ratings, Removals, Remaining),
		(	Remaining == Ratings ->
			UpdatedModel = Model
		;	(	Remaining == [] ->
				domain_error(non_empty_ratings, [])
			;	^^users(Removals, Affected),
				rebuild_feedback_model(Remaining, Contents, Weights, Profiles, Corpus, Scale, Diagnostics, Affected, UpdatedModel)
			)
		).

	extend_catalog(Model, ItemContents, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, ItemContents, Context),
		(	ItemContents == [] ->
			UpdatedModel = Model
		;	Model = bm25_model(Ratings, Contents, _, Profiles, _, Scale, Diagnostics),
			check_catalog_entries(ItemContents),
			^^canonical_contents(ItemContents, Additions, Kind),
			check_catalog_kind(Additions, Kind, Diagnostics),
			check_new_catalog_items(Additions, Contents),
			append(Contents, Additions, Merged),
			keysort(Merged, UpdatedContents),
			rebuild_catalog_model(Ratings, UpdatedContents, Profiles, Scale, Diagnostics, UpdatedModel)
		).

	replace_content(Model, ItemContents, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, ItemContents, Context),
		(	ItemContents == [] ->
			UpdatedModel = Model
		;	Model = bm25_model(Ratings, Contents, _, OldProfiles, _, Scale, Diagnostics),
			check_catalog_entries(ItemContents),
			^^canonical_contents(ItemContents, Replacements, Kind),
			check_catalog_kind(Replacements, Kind, Diagnostics),
			check_existing_catalog_items(Replacements, Contents),
			replace_catalog_descriptors(Contents, Replacements, UpdatedContents),
			(	UpdatedContents == Contents ->
				UpdatedModel = Model
			;	changed_catalog_items(Contents, UpdatedContents, Changed),
				affected_catalog_users(Ratings, Changed, Affected),
				memberchk(options(Options), Diagnostics),
				updated_profiles(Ratings, UpdatedContents, Options, OldProfiles, Affected, Profiles),
				rebuild_catalog_model(Ratings, UpdatedContents, Profiles, Scale, Diagnostics, UpdatedModel)
			)
		).

	remove_catalog(Model, Items, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, Items, Context),
		check_catalog_identifiers(Items),
		Model = bm25_model(Ratings, Contents, _, OldProfiles, _, Scale, Diagnostics),
		retain_catalog_items(Contents, Items, RemainingContents),
		(	RemainingContents == Contents ->
			UpdatedModel = Model
		;	(	RemainingContents == [] ->
				domain_error(non_empty_catalog, [])
			;	true
			),
			retain_catalog_ratings(Ratings, Items, RemainingRatings, RemovedRatings),
			(	RemainingRatings == [] ->
				domain_error(non_empty_ratings, [])
			;	true
			),
			^^users(RemovedRatings, Affected),
			memberchk(options(Options), Diagnostics),
			updated_profiles(RemainingRatings, RemainingContents, Options, OldProfiles, Affected, Profiles),
			rebuild_catalog_model(RemainingRatings, RemainingContents, Profiles, Scale, Diagnostics, UpdatedModel)
		).

	:- private(check_catalog_identifiers/1).
	:- mode(check_catalog_identifiers(+list(term)), one_or_error).
	:- info(check_catalog_identifiers/1, [
		comment is 'Checks all catalog request identifiers before applying removals.',
		argnames is ['Items'],
		exceptions is [
			'An identifier is a variable' - instantiation_error,
			'An identifier is not atomic' - type_error(atomic, 'Item')
		]
	]).

	check_catalog_identifiers([]).
	check_catalog_identifiers([Item| Items]) :-
		context(Context),
		check(atomic, Item, Context),
		check_catalog_identifiers(Items).

	retain_catalog_items([], _, []).
	retain_catalog_items([Item-Content| Contents], Items, Remaining) :-
		(	member(Item, Items) ->
			Remaining = Rest
		;	Remaining = [Item-Content| Rest]
		),
		retain_catalog_items(Contents, Items, Rest).

	retain_catalog_ratings([], _, [], []).
	retain_catalog_ratings([rating(User,Item,Rating)| Ratings], Items, Remaining, Removed) :-
		(	member(Item, Items) ->
			Remaining = Rest,
			Removed = [rating(User,Item,Rating)| OtherRemoved]
		;	Remaining = [rating(User,Item,Rating)| Rest],
			Removed = OtherRemoved
		),
		retain_catalog_ratings(Ratings, Items, Rest, OtherRemoved).

	set_rating_scale(Model, Scale, UpdatedModel) :-
		^^check_recommender(Model),
		Model = bm25_model(Ratings, Contents, Weights, Profiles, Corpus, _, Diagnostics),
		check_rating_scale(Scale, Ratings),
		UpdatedModel = bm25_model(Ratings, Contents, Weights, Profiles, Corpus, Scale, Diagnostics).

	:- private(check_rating_scale/2).
	:- mode(check_rating_scale(+term, +list(compound)), one_or_error).
	:- info(check_rating_scale/2, [
		comment is 'Checks a proposed optional finite rating scale against existing ratings.',
		argnames is ['Scale', 'Ratings'],
		exceptions is [
			'The scale or a bound is a variable' - instantiation_error,
			'A bound is not numeric' - type_error(number, 'Bound'),
			'The scale descriptor is invalid' - domain_error(rating_scale, 'Scale'),
			'Bounds are nonfinite or reversed' - domain_error(rating_scale, 'Min'-'Max'),
			'A rating is outside the proposed bounds' - domain_error(rating_scale('Min','Max'), 'Rating')
		]
	]).

	check_rating_scale(Scale, Ratings) :-
		(	var(Scale) ->
			instantiation_error
		;	(	Scale == none ->
				true
			;	(	Scale = scale(Min,Max) ->
					context(Context),
					check(number, Min, Context),
					check(number, Max, Context),
					(	finite_number(Min),
						finite_number(Max),
						Min =< Max ->
						check_scale_ratings(Ratings, Min, Max)
					;	domain_error(rating_scale, Min-Max)
					)
				;	domain_error(rating_scale, Scale)
				)
			)
		).

	:- private(check_scale_ratings/3).
	:- mode(check_scale_ratings(+list(compound), +number, +number), one_or_error).
	:- info(check_scale_ratings/3, [
		comment is 'Checks stored ratings against inclusive validated scale bounds.',
		argnames is ['Ratings', 'Min', 'Max'],
		exceptions is [
			'A rating is outside the proposed bounds' - domain_error(rating_scale('Min','Max'), 'Rating')
		]
	]).

	check_scale_ratings([], _, _).
	check_scale_ratings([rating(_,_,Rating)| Ratings], Min, Max) :-
		(	Rating >= Min,
			Rating =< Max ->
			check_scale_ratings(Ratings, Min, Max)
		;	domain_error(rating_scale(Min,Max), Rating)
		).

	changed_rating_records([], _, []).
	changed_rating_records([Entry| Entries], Ratings, Changed) :-
		(	member(Entry, Ratings) ->
			Changed = Rest
		;	Changed = [Entry| Rest]
		),
		changed_rating_records(Entries, Ratings, Rest).

	changed_catalog_items([], _, []).
	changed_catalog_items([Item-Content| Contents], UpdatedContents, Changed) :-
		memberchk(Item-Updated, UpdatedContents),
		(	Content == Updated ->
			Changed = Rest
		;	Changed = [Item| Rest]
		),
		changed_catalog_items(Contents, UpdatedContents, Rest).

	affected_catalog_users(Ratings, Items, Users) :-
		findall(
			User,
			(	member(rating(User,Item,_), Ratings),
				member(Item, Items)
			),
			Users0
		),
		sort(Users0, Users).

	:- private(check_catalog_entries/1).
	:- mode(check_catalog_entries(+list(term)), one_or_error).
	:- info(check_catalog_entries/1, [
		comment is 'Checks that all supplied catalog entries are pairs before canonicalization.',
		argnames is ['ItemContents'],
		exceptions is [
			'An entry is a variable' - instantiation_error,
			'An entry is not a pair' - type_error(pair, 'Entry')
		]
	]).

	check_catalog_entries([]).
	check_catalog_entries([Entry| Entries]) :-
		context(Context),
		check(pair, Entry, Context),
		check_catalog_entries(Entries).

	:- private(check_existing_catalog_items/2).
	:- mode(check_existing_catalog_items(+list(pair), +list(pair)), one_or_error).
	:- info(check_existing_catalog_items/2, [
		comment is 'Checks that replacements use existing catalog identifiers.',
		argnames is ['Replacements', 'Contents'],
		exceptions is [
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	check_existing_catalog_items([], _).
	check_existing_catalog_items([Item-_| Items], Contents) :-
		(	member(Item-_, Contents) ->
			true
		;	domain_error(catalog_item, Item)
		),
		check_existing_catalog_items(Items, Contents).

	replace_catalog_descriptors([], _, []).
	replace_catalog_descriptors([Item-Content| Contents], Replacements, [Item-UpdatedContent| Updated]) :-
		(	member(Item-Replacement, Replacements) ->
			UpdatedContent = Replacement
		;	UpdatedContent = Content
		),
		replace_catalog_descriptors(Contents, Replacements, Updated).

	:- private(check_catalog_kind/3).
	:- mode(check_catalog_kind(+list(pair), +atom, +list(compound)), one_or_error).
	:- info(check_catalog_kind/3, [
		comment is 'Checks the canonical catalog descriptor representation against the stored representation.',
		argnames is ['ItemContents', 'Kind', 'Diagnostics'],
		exceptions is [
			'A descriptor differs from the trained representation' - domain_error(content_representation, 'Content')
		]
	]).

	check_catalog_kind([_-Content|_], Kind, Diagnostics) :-
		check_content_kind(Kind, Content, Diagnostics).

	:- private(check_new_catalog_items/2).
	:- mode(check_new_catalog_items(+list(pair), +list(pair)), one_or_error).
	:- info(check_new_catalog_items/2, [
		comment is 'Checks that additions use new catalog identifiers.',
		argnames is ['Additions', 'Contents'],
		exceptions is [
			'An item already belongs to the catalog' - domain_error(new_catalog_item, 'Item')
		]
	]).

	check_new_catalog_items([], _).
	check_new_catalog_items([Item-_| Items], Contents) :-
		(	member(Item-_, Contents) ->
			domain_error(new_catalog_item, Item)
		;	true
		),
		check_new_catalog_items(Items, Contents).

	:- private(rebuild_catalog_model/6).
	:- mode(rebuild_catalog_model(+list(compound), +list(pair), +list(pair), +term, +list(compound), -compound), one_or_error).
	:- info(rebuild_catalog_model/6, [
		comment is 'Refits corpus statistics and all item weights using prepared raw-count profiles.',
		argnames is ['Ratings', 'Contents', 'Profiles', 'Scale', 'Diagnostics', 'UpdatedRecommender'],
		exceptions is [
			'A nonzero vector count is not an integer' - type_error(integer, 'Count')
		]
	]).

	rebuild_catalog_model(Ratings, Contents, Profiles, Scale, Diagnostics, UpdatedModel) :-
		memberchk(options(Options), Diagnostics),
		memberchk(content_representation(Kind), Diagnostics),
		content_documents(Contents, Documents),
		fit_corpus(Documents, Corpus),
		build_item_weights(Documents, Corpus, Options, Weights),
		model_diagnostics(Ratings, Profiles, Corpus, Kind, Options, Expected),
		update_model_diagnostics(Diagnostics, Expected, UpdatedDiagnostics),
		UpdatedModel = bm25_model(Ratings, Contents, Weights, Profiles, Corpus, Scale, UpdatedDiagnostics).

	:- private(rating_removal_records/3).
	:- mode(rating_removal_records(+list(pair), +list(compound), -list(compound)), one_or_error).
	:- info(rating_removal_records/3, [
		comment is 'Validates user-item pairs and collects matching stored ratings, ignoring missing pairs.',
		argnames is ['UserItemPairs', 'Ratings', 'Removals'],
		exceptions is [
			'An entry or identifier is a variable' - instantiation_error,
			'An entry is not a pair' - type_error(pair, 'Entry'),
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier')
		]
	]).

	rating_removal_records([], _, []).
	rating_removal_records([Entry| Entries], Ratings, Removals) :-
		context(Context),
		check(pair, Entry, Context),
		Entry = User-Item,
		^^check_query_identifiers(User, Item),
		(	member(rating(User,Item,Rating), Ratings) ->
			Removals = [rating(User,Item,Rating)| Rest]
		;	Removals = Rest
		),
		rating_removal_records(Entries, Ratings, Rest).

	:- private(check_rating_updates/3).
	:- mode(check_rating_updates(+list(compound), +list(pair), +term), one_or_error).
	:- info(check_rating_updates/3, [
		comment is 'Checks rating records against the catalog and stored rating scale.',
		argnames is ['Ratings', 'Contents', 'Scale'],
		exceptions is [
			'A record, identifier, or value is a variable' - instantiation_error,
			'An entry is not a rating record' - type_error(rating, 'Entry'),
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'A rating value is not numeric' - type_error(number, 'Rating'),
			'A rating value is nonfinite' - domain_error(finite_rating, 'Rating'),
			'A rating value is outside the stored scale' - domain_error(rating_scale('Min', 'Max'), 'Rating'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	check_rating_updates([], _, _).
	check_rating_updates([Entry| Entries], Contents, Scale) :-
		(	var(Entry) ->
			instantiation_error
		;	true
		),
		(	Entry = rating(User,Item,Rating) ->
			^^check_query_identifiers(User, Item),
			context(Context),
			check(number, Rating, Context),
			check_finite_ratings([Entry]),
			(	member(Item-_, Contents) ->
				true
			;	domain_error(catalog_item, Item)
			),
			(	Scale == none ->
				true
			;	Scale = scale(Min,Max),
				(	Rating >= Min,
					Rating =< Max ->
					true
				;	domain_error(rating_scale(Min,Max), Rating)
				)
			),
			check_rating_updates(Entries, Contents, Scale)
		;	type_error(rating, Entry)
		).

	retain_unchanged_ratings([], _, []).
	retain_unchanged_ratings([rating(User,Item,Rating)| Ratings], Updates, Retained) :-
		(	member(rating(User,Item,_), Updates) ->
			Retained = Rest
		;	Retained = [rating(User,Item,Rating)| Rest]
		),
		retain_unchanged_ratings(Ratings, Updates, Rest).

	:- private(rebuild_feedback_model/9).
	:- mode(rebuild_feedback_model(+list(compound), +list(pair), +list(pair), +list(pair), +compound, +term, +list(compound), +list(atomic), -compound), one_or_error).
	:- info(rebuild_feedback_model/9, [
		comment is 'Rebuilds affected raw-count profiles and diagnostics, retaining fitted corpus and item weights.',
		argnames is ['Ratings', 'Contents', 'ItemWeights', 'Profiles', 'Corpus', 'Scale', 'Diagnostics', 'AffectedUsers', 'UpdatedRecommender'],
		exceptions is [
			'A nonzero vector count is not an integer' - type_error(integer, 'Count'),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	rebuild_feedback_model(Ratings, Contents, Weights, OldProfiles, Corpus, Scale, Diagnostics, Affected, UpdatedModel) :-
		memberchk(options(Options), Diagnostics),
		memberchk(content_representation(Kind), Diagnostics),
		updated_profiles(Ratings, Contents, Options, OldProfiles, Affected, Profiles),
		model_diagnostics(Ratings, Profiles, Corpus, Kind, Options, Expected),
		update_model_diagnostics(Diagnostics, Expected, UpdatedDiagnostics),
		UpdatedModel = bm25_model(Ratings, Contents, Weights, Profiles, Corpus, Scale, UpdatedDiagnostics).

	:- private(updated_profiles/6).
	:- mode(updated_profiles(+list(compound), +list(pair), +list(compound), +list(pair), +list(atomic), -list(pair)), one_or_error).
	:- info(updated_profiles/6, [
		comment is 'Rebuilds affected users from complete history and retains unaffected raw profiles.',
		argnames is ['Ratings', 'Contents', 'Options', 'Profiles', 'AffectedUsers', 'UpdatedProfiles'],
		exceptions is [
			'A nonzero vector count is not an integer' - type_error(integer, 'Count'),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	updated_profiles(_, _, _, Profiles, [], Profiles) :-
		!.
	updated_profiles(Ratings, Contents, Options, OldProfiles, Affected, Profiles) :-
		^^users(Ratings, Users),
		content_documents(Contents, Documents),
		refresh_user_profiles(Users, Ratings, Documents, Options, OldProfiles, Affected, Profiles).

	refresh_user_profiles([], _, _, _, _, _, []).
	refresh_user_profiles([User| Users], Ratings, Documents, Options, OldProfiles, Affected, [User-Profile| Profiles]) :-
		(	member(User, Affected) ->
			::user_profile(User, Ratings, Documents, Options, Profile)
		;	memberchk(User-Profile, OldProfiles)
		),
		refresh_user_profiles(Users, Ratings, Documents, Options, OldProfiles, Affected, Profiles).

	update_model_diagnostics([], _, []).
	update_model_diagnostics([Diagnostic| Diagnostics], Expected, [UpdatedDiagnostic| Updated]) :-
		functor(Diagnostic, Functor, Arity),
		functor(Template, Functor, Arity),
		(	member(Template, Expected) ->
			UpdatedDiagnostic = Template
		;	UpdatedDiagnostic = Diagnostic
		),
		update_model_diagnostics(Diagnostics, Expected, Updated).

	:- private(check_content_kind/3).
	:- mode(check_content_kind(+atom, +compound, +list(compound)), one_or_error).
	:- info(check_content_kind/3, [
		comment is 'Checks that supplied content matches the stored representation.',
		argnames is ['Kind', 'Content', 'Diagnostics'],
		exceptions is [
			'The descriptor differs from the trained representation' - domain_error(content_representation, 'Content')
		]
	]).

	check_content_kind(Kind, Content, Diagnostics) :-
		memberchk(content_representation(StoredKind), Diagnostics),
		(	Kind == StoredKind ->
			true
		;	domain_error(content_representation, Content)
		).

	:- private(score_catalog_items/6).
	:- mode(score_catalog_items(+list(pair), +list(pair), +atomic, +list(atomic), -list(pair), +list(compound)), one_or_error).
	:- info(score_catalog_items/6, [
		comment is 'Validates and scores catalog identifiers in input order using validated model data.',
		argnames is ['ItemWeights', 'Profiles', 'User', 'Items', 'Scores', 'Options'],
		exceptions is [
			'An identifier is a variable' - instantiation_error,
			'An identifier is not atomic' - type_error(atomic, 'Item'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	score_catalog_items(Weights, Profiles, User, Items, Scores, Options) :-
		user_query(Profiles, User, Raw),
		effective_query(Raw, Options, Query),
		findall(
			Item,
			(	member(Item, Items),
				atomic(Item)
			),
			Identifiers0
		),
		sort(Identifiers0, Identifiers),
		resolve_catalog_items(Identifiers, Weights, Resolved),
		score_query_items(Resolved, Query, Items, Scores).

	resolve_catalog_items([], _, []) :-
		!.
	resolve_catalog_items([Item| Items], [], [Item-missing| Resolved]) :-
		!,
		resolve_catalog_items(Items, [], Resolved).
	resolve_catalog_items([Item| Items], [Key-Vector| Weights], Resolved) :-
		compare(Order, Item, Key),
		resolve_catalog_order(Order, Item, Items, Key-Vector, Weights, Resolved).

	resolve_catalog_order(=, Item, Items, _-Vector, Weights, [Item-found(Vector,_)| Resolved]) :-
		resolve_catalog_items(Items, Weights, Resolved).
	resolve_catalog_order(<, Item, Items, Entry, Weights, [Item-missing| Resolved]) :-
		resolve_catalog_items(Items, [Entry| Weights], Resolved).
	resolve_catalog_order(>, Item, Items, _, Weights, Resolved) :-
		resolve_catalog_items([Item| Items], Weights, Resolved).

	:- private(score_query_items/4).
	:- mode(score_query_items(+list(pair), +list(pair), +list(atomic), -list(pair)), one_or_error).
	:- info(score_query_items/4, [
		comment is 'Restores requested score order, checking identifier errors in original request order.',
		argnames is ['ResolvedScores', 'Query', 'Items', 'Scores'],
		exceptions is [
			'An identifier is a variable' - instantiation_error,
			'An identifier is not atomic' - type_error(atomic, 'Item'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	score_query_items(_, _, [], []) :-
		!.
	score_query_items(Resolved, Query, [Item| Items], [Item-Score| Scores]) :-
		context(Context),
		check(atomic, Item, Context),
		(	member(Item-found(Vector,Cached), Resolved) ->
			(	var(Cached) ->
				sparse_score(Query, Vector, 0.0, Cached)
			;	true
			),
			Score = Cached
		;	domain_error(catalog_item, Item)
		),
		score_query_items(Resolved, Query, Items, Scores).

	:- private(catalog_score/6).
	:- mode(catalog_score(+list(pair), +list(pair), +atomic, +atomic, -float, +list(compound)), one_or_error).
	:- info(catalog_score/6, [
		comment is 'Scores a catalog identifier using validated model data.',
		argnames is ['ItemWeights', 'Profiles', 'User', 'Item', 'Score', 'Options'],
		exceptions is [
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	catalog_score(Weights, Profiles, User, Item, Score, Options) :-
		(	member(Item-Vector, Weights) ->
			profile_score(Profiles, User, Vector, Score, Options)
		;	domain_error(catalog_item, Item)
		).

	recommend(Model, User, N, Recommendations) :-
		^^check_recommender(Model),
		context(Context),
		check(atomic, User, Context),
		^^check_top_n(N),
		Model = bm25_model(Ratings, _, Weights, Profiles, _, _, Diagnostics),
		memberchk(options(Options), Diagnostics),
		user_query(Profiles, User, Raw),
		effective_query(Raw, Options, Query),
		findall(Item, member(rating(User,Item,_), Ratings), Rated0),
		sort(Rated0, Rated),
		unrated_weights(Weights, Rated, Candidates),
		candidate_scores(Candidates, Query, Pairs),
		^^top_k(Pairs, N, Recommendations).

	unrated_weights([], _, []) :-
		!.
	unrated_weights(Weights, [], Weights) :-
		!.
	unrated_weights([Item-Vector| Weights], [Rated| Ratings], Candidates) :-
		compare(Order, Item, Rated),
		unrated_weights_order(Order, Item-Vector, Weights, Rated, Ratings, Candidates).

	unrated_weights_order(=, _, Weights, _, Ratings, Candidates) :-
		unrated_weights(Weights, Ratings, Candidates).
	unrated_weights_order(<, Entry, Weights, Rated, Ratings, [Entry| Candidates]) :-
		unrated_weights(Weights, [Rated| Ratings], Candidates).
	unrated_weights_order(>, Entry, Weights, _, Ratings, Candidates) :-
		unrated_weights([Entry| Weights], Ratings, Candidates).

	candidate_scores([], _, []).
	candidate_scores([Item-Vector| Candidates], Query, [Item-Score| Scores]) :-
		sparse_score(Query, Vector, 0.0, Score),
		candidate_scores(Candidates, Query, Scores).

	profile_score(Profiles, User, Vector, Score, Options) :-
		user_query(Profiles, User, Raw),
		effective_query(Raw, Options, Query),
		sparse_score(Query, Vector, 0.0, Score).

	effective_query(Raw, Options, Query) :-
		^^option(query_saturation(Saturation), Options),
		(	Saturation == none ->
			Query = Raw
		;	saturate_query(Raw, Saturation, Query)
		).

	saturate_query([], _, []).
	saturate_query([Feature-Coefficient| Raw], K3, [Feature-Saturated| Query]) :-
		(	K3 =:= 0 ->
			Saturated = 1.0
		;	Maximum is max(Coefficient, K3),
			ScaledQuery is Coefficient / Maximum,
			ScaledK3 is K3 / Maximum,
			Fraction is ScaledQuery / (ScaledQuery + ScaledK3),
			Saturated is Fraction * K3 + Fraction
		),
		saturate_query(Raw, K3, Query).

	user_query(Profiles, User, Query) :-
		(	member(User-Profile, Profiles) ->
			Query = Profile
		;	Query = []
		).

	sparse_score([], _, Score, Score) :-
		!.
	sparse_score(_, [], Score, Score) :-
		!.
	sparse_score([Feature-Coefficient| Profile], [Key-Weight| Vector], Score0, Score) :-
		compare(Order, Feature, Key),
		sparse_score_order(Order, Feature-Coefficient, Profile, Key-Weight, Vector, Score0, Score).

	sparse_score_order(=, _-Coefficient, Profile, _-Weight, Vector, Score0, Score) :-
		Score1 is Score0 + Coefficient * Weight,
		sparse_score(Profile, Vector, Score1, Score).
	sparse_score_order(<, _, Profile, Entry, Vector, Score0, Score) :-
		sparse_score(Profile, [Entry| Vector], Score0, Score).
	sparse_score_order(>, Entry, Profile, _, Vector, Score0, Score) :-
		sparse_score([Entry| Profile], Vector, Score0, Score).

	:- private(content_documents/2).
	:- mode(content_documents(+list(pair), -list(pair)), one_or_error).
	:- info(content_documents/2, [
		comment is 'Converts canonical descriptors to raw counts and complete document lengths.',
		argnames is ['Contents', 'Documents'],
		exceptions is [
			'A nonzero vector count is not an integer' - type_error(integer, 'Count')
		]
	]).

	content_documents([], []).
	content_documents([Item-Content| Contents], [Item-Document| Documents]) :-
		descriptor_document(Content, Document),
		content_documents(Contents, Documents).

	:- private(descriptor_document/2).
	:- mode(descriptor_document(+compound, -compound), one_or_error).
	:- info(descriptor_document/2, [
		comment is 'Converts a canonical descriptor to its raw-count document.',
		argnames is ['Content', 'Document'],
		exceptions is [
			'A nonzero vector count is not an integer' - type_error(integer, 'Count')
		]
	]).

	descriptor_document(features(Occurrences), document(Length, Counts)) :-
		length(Occurrences, Length),
		occurrence_counts(Occurrences, Counts).
	descriptor_document(vector(Counts), document(Length, Counts)) :-
		count_length(Counts, 0, Length).

	occurrence_counts([], []).
	occurrence_counts([Feature| Occurrences], [Feature-Count| Counts]) :-
		same_occurrences(Occurrences, Feature, 1, Count, Rest),
		occurrence_counts(Rest, Counts).

	same_occurrences([Feature0| Occurrences], Feature, Count0, Count, Rest) :-
		Feature0 == Feature,
		!,
		Count1 is Count0 + 1,
		same_occurrences(Occurrences, Feature, Count1, Count, Rest).
	same_occurrences(Rest, _, Count, Count, Rest).

	:- private(count_length/3).
	:- mode(count_length(+list(pair), +integer, -integer), one_or_error).
	:- info(count_length/3, [
		comment is 'Checks canonical nonzero count values and sums the document length.',
		argnames is ['Counts', 'Length0', 'Length'],
		exceptions is [
			'A nonzero vector count is not an integer' - type_error(integer, 'Count')
		]
	]).

	count_length([], Length, Length).
	count_length([_-Count| Counts], Length0, Length) :-
		context(Context),
		check(integer, Count, Context),
		Length1 is Length0 + Count,
		count_length(Counts, Length1, Length).

	fit_corpus(Documents, bm25_corpus(N, Average, Statistics)) :-
		length(Documents, N),
		document_lengths(Documents, 0, Total),
		Average is Total / N,
		findall(
			Feature-1,
			(	member(_-document(_,Counts), Documents),
				member(Feature-_, Counts)
			),
			Features0
		),
		keysort(Features0, Features),
		feature_statistics(Features, N, Statistics).

	document_lengths([], Total, Total).
	document_lengths([_-document(Length,_)| Documents], Total0, Total) :-
		Total1 is Total0 + Length,
		document_lengths(Documents, Total1, Total).

	feature_statistics([], _, []).
	feature_statistics([Feature-Count| Features], N, [Feature-statistics(Frequency,IDF)| Statistics]) :-
		same_feature(Features, Feature, Count, Frequency, Rest),
		IDF is log((N + 1) / (Frequency + 0.5)),
		feature_statistics(Rest, N, Statistics).

	build_item_weights([], _, _, []).
	build_item_weights([Item-Document| Documents], Corpus, Options, [Item-Weights| Items]) :-
		document_weights(Document, Corpus, Options, Weights),
		build_item_weights(Documents, Corpus, Options, Items).

	document_weights(document(Length,Counts), bm25_corpus(_,Average,Statistics), Options, Weights) :-
		^^option(k1(K1), Options),
		^^option(b(B), Options),
		term_weights(Counts, Length, Average, Statistics, K1, B, Weights).

	term_weights([], _, _, _, _, _, []).
	term_weights([_|_], _, _, [], _, _, []) :-
		!.
	term_weights([Feature-Count| Counts], Length, Average, [Key-Statistic| Statistics], K1, B, Weights) :-
		compare(Order, Feature, Key),
		term_weights_order(Order, Feature-Count, Counts, Key-Statistic, Statistics, Length, Average, K1, B, Weights).

	term_weights_order(=, Feature-Count, Counts, _-statistics(_,IDF), Statistics, Length, Average, K1, B, [Feature-Weight| Weights]) :-
		(	K1 =:= 0 ->
			Saturation = 1.0
		;	(	B =:= 0 ->
				Norm = 1.0
			;	Norm is 1 - B + B * (Length / Average)
			),
			Maximum is max(Count, K1),
			ScaledCount is Count / Maximum,
			ScaledK1 is K1 / Maximum,
			Saturation is ((ScaledK1 + 1 / Maximum) / (ScaledCount + ScaledK1 * Norm)) * Count
		),
		Weight is IDF * Saturation,
		term_weights(Counts, Length, Average, Statistics, K1, B, Weights).
	term_weights_order(<, _, Counts, Statistic, Statistics, Length, Average, K1, B, Weights) :-
		term_weights(Counts, Length, Average, [Statistic| Statistics], K1, B, Weights).
	term_weights_order(>, Count, Counts, _, Statistics, Length, Average, K1, B, Weights) :-
		term_weights([Count| Counts], Length, Average, Statistics, K1, B, Weights).

	build_profiles(Ratings, Documents, Options, Profiles) :-
		^^users(Ratings, Users),
		user_profiles(Users, Ratings, Documents, Options, Profiles).

	user_profiles([], _, _, _, []).
	user_profiles([User| Users], Ratings, Documents, Options, [User-Profile| Profiles]) :-
		::user_profile(User, Ratings, Documents, Options, Profile),
		user_profiles(Users, Ratings, Documents, Options, Profiles).

	:- protected(user_profile/5).
	:- mode(user_profile(+atomic, +list(compound), +list(pair), +list(compound), -list(pair)), one_or_error).
	:- info(user_profile/5, [
		comment is 'Builds one raw-count query profile from the complete user history.',
		argnames is ['User', 'Ratings', 'Documents', 'Options', 'Profile'],
		exceptions is [
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	user_profile(User, Ratings, Documents, Options, Profile) :-
		^^option(positive_threshold(ThresholdOption), Options),
		(	ThresholdOption == user_mean ->
			^^user_mean_rating(Ratings, User, Threshold)
		;	Threshold = ThresholdOption
		),
		^^option(profile_weighting(Weighting), Options),
		findall(
			Rating-Counts,
			(	member(rating(User,Item,Rating), Ratings),
				Rating >= Threshold,
				member(Item-document(_,Counts), Documents)
			),
			Selected
		),
		profile_weights(Selected, Weighting, Weighted),
		centroid(Weighted, Profile).

	:- private(profile_weights/3).
	:- mode(profile_weights(+list(pair), +atom, -list(pair)), one_or_error).
	:- info(profile_weights/3, [
		comment is 'Assigns uniform or strictly positive rating weights to selected count vectors.',
		argnames is ['Selected', 'Weighting', 'Weighted'],
		exceptions is [
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	profile_weights([], _, []).
	profile_weights([Rating-Counts| Selected], Weighting, [Weight-Counts| Weighted]) :-
		(	Weighting == uniform ->
			Weight = 1
		;	(	Rating > 0 ->
				Weight = Rating
			;	domain_error(positive_rating_weight, Rating)
			)
		),
		profile_weights(Selected, Weighting, Weighted).

	centroid([], []) :-
		!.
	centroid(Weighted, Profile) :-
		profile_bounds(Weighted, 0, MaxWeight, 0, MaxValue),
		(	MaxValue =:= 0 ->
			Profile = []
		;	weight_total(Weighted, MaxWeight, 0.0, Total),
			profile_contributions(Weighted, MaxWeight, Total, MaxValue, Pairs, []),
			keysort(Pairs, Sorted),
			sum_features(Sorted, MaxValue, Profile)
		).

	profile_bounds([], MaxWeight, MaxWeight, MaxValue, MaxValue).
	profile_bounds([Weight-Counts| Weighted], MaxWeight0, MaxWeight, MaxValue0, MaxValue) :-
		MaxWeight1 is max(Weight, MaxWeight0),
		vector_max(Counts, MaxValue0, MaxValue1),
		profile_bounds(Weighted, MaxWeight1, MaxWeight, MaxValue1, MaxValue).

	vector_max([], MaxValue, MaxValue).
	vector_max([_-Value| Counts], MaxValue0, MaxValue) :-
		MaxValue1 is max(Value, MaxValue0),
		vector_max(Counts, MaxValue1, MaxValue).

	weight_total([], _, Total, Total).
	weight_total([Weight-_| Weighted], MaxWeight, Total0, Total) :-
		Total1 is Total0 + Weight / MaxWeight,
		weight_total(Weighted, MaxWeight, Total1, Total).

	profile_contributions([], _, _, _, Pairs, Pairs).
	profile_contributions([Weight-Counts| Weighted], MaxWeight, Total, MaxValue, Pairs, Tail) :-
		Factor is (Weight / MaxWeight) / Total,
		vector_contributions(Counts, Factor, MaxValue, Pairs, Rest),
		profile_contributions(Weighted, MaxWeight, Total, MaxValue, Rest, Tail).

	vector_contributions([], _, _, Pairs, Pairs).
	vector_contributions([Feature-Value| Counts], Factor, MaxValue, [Feature-Contribution| Pairs], Tail) :-
		Contribution is (Value / MaxValue) * Factor,
		vector_contributions(Counts, Factor, MaxValue, Pairs, Tail).

	sum_features([], _, []).
	sum_features([Feature-Value| Pairs], MaxValue, Profile) :-
		same_feature(Pairs, Feature, Value, Sum, Rest),
		Weight is min(1.0, Sum) * MaxValue,
		(	Weight =:= 0 ->
			Profile = Tail
		;	Profile = [Feature-Weight| Tail]
		),
		sum_features(Rest, MaxValue, Tail).

	same_feature([Feature0-Value| Pairs], Feature, Sum0, Sum, Rest) :-
		Feature0 == Feature,
		!,
		Sum1 is Sum0 + Value,
		same_feature(Pairs, Feature, Sum1, Sum, Rest).
	same_feature(Rest, _, Sum, Sum, Rest).

	model_diagnostics(Ratings, Profiles, bm25_corpus(N,Average,Statistics), Kind, Options, Diagnostics) :-
		length(Ratings, RatingCount),
		length(Profiles, UserCount),
		length(Statistics, FeatureCount),
		findall(
			User,
			(	member(User-Profile, Profiles),
				Profile \== []
			),
			NonEmpty
		),
		length(NonEmpty, ProfileCount),
		^^base_recommender_diagnostics(bm25_recommender, RatingCount, Options,
			[user_count(UserCount),item_count(N),content_representation(Kind),feature_count(FeatureCount),
			 non_empty_profile_count(ProfileCount),average_document_length(Average)], Diagnostics).

	recommender_valid_data(Model) :-
		ground(Model),
		catch(valid_model(Model), error(_,_), fail).

	valid_model(bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics)) :-
		Ratings = [_| _],
		valid(list(compound), Ratings),
		valid_records(Ratings),
		^^check_no_duplicate_ratings(Ratings),
		sort(Ratings, Ratings),
		valid_scale(Scale, Ratings),
		^^canonical_contents(Contents, Canonical, Kind),
		Contents == Canonical,
		^^check_rated_catalog(Ratings, Contents),
		valid(list(compound), Diagnostics),
		memberchk(options(Options), Diagnostics),
		::valid_options(Options),
		memberchk(k1(_), Options),
		memberchk(b(_), Options),
		memberchk(positive_threshold(_), Options),
		memberchk(profile_weighting(_), Options),
		memberchk(query_saturation(_), Options),
		content_documents(Contents, Documents),
		fit_corpus(Documents, ExpectedCorpus),
		Corpus == ExpectedCorpus,
		build_item_weights(Documents, Corpus, Options, ExpectedWeights),
		Weights == ExpectedWeights,
		build_profiles(Ratings, Documents, Options, ExpectedProfiles),
		Profiles == ExpectedProfiles,
		model_diagnostics(Ratings, Profiles, Corpus, Kind, Options, ExpectedDiagnostics),
		matching_diagnostics(ExpectedDiagnostics, Diagnostics).

	matching_diagnostics([], _).
	matching_diagnostics([Expected| ExpectedDiagnostics], Diagnostics) :-
		functor(Expected, Functor, Arity),
		functor(Template, Functor, Arity),
		findall(Template, member(Template, Diagnostics), [Stored]),
		Stored == Expected,
		matching_diagnostics(ExpectedDiagnostics, Diagnostics).

	valid_records([]).
	valid_records([rating(User,Item,Rating)| Ratings]) :-
		atomic(User),
		atomic(Item),
		finite_number(Rating),
		valid_records(Ratings).

	valid_scale(none, _) :-
		!.
	valid_scale(scale(Min,Max), Ratings) :-
		finite_number(Min),
		finite_number(Max),
		Min =< Max,
		forall(
			member(rating(_,_,Rating), Ratings),
			(Min =< Rating, Rating =< Max)
		).

	:- private(check_finite_ratings/1).
	:- mode(check_finite_ratings(+list(compound)), one_or_error).
	:- info(check_finite_ratings/1, [
		comment is 'Checks that rating values are finite.',
		argnames is ['Ratings'],
		exceptions is [
			'A rating value is nonfinite' - domain_error(finite_rating, 'Rating')
		]
	]).

	check_finite_ratings([]).
	check_finite_ratings([rating(_,_,Rating)| Ratings]) :-
		(	finite_number(Rating) ->
			true
		;	domain_error(finite_rating, Rating)
		),
		check_finite_ratings(Ratings).

	finite_number(Value) :-
		number(Value),
		catch((Zero is Value - Value, Zero =:= 0), _, fail).

	default_option(k1(1.2)).
	default_option(b(0.75)).
	default_option(positive_threshold(user_mean)).
	default_option(profile_weighting(uniform)).
	default_option(query_saturation(none)).

	valid_option(k1(Value)) :-
		finite_number(Value),
		Value >= 0.
	valid_option(b(Value)) :-
		finite_number(Value),
		Value >= 0,
		Value =< 1.
	valid_option(positive_threshold(Value)) :-
		(	Value == user_mean ->
			true
		;	finite_number(Value)
		).
	valid_option(profile_weighting(Value)) :-
		once((Value == uniform; Value == rating)).
	valid_option(query_saturation(Value)) :-
		(	Value == none ->
			true
		;	finite_number(Value),
			Value >= 0
		).

	recommender_export_template(_, _, Functor, Template) :-
		Template =.. [Functor, 'Recommender'].

	recommender_term_template(bm25_model(_,_,_,_,_,_,_),
		bm25_model('Ratings','Contents','ItemWeights','Profiles','Corpus','Scale','Diagnostics')).

	export_to_clauses(_, Model, Functor, [Clause]) :-
		^^check_recommender(Model),
		Clause =.. [Functor, Model].

	print_recommender(Model) :-
		^^check_recommender(Model),
		^^print_recommender_template(Model),
		writeq(Model),
		nl.

:- end_object.

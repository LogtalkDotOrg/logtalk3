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


:- object(tfidf_recommender,
	imports([recommender_common, item_content_dataset_validation, similarity_metric_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Content-based recommender using TF-IDF item vectors, positive-feedback centroid profiles, and cosine relevance scores.',
		see_also is [recommender_protocol, item_content_dataset_protocol, text_vectorizer, cosine_similarity]
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
	:- mode(score_content(+compound, +atomic, +compound, -number), one_or_error).
	:- info(score_content/4, [
		comment is 'Scores supplied content of the trained representation without changing the model. Features use the fitted vocabulary and weighting; vectors use the stored normalization.',
		argnames is ['Recommender', 'User', 'Content', 'Score'],
		exceptions is [
			'A required argument, feature, entry, or weight is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'User is not atomic' - type_error(atomic, 'User'),
			'A content descriptor is invalid' - domain_error(item_content, 'Content'),
			'The descriptor differs from the trained representation' - domain_error(content_representation, 'Content'),
			'A content list is not a proper list' - type_error(list, 'List'),
			'A vector entry is not a pair' - type_error(pair, 'Entry'),
			'A vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A vector weight is not numeric' - type_error(number, 'Weight'),
			'A vector weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight')
		]
	]).

	:- public(update_ratings/3).
	:- mode(update_ratings(+compound, +list(compound), -compound), one_or_error).
	:- info(update_ratings/3, [
		comment is 'Returns a model with inserted or replaced ratings and rebuilt profiles, accepting new users but requiring catalog items. An empty update returns the validated original model.',
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
		comment is 'Returns a model with requested ratings removed and profiles rebuilt. Missing and repeated pairs are accepted; no matching ratings returns the validated original model.',
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
		comment is 'Returns a model with new catalog items, refitting the full feature corpus and rebuilding profiles. An empty extension returns the validated original model.',
		argnames is ['Recommender', 'ItemContents', 'UpdatedRecommender'],
		exceptions is [
			'A required argument, entry, identifier, feature, or weight is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'A supplied list is not a proper list' - type_error(list, 'List'),
			'A content declaration or vector entry is not a pair' - type_error(pair, 'Entry'),
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'An identifier already belongs to the catalog' - domain_error(new_catalog_item, 'Item'),
			'An identifier occurs more than once in the extension' - domain_error(duplicate_item, 'Item'),
			'A content descriptor is invalid' - domain_error(item_content, 'Content'),
			'Content representations are mixed or differ from the catalog' - domain_error(content_representation, 'Content'),
			'A vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A vector weight is not numeric' - type_error(number, 'Weight'),
			'A vector weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight'),
			'Feature fitting produces no vocabulary' - domain_error(non_empty_vocabulary, 'Corpus'),
			'A vectorizer frequency bound is inconsistent with the corpus' - domain_error(option, 'Option'),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	:- public(replace_content/3).
	:- mode(replace_content(+compound, +list(pair), -compound), one_or_error).
	:- info(replace_content/3, [
		comment is 'Returns a model with replaced catalog content, refitting the full feature corpus and rebuilding profiles. An empty replacement returns the validated original model.',
		argnames is ['Recommender', 'ItemContents', 'UpdatedRecommender'],
		exceptions is [
			'A required argument, entry, identifier, feature, or weight is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'A supplied list is not a proper list' - type_error(list, 'List'),
			'A content declaration or vector entry is not a pair' - type_error(pair, 'Entry'),
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item'),
			'An identifier occurs more than once in the replacements' - domain_error(duplicate_item, 'Item'),
			'A content descriptor is invalid' - domain_error(item_content, 'Content'),
			'Content representations are mixed or differ from the catalog' - domain_error(content_representation, 'Content'),
			'A vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A vector weight is not numeric' - type_error(number, 'Weight'),
			'A vector weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight'),
			'Feature fitting produces no vocabulary' - domain_error(non_empty_vocabulary, 'Corpus'),
			'A vectorizer frequency bound is inconsistent with the corpus' - domain_error(option, 'Option'),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	learn(Dataset, tfidf_model(Ratings, Contents, Vectors, Profiles, Vectorizer, Scale, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_ratings(Dataset, Ratings0),
		^^check_ratings(Dataset, Ratings0),
		sort(Ratings0, Ratings),
		check_finite_ratings(Ratings),
		^^dataset_rating_scale(Dataset, Scale),
		^^collect_contents(Dataset, Contents, Kind),
		^^check_rated_catalog(Ratings, Contents),
		build_vectors(Kind, Contents, Options, Vectorizer, Vectors),
		build_profiles(Ratings, Vectors, Options, Profiles),
		model_diagnostics(Ratings, Vectors, Profiles, Vectorizer, Kind, Options, Diagnostics).

	score(Model, User, Item, Score) :-
		^^check_recommender(Model),
		^^check_query_identifiers(User, Item),
		Model = tfidf_model(_, _, Vectors, Profiles, _, _, _),
		catalog_score(Vectors, Profiles, User, Item, Score).

	score_all(Model, User, Items, Scores) :-
		^^check_recommender(Model),
		context(Context),
		check(atomic, User, Context),
		check(list, Items, Context),
		Model = tfidf_model(_, _, Vectors, Profiles, _, _, _),
		score_catalog_items(Vectors, Profiles, User, Items, Scores).

	score_content(Model, User, Content, Score) :-
		^^check_recommender(Model),
		context(Context),
		check(atomic, User, Context),
		^^canonical_contents([content-Content], [content-Canonical], ContentKind),
		Model = tfidf_model(_, _, _, Profiles, Vectorizer, _, Diagnostics),
		check_content_kind(ContentKind, Content, Diagnostics),
		memberchk(options(Options), Diagnostics),
		content_vector(ContentKind, Canonical, Vectorizer, Options, Vector),
		profile_score(Profiles, User, Vector, Score).

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

	content_vector(features, features(Document), Vectorizer, _Options, Vector) :-
		text_vectorizer::transform(Vectorizer, Document, Vector).
	content_vector(vectors, Content, _Vectorizer, Options, Vector) :-
		^^option(normalization(Normalization), Options),
		normalize_contents([content-Content], Normalization, [content-Vector]).

	extend_catalog(Model, ItemContents, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, ItemContents, Context),
		(	ItemContents == [] ->
			UpdatedModel = Model
		;	Model = tfidf_model(Ratings, Contents, _, _, _, Scale, Diagnostics),
			check_catalog_additions(ItemContents, Contents),
			^^canonical_contents(ItemContents, Additions, Kind),
			ItemContents = [_-Content| _],
			check_content_kind(Kind, Content, Diagnostics),
			append(Contents, Additions, AllContents),
			keysort(AllContents, UpdatedContents),
			rebuild_catalog_model(Ratings, UpdatedContents, Scale, Diagnostics, UpdatedModel)
		).

	replace_content(Model, ItemContents, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, ItemContents, Context),
		(	ItemContents == [] ->
			UpdatedModel = Model
		;	Model = tfidf_model(Ratings, Contents, _, _, _, Scale, Diagnostics),
			check_catalog_replacements(ItemContents, Contents),
			^^canonical_contents(ItemContents, Replacements, Kind),
			ItemContents = [_-Content| _],
			check_content_kind(Kind, Content, Diagnostics),
			replace_catalog_contents(Contents, Replacements, UpdatedContents),
			rebuild_catalog_model(Ratings, UpdatedContents, Scale, Diagnostics, UpdatedModel)
		).

	:- private(check_catalog_replacements/2).
	:- mode(check_catalog_replacements(+list(pair), +list(pair)), one_or_error).
	:- info(check_catalog_replacements/2, [
		comment is 'Checks that replacement declarations have atomic catalog identifiers.',
		argnames is ['ItemContents', 'Contents'],
		exceptions is [
			'An entry or identifier is a variable' - instantiation_error,
			'An entry is not a pair' - type_error(pair, 'Entry'),
			'An identifier is not atomic' - type_error(atomic, 'Item'),
			'An identifier is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	check_catalog_replacements([], _).
	check_catalog_replacements([Entry| Entries], Contents) :-
		context(Context),
		check(pair, Entry, Context),
		Entry = Item-_,
		check(atomic, Item, Context),
		(	member(Item-_, Contents) ->
			true
		;	domain_error(catalog_item, Item)
		),
		check_catalog_replacements(Entries, Contents).

	replace_catalog_contents([], _, []).
	replace_catalog_contents([Item-Content| Contents], Replacements, [Item-UpdatedContent| Updated]) :-
		(	member(Item-Replacement, Replacements) ->
			UpdatedContent = Replacement
		;	UpdatedContent = Content
		),
		replace_catalog_contents(Contents, Replacements, Updated).

	:- private(check_catalog_additions/2).
	:- mode(check_catalog_additions(+list(pair), +list(pair)), one_or_error).
	:- info(check_catalog_additions/2, [
		comment is 'Checks that content declarations have atomic identifiers absent from the catalog.',
		argnames is ['ItemContents', 'Contents'],
		exceptions is [
			'An entry or identifier is a variable' - instantiation_error,
			'An entry is not a pair' - type_error(pair, 'Entry'),
			'An identifier is not atomic' - type_error(atomic, 'Item'),
			'An identifier already belongs to the catalog' - domain_error(new_catalog_item, 'Item')
		]
	]).

	check_catalog_additions([], _).
	check_catalog_additions([Entry| Entries], Contents) :-
		context(Context),
		check(pair, Entry, Context),
		Entry = Item-_,
		check(atomic, Item, Context),
		(	member(Item-_, Contents) ->
			domain_error(new_catalog_item, Item)
		;	true
		),
		check_catalog_additions(Entries, Contents).

	:- private(rebuild_catalog_model/5).
	:- mode(rebuild_catalog_model(+list(compound), +list(pair), +term, +list(compound), -compound), one_or_error).
	:- info(rebuild_catalog_model/5, [
		comment is 'Rebuilds the full catalog fitted state, profiles, and diagnostics using the stored options.',
		argnames is ['Ratings', 'Contents', 'Scale', 'Diagnostics', 'UpdatedRecommender'],
		exceptions is [
			'Feature fitting produces no vocabulary' - domain_error(non_empty_vocabulary, 'Corpus'),
			'A vectorizer frequency bound is inconsistent with the corpus' - domain_error(option, 'Option'),
			'Vectorizer options are supplied for preweighted content' - domain_error(option, vectorizer_options('Options')),
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	rebuild_catalog_model(Ratings, Contents, Scale, Diagnostics, UpdatedModel) :-
		memberchk(options(Options), Diagnostics),
		memberchk(content_representation(Kind), Diagnostics),
		once(build_vectors(Kind, Contents, Options, Vectorizer, Vectors)),
		rebuild_feedback_model(Ratings, Contents, Vectors, Vectorizer, Scale, Diagnostics, UpdatedModel).

	update_ratings(Model, Updates, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, Updates, Context),
		(	Updates == [] ->
			UpdatedModel = Model
		;	Model = tfidf_model(Ratings, Contents, Vectors, _, Vectorizer, Scale, Diagnostics),
			check_rating_updates(Updates, Vectors, Scale),
			^^check_no_duplicate_ratings(Updates),
			retain_unchanged_ratings(Ratings, Updates, Retained),
			append(Retained, Updates, Merged),
			sort(Merged, UpdatedRatings),
			rebuild_feedback_model(UpdatedRatings, Contents, Vectors, Vectorizer, Scale, Diagnostics, UpdatedModel)
		).

	remove_ratings(Model, UserItemPairs, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, UserItemPairs, Context),
		Model = tfidf_model(Ratings, Contents, Vectors, _, Vectorizer, Scale, Diagnostics),
		rating_removal_records(UserItemPairs, Ratings, Removals),
		retain_unchanged_ratings(Ratings, Removals, Remaining),
		(	Remaining == Ratings ->
			UpdatedModel = Model
		;	(	Remaining == [] ->
				domain_error(non_empty_ratings, [])
			;	rebuild_feedback_model(Remaining, Contents, Vectors, Vectorizer, Scale, Diagnostics, UpdatedModel)
			)
		).

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
		argnames is ['Ratings', 'Vectors', 'Scale'],
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
	check_rating_updates([Entry| Entries], Vectors, Scale) :-
		(	var(Entry) ->
			instantiation_error
		;	true
		),
		(	Entry = rating(User, Item, Rating) ->
			^^check_query_identifiers(User, Item),
			context(Context),
			check(number, Rating, Context),
			check_finite_ratings([Entry]),
			(	member(Item-_, Vectors) ->
				true
			;	domain_error(catalog_item, Item)
			),
			(	Scale == none ->
				true
			;	Scale = scale(Min, Max),
				(	Rating >= Min,
					Rating =< Max ->
					true
				;	domain_error(rating_scale(Min, Max), Rating)
				)
			),
			check_rating_updates(Entries, Vectors, Scale)
		;	type_error(rating, Entry)
		).

	retain_unchanged_ratings([], _, []).
	retain_unchanged_ratings([rating(User,Item,Rating)| Ratings], Updates, Retained) :-
		(	member(rating(User,Item,_), Updates) ->
			Retained = Rest
		;	Retained = [rating(User,Item,Rating)| Rest]
		),
		retain_unchanged_ratings(Ratings, Updates, Rest).

	:- private(rebuild_feedback_model/7).
	:- mode(rebuild_feedback_model(+list(compound), +list(pair), +list(pair), +term, +term, +list(compound), -compound), one_or_error).
	:- info(rebuild_feedback_model/7, [
		comment is 'Rebuilds profiles and diagnostics while retaining the fitted catalog state.',
		argnames is ['Ratings', 'Contents', 'Vectors', 'Vectorizer', 'Scale', 'Diagnostics', 'UpdatedRecommender'],
		exceptions is [
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	rebuild_feedback_model(Ratings, Contents, Vectors, Vectorizer, Scale, Diagnostics, UpdatedModel) :-
		memberchk(options(Options), Diagnostics),
		memberchk(content_representation(Kind), Diagnostics),
		build_profiles(Ratings, Vectors, Options, Profiles),
		model_diagnostics(Ratings, Vectors, Profiles, Vectorizer, Kind, Options, ExpectedDiagnostics),
		update_model_diagnostics(Diagnostics, ExpectedDiagnostics, UpdatedDiagnostics),
		UpdatedModel = tfidf_model(Ratings, Contents, Vectors, Profiles, Vectorizer, Scale, UpdatedDiagnostics).

	update_model_diagnostics([], _, []).
	update_model_diagnostics([Diagnostic| Diagnostics], Expected, [UpdatedDiagnostic| Updated]) :-
		functor(Diagnostic, Functor, Arity),
		functor(Template, Functor, Arity),
		(	member(Template, Expected) ->
			UpdatedDiagnostic = Template
		;	UpdatedDiagnostic = Diagnostic
		),
		update_model_diagnostics(Diagnostics, Expected, Updated).

	:- private(catalog_score/5).
	:- mode(catalog_score(+list(pair), +list(pair), +atomic, +atomic, -number), one_or_error).
	:- info(catalog_score/5, [
		comment is 'Scores a catalog identifier using validated model data.',
		argnames is ['Vectors', 'Profiles', 'User', 'Item', 'Score'],
		exceptions is [
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	catalog_score(Vectors, Profiles, User, Item, Score) :-
		(	member(Item-Vector, Vectors) ->
			profile_score(Profiles, User, Vector, Score)
		;	domain_error(catalog_item, Item)
		).

	:- private(score_catalog_items/5).
	:- mode(score_catalog_items(+list(pair), +list(pair), +atomic, +list(atomic), -list(pair)), one_or_error).
	:- info(score_catalog_items/5, [
		comment is 'Validates and scores catalog identifiers in input order using validated model data.',
		argnames is ['Vectors', 'Profiles', 'User', 'Items', 'Scores'],
		exceptions is [
			'An identifier is a variable' - instantiation_error,
			'An identifier is not atomic' - type_error(atomic, 'Item'),
			'An item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	score_catalog_items(_, _, _, [], []) :-
		!.
	score_catalog_items(Vectors, Profiles, User, [Item| Items], [Item-Score| Scores]) :-
		context(Context),
		check(atomic, Item, Context),
		catalog_score(Vectors, Profiles, User, Item, Score),
		score_catalog_items(Vectors, Profiles, User, Items, Scores).

	recommend(Model, User, N, Recommendations) :-
		^^check_recommender(Model),
		context(Context), check(atomic, User, Context),
		^^check_top_n(N),
		Model = tfidf_model(Ratings, _, Vectors, Profiles, _, _, _),
		findall(
			Item-Score,
			(	member(Item-Vector, Vectors),
				\+ member(rating(User, Item, _), Ratings),
				profile_score(Profiles, User, Vector, Score)
			),
			Pairs
		),
		^^top_k(Pairs, N, Recommendations).

	profile_score(Profiles, User, Vector, Score) :-
		(	member(User-Profile, Profiles) ->
			cosine_similarity::similarity(Profile, Vector, Score)
		;	Score = 0.0
		).

	:- private(check_finite_ratings/1).
	:- mode(check_finite_ratings(+list(compound)), one_or_error).
	:- info(check_finite_ratings/1, [
		comment is 'Checks that rating values are finite.',
		argnames is ['Ratings'],
		exceptions is [
			'A rating is not finite' - domain_error(finite_rating, 'Rating')
		]
	]).

	check_finite_ratings([]).
	check_finite_ratings([rating(_, _, Rating)| Ratings]) :-
		(	finite_number(Rating) ->
			true
		;	domain_error(finite_rating, Rating)
		),
		check_finite_ratings(Ratings).

	:- private(build_vectors/5).
	:- mode(build_vectors(+atom, +list(pair), +list(compound), -term, -list(pair)), one_or_error).
	:- info(build_vectors/5, [
		comment is 'Fits catalog feature vectors or normalizes supplied vectors using the effective options.',
		argnames is ['Kind', 'Contents', 'Options', 'Vectorizer', 'Vectors'],
		exceptions is [
			'Feature fitting produces no vocabulary' - domain_error(non_empty_vocabulary, 'Corpus'),
			'A vectorizer frequency bound is inconsistent with the corpus' - domain_error(option, 'Option'),
			'Vectorizer options are supplied for preweighted content' - domain_error(option, vectorizer_options('Options'))
		]
	]).

	build_vectors(features, Contents, Options, Vectorizer, Vectors) :-
		feature_documents(Contents, Items, Corpus),
		^^option(vectorizer_options(VectorOptions), Options),
		^^option(normalization(Normalization), Options),
		text_vectorizer::learn_transform(Corpus, Vectorizer, Sparse,
			[normalization(Normalization)| VectorOptions]),
		item_vectors(Items, Sparse, Vectors).
	build_vectors(vectors, Contents, Options, none, Vectors) :-
		^^option(vectorizer_options(VectorOptions), Options),
		(	VectorOptions == [] ->
			true
		;	domain_error(option, vectorizer_options(VectorOptions))
		),
		^^option(normalization(Normalization), Options),
		normalize_contents(Contents, Normalization, Vectors).

	feature_documents([], [], []).
	feature_documents([Item-features(Document)| Contents], [Item| Items], [Document| Corpus]) :-
		feature_documents(Contents, Items, Corpus).

	item_vectors([], [], []).
	item_vectors([Item| Items], [Vector| Sparse], [Item-Vector| Vectors]) :-
		item_vectors(Items, Sparse, Vectors).

	normalize_contents([], _Normalization, []).
	normalize_contents([Item-vector(Vector)| Contents], Normalization, [Item-Normalized| Vectors]) :-
		(	Normalization == l2 ->
			^^normalize_vector(Vector, Normalized)
		;	Normalized = Vector
		),
		normalize_contents(Contents, Normalization, Vectors).

	build_profiles(Ratings, Vectors, Options, Profiles) :-
		^^users(Ratings, Users),
		user_profiles(Users, Ratings, Vectors, Options, Profiles).

	user_profiles([], _Ratings, _Vectors, _Options, []).
	user_profiles([User| Users], Ratings, Vectors, Options, [User-Profile| Profiles]) :-
		^^option(positive_threshold(ThresholdOption), Options),
		(	ThresholdOption == user_mean ->
			^^user_mean_rating(Ratings, User, Threshold)
		;	Threshold = ThresholdOption
		),
		^^option(profile_weighting(Weighting), Options),
		findall(
			Rating-Vector,
			(	member(rating(User, Item, Rating), Ratings),
				Rating >= Threshold,
				member(Item-Vector, Vectors)
			),
			Selected
		),
		profile_weights(Selected, Weighting, Weighted),
		centroid(Weighted, Profile),
		user_profiles(Users, Ratings, Vectors, Options, Profiles).

	:- private(profile_weights/3).
	:- mode(profile_weights(+list(pair), +atom, -list(pair)), one_or_error).
	:- info(profile_weights/3, [
		comment is 'Assigns uniform or strictly positive raw-rating weights to selected item vectors.',
		argnames is ['Selected', 'Weighting', 'Weighted'],
		exceptions is [
			'A selected rating is not positive in rating-weighted mode' - domain_error(positive_rating_weight, 'Rating')
		]
	]).

	profile_weights([], _Weighting, []).
	profile_weights([Rating-Vector| Selected], Weighting, [Weight-Vector| Weighted]) :-
		(	Weighting == uniform ->
			Weight = 1
		;	Rating > 0 ->
			Weight = Rating
		;	domain_error(positive_rating_weight, Rating)
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
	profile_bounds([Weight-Vector| Weighted], MaxWeight0, MaxWeight, MaxValue0, MaxValue) :-
		MaxWeight1 is max(Weight, MaxWeight0),
		vector_max(Vector, MaxValue0, MaxValue1),
		profile_bounds(Weighted, MaxWeight1, MaxWeight, MaxValue1, MaxValue).

	vector_max([], MaxValue, MaxValue).
	vector_max([_-Value| Vector], MaxValue0, MaxValue) :-
		MaxValue1 is max(Value, MaxValue0), vector_max(Vector, MaxValue1, MaxValue).

	weight_total([], _MaxWeight, Total, Total).
	weight_total([Weight-_| Weighted], MaxWeight, Total0, Total) :-
		Total1 is Total0 + Weight / MaxWeight,
		weight_total(Weighted, MaxWeight, Total1, Total).

	profile_contributions([], _MaxWeight, _Total, _MaxValue, Pairs, Pairs).
	profile_contributions([Weight-Vector| Weighted], MaxWeight, Total, MaxValue, Pairs, Tail) :-
		Factor is (Weight / MaxWeight) / Total,
		vector_contributions(Vector, Factor, MaxValue, Pairs, Rest),
		profile_contributions(Weighted, MaxWeight, Total, MaxValue, Rest, Tail).

	vector_contributions([], _Factor, _MaxValue, Pairs, Pairs).
	vector_contributions([Feature-Value| Vector], Factor, MaxValue, [Feature-Contribution| Pairs], Tail) :-
		Contribution is (Value / MaxValue) * Factor,
		vector_contributions(Vector, Factor, MaxValue, Pairs, Tail).

	sum_features([], _MaxValue, []).
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
	same_feature(Rest, _Feature, Sum, Sum, Rest).

	model_diagnostics(Ratings, Vectors, Profiles, Vectorizer, Kind, Options, Diagnostics) :-
		length(Ratings, RatingCount), length(Vectors, ItemCount), length(Profiles, UserCount),
		feature_count(Kind, Vectorizer, Vectors, FeatureCount),
		findall(User, (member(User-Profile, Profiles), Profile \== []), NonEmpty),
		length(NonEmpty, ProfileCount),
		^^base_recommender_diagnostics(tfidf_recommender, RatingCount, Options,
			[user_count(UserCount), item_count(ItemCount), content_representation(Kind),
			 feature_count(FeatureCount), non_empty_profile_count(ProfileCount)], Diagnostics).

	feature_count(features, Vectorizer, _Vectors, Count) :-
		text_vectorizer::vocabulary(Vectorizer, Features), length(Features, Count).
	feature_count(vectors, _Vectorizer, Vectors, Count) :-
		findall(Feature, (member(_-Vector, Vectors), member(Feature-_, Vector)), Features0),
		sort(Features0, Features), length(Features, Count).

	recommender_valid_data(Model) :-
		ground(Model),
		catch(valid_model(Model), error(_, _), fail).

	valid_model(tfidf_model(Ratings, Contents, Vectors, Profiles, Vectorizer, Scale, Diagnostics)) :-
		Ratings = [_| _], valid(list(compound), Ratings),
		valid_records(Ratings),
		^^check_no_duplicate_ratings(Ratings),
		sort(Ratings, Ratings),
		valid_scale(Scale, Ratings),
		^^canonical_contents(Contents, Canonical, Kind), Contents == Canonical,
		^^check_rated_catalog(Ratings, Contents),
		valid(list(compound), Diagnostics), memberchk(options(Options), Diagnostics),
		::valid_options(Options),
		build_vectors(Kind, Contents, Options, ExpectedVectorizer, ExpectedVectors),
		Vectorizer == ExpectedVectorizer, Vectors == ExpectedVectors,
		build_profiles(Ratings, Vectors, Options, ExpectedProfiles), Profiles == ExpectedProfiles,
		model_diagnostics(Ratings, Vectors, Profiles, Vectorizer, Kind, Options, ExpectedDiagnostics),
		matching_diagnostics(ExpectedDiagnostics, Diagnostics).

	matching_diagnostics([], _Diagnostics).
	matching_diagnostics([Expected| ExpectedDiagnostics], Diagnostics) :-
		functor(Expected, Functor, Arity),
		functor(Template, Functor, Arity),
		findall(Template, member(Template, Diagnostics), [Stored]),
		Stored == Expected,
		matching_diagnostics(ExpectedDiagnostics, Diagnostics).

	valid_records([]).
	valid_records([rating(User, Item, Rating)| Ratings]) :-
		atomic(User),
		atomic(Item),
		finite_number(Rating),
		valid_records(Ratings).

	valid_scale(none, _Ratings) :-
		!.
	valid_scale(scale(Min, Max), Ratings) :-
		finite_number(Min),
		finite_number(Max),
		Min =< Max,
		forall(member(rating(_, _, Rating), Ratings), (Rating >= Min, Rating =< Max)).

	finite_number(Value) :-
		number(Value),
		catch((Zero is Value - Value, Zero =:= 0), _, fail).

	default_option(positive_threshold(user_mean)).
	default_option(profile_weighting(uniform)).
	default_option(normalization(l2)).
	default_option(vectorizer_options([])).

	valid_option(positive_threshold(Threshold)) :-
		(	Threshold == user_mean ->
			true
		;	finite_number(Threshold)
		).
	valid_option(profile_weighting(Weighting)) :-
		once((Weighting == uniform; Weighting == rating)).
	valid_option(normalization(Normalization)) :-
		once((Normalization == none; Normalization == l2)).
	valid_option(vectorizer_options(Options)) :-
		valid(list(compound), Options),
		text_vectorizer::valid_options(Options),
		\+ member(normalization(_), Options).

	recommender_export_template(_, _, Functor, Template) :-
		Template =.. [Functor, 'Recommender'].

	recommender_term_template(tfidf_model(_, _, _, _, _, _, _),
		tfidf_model('Ratings', 'Contents', 'ItemVectors', 'Profiles', 'Vectorizer', 'Scale', 'Diagnostics')).

	export_to_clauses(_, Model, Functor, [Clause]) :-
		^^check_recommender(Model),
		Clause =.. [Functor, Model].

	print_recommender(Model) :-
		^^check_recommender(Model),
		^^print_recommender_template(Model),
		writeq(Model), nl.

:- end_object.

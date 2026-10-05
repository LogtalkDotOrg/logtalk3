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


:- object(jaccard_recommender,
	imports([recommender_common, item_content_dataset_validation])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Content-based recommender using binary item features, support-filtered positive-feedback profiles, and Jaccard relevance scores.',
		see_also is [recommender_protocol, item_content_dataset_protocol, jaccard_similarity]
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
		comment is 'Scores catalog items in input order, preserving duplicates and validating the model once. An empty list returns an empty list after validating the model and user.',
		argnames is ['Recommender', 'User', 'Items', 'Scores'],
		exceptions is [
			'A required argument or item identifier is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'User or an item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'Items is not a proper list' - type_error(list, 'Items'),
			'An item is absent from the learned catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	:- public(score_content/4).
	:- mode(score_content(+compound, +atomic, +compound, -float), one_or_error).
	:- info(score_content/4, [
		comment is 'Scores supplied binary content against a learned user profile without changing the model or catalog. Either descriptor kind is accepted regardless of the training representation.',
		argnames is ['Recommender', 'User', 'Content', 'Score'],
		exceptions is [
			'A required argument, feature, entry, or weight is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'User is not atomic' - type_error(atomic, 'User'),
			'The content descriptor is invalid' - domain_error(item_content, 'Content'),
			'A content list is not a proper list' - type_error(list, 'List'),
			'A vector entry is not a pair' - type_error(pair, 'Entry'),
			'A vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A vector weight is not numeric' - type_error(number, 'Weight'),
			'A vector weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight'),
			'A vector weight is neither zero nor one' - domain_error(binary_weight, 'Weight')
		]
	]).

	:- public(extend_catalog/3).
	:- mode(extend_catalog(+compound, +list(pair), -compound), one_or_error).
	:- info(extend_catalog/3, [
		comment is 'Returns a model with additional unrated items of the existing content representation, preserving ratings and profiles. An empty extension returns the validated original model.',
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
			'A vector weight is neither zero nor one' - domain_error(binary_weight, 'Weight')
		]
	]).

	:- public(replace_content/3).
	:- mode(replace_content(+compound, +list(pair), -compound), one_or_error).
	:- info(replace_content/3, [
		comment is 'Returns a model with replaced catalog content and rebuilt profiles. An empty replacement returns the validated original model.',
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
			'A vector weight is neither zero nor one' - domain_error(binary_weight, 'Weight')
		]
	]).

	:- public(update_ratings/3).
	:- mode(update_ratings(+compound, +list(compound), -compound), one_or_error).
	:- info(update_ratings/3, [
		comment is 'Returns a model with inserted or replaced user-item ratings and rebuilt profiles, accepting new users but requiring catalog items. An empty update returns the validated original model.',
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
			'A user-item pair occurs more than once in the updates' - domain_error(duplicate_rating, 'User'-'Item')
		]
	]).

	:- public(remove_ratings/3).
	:- mode(remove_ratings(+compound, +list(pair), -compound), one_or_error).
	:- info(remove_ratings/3, [
		comment is 'Returns a model with requested user-item ratings removed and profiles rebuilt. Missing and repeated pairs are accepted; no matching ratings returns the validated original model.',
		argnames is ['Recommender', 'UserItemPairs', 'UpdatedRecommender'],
		exceptions is [
			'A required argument, entry, or identifier is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'UserItemPairs is not a proper list' - type_error(list, 'UserItemPairs'),
			'An entry is not a pair' - type_error(pair, 'Entry'),
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'No ratings would remain' - domain_error(non_empty_ratings, [])
		]
	]).

	learn(Dataset, jaccard_model(Ratings, Contents, Vectors, Profiles, Scale, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_ratings(Dataset, Ratings0),
		^^check_ratings(Dataset, Ratings0),
		sort(Ratings0, Ratings),
		check_finite_ratings(Ratings),
		^^dataset_rating_scale(Dataset, Scale),
		^^collect_contents(Dataset, Contents, Kind),
		^^check_rated_catalog(Ratings, Contents),
		binary_contents(Contents, Vectors),
		build_profiles(Ratings, Vectors, Options, Profiles),
		model_diagnostics(Ratings, Vectors, Profiles, Kind, Options, Diagnostics).

	score(Model, User, Item, Score) :-
		^^check_recommender(Model),
		^^check_query_identifiers(User, Item),
		Model = jaccard_model(_, _, Vectors, Profiles, _, _),
		catalog_score(Vectors, Profiles, User, Item, Score).

	score_all(Model, User, Items, Scores) :-
		^^check_recommender(Model),
		context(Context),
		check(atomic, User, Context),
		check(list, Items, Context),
		Model = jaccard_model(_, _, Vectors, Profiles, _, _),
		score_catalog_items(Vectors, Profiles, User, Items, Scores).

	score_content(Model, User, Content, Score) :-
		^^check_recommender(Model),
		context(Context),
		check(atomic, User, Context),
		^^canonical_contents([content-Content], [content-Canonical], _Kind),
		binary_descriptor(Canonical, Vector),
		Model = jaccard_model(_, _, _, Profiles, _, _),
		profile_score(Profiles, User, Vector, Score).

	extend_catalog(Model, ItemContents, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, ItemContents, Context),
		(	ItemContents == [] ->
			UpdatedModel = Model
		;	Model = jaccard_model(Ratings, Contents, Vectors, Profiles, Scale, Diagnostics),
			check_catalog_additions(ItemContents, Vectors),
			^^canonical_contents(ItemContents, Additions, NewKind),
			memberchk(content_representation(Kind), Diagnostics),
			(	NewKind == Kind ->
				true
			;	ItemContents = [_-Content| _],
				domain_error(content_representation, Content)
			),
			binary_contents(Additions, AddedVectors),
			append(Contents, Additions, AllContents),
			keysort(AllContents, UpdatedContents),
			append(Vectors, AddedVectors, AllVectors),
			keysort(AllVectors, UpdatedVectors),
			catalog_counts(UpdatedVectors, ItemCount, FeatureCount),
			update_model_diagnostics(Diagnostics, [item_count(ItemCount),feature_count(FeatureCount)], UpdatedDiagnostics),
			UpdatedModel = jaccard_model(Ratings, UpdatedContents, UpdatedVectors, Profiles, Scale, UpdatedDiagnostics)
		).

	replace_content(Model, ItemContents, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, ItemContents, Context),
		(	ItemContents == [] ->
			UpdatedModel = Model
		;	Model = jaccard_model(Ratings, Contents, Vectors, _, Scale, Diagnostics),
			check_catalog_replacements(ItemContents, Vectors),
			^^canonical_contents(ItemContents, Replacements, NewKind),
			memberchk(content_representation(Kind), Diagnostics),
			(	NewKind == Kind ->
				true
			;	ItemContents = [_-Content| _],
				domain_error(content_representation, Content)
			),
			replace_catalog_contents(Contents, Replacements, UpdatedContents),
			binary_contents(UpdatedContents, UpdatedVectors),
			rebuild_model(Ratings, UpdatedContents, UpdatedVectors, Scale, Diagnostics, UpdatedModel)
		).

	update_ratings(Model, Updates, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, Updates, Context),
		(	Updates == [] ->
			UpdatedModel = Model
		;	Model = jaccard_model(Ratings, Contents, Vectors, _, Scale, Diagnostics),
			check_rating_updates(Updates, Vectors, Scale),
			^^check_no_duplicate_ratings(Updates),
			retain_unchanged_ratings(Ratings, Updates, Retained),
			append(Retained, Updates, Merged),
			sort(Merged, UpdatedRatings),
			rebuild_model(UpdatedRatings, Contents, Vectors, Scale, Diagnostics, UpdatedModel)
		).

	remove_ratings(Model, UserItemPairs, UpdatedModel) :-
		^^check_recommender(Model),
		context(Context),
		check(list, UserItemPairs, Context),
		Model = jaccard_model(Ratings, Contents, Vectors, _, Scale, Diagnostics),
		rating_removal_records(UserItemPairs, Ratings, Removals),
		retain_unchanged_ratings(Ratings, Removals, Remaining),
		(	Remaining == Ratings ->
			UpdatedModel = Model
		;	(	Remaining == [] ->
				domain_error(non_empty_ratings, [])
			;	rebuild_model(Remaining, Contents, Vectors, Scale, Diagnostics, UpdatedModel)
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

	:- private(check_catalog_replacements/2).
	:- mode(check_catalog_replacements(+list(pair), +list(pair)), one_or_error).
	:- info(check_catalog_replacements/2, [
		comment is 'Checks that replacement declarations are pairs with atomic catalog identifiers.',
		argnames is ['ItemContents', 'Vectors'],
		exceptions is [
			'An entry or identifier is a variable' - instantiation_error,
			'An entry is not a pair' - type_error(pair, 'Entry'),
			'An identifier is not atomic' - type_error(atomic, 'Item'),
			'An identifier is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	check_catalog_replacements([], _).
	check_catalog_replacements([Entry| Entries], Vectors) :-
		context(Context),
		check(pair, Entry, Context),
		Entry = Item-_,
		check(atomic, Item, Context),
		(	member(Item-_, Vectors) ->
			true
		;	domain_error(catalog_item, Item)
		),
		check_catalog_replacements(Entries, Vectors).

	replace_catalog_contents([], _, []).
	replace_catalog_contents([Item-Content| Contents], Replacements, [Item-UpdatedContent| Updated]) :-
		(	member(Item-Replacement, Replacements) ->
			UpdatedContent = Replacement
		;	UpdatedContent = Content
		),
		replace_catalog_contents(Contents, Replacements, Updated).

	rebuild_model(Ratings, Contents, Vectors, Scale, Diagnostics, UpdatedModel) :-
		memberchk(options(Options), Diagnostics),
		memberchk(content_representation(Kind), Diagnostics),
		build_profiles(Ratings, Vectors, Options, Profiles),
		model_diagnostics(Ratings, Vectors, Profiles, Kind, Options, ExpectedDiagnostics),
		update_model_diagnostics(Diagnostics, ExpectedDiagnostics, UpdatedDiagnostics),
		UpdatedModel = jaccard_model(Ratings, Contents, Vectors, Profiles, Scale, UpdatedDiagnostics).

	:- private(check_catalog_additions/2).
	:- mode(check_catalog_additions(+list(pair), +list(pair)), one_or_error).
	:- info(check_catalog_additions/2, [
		comment is 'Checks that content declarations are pairs with atomic identifiers absent from the existing catalog.',
		argnames is ['ItemContents', 'Vectors'],
		exceptions is [
			'An entry or identifier is a variable' - instantiation_error,
			'An entry is not a pair' - type_error(pair, 'Entry'),
			'An identifier is not atomic' - type_error(atomic, 'Item'),
			'An identifier already belongs to the catalog' - domain_error(new_catalog_item, 'Item')
		]
	]).

	check_catalog_additions([], _).
	check_catalog_additions([Entry| Entries], Vectors) :-
		context(Context),
		check(pair, Entry, Context),
		Entry = Item-_,
		check(atomic, Item, Context),
		(	member(Item-_, Vectors) ->
			domain_error(new_catalog_item, Item)
		;	true
		),
		check_catalog_additions(Entries, Vectors).

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
	:- mode(catalog_score(+list(pair), +list(pair), +atomic, +atomic, -float), one_or_error).
	:- info(catalog_score/5, [
		comment is 'Scores an instantiated catalog identifier using validated model data.',
		argnames is ['Vectors', 'Profiles', 'User', 'Item', 'Score'],
		exceptions is [
			'The item is absent from the learned catalog' - domain_error(catalog_item, 'Item')
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
		comment is 'Validates and scores catalog identifiers in order using validated model data.',
		argnames is ['Vectors', 'Profiles', 'User', 'Items', 'Scores'],
		exceptions is [
			'An item identifier is a variable' - instantiation_error,
			'An item identifier is not atomic' - type_error(atomic, 'Item'),
			'An item is absent from the learned catalog' - domain_error(catalog_item, 'Item')
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
		context(Context),
		check(atomic, User, Context),
		^^check_top_n(N),
		Model = jaccard_model(Ratings, _, Vectors, Profiles, _, _),
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
			jaccard_similarity::similarity(Profile, Vector, Similarity),
			Score is float(Similarity)
		;	Score = 0.0
		).

	:- private(binary_contents/2).
	:- mode(binary_contents(+list(pair), -list(pair)), one_or_error).
	:- info(binary_contents/2, [
		comment is 'Converts canonical descriptors into sorted binary feature vectors, ignoring feature multiplicity.',
		argnames is ['Contents', 'Vectors'],
		exceptions is [
			'A nonzero canonical vector weight is not one' - domain_error(binary_weight, 'Weight')
		]
	]).

	binary_contents([], []).
	binary_contents([Item-Content| Contents], [Item-Vector| Vectors]) :-
		binary_descriptor(Content, Vector),
		binary_contents(Contents, Vectors).

	:- private(binary_descriptor/2).
	:- mode(binary_descriptor(+compound, -list(pair)), one_or_error).
	:- info(binary_descriptor/2, [
		comment is 'Converts a canonical descriptor to a binary feature vector.',
		argnames is ['Content', 'Vector'],
		exceptions is [
			'A nonzero canonical vector weight is not one' - domain_error(binary_weight, 'Weight')
		]
	]).

	binary_descriptor(features(Occurrences), Vector) :-
		sort(Occurrences, Features),
		feature_pairs(Features, Vector).
	binary_descriptor(vector(Pairs), Vector) :-
		binary_pairs(Pairs, Vector).

	:- private(binary_pairs/2).
	:- mode(binary_pairs(+list(pair), -list(pair)), one_or_error).
	:- info(binary_pairs/2, [
		comment is 'Checks that nonzero canonical weights are one and replaces them with integer unit weights.',
		argnames is ['Pairs', 'Vector'],
		exceptions is [
			'A nonzero canonical vector weight is not one' - domain_error(binary_weight, 'Weight')
		]
	]).

	binary_pairs([], []).
	binary_pairs([Feature-Weight| Pairs], [Feature-1| Vector]) :-
		(	Weight =:= 1 ->
			true
		;	domain_error(binary_weight, Weight)
		),
		binary_pairs(Pairs, Vector).

	feature_pairs([], []).
	feature_pairs([Feature| Features], [Feature-1| Pairs]) :-
		feature_pairs(Features, Pairs).

	build_profiles(Ratings, Vectors, Options, Profiles) :-
		^^users(Ratings, Users),
		^^option(positive_threshold(ThresholdOption), Options),
		^^option(min_feature_support(MinSupport), Options),
		user_profiles(Users, Ratings, Vectors, ThresholdOption, MinSupport, Profiles).

	user_profiles([], _Ratings, _Vectors, _ThresholdOption, _MinSupport, []).
	user_profiles([User| Users], Ratings, Vectors, ThresholdOption, MinSupport, [User-Profile| Profiles]) :-
		(	ThresholdOption == user_mean ->
			^^user_mean_rating(Ratings, User, Threshold)
		;	Threshold = ThresholdOption
		),
		findall(
			Feature,
			(	member(rating(User, Item, Rating), Ratings),
				Rating >= Threshold,
				member(Item-Vector, Vectors),
				member(Feature-1, Vector)
			),
			Occurrences
		),
		feature_pairs(Occurrences, OccurrencePairs),
		keysort(OccurrencePairs, Sorted),
		supported_feature_pairs(Sorted, MinSupport, Profile),
		user_profiles(Users, Ratings, Vectors, ThresholdOption, MinSupport, Profiles).

	supported_feature_pairs([], _, []).
	supported_feature_pairs([Feature-_| Pairs], MinSupport, Profile) :-
		feature_support(Pairs, Feature, 1, Support, Rest),
		(	Support >= MinSupport ->
			Profile = [Feature-1| Next]
		;	Profile = Next
		),
		supported_feature_pairs(Rest, MinSupport, Next).

	feature_support([NextFeature-_| Pairs], Feature, Count, Support, Rest) :-
		NextFeature == Feature,
		!,
		NextCount is Count + 1,
		feature_support(Pairs, Feature, NextCount, Support, Rest).
	feature_support(Rest, _, Support, Support, Rest).

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

	model_diagnostics(Ratings, Vectors, Profiles, Kind, Options, Diagnostics) :-
		length(Ratings, RatingCount),
		catalog_counts(Vectors, ItemCount, FeatureCount),
		length(Profiles, UserCount),
		findall(
			User,
			(	member(User-Profile, Profiles),
				Profile \== []
			),
			NonEmpty
		),
		length(NonEmpty, ProfileCount),
		^^base_recommender_diagnostics(jaccard_recommender, RatingCount, Options,
			[user_count(UserCount), item_count(ItemCount), content_representation(Kind),
			 feature_count(FeatureCount), non_empty_profile_count(ProfileCount)], Diagnostics).

	catalog_counts(Vectors, ItemCount, FeatureCount) :-
		length(Vectors, ItemCount),
		findall(
			Feature,
			(	member(_-Vector, Vectors),
				member(Feature-1, Vector)
			),
			Occurrences
		),
		sort(Occurrences, Features),
		length(Features, FeatureCount).

	recommender_valid_data(Model) :-
		ground(Model),
		catch(valid_model(Model), error(_, _), fail).

	valid_model(jaccard_model(Ratings, Contents, Vectors, Profiles, Scale, Diagnostics)) :-
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
		binary_contents(Contents, ExpectedVectors),
		Vectors == ExpectedVectors,
		build_profiles(Ratings, Vectors, Options, ExpectedProfiles),
		Profiles == ExpectedProfiles,
		model_diagnostics(Ratings, Vectors, Profiles, Kind, Options, ExpectedDiagnostics),
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
		forall(
			member(rating(_, _, Rating), Ratings),
			(Min =< Rating, Rating =< Max)
		).

	matching_diagnostics([], _Diagnostics).
	matching_diagnostics([Expected| ExpectedDiagnostics], Diagnostics) :-
		functor(Expected, Functor, Arity),
		functor(Template, Functor, Arity),
		findall(Template, member(Template, Diagnostics), [Stored]),
		Stored == Expected,
		matching_diagnostics(ExpectedDiagnostics, Diagnostics).

	finite_number(Value) :-
		number(Value),
		catch((Zero is Value - Value, Zero =:= 0), _, fail).

	default_option(positive_threshold(user_mean)).
	default_option(min_feature_support(1)).

	valid_option(positive_threshold(Threshold)) :-
		(	Threshold == user_mean ->
			true
		;	finite_number(Threshold)
		).
	valid_option(min_feature_support(Support)) :-
		integer(Support),
		Support > 0.

	recommender_export_template(_, _, Functor, Template) :-
		Template =.. [Functor, 'Recommender'].

	recommender_term_template(jaccard_model(_, _, _, _, _, _),
		jaccard_model('Ratings', 'Contents', 'ItemVectors', 'Profiles', 'Scale', 'Diagnostics')).

	export_to_clauses(_, Model, Functor, [Clause]) :-
		^^check_recommender(Model),
		Clause =.. [Functor, Model].

	print_recommender(Model) :-
		^^check_recommender(Model),
		^^print_recommender_template(Model),
		writeq(Model), nl.

:- end_object.

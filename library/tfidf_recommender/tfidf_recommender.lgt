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
	imports([recommender_common, similarity_metric_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Content-based recommender using TF-IDF item vectors, positive-feedback centroid profiles, and cosine relevance scores.',
		see_also is [recommender_protocol, item_content_dataset_protocol, text_vectorizer, cosine_similarity]
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	learn(Dataset, tfidf_model(Ratings, Contents, Vectors, Profiles, Vectorizer, Scale, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_ratings(Dataset, Ratings0),
		^^check_ratings(Dataset, Ratings0),
		sort(Ratings0, Ratings),
		check_finite_ratings(Ratings),
		^^dataset_rating_scale(Dataset, Scale),
		collect_contents(Dataset, Contents, Kind),
		check_rated_catalog(Ratings, Contents),
		build_vectors(Kind, Contents, Options, Vectorizer, Vectors),
		build_profiles(Ratings, Vectors, Options, Profiles),
		model_diagnostics(Ratings, Vectors, Profiles, Vectorizer, Kind, Options, Diagnostics).

	score(Model, User, Item, Score) :-
		^^check_recommender(Model),
		^^check_query_identifiers(User, Item),
		Model = tfidf_model(_, _, Vectors, Profiles, _, _, _),
		(	member(Item-Vector, Vectors) ->
			profile_score(Profiles, User, Vector, Score)
		;	domain_error(catalog_item, Item)
		).

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

	:- private(collect_contents/3).
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
		; domain_error(item_content, Content)
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
		context(Context), check(list, Features, Context),
		( ground(Features) -> true; instantiation_error ),
		occurrence_pairs(Features, Pairs), keysort(Pairs, Ordered), keys(Ordered, Sorted).
	canonical_descriptor(vector(Pairs), vector(Vector)) :-
		context(Context), check(list, Pairs, Context),
		check_vector_entries(Pairs, []),
		keysort(Pairs, Sorted), remove_zero_weights(Sorted, Vector).

	occurrence_pairs([], []).
	occurrence_pairs([Feature| Features], [Feature-1| Pairs]) :-
		occurrence_pairs(Features, Pairs).

	check_vector_entries([], _Seen).
	check_vector_entries([Entry| Entries], Seen) :-
		( var(Entry) -> instantiation_error
		; ( Entry = Feature-Weight ->
			( ground(Feature) -> true; instantiation_error ),
			( member(Feature, Seen) -> domain_error(duplicate_feature, Feature); true ),
			context(Context), check(number, Weight, Context),
			( finite_number(Weight), Weight >= 0 -> true
			; domain_error(non_negative_finite_weight, Weight) ),
			check_vector_entries(Entries, [Feature| Seen])
		  ; type_error(pair, Entry)
		  )
		).

	remove_zero_weights([], []).
	remove_zero_weights([Feature-Weight| Pairs], Vector) :-
		(	Weight =:= 0 ->
			Vector = Rest
		;	Vector = [Feature-Weight| Rest]
		),
		remove_zero_weights(Pairs, Rest).

	:- private(check_rated_catalog/2).
	:- mode(check_rated_catalog(+list(compound), +list(pair)), one_or_error).
	:- info(check_rated_catalog/2, [
		comment is 'Checks that every rated item belongs to the content catalog.',
		argnames is ['Ratings', 'Contents'],
		exceptions is [
			'A rated item is absent from the catalog' - domain_error(catalog_item, 'Item')
		]
	]).

	check_rated_catalog([], _Contents).
	check_rated_catalog([rating(_, Item, _)| Ratings], Contents) :-
		(	member(Item-_, Contents) ->
			true
		;	domain_error(catalog_item, Item)
		),
		check_rated_catalog(Ratings, Contents).

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
		canonical_contents(Contents, Canonical, Kind), Contents == Canonical,
		check_rated_catalog(Ratings, Contents),
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

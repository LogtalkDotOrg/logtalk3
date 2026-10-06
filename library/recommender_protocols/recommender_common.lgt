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


:- category(recommender_common,
	implements(recommender_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Shared predicates for recommender diagnostics, rating dataset validation, rating-matrix utilities, similarity metrics, and top-k retrieval.'
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		last/2, length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	% hook predicates that concrete recommender implementations must define

	:- protected(recommender_valid_data/1).
	:- mode(recommender_valid_data(@compound), zero_or_one).
	:- info(recommender_valid_data/1, [
		comment is 'Required hook that validates implementation-specific model data and its consistency with diagnostics without instantiating the recommender.',
		argnames is ['Recommender']
	]).

	:- protected(recommender_diagnostics_data/2).
	:- mode(recommender_diagnostics_data(+compound, -list(compound)), one).
	:- info(recommender_diagnostics_data/2, [
		comment is 'Hook predicate that importing recommender implementations must define in order to expose diagnostics metadata. A default implementation is provided that assumes the diagnostics list is the last argument of the recommender term; concrete implementations following that convention do not need to override it.',
		argnames is ['Recommender', 'Diagnostics']
	]).

	:- protected(recommender_export_template/4).
	:- mode(recommender_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(recommender_export_template/4, [
		comment is 'Hook predicate that importing recommender implementations must define in order to expose the exported recommender template for a given functor.',
		argnames is ['Dataset', 'Recommender', 'Functor', 'Template']
	]).

	:- protected(recommender_term_template/2).
	:- mode(recommender_term_template(+compound, -callable), one).
	:- info(recommender_term_template/2, [
		comment is 'Hook predicate that importing recommender implementations must define in order to expose the learned recommender term template used by pretty-printing helpers.',
		argnames is ['Recommender', 'Template']
	]).

	% pretty-printing helper

	:- protected(print_recommender_template/1).
	:- mode(print_recommender_template(+compound), one).
	:- info(print_recommender_template/1, [
		comment is 'Pretty-printing helper predicate used by importing recommender implementations to show the learned recommender term template.',
		argnames is ['Recommender']
	]).

	% default protocol predicate implementations

	learn(Dataset, Recommender) :-
		::learn(Dataset, Recommender, []).

	check_recommender(Recommender) :-
		(	var(Recommender) ->
			instantiation_error
		;	compound(Recommender),
			::recommender_valid_data(Recommender),
			::recommender_diagnostics_data(Recommender, Diagnostics),
			valid_common_diagnostics(Diagnostics) ->
			true
		;	domain_error(recommender, Recommender)
		).

	valid_recommender(Recommender) :-
		catch(::check_recommender(Recommender), _Error, fail).

	valid_common_diagnostics(Diagnostics) :-
		valid(list(compound), Diagnostics),
		findall(Model, member(model(Model), Diagnostics), [Model]),
		atom(Model),
		findall(Count, member(rating_count(Count), Diagnostics), [Count]),
		valid(positive_integer, Count),
		findall(Options, member(options(Options), Diagnostics), [Options]),
		ground(Options),
		valid(list(compound), Options),
		::valid_options(Options).

	diagnostics(Recommender, Diagnostics) :-
		::recommender_diagnostics_data(Recommender, Diagnostics).

	diagnostic(Recommender, Diagnostic) :-
		::recommender_diagnostics_data(Recommender, Diagnostics),
		member(Diagnostic, Diagnostics).

	recommender_options(Recommender, Options) :-
		::recommender_diagnostics_data(Recommender, Diagnostics),
		memberchk(options(Options), Diagnostics).

	recommender_diagnostics_data(Recommender, Diagnostics) :-
		Recommender =.. [_| Arguments],
		last(Arguments, Diagnostics).

	print_recommender_template(Recommender) :-
		::recommender_term_template(Recommender, Template),
		format('Template: ~w~n', [Template]).

	% dataset collection and validation

	:- protected(dataset_ratings/2).
	:- mode(dataset_ratings(+object_identifier, -list(compound)), one_or_error).
	:- info(dataset_ratings/2, [
		comment is 'Collects ratings with atomic user and item identifiers as ``rating(User, Item, Rating)`` terms. Checks for duplicate user-item pairs and a declared rating count matching the observed count.',
		argnames is ['Dataset', 'Ratings'],
		exceptions is [
			'The dataset contains no ratings' - domain_error(non_empty_ratings, 'Dataset'),
			'A user or item identifier is a variable' - instantiation_error,
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'The same ``User``-``Item`` pair is rated more than once' - domain_error(duplicate_rating, 'User'-'Item'),
			'The declared rating count is a variable' - instantiation_error,
			'The declared rating count is neither a variable nor an integer' - type_error(integer, 'DeclaredCount'),
			'The declared rating count is an integer but is not positive' - domain_error(positive_integer, 'DeclaredCount'),
			'The declared and observed rating counts differ' - consistency_error(rating_count, 'DeclaredCount', 'ObservedCount')
		]
	]).

	dataset_ratings(Dataset, Ratings) :-
		findall(
			rating(User, Item, Rating),
			Dataset::rating(User, Item, Rating),
			Ratings0
		),
		(	Ratings0 == [] ->
			domain_error(non_empty_ratings, Dataset)
		;	true
		),
		check_rating_identifiers(Ratings0),
		check_no_duplicate_ratings(Ratings0),
		length(Ratings0, ObservedCount),
		Dataset::rating_count(DeclaredCount),
		context(Context),
		check(positive_integer, DeclaredCount, Context),
		(	DeclaredCount =:= ObservedCount ->
			Ratings = Ratings0
		;	consistency_error(rating_count, DeclaredCount, ObservedCount)
		).

	check_rating_identifiers([]).
	check_rating_identifiers([rating(User, Item, _Rating)| Ratings]) :-
		check_rating_identifier(User, Item),
		check_rating_identifiers(Ratings).

	check_rating_identifier(User, Item) :-
		context(Context),
		check(atomic, User, Context),
		check(atomic, Item, Context).

	:- protected(check_no_duplicate_ratings/1).
	:- mode(check_no_duplicate_ratings(+list(compound)), one_or_error).
	:- info(check_no_duplicate_ratings/1, [
		comment is 'Checks that ratings with atomic user and item identifiers contain no duplicate user-item pair.',
		argnames is ['Ratings'],
		exceptions is [
			'The same user-item pair is rated more than once' - domain_error(duplicate_rating, 'User'-'Item')
		]
	]).

	check_no_duplicate_ratings(Ratings) :-
		check_no_duplicate_ratings(Ratings, []).

	check_no_duplicate_ratings([], _Seen).
	check_no_duplicate_ratings([rating(User, Item, _Rating)| Ratings], Seen) :-
		(	memberchk(User-Item, Seen) ->
			domain_error(duplicate_rating, User-Item)
		;	true
		),
		check_no_duplicate_ratings(Ratings, [User-Item| Seen]).

	:- protected(check_ratings/2).
	:- mode(check_ratings(+object_identifier, +list(compound)), one_or_error).
	:- info(check_ratings/2, [
		comment is 'Checks that ratings are non-empty, have atomic user and item identifiers and numeric values, and fall within any declared ``rating_scale/2``.',
		argnames is ['Dataset', 'Ratings'],
		exceptions is [
			'``Ratings`` is empty' - domain_error(non_empty_ratings, 'Dataset'),
			'A user or item identifier is a variable' - instantiation_error,
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'A rating value is not a number' - type_error(number, 'Rating'),
			'A scale bound is not numeric' - type_error(number, 'Bound'),
			'Scale bounds are reversed' - domain_error(rating_scale, 'Min'-'Max'),
			'A rating value is a number but falls outside the declared rating scale' - domain_error(rating_scale('Min', 'Max'), 'Rating')
		]
	]).

	check_ratings(Dataset, Ratings) :-
		(	Ratings == [] ->
			domain_error(non_empty_ratings, Dataset)
		;	true
		),
		dataset_rating_scale(Dataset, Scale),
		check_rating_values(Ratings, Scale).

	check_rating_values([], _Scale).
	check_rating_values([rating(User, Item, Rating)| Ratings], Scale) :-
		check_rating_identifier(User, Item),
		(	number(Rating) ->
			true
		;	type_error(number, Rating)
		),
		check_rating_scale(Scale, Rating),
		check_rating_values(Ratings, Scale).

	check_rating_scale(none, _Rating).
	check_rating_scale(scale(Min, Max), Rating) :-
		(	Rating >= Min,
			Rating =< Max ->
			true
		;	domain_error(rating_scale(Min, Max), Rating)
		).

	% rating-matrix utilities

	:- protected(check_query_identifiers/2).
	:- mode(check_query_identifiers(@term, @term), one_or_error).
	:- info(check_query_identifiers/2, [
		comment is 'Checks that prediction identifiers are instantiated atomic terms.',
		argnames is ['User', 'Item'],
		exceptions is [
			'An identifier is a variable' - instantiation_error,
			'An identifier is not atomic' - type_error(atomic, 'Identifier')
		]
	]).

	check_query_identifiers(User, Item) :-
		check_rating_identifier(User, Item).

	:- protected(dataset_rating_scale/2).
	:- mode(dataset_rating_scale(+object_identifier, -term), one_or_error).
	:- info(dataset_rating_scale/2, [
		comment is 'Returns ``none`` or a validated inclusive ``scale(Min, Max)``.',
		argnames is ['Dataset', 'Scale'],
		exceptions is [
			'A scale bound is a variable' - instantiation_error,
			'A scale bound is not numeric' - type_error(number, 'Bound'),
			'Scale bounds are reversed' - domain_error(rating_scale, 'Min'-'Max')
		]
	]).

	dataset_rating_scale(Dataset, Scale) :-
		(	Dataset::rating_scale(Min, Max) ->
			context(Context),
			check(number, Min, Context),
			check(number, Max, Context),
			(	Min =< Max ->
				Scale = scale(Min, Max)
			;	domain_error(rating_scale, Min-Max)
			)
		;	Scale = none
		).

	:- protected(fallback_rating/5).
	:- mode(fallback_rating(+list(compound), +number, +atomic, +atomic, -number), one).
	:- info(fallback_rating/5, [
		comment is 'Returns the user mean, otherwise the item mean, otherwise the supplied global mean.',
		argnames is ['Ratings', 'GlobalMean', 'User', 'Item', 'Rating']
	]).

	fallback_rating(Ratings, GlobalMean, User, Item, Rating) :-
		user_vector(Ratings, User, UserVector),
		(	UserVector \== [] ->
			user_mean_rating(Ratings, User, Rating)
		;	item_vector(Ratings, Item, ItemVector),
			(	ItemVector \== [] ->
				item_mean_rating(Ratings, Item, Rating)
			;	Rating = GlobalMean
			)
		).

	:- protected(clip_rating/3).
	:- mode(clip_rating(+term, +number, -number), one).
	:- info(clip_rating/3, [
		comment is 'Clips a rating to a validated ``scale(Min, Max)``; ``none`` leaves it unchanged.',
		argnames is ['Scale', 'Rating', 'Clipped']
	]).

	clip_rating(none, Rating, Rating).
	clip_rating(scale(Min, Max), Rating, Clipped) :-
		(	Rating < Min ->
			Clipped = Min
		;	(	Rating > Max ->
				Clipped = Max
			;	Clipped = Rating
			)
		).

	:- protected(recommend_from_ratings/5).
	:- mode(recommend_from_ratings(+compound, +list(compound), +atomic, +positive_integer, -list(pair)), one_or_error).
	:- info(recommend_from_ratings/5, [
		comment is 'Scores unrated catalog items using self ``score/4`` and returns up to ``N`` descending-score pairs; ties use descending standard item order.',
		argnames is ['Recommender', 'Ratings', 'User', 'N', 'Recommendations'],
		exceptions is [
			'A required argument is a variable' - instantiation_error,
			'The model is invalid' - domain_error(recommender, 'Recommender'),
			'User is not atomic' - type_error(atomic, 'User'),
			'N is not an integer' - type_error(integer, 'N'),
			'N is not positive' - domain_error(positive_integer, 'N')
		]
	]).

	recommend_from_ratings(Recommender, Ratings, User, N, Recommendations) :-
		context(Context),
		::check_recommender(Recommender),
		check(atomic, User, Context),
		check(positive_integer, N, Context),
		items(Ratings, Items),
		findall(Item-Score,
			(	member(Item, Items),
				\+ member(rating(User, Item, _), Ratings),
				::score(Recommender, User, Item, Score)
			),
			Pairs
		),
		top_k(Pairs, N, Recommendations).

	:- protected(users/2).
	:- mode(users(+list(compound), -list(atomic)), one).
	:- info(users/2, [
		comment is 'Returns the sorted list of distinct users appearing in a list of ``rating/3`` terms.',
		argnames is ['Ratings', 'Users']
	]).

	users(Ratings, Users) :-
		findall(User, member(rating(User, _Item, _Rating), Ratings), Users0),
		sort(Users0, Users).

	:- protected(items/2).
	:- mode(items(+list(compound), -list(atomic)), one).
	:- info(items/2, [
		comment is 'Returns the sorted list of distinct items appearing in a list of ``rating/3`` terms.',
		argnames is ['Ratings', 'Items']
	]).

	items(Ratings, Items) :-
		findall(Item, member(rating(_User, Item, _Rating), Ratings), Items0),
		sort(Items0, Items).

	:- protected(user_vector/3).
	:- mode(user_vector(+list(compound), +atomic, -list(pair)), one).
	:- info(user_vector/3, [
		comment is 'Returns the sparse rating vector of a user, as a list of ``Item-Rating`` pairs, from a list of ``rating/3`` terms. The vector is empty when the user has no ratings.',
		argnames is ['Ratings', 'User', 'Vector']
	]).

	user_vector(Ratings, User, Vector) :-
		findall(Item-Rating, member(rating(User, Item, Rating), Ratings), Vector).

	:- protected(item_vector/3).
	:- mode(item_vector(+list(compound), +atomic, -list(pair)), one).
	:- info(item_vector/3, [
		comment is 'Returns the sparse rating vector of an item, as a list of ``User-Rating`` pairs, from a list of ``rating/3`` terms. The vector is empty when the item has no ratings.',
		argnames is ['Ratings', 'Item', 'Vector']
	]).

	item_vector(Ratings, Item, Vector) :-
		findall(User-Rating, member(rating(User, Item, Rating), Ratings), Vector).

	:- protected(global_mean_rating/2).
	:- mode(global_mean_rating(+list(compound), -float), one_or_error).
	:- info(global_mean_rating/2, [
		comment is 'Computes the mean of every rating in a list of ``rating/3`` terms (the global mean baseline).',
		argnames is ['Ratings', 'Mean'],
		exceptions is [
			'``Ratings`` is empty' - evaluation_error(zero_divisor)
		]
	]).

	global_mean_rating(Ratings, Mean) :-
		findall(Rating, member(rating(_User, _Item, Rating), Ratings), Values),
		mean_list(Values, Mean).

	:- protected(user_mean_rating/3).
	:- mode(user_mean_rating(+list(compound), +atomic, -float), one_or_error).
	:- info(user_mean_rating/3, [
		comment is 'Computes the mean of the ratings given by a user (the user mean baseline).',
		argnames is ['Ratings', 'User', 'Mean'],
		exceptions is [
			'``User`` has no ratings' - evaluation_error(zero_divisor)
		]
	]).

	user_mean_rating(Ratings, User, Mean) :-
		user_vector(Ratings, User, Vector),
		pair_values(Vector, Values),
		mean_list(Values, Mean).

	:- protected(item_mean_rating/3).
	:- mode(item_mean_rating(+list(compound), +atomic, -float), one_or_error).
	:- info(item_mean_rating/3, [
		comment is 'Computes the mean of the ratings received by an item (the item mean baseline).',
		argnames is ['Ratings', 'Item', 'Mean'],
		exceptions is [
			'``Item`` has no ratings' - evaluation_error(zero_divisor)
		]
	]).

	item_mean_rating(Ratings, Item, Mean) :-
		item_vector(Ratings, Item, Vector),
		pair_values(Vector, Values),
		mean_list(Values, Mean).

	pair_values([], []).
	pair_values([_Key-Value| Pairs], [Value| Values]) :-
		pair_values(Pairs, Values).

	mean_list(Values, Mean) :-
		sum(Values, Sum),
		length(Values, Count),
		(	Count =:= 0 ->
			evaluation_error(zero_divisor)
		;	Mean is Sum / Count
		).

	% similarity (delegating to the similarity metric
	% objects, which implement similarity_metric_protocol and can also be
	% used directly, or via a pluggable similarity_metric(Metric) option,
	% by concrete recommender implementations)

	:- protected(cosine_similarity/3).
	:- mode(cosine_similarity(+list(pair), +list(pair), -number), one).
	:- info(cosine_similarity/3, [
		comment is 'Convenience predicate equivalent to ``cosine_similarity::similarity/3``.',
		argnames is ['Vector1', 'Vector2', 'Similarity']
	]).

	cosine_similarity(Vector1, Vector2, Similarity) :-
		cosine_similarity::similarity(Vector1, Vector2, Similarity).

	:- protected(pearson_similarity/3).
	:- mode(pearson_similarity(+list(pair), +list(pair), -number), one).
	:- info(pearson_similarity/3, [
		comment is 'Convenience predicate equivalent to ``pearson_similarity::similarity/3``.',
		argnames is ['Vector1', 'Vector2', 'Similarity']
	]).

	pearson_similarity(Vector1, Vector2, Similarity) :-
		pearson_similarity::similarity(Vector1, Vector2, Similarity).

	:- protected(jaccard_similarity/3).
	:- mode(jaccard_similarity(+list(pair), +list(pair), -number), one).
	:- info(jaccard_similarity/3, [
		comment is 'Convenience predicate equivalent to ``jaccard_similarity::similarity/3``.',
		argnames is ['Vector1', 'Vector2', 'Similarity']
	]).

	jaccard_similarity(Vector1, Vector2, Similarity) :-
		jaccard_similarity::similarity(Vector1, Vector2, Similarity).

	:- protected(msd_similarity/3).
	:- mode(msd_similarity(+list(pair), +list(pair), -number), one).
	:- info(msd_similarity/3, [
		comment is 'Convenience predicate equivalent to ``msd_similarity::similarity/3``.',
		argnames is ['Vector1', 'Vector2', 'Similarity']
	]).

	msd_similarity(Vector1, Vector2, Similarity) :-
		msd_similarity::similarity(Vector1, Vector2, Similarity).

	:- protected(spearman_similarity/3).
	:- mode(spearman_similarity(+list(pair), +list(pair), -number), one).
	:- info(spearman_similarity/3, [
		comment is 'Convenience predicate equivalent to ``spearman_similarity::similarity/3``.',
		argnames is ['Vector1', 'Vector2', 'Similarity']
	]).

	spearman_similarity(Vector1, Vector2, Similarity) :-
		spearman_similarity::similarity(Vector1, Vector2, Similarity).

	% top-k retrieval

	:- protected(top_k/3).
	:- mode(top_k(+list(pair), +positive_integer, -list(pair)), one).
	:- info(top_k/3, [
		comment is 'Returns the ``K`` highest-scoring ``Key-Score`` pairs, sorted by decreasing score. Returns every pair, still sorted, when fewer than ``K`` are given; does not fail or throw an exception in that case.',
		argnames is ['Pairs', 'K', 'TopK']
	]).

	top_k(Pairs, K, TopK) :-
		swap_pairs(Pairs, Swapped),
		keysort(Swapped, Sorted),
		reverse(Sorted, Descending),
		take_at_most(K, Descending, TopKSwapped),
		swap_pairs(TopKSwapped, TopK).

	swap_pairs([], []).
	swap_pairs([Key-Value| Pairs], [Value-Key| Swapped]) :-
		swap_pairs(Pairs, Swapped).

	take_at_most(_K, [], []) :-
		!.
	take_at_most(0, _List, []) :-
		!.
	take_at_most(K, [Value| Values], [Value| Taken]) :-
		K > 0,
		K1 is K - 1,
		take_at_most(K1, Values, Taken).

	% diagnostics helpers

	:- protected(base_recommender_diagnostics/5).
	:- mode(base_recommender_diagnostics(+atom, +positive_integer, +list(compound), +list(compound), -list(compound)), one).
	:- info(base_recommender_diagnostics/5, [
		comment is 'Builds the common part of a recommender diagnostics metadata list, combined with implementation-specific extra diagnostics terms.',
		argnames is ['Model', 'RatingCount', 'Options', 'ExtraDiagnostics', 'Diagnostics']
	]).

	base_recommender_diagnostics(Model, RatingCount, Options, ExtraDiagnostics, Diagnostics) :-
		Diagnostics = [
			model(Model),
			rating_count(RatingCount),
			options(Options)
		| ExtraDiagnostics
		].

	:- protected(valid_recommender_metadata/2).
	:- mode(valid_recommender_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_recommender_metadata/2, [
		comment is 'True when diagnostics metadata contains the expected model term, without instantiating the metadata.',
		argnames is ['Model', 'Diagnostics']
	]).

	valid_recommender_metadata(Model, Diagnostics) :-
		valid(list(compound), Diagnostics),
		member(model(StoredModel), Diagnostics),
		StoredModel == Model,
		!.

	:- protected(valid_recommender_metadata/3).
	:- mode(valid_recommender_metadata(+atom, +list(compound), +list(compound)), zero_or_one).
	:- info(valid_recommender_metadata/3, [
		comment is 'True when diagnostics metadata contains the expected model term and effective options, without instantiating the metadata.',
		argnames is ['Model', 'Options', 'Diagnostics']
	]).

	valid_recommender_metadata(Model, Options, Diagnostics) :-
		valid_recommender_metadata(Model, Diagnostics),
		member(options(StoredOptions), Diagnostics),
		StoredOptions == Options,
		!.

	% export

	export_to_file(Dataset, Recommender, Functor, File) :-
		::export_to_clauses(Dataset, Recommender, Functor, Clauses),
		open(File, write, Stream),
		(	catch(
				(	write_comment_header(Dataset, Functor, Recommender, Stream),
					write_clauses(Clauses, Stream)
				),
				Error,
				(safe_close_stream(Stream), throw(Error))
			) ->
			close(Stream)
		;	safe_close_stream(Stream),
			fail
		).

	safe_close_stream(Stream) :-
		catch(close(Stream), _, true).

	write_comment_header(Dataset, Functor, Recommender, Stream) :-
		::recommender_export_template(Dataset, Recommender, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported recommender predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		::dataset_ratings(Dataset, Ratings),
		length(Ratings, Count),
		format(Stream, '% training rating count: ~d~n', [Count]),
		(	::diagnostics(Recommender, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;	true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.

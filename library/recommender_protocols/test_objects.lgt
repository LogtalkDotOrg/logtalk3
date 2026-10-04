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


:- object(sample_recommender,
	imports(recommender_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
		comment is 'Minimal baseline-predictor recommender used to exercise the recommender_protocol and recommender_common shared code end-to-end. Predicts ratings using the classic global mean plus user and item bias baseline, or any one of its components alone.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- public(shared_recommend/4).

	shared_recommend(Model, User, N, Recommendations) :-
		^^check_recommender(Model),
		Model = sample_recommender(Ratings, _, _, _),
		^^recommend_from_ratings(Model, Ratings, User, N, Recommendations).

	learn(Dataset, sample_recommender(Ratings, GlobalMean, Baseline, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(baseline(Baseline), Options),
		^^dataset_ratings(Dataset, Ratings),
		^^check_ratings(Dataset, Ratings),
		^^global_mean_rating(Ratings, GlobalMean),
		length(Ratings, RatingCount),
		^^base_recommender_diagnostics(sample_recommender, RatingCount, Options, [], Diagnostics).

	score(Recommender, User, Item, Rating) :-
		^^check_recommender(Recommender),
		Recommender = sample_recommender(Ratings, GlobalMean, Baseline, _Diagnostics),
		predict_with_baseline(Baseline, Ratings, GlobalMean, User, Item, Rating).

	predict_with_baseline(global, _Ratings, GlobalMean, _User, _Item, GlobalMean).
	predict_with_baseline(user, Ratings, GlobalMean, User, _Item, Rating) :-
		(	catch(^^user_mean_rating(Ratings, User, Mean), _Error, fail) ->
			Rating = Mean
		;	Rating = GlobalMean
		).
	predict_with_baseline(item, Ratings, GlobalMean, _User, Item, Rating) :-
		(	catch(^^item_mean_rating(Ratings, Item, Mean), _Error, fail) ->
			Rating = Mean
		;	Rating = GlobalMean
		).
	predict_with_baseline(blend, Ratings, GlobalMean, User, Item, Rating) :-
		(	catch(^^user_mean_rating(Ratings, User, UserMean), _Error, fail) ->
			UserBias is UserMean - GlobalMean
		;	UserBias = 0.0
		),
		(	catch(^^item_mean_rating(Ratings, Item, ItemMean), _Error, fail) ->
			ItemBias is ItemMean - GlobalMean
		;	ItemBias = 0.0
		),
		Rating is GlobalMean + UserBias + ItemBias.

	recommend(Recommender, User, N, Recommendations) :-
		^^check_recommender(Recommender),
		^^check_top_n(N),
		Recommender = sample_recommender(Ratings, GlobalMean, Baseline, _Diagnostics),
		^^items(Ratings, AllItems),
		^^user_vector(Ratings, User, UserVector),
		rated_items(UserVector, RatedItems),
		exclude_rated(AllItems, RatedItems, CandidateItems),
		score_items(CandidateItems, Baseline, Ratings, GlobalMean, User, Pairs),
		^^top_k(Pairs, N, Recommendations).

	rated_items([], []).
	rated_items([Item-_Rating| Vector], [Item| Items]) :-
		rated_items(Vector, Items).

	exclude_rated([], _Rated, []).
	exclude_rated([Item| Items], Rated, Candidates) :-
		(	memberchk(Item, Rated) ->
			Candidates = Rest
		;	Candidates = [Item| Rest]
		),
		exclude_rated(Items, Rated, Rest).

	score_items([], _Baseline, _Ratings, _GlobalMean, _User, []).
	score_items([Item| Items], Baseline, Ratings, GlobalMean, User, [Item-Score| Pairs]) :-
		predict_with_baseline(Baseline, Ratings, GlobalMean, User, Item, Score),
		score_items(Items, Baseline, Ratings, GlobalMean, User, Pairs).

	recommender_valid_data(Recommender) :-
		ground(Recommender),
		Recommender = sample_recommender(Ratings, GlobalMean, Baseline, Diagnostics),
		Ratings \== [],
		valid(list(compound), Ratings),
		valid_rating_records(Ratings),
		catch(^^check_no_duplicate_ratings(Ratings), error(domain_error(duplicate_rating, _), _), fail),
		number(GlobalMean),
		valid_baseline(Baseline),
		^^valid_recommender_metadata(sample_recommender, [baseline(Baseline)], Diagnostics),
		length(Ratings, Count),
		member(rating_count(StoredCount), Diagnostics),
		StoredCount == Count,
		!.

	valid_rating_records([]).
	valid_rating_records([rating(User, Item, Rating)| Ratings]) :-
		atomic(User),
		atomic(Item),
		number(Rating),
		valid_rating_records(Ratings).

	valid_baseline(Baseline) :-
		nonvar(Baseline),
		memberchk(Baseline, [global, user, item, blend]).

	recommender_export_template(_Dataset, _Recommender, Functor, Template) :-
		Template =.. [Functor, 'Recommender'].

	recommender_term_template(
		sample_recommender(_Ratings, _GlobalMean, _Baseline, _Diagnostics),
		sample_recommender('Ratings', 'GlobalMean', 'Baseline', 'Diagnostics')
	).

	export_to_clauses(_Dataset, Recommender, Functor, [Clause]) :-
		^^check_recommender(Recommender),
		Clause =.. [Functor, Recommender].

	print_recommender(Recommender) :-
		^^check_recommender(Recommender),
		^^print_recommender_template(Recommender),
		writeq(Recommender), nl.

	default_option(baseline(blend)).

	valid_option(baseline(Baseline)) :-
		valid_baseline(Baseline).

:- end_object.


:- object(sample_score_override,
	extends(sample_recommender)).

	score(_, _, _, 42).

:- end_object.


:- object(identifier_ratings(_, _),
	implements(rating_dataset_protocol)).

	rating(alice, m1, 5).
	rating(User, Item, 4) :-
		parameter(1, User),
		parameter(2, Item).

	rating_count(2).

	rating_scale(1, 5).

:- end_object.


:- object(validation_recommender,
	imports(recommender_common)).

	recommender_valid_data(Recommender) :-
		nonvar(Recommender),
		Recommender = validation_model(Value, _Diagnostics),
		number(Value).

:- end_object.


:- object(front_diagnostics_recommender,
	imports(recommender_common)).

	recommender_valid_data(Recommender) :-
		nonvar(Recommender),
		Recommender = front_model(_Diagnostics, Value),
		number(Value).

	recommender_diagnostics_data(front_model(Diagnostics, _Value), Diagnostics).

:- end_object.


:- object(similarity_helpers,
	imports(similarity_metric_common)).

	:- public([normalize/2, centered/2, bound/2]).

	normalize(Values, Normalized) :-
		^^normalize_values(Values, Normalized).

	centered(Values, Normalized) :-
		^^centered_normalized_values(Values, Normalized).

	bound(Score, Similarity) :-
		^^bounded_similarity(Score, Similarity).

:- end_object.


:- object(rating_scale_fixture(_, _),
	implements(rating_dataset_protocol)).

	rating(u, a, 1).
	rating_count(1).
	rating_scale(Min, Max) :-
		parameter(1, Min),
		parameter(2, Max).

:- end_object.


:- object(unscaled_ratings,
	implements(rating_dataset_protocol)).

	rating(u, a, 1).
	rating_count(1).
	rating_scale(_, _) :- fail.

:- end_object.

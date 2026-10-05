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


:- object(tests,
	extends(lgtunit),
	imports([recommender_common, item_content_dataset_validation])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Smoke tests for the "recommender_protocols" library. Reference values for the movie_ratings dataset were computed independently in Python.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	test(recommender_score_self_dispatch, deterministic(Score == 42)) :-
		sample_score_override::learn(movie_ratings, Model),
		sample_score_override::shared_recommend(Model, alice, 1, [_-Score]).

	test(common_scoring_contract, deterministic) :-
		sample_recommender::current_predicate(score/4),
		validation_recommender::current_predicate(score/4).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	cover(recommender_common).
	cover(item_content_dataset_validation).
	cover(similarity_metric_common).
	cover(cosine_similarity).
	cover(pearson_similarity).
	cover(jaccard_similarity).
	cover(msd_similarity).
	cover(spearman_similarity).
	cover(sample_recommender).

	test(content_occurrences_preserved, deterministic(Contents == [x-features([a,a,b])])) :-
		^^collect_contents(content_catalog_fixture([x], [x-features([b,a,a])]), Contents, features).

	test(content_zero_weights_removed, deterministic(Contents == [x-vector([a-2])])) :-
		^^collect_contents(content_catalog_fixture([x], [x-vector([b-0,a-2])]), Contents, vectors).

	test(content_canonical_order, deterministic(Contents == [x-features([a]),y-features([b])])) :-
		^^canonical_contents([y-features([b]),x-features([a])], Contents, features).

	test(content_empty_canonical_list, fail) :-
		^^canonical_contents([], _, _).

	test(content_rated_catalog_membership, deterministic) :-
		^^check_rated_catalog([rating(u,x,1)], [x-features([]),y-features([])]).

	test(content_missing_rated_item, error(domain_error(catalog_item, missing))) :-
		^^check_rated_catalog([rating(u,missing,1)], [x-features([])]).

	cleanup :-
		^^clean_file('test_output.pl').

	% rating_dataset_protocol tests

	test(movie_ratings_rating_3, deterministic(Count == 24)) :-
		findall(User-Item-Rating, movie_ratings::rating(User, Item, Rating), Ratings),
		length(Ratings, Count).

	test(movie_ratings_rating_count_1, deterministic(Count == 24)) :-
		movie_ratings::rating_count(Count).

	test(movie_ratings_rating_scale_2, deterministic(Min-Max == 1-5)) :-
		movie_ratings::rating_scale(Min, Max).

	% recommender_common shared helper tests: dataset collection and validation

	test(dataset_ratings_movie_ratings, deterministic(Count == 24)) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		length(Ratings, Count).

	test(dataset_ratings_duplicate_rating, error(domain_error(duplicate_rating, alice-m1))) :-
		sample_recommender::learn(duplicate_rating, _).

	test(dataset_ratings_inconsistent_count, error(consistency_error(rating_count, 4, 3))) :-
		sample_recommender::learn(inconsistent_rating_count, _).

	test(dataset_ratings_no_ratings, error(domain_error(non_empty_ratings, no_ratings))) :-
		sample_recommender::learn(no_ratings, _).

	test(check_ratings_non_numeric_rating, error(type_error(number, bad))) :-
		sample_recommender::learn(non_numeric_rating, _).

	test(check_ratings_out_of_scale_rating, error(domain_error(rating_scale(1, 5), 7))) :-
		sample_recommender::learn(out_of_scale_rating, _).

	test(dataset_ratings_variable_user, error(instantiation_error)) :-
		sample_recommender::learn(identifier_ratings(_, m1), _).

	test(dataset_ratings_variable_item, error(instantiation_error)) :-
		sample_recommender::learn(identifier_ratings(alice, _), _).

	test(dataset_ratings_compound_user, error(type_error(atomic, user(alice)))) :-
		sample_recommender::learn(identifier_ratings(user(alice), m2), _).

	test(dataset_ratings_compound_item, error(type_error(atomic, item(m2)))) :-
		sample_recommender::learn(identifier_ratings(bob, item(m2)), _).

	test(dataset_ratings_numeric_identifiers, deterministic(ground(Recommender))) :-
		sample_recommender::learn(identifier_ratings(1, 2), Recommender).

	test(check_ratings_variable_identifier, error(instantiation_error)) :-
		^^check_ratings(movie_ratings, [rating(_, m1, 4)]).

	test(check_ratings_compound_identifier, error(type_error(atomic, item(m1)))) :-
		^^check_ratings(movie_ratings, [rating(alice, item(m1), 4)]).

	% recommender_common shared helper tests: rating-matrix utilities

	test(query_identifier_variable, error(instantiation_error)) :-
		^^check_query_identifiers(_, a).

	test(query_identifier_compound, error(type_error(atomic, item(a)))) :-
		^^check_query_identifiers(u, item(a)).

	test(dataset_rating_scale_valid, deterministic(Scale == scale(1, 5))) :-
		^^dataset_rating_scale(movie_ratings, Scale).

	test(dataset_rating_scale_absent, deterministic(Scale == none)) :-
		^^dataset_rating_scale(unscaled_ratings, Scale).

	test(dataset_rating_scale_reversed, error(domain_error(rating_scale, 5-1))) :-
		^^dataset_rating_scale(rating_scale_fixture(5, 1), _).

	test(dataset_rating_scale_variable, error(instantiation_error)) :-
		^^dataset_rating_scale(rating_scale_fixture(_, 5), _).

	test(dataset_rating_scale_non_numeric, error(type_error(number, bad))) :-
		^^dataset_rating_scale(rating_scale_fixture(1, bad), _).

	test(fallback_rating_hierarchy, deterministic) :-
		Ratings = [rating(u, a, 1), rating(u, b, 3), rating(v, a, 5)],
		^^fallback_rating(Ratings, 3.0, u, unknown, UserMean),
		^^fallback_rating(Ratings, 3.0, unknown, a, ItemMean),
		^^fallback_rating(Ratings, 3.0, unknown, unknown, GlobalMean),
		assertion(UserMean =~= 2.0),
		assertion(ItemMean =~= 3.0),
		assertion(GlobalMean =~= 3.0).

	test(clip_rating_cases, deterministic) :-
		^^clip_rating(scale(1, 5), 0.0, Low),
		^^clip_rating(scale(1, 5), 6.0, High),
		^^clip_rating(scale(1, 5), 3.0, Middle),
		^^clip_rating(none, 6.0, Unchanged),
		assertion(Low == 1),
		assertion(High == 5),
		assertion(Middle =~= 3.0),
		assertion(Unchanged =~= 6.0).

	test(shared_recommendation_ties, deterministic) :-
		sample_recommender::learn(movie_ratings, Model, [baseline(global)]),
		sample_recommender::shared_recommend(Model, alice, 10, Recommendations),
		Recommendations = [m6-_, m5-_].

	test(users_2, deterministic(Users == [alice, bob, carol, dave, erin, frank])) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^users(Ratings, Users).

	test(items_2, deterministic(Items == [m1, m2, m3, m4, m5, m6])) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^items(Ratings, Items).

	test(user_vector_3, true) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^user_vector(Ratings, alice, Vector),
		memberchk(m1-5, Vector),
		memberchk(m2-4, Vector),
		memberchk(m3-5, Vector),
		memberchk(m4-1, Vector),
		length(Vector, 4).

	test(user_vector_unknown_user, deterministic(Vector == [])) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^user_vector(Ratings, nobody, Vector).

	test(item_vector_3, deterministic) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^item_vector(Ratings, m1, Vector),
		memberchk(alice-5, Vector),
		memberchk(bob-4, Vector),
		memberchk(carol-5, Vector),
		memberchk(dave-1, Vector),
		length(Vector, 4).

	test(global_mean_rating_2, true(Mean =~= 3.6666666666666665)) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^global_mean_rating(Ratings, Mean).

	test(user_mean_rating_3, true(Mean =~= 3.75)) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^user_mean_rating(Ratings, alice, Mean).

	test(user_mean_rating_no_ratings, error(evaluation_error(zero_divisor))) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^user_mean_rating(Ratings, nobody, _Mean).

	test(item_mean_rating_3, true(Mean =~= 4.0)) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^item_mean_rating(Ratings, m5, Mean).

	test(valid_recommender_metadata_3_true, deterministic) :-
		^^valid_recommender_metadata(sample_recommender, [baseline(blend)], [model(sample_recommender), rating_count(24), options([baseline(blend)])]).

	test(valid_recommender_metadata_3_false, fail) :-
		^^valid_recommender_metadata(sample_recommender, [baseline(global)], [model(sample_recommender), rating_count(24), options([baseline(blend)])]).

	test(valid_recommender_metadata_model_unchanged, true(var(Model))) :-
		\+ ^^valid_recommender_metadata(sample_recommender, [model(Model)]).

	test(valid_recommender_metadata_options_unchanged, true(var(Options))) :-
		\+ ^^valid_recommender_metadata(sample_recommender, [baseline(blend)], [model(sample_recommender), options(Options)]).

	% recommender_common shared helper tests: similarity metrics

	test(normalize_values_empty, deterministic(Normalized == [])) :-
		similarity_helpers::normalize([], Normalized).

	test(centered_normalized_values_empty, deterministic(Normalized == [])) :-
		similarity_helpers::centered([], Normalized).

	test(bounded_similarity_upper, deterministic(Similarity =~= 1.0)) :-
		similarity_helpers::bound(1.01, Similarity).

	test(bounded_similarity_lower, deterministic(Similarity =~= -1.0)) :-
		similarity_helpers::bound(-1.01, Similarity).

	test(cosine_similarity_like_minded_users, true(Similarity =~= 0.938531668711012)) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^user_vector(Ratings, alice, VectorAlice),
		^^user_vector(Ratings, bob, VectorBob),
		^^cosine_similarity(VectorAlice, VectorBob, Similarity).

	test(cosine_similarity_unlike_minded_users, true(Similarity =~= 0.14925373134328357)) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^user_vector(Ratings, alice, VectorAlice),
		^^user_vector(Ratings, dave, VectorDave),
		^^cosine_similarity(VectorAlice, VectorDave, Similarity).

	test(cosine_similarity_no_common_keys, deterministic(Similarity =~= 0.0)) :-
		cosine_similarity::similarity([m1-5], [m2-4], Similarity).

	test(cosine_similarity_all_zero_vector, deterministic(Similarity =~= 0.0)) :-
		cosine_similarity::similarity([m1-0], [m1-5], Similarity).

	test(cosine_similarity_tiny_identity, deterministic(Similarity =~= 1.0)) :-
		cosine_similarity::similarity([a-1.0e-200], [a-1.0e-200], Similarity).

	test(cosine_similarity_large_identity, deterministic(Similarity =~= 1.0)) :-
		cosine_similarity::similarity([a-1.0e200], [a-1.0e200], Similarity).

	test(cosine_similarity_opposite_direction, deterministic(Similarity =~= -1.0)) :-
		cosine_similarity::similarity([a-1.0e200, b-2.0e200], [a-(-1.0e-200), b-(-2.0e-200)], Similarity).

	test(cosine_similarity_empty_vector, deterministic(Similarity =~= 0.0)) :-
		cosine_similarity::similarity([], [a-1], Similarity).

	test(cosine_similarity_partial_overlap, deterministic(Similarity =~= 0.36)) :-
		cosine_similarity::similarity([a-3, b-4], [a-3, c-4], Similarity).

	test(cosine_similarity_symmetry_and_scale, true) :-
		cosine_similarity::similarity([a-3, b-4], [a-4, b-3], Reference),
		cosine_similarity::similarity([a-4.0e200, b-3.0e200], [a-3.0e-200, b-4.0e-200], Similarity),
		Reference =~= 0.96,
		Similarity =~= Reference,
		Similarity >= -1.0,
		Similarity =< 1.0.

	test(pearson_similarity_inverted_preferences, true(Similarity =~= -1.0)) :-
		sample_recommender::learn(movie_ratings, sample_recommender(Ratings, _, _, _)),
		^^user_vector(Ratings, alice, VectorAlice),
		^^user_vector(Ratings, bob, VectorBob),
		^^pearson_similarity(VectorAlice, VectorBob, Similarity).

	test(pearson_similarity_fewer_than_two_common_keys, deterministic(Similarity =~= 0.0)) :-
		pearson_similarity::similarity([m1-5], [m1-4], Similarity).

	test(pearson_similarity_no_common_keys, deterministic(Similarity =~= 0.0)) :-
		pearson_similarity::similarity([m1-5, m2-4], [m3-2, m4-1], Similarity).

	test(pearson_similarity_constant_values, deterministic(Similarity =~= 0.0)) :-
		pearson_similarity::similarity([m1-3, m2-3], [m1-5, m2-1], Similarity).

	test(pearson_similarity_constant_floats, deterministic(Similarity =~= 0.0)) :-
		pearson_similarity::similarity([a-0.1, b-0.1, c-0.1], [a-0.1, b-0.1, c-0.1], Similarity).

	test(pearson_similarity_constant_second_vector, deterministic(Similarity =~= 0.0)) :-
		pearson_similarity::similarity([a-1, b-2, c-3], [a-0.1, b-0.1, c-0.1], Similarity).

	test(pearson_similarity_constant_numeric_representations, deterministic(Similarity =~= 0.0)) :-
		pearson_similarity::similarity([a-1, b-1.0, c-1], [a-1, b-2, c-3], Similarity).

	test(pearson_similarity_large_integer_offset, deterministic(Similarity =~= 1.0)) :-
		pearson_similarity::similarity([a-9007199254740992, b-9007199254740993], [a-1, b-2], Similarity).

	test(pearson_similarity_tiny_values, deterministic(Similarity =~= 1.0)) :-
		pearson_similarity::similarity([a-1.0e-200, b-2.0e-200, c-3.0e-200], [a-1, b-2, c-3], Similarity).

	test(pearson_similarity_large_values, deterministic(Similarity =~= 1.0)) :-
		pearson_similarity::similarity([a-1.0e200, b-2.0e200, c-3.0e200], [a-1, b-2, c-3], Similarity).

	test(pearson_similarity_large_opposite_signs, deterministic(Similarity =~= -1.0)) :-
		pearson_similarity::similarity([a-(-1.0e308), b-1.0e308], [a-1, b-(-1)], Similarity).

	test(pearson_similarity_negative_reference, deterministic(Similarity =~= 1.0)) :-
		pearson_similarity::similarity([a-(-9007199254740993), b-(-9007199254740992)], [a-1, b-2], Similarity).

	test(pearson_similarity_common_subset, deterministic(Similarity =~= -1.0)) :-
		pearson_similarity::similarity([a-1, b-2, c-1000], [a-2, b-1, d-(-1000)], Similarity).

	test(pearson_similarity_symmetry_offset_and_scale, deterministic) :-
		pearson_similarity::similarity([a-1, b-2, c-4], [a-2, b-4, c-3], Reference),
		pearson_similarity::similarity([a-120, b-140, c-130], [a-5, b-7, c-11], Similarity),
		assertion(Similarity =~= Reference),
		assertion(Similarity >= -1.0),
		assertion(Similarity =< 1.0).

	test(similarity_metric_protocol_polymorphism, true) :-
		forall(
			member(Metric, [cosine_similarity, pearson_similarity, jaccard_similarity, msd_similarity, spearman_similarity]),
			(	Metric::similarity([m1-5, m2-4], [m1-4, m2-5], Similarity),
				number(Similarity)
			)
		).

	test(jaccard_similarity_partial_overlap, deterministic(Similarity =~= 0.5)) :-
		^^jaccard_similarity([a-1, b-2, c-3], [d-4, c-5, b-6], Similarity).

	test(jaccard_similarity_ignores_values, deterministic(Similarity =~= 1.0)) :-
		jaccard_similarity::similarity([a-0, b-(-3)], [b-100, a-7], Similarity).

	test(jaccard_similarity_disjoint, deterministic(Similarity =~= 0.0)) :-
		jaccard_similarity::similarity([a-1], [b-1], Similarity).

	test(jaccard_similarity_both_empty, deterministic(Similarity =~= 0.0)) :-
		jaccard_similarity::similarity([], [], Similarity).

	test(jaccard_similarity_one_empty, deterministic(Similarity =~= 0.0)) :-
		jaccard_similarity::similarity([a-1], [], Similarity).

	test(jaccard_similarity_symmetry, deterministic) :-
		jaccard_similarity::similarity([a-1, b-2], [b-0, c-3, d-4], Forward),
		jaccard_similarity::similarity([b-0, c-3, d-4], [a-1, b-2], Backward),
		assertion(Forward =~= 0.25),
		assertion(Backward =~= Forward).

	test(msd_similarity_reference, deterministic(Similarity =~= 0.2857142857142857)) :-
		^^msd_similarity([a-1, b-2], [b-4, a-2], Similarity).

	test(msd_similarity_identical_constant, deterministic(Similarity =~= 1.0)) :-
		msd_similarity::similarity([a-0.1, b-0.1], [a-0.1, b-0.1], Similarity).

	test(msd_similarity_single_common_key, deterministic(Similarity =~= 0.5)) :-
		msd_similarity::similarity([a-1, b-1000], [a-2, c-(-1000)], Similarity).

	test(msd_similarity_no_common_keys, deterministic(Similarity =~= 0.0)) :-
		msd_similarity::similarity([a-1], [b-2], Similarity).

	test(msd_similarity_empty_vectors, deterministic(Similarity =~= 0.0)) :-
		msd_similarity::similarity([], [], Similarity).

	test(msd_similarity_small_difference, deterministic(Similarity =~= 0.8)) :-
		msd_similarity::similarity([a-1], [a-1.5], Similarity).

	test(msd_similarity_large_integer_offset, deterministic(Similarity =~= 0.5)) :-
		msd_similarity::similarity([a-9007199254740992], [a-9007199254740993], Similarity).

	test(msd_similarity_large_opposite_signs, deterministic(Similarity =~= 0.0)) :-
		msd_similarity::similarity([a-1.0e308], [a-(-1.0e308)], Similarity).

	test(msd_similarity_tiny_difference, deterministic(Similarity =~= 1.0)) :-
		msd_similarity::similarity([a-1.0e-200], [a-2.0e-200], Similarity).

	test(msd_similarity_symmetry, deterministic) :-
		msd_similarity::similarity([a-(-2), b-0], [a-3, b-1], Forward),
		msd_similarity::similarity([b-1, a-3], [b-0, a-(-2)], Backward),
		assertion(Forward =~= 0.07142857142857142),
		assertion(Backward =~= Forward).

	test(spearman_similarity_monotonic, deterministic(Similarity =~= 1.0)) :-
		^^spearman_similarity([a-1, b-2, c-3], [c-9, a-1, b-4], Similarity).

	test(spearman_similarity_reverse_order, deterministic(Similarity =~= -1.0)) :-
		spearman_similarity::similarity([a-1, b-2, c-3], [a-9, b-4, c-1], Similarity).

	test(spearman_similarity_non_monotonic, deterministic(Similarity =~= 0.5)) :-
		spearman_similarity::similarity([a-1, b-2, c-3], [a-1, b-3, c-2], Similarity).

	test(spearman_similarity_average_ties, deterministic(Similarity =~= 0.8660254037844386)) :-
		spearman_similarity::similarity([a-1, b-1.0, c-2], [a-1, b-2, c-3], Similarity).

	test(spearman_similarity_ties_both_vectors, deterministic(Similarity =~= 0.8333333333333334)) :-
		spearman_similarity::similarity([a-1, b-1, c-2, d-3], [a-1, b-2, c-2, d-3], Similarity).

	test(spearman_similarity_constant, deterministic(Similarity =~= 0.0)) :-
		spearman_similarity::similarity([a-0.1, b-0.1, c-0.1], [a-1, b-2, c-3], Similarity).

	test(spearman_similarity_one_common_key, deterministic(Similarity =~= 0.0)) :-
		spearman_similarity::similarity([a-1, b-2], [a-3, c-4], Similarity).

	test(spearman_similarity_no_common_keys, deterministic(Similarity =~= 0.0)) :-
		spearman_similarity::similarity([a-1], [b-2], Similarity).

	test(spearman_similarity_empty_vectors, deterministic(Similarity =~= 0.0)) :-
		spearman_similarity::similarity([], [], Similarity).

	test(spearman_similarity_common_subset, deterministic(Similarity =~= 1.0)) :-
		spearman_similarity::similarity([a-1, b-2, c-0], [a-4, b-9, d-1000], Similarity).

	test(spearman_similarity_large_integers, deterministic(Similarity =~= 1.0)) :-
		spearman_similarity::similarity([a-9007199254740992, b-9007199254740993], [a-1, b-2], Similarity).

	test(spearman_similarity_large_floats, deterministic(Similarity =~= -1.0)) :-
		spearman_similarity::similarity([a-(-1.0e308), b-0, c-1.0e308], [a-1, b-0, c-(-1)], Similarity).

	test(spearman_similarity_symmetry, deterministic) :-
		spearman_similarity::similarity([a-1, b-1, c-2], [a-1, b-2, c-3], Forward),
		spearman_similarity::similarity([c-3, b-2, a-1], [c-2, b-1, a-1], Backward),
		assertion(Backward =~= Forward).

	% recommender_common shared helper tests: top-k retrieval

	test(top_k_3, deterministic(TopK == [c-3.0, b-2.0, a-1.0])) :-
		^^top_k([a-1.0, b-2.0, c-3.0], 3, TopK).

	test(top_k_fewer_than_requested, deterministic(TopK == [c-3.0, b-2.0, a-1.0])) :-
		^^top_k([a-1.0, b-2.0, c-3.0], 10, TopK).

	test(top_k_zero, deterministic(TopK == [])) :-
		^^top_k([a-1.0, b-2.0, c-3.0], 0, TopK).

	test(top_k_empty_pairs, deterministic(TopK == [])) :-
		^^top_k([], 3, TopK).

	% sample_recommender end-to-end tests

	test(sample_recommender_learn_2, deterministic(ground(Recommender))) :-
		sample_recommender::learn(movie_ratings, Recommender).

	test(sample_recommender_learn_3, deterministic(ground(Recommender))) :-
		sample_recommender::learn(movie_ratings, Recommender, [baseline(blend)]).

	test(sample_recommender_valid_recommender, deterministic(sample_recommender::valid_recommender(Recommender))) :-
		sample_recommender::learn(movie_ratings, Recommender).

	test(sample_recommender_invalid_recommender, fail) :-
		sample_recommender::valid_recommender(sample_recommender(not_a_list, 3.5, blend, [model(sample_recommender), rating_count(24), options([])])).

	test(default_recommender_validation, deterministic) :-
		validation_recommender::check_recommender(validation_model(1, [model(validation), rating_count(1), options([]), extra(data)])).

	test(overridden_diagnostics_validation, deterministic) :-
		front_diagnostics_recommender::check_recommender(front_model([model(front), rating_count(1), options([])], 1)).

	test(default_recommender_invalid_diagnostics, true) :-
		forall(
			member(Diagnostics, [not_a_list, [], [model(validation), options([])], [model(validation), rating_count(1)], [model(1), rating_count(1), options([])], [model(validation), rating_count(0), options([])], [model(validation), rating_count(1), options(bad)], [model(validation), rating_count(1), options([bad])], [model(validation), model(validation), rating_count(1), options([])], [model(validation), rating_count(1), rating_count(1), options([])], [model(validation), rating_count(1), options([]), options([])]]),
			\+ validation_recommender::valid_recommender(validation_model(1, Diagnostics))
		).

	test(default_recommender_invalid_shape, error(domain_error(recommender, other(1)))) :-
		validation_recommender::check_recommender(other(1)).

	test(default_recommender_bad_metadata_error, error(domain_error(recommender, validation_model(1, not_a_list)))) :-
		validation_recommender::check_recommender(validation_model(1, not_a_list)).

	test(default_recommender_partial_metadata_unchanged, variant(Recommender, Copy)) :-
		Recommender = validation_model(1, [model(_), rating_count(1), options([])]),
		copy_term(Recommender, Copy),
		\+ validation_recommender::valid_recommender(Recommender).

	test(default_recommender_unbound_diagnostics_unchanged, true(var(Diagnostics))) :-
		\+ validation_recommender::valid_recommender(validation_model(1, Diagnostics)).

	test(default_recommender_partial_options_unchanged, true(var(Baseline))) :-
		\+ validation_recommender::valid_recommender(validation_model(1, [model(validation), rating_count(1), options([baseline(Baseline)])])).

	test(sample_recommender_malformed_data, true) :-
		forall(
			member(Recommender, [sample_recommender([garbage], 3.0, blend, [model(sample_recommender)]), sample_recommender([], 3.0, blend, [model(sample_recommender), rating_count(1), options([baseline(blend)])]), sample_recommender([rating(alice, m1, bad)], 3.0, blend, [model(sample_recommender), rating_count(1), options([baseline(blend)])]), sample_recommender([rating(user(alice), m1, 4)], 3.0, blend, [model(sample_recommender), rating_count(1), options([baseline(blend)])]), sample_recommender([rating(alice, m1, 4), rating(alice, m1, 5)], 3.0, blend, [model(sample_recommender), rating_count(2), options([baseline(blend)])]), sample_recommender([rating(alice, m1, 4)], 3.0, blend, [model(sample_recommender), rating_count(2), options([baseline(blend)])]), sample_recommender([rating(alice, m1, 4)], 3.0, blend, [model(sample_recommender), rating_count(1), options([baseline(global)])])]),
			\+ sample_recommender::valid_recommender(Recommender)
		).

	test(sample_recommender_partial_baseline_unchanged, variant(Recommender, Copy)) :-
		Recommender = sample_recommender([rating(alice, m1, 4)], 4.0, _, [model(sample_recommender), rating_count(1), options([baseline(blend)])]),
		copy_term(Recommender, Copy),
		catch(sample_recommender::check_recommender(Recommender), error(domain_error(recommender, _), _), true),
		\+ sample_recommender::valid_recommender(Recommender).

	test(sample_recommender_partial_list_unchanged, variant(Recommender, Copy)) :-
		Recommender = sample_recommender([rating(alice, m1, 4)| _], 4.0, blend, [model(sample_recommender), rating_count(1), options([baseline(blend)])]),
		copy_term(Recommender, Copy),
		\+ sample_recommender::valid_recommender(Recommender).

	test(sample_recommender_score_invalid, error(domain_error(recommender, sample_recommender([garbage], 3.0, global, [model(sample_recommender)])))) :-
		sample_recommender::score(sample_recommender([garbage], 3.0, global, [model(sample_recommender)]), alice, m1, _).

	test(sample_recommender_score_blend, true(Rating =~= 4.083333333333334)) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::score(Recommender, alice, m5, Rating).

	test(sample_recommender_score_global, deterministic(Rating =~= 3.6666666666666665)) :-
		sample_recommender::learn(movie_ratings, Recommender, [baseline(global)]),
		sample_recommender::score(Recommender, alice, m5, Rating).

	test(sample_recommender_score_unknown_user, true(Rating =~= 4.0)) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::score(Recommender, nobody, m5, Rating).

	test(sample_recommender_score_unknown_item, true(Rating =~= 3.75)) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::score(Recommender, alice, nothing, Rating).

	test(sample_recommender_recommend_alice, true(Recommendations == [m5-4.083333333333334, m6-3.8333333333333335])) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::recommend(Recommender, alice, 2, Recommendations).

	test(sample_recommender_recommend_dave, true(Recommendations == [m3-3.8333333333333335, m2-3.75])) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::recommend(Recommender, dave, 2, Recommendations).

	test(sample_recommender_recommend_excludes_rated_items, true) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::recommend(Recommender, alice, 6, Recommendations),
		\+ (member(Item-_Score, Recommendations), memberchk(Item, [m1, m2, m3, m4])).

	test(sample_recommender_recommend_negative_n, error(domain_error(positive_integer, -1))) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::recommend(Recommender, alice, -1, _).

	test(sample_recommender_recommend_non_integer_n, error(type_error(integer, foo))) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::recommend(Recommender, alice, foo, _).

	test(sample_recommender_recommend_unbound_n, error(instantiation_error)) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::recommend(Recommender, alice, _, _).

	test(sample_recommender_diagnostics_2, true) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::diagnostics(Recommender, Diagnostics),
		memberchk(model(sample_recommender), Diagnostics),
		memberchk(rating_count(24), Diagnostics),
		sample_recommender::diagnostic(Recommender, model(sample_recommender)).

	test(sample_recommender_forecaster_options_2, true(Options == [baseline(blend)])) :-
		sample_recommender::learn(movie_ratings, Recommender, [baseline(blend)]),
		sample_recommender::recommender_options(Recommender, Options).

	test(sample_recommender_learn_2_same_as_learn_3, true(Recommender0 == Recommender1)) :-
		sample_recommender::learn(movie_ratings, Recommender0),
		sample_recommender::learn(movie_ratings, Recommender1, []).

	test(sample_recommender_invalid_baseline_option, error(domain_error(option, baseline(average)))) :-
		sample_recommender::learn(movie_ratings, _, [baseline(average)]).

	% export and printing

	test(sample_recommender_export_to_clauses_4, true(Clause == recommender_model(Recommender))) :-
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::export_to_clauses(movie_ratings, Recommender, recommender_model, [Clause]).

	test(sample_recommender_export_to_file_4_header, true(HeaderLine == '% exported recommender predicate: recommender_model/1')) :-
		^^file_path('test_output.pl', File),
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::export_to_file(movie_ratings, Recommender, recommender_model, File),
		first_header_line(File, HeaderLine).

	test(sample_recommender_export_to_file_4_loadable, true(Rating =~= 4.083333333333334)) :-
		^^file_path('test_output.pl', File),
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::export_to_file(movie_ratings, Recommender, recommender_model, File),
		logtalk_load(File),
		{recommender_model(Loaded)},
		sample_recommender::valid_recommender(Loaded),
		sample_recommender::score(Loaded, alice, m5, Rating).

	test(sample_recommender_print_recommender_1, true) :-
		^^suppress_text_output,
		sample_recommender::learn(movie_ratings, Recommender),
		sample_recommender::print_recommender(Recommender).

	% forecaster protocol predicates (generic)

	test(sample_recommender_check_recommender_unbound, error(instantiation_error)) :-
		sample_recommender::check_recommender(_).

	test(sample_recommender_check_recommender_invalid, error(domain_error(recommender, foo))) :-
		sample_recommender::check_recommender(foo).

	% auxiliary predicates

	first_header_line(File, Line) :-
		open(File, read, Stream),
		read_line_atom(Stream, Line),
		close(Stream).

	read_line_atom(Stream, Line) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Line = end_of_file
		;	read_line_codes(Code, Stream, Codes),
			atom_codes(Line, Codes)
		).

	read_line_codes(-1, _Stream, []) :-
		!.
	read_line_codes(10, _Stream, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		!,
		get_code(Stream, NextCode),
		(	NextCode == 10 ->
			Codes = []
		;	read_line_codes(NextCode, Stream, Codes)
		).
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

:- end_object.

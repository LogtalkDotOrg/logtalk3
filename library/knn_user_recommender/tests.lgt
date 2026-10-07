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
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Unit tests for the "knn_user_recommender" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2
	]).

	cover(knn_user_recommender).

	cleanup :-
		^^clean_file('user_knn_export.pl').

	test(knn_user_recommender_movie_score, deterministic(Score =~= 1.5)) :-
		knn_user_recommender::learn(movie_ratings, Model),
		knn_user_recommender::score(Model, alice, m6, Score).

	test(knn_user_recommender_score_implemented_locally, deterministic) :-
		knn_user_recommender::predicate_property(score(_, _, _, _), defined_in(knn_user_recommender)).

	test(knn_user_recommender_learn, deterministic(ground(Model))) :-
		knn_user_recommender::learn(user_knn_ratings, Model).

	test(knn_user_recommender_reference_prediction, deterministic(Rating =~= 3.3333333333333335)) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::score(Model, u, c, Rating).

	test(knn_user_recommender_k_larger_than_available, deterministic(Rating =~= 3.3333333333333335)) :-
		knn_user_recommender::learn(user_knn_ratings, Model, [k(20)]),
		knn_user_recommender::score(Model, u, c, Rating).

	test(knn_user_recommender_overlap_fallback, deterministic(Rating =~= 2.0)) :-
		knn_user_recommender::learn(user_knn_ratings, Model, [min_overlap(3)]),
		knn_user_recommender::score(Model, u, c, Rating).

	test(knn_user_recommender_threshold_fallback, deterministic(Rating =~= 2.0)) :-
		knn_user_recommender::learn(user_knn_ratings, Model, [min_similarity(2.0)]),
		knn_user_recommender::score(Model, u, c, Rating).

	test(knn_user_recommender_unknown_identifiers, deterministic) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::score(Model, unknown, c, ItemMean),
		knn_user_recommender::score(Model, u, unknown, UserMean),
		knn_user_recommender::score(Model, unknown, unknown, GlobalMean),
		assertion(ItemMean =~= 5.0), assertion(UserMean =~= 2.0), assertion(GlobalMean =~= 3.0).

	test(knn_user_recommender_recommendation, deterministic) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::recommend(Model, u, 10, [c-Rating]),
		assertion(Rating =~= 3.3333333333333335).

	test(knn_user_recommender_no_candidates, deterministic(Recommendations == [])) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::recommend(Model, v, 10, Recommendations).

	test(knn_user_recommender_metrics, true) :-
		forall(member(Metric, [cosine_similarity, pearson_similarity, jaccard_similarity, msd_similarity, spearman_similarity, user_knn_metric(0.5)]),
			(knn_user_recommender::learn(user_knn_ratings, Model, [similarity_metric(Metric)]),
			 knn_user_recommender::score(Model, u, c, Rating), Rating =~= 3.3333333333333335)).

	test(knn_user_recommender_nonpositive_metric_fallback, true) :-
		forall(member(Score, [0.0, -1.0]),
			(knn_user_recommender::learn(user_knn_ratings, Model, [similarity_metric(user_knn_metric(Score))]),
			 knn_user_recommender::score(Model, u, c, Rating), Rating =~= 2.0)).

	test(knn_user_recommender_invalid_metric_score, error(domain_error(similarity_score, [bad]))) :-
		knn_user_recommender::learn(user_knn_ratings, Model, [similarity_metric(user_knn_metric(bad))]),
		knn_user_recommender::score(Model, u, c, _).

	test(knn_user_recommender_nondeterministic_metric, error(domain_error(similarity_score, [0.5, 1.0]))) :-
		knn_user_recommender::learn(user_knn_ratings, Model, [similarity_metric(user_knn_multiple_metric)]),
		knn_user_recommender::score(Model, u, c, _).

	test(knn_user_recommender_invalid_k, error(domain_error(option, k(0)))) :-
		knn_user_recommender::learn(user_knn_ratings, _, [k(0)]).

	test(knn_user_recommender_duplicate_options, deterministic(Rating =~= 1.3333333333333333)) :-
		knn_user_recommender::learn(user_knn_tied_ratings, Model, [k(1), k(2)]),
		knn_user_recommender::valid_recommender(Model),
		knn_user_recommender::score(Model, u, c, Rating).

	test(knn_user_recommender_duplicate_ratings, error(domain_error(duplicate_rating, alice-m1))) :-
		knn_user_recommender::learn(duplicate_rating, _).

	test(knn_user_recommender_query_variable, error(instantiation_error)) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::score(Model, _, c, _).

	test(knn_user_recommender_query_compound, error(type_error(atomic, item(c)))) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::score(Model, u, item(c), _).

	test(knn_user_recommender_nonpositive_n, error(domain_error(positive_integer, 0))) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::recommend(Model, u, 0, _).

	test(knn_user_recommender_incomplete_model, true) :-
		Model = knn_user_model(_, _, _, _, _),
		\+ knn_user_recommender::valid_recommender(Model).

	test(knn_user_recommender_tampered_profiles, fail) :-
		knn_user_recommender::learn(user_knn_ratings, knn_user_model(Ratings, _, Mean, Scale, Diagnostics)),
		knn_user_recommender::valid_recommender(knn_user_model(Ratings, [], Mean, Scale, Diagnostics)).

	test(knn_user_recommender_diagnostics, true) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::diagnostic(Model, model(knn_user_recommender)),
		knn_user_recommender::diagnostic(Model, rating_count(5)).

	test(knn_user_recommender_export_round_trip, deterministic) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		^^file_path('user_knn_export.pl', File),
		knn_user_recommender::export_to_file(user_knn_ratings, Model, user_knn_saved, File),
		logtalk_load(File), {user_knn_saved(Loaded)},
		knn_user_recommender::score(Loaded, u, c, Rating),
		assertion(Rating =~= 3.3333333333333335).

	test(knn_user_recommender_print, deterministic) :-
		^^suppress_text_output,
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::print_recommender(Model).

	test(knn_user_recommender_clipping, true) :-
		forall(member(Clip-Expected, [true-5.0, false-6.833333333333333]),
			(knn_user_recommender::learn(user_knn_clipped_ratings, Model, [clip_to_scale(Clip)]),
			 knn_user_recommender::score(Model, u, c, Rating), Rating =~= Expected)).

	test(knn_user_recommender_filter_before_k, deterministic(Rating =~= 3.3333333333333335)) :-
		knn_user_recommender::learn(user_knn_filtered_ratings, Model, [k(1)]),
		knn_user_recommender::score(Model, u, c, Rating).

	test(knn_user_recommender_neighbor_tie_order, deterministic(Rating =~= 1.3333333333333333)) :-
		knn_user_recommender::learn(user_knn_tied_ratings, Model, [k(1)]),
		knn_user_recommender::score(Model, u, c, Rating).

	test(knn_user_recommender_self_exclusion, deterministic(Rating =~= 2.3333333333333335)) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::score(Model, u, b, Rating).

	test(knn_user_recommender_declared_metric, error(domain_error(option, similarity_metric(user_knn_declared_metric)))) :-
		knn_user_recommender::learn(user_knn_ratings, _, [similarity_metric(user_knn_declared_metric)]).

	test(knn_user_recommender_failing_metric, error(domain_error(similarity_score, []))) :-
		knn_user_recommender::learn(user_knn_ratings, Model, [similarity_metric(user_knn_failing_metric)]),
		knn_user_recommender::score(Model, u, c, _).

	test(knn_user_recommender_tampered_counts, fail) :-
		knn_user_recommender::learn(user_knn_ratings, knn_user_model(Ratings, Profiles, Mean, Scale, _)),
		knn_user_recommender::valid_recommender(knn_user_model(Ratings, Profiles, Mean, Scale,
			[model(knn_user_recommender), rating_count(5), options([k(3), similarity_metric(pearson_similarity), min_overlap(1), min_similarity(0.0), clip_to_scale(true)]), user_count(999), item_count(3), neighbor_axis(user)])).

	test(knn_user_recommender_default_options, deterministic(Model == Explicit)) :-
		knn_user_recommender::learn(movie_ratings, Model),
		knn_user_recommender::learn(movie_ratings, Explicit, []),
		knn_user_recommender::valid_recommender(Model).

	test(knn_user_recommender_atomic_numeric_queries, deterministic(Rating =~= 3.0)) :-
		knn_user_recommender::learn(user_knn_ratings, Model),
		knn_user_recommender::score(Model, 123, 456, Rating).

:- end_object.

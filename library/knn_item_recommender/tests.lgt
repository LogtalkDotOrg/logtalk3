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
		date is 2026-10-03,
		comment is 'Unit tests for the "knn_item_recommender" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2
	]).

	cover(knn_item_recommender).

	cleanup :-
		^^clean_file('item_knn_export.pl').

	test(learn, deterministic(ground(Model))) :-
		knn_item_recommender::learn(item_knn_ratings, Model).

	test(reference_prediction, deterministic) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::predict_rating(Model, u, c, Rating),
		Expected is (2 / sqrt(5) + 3 * 0.8) / (2 / sqrt(5) + 0.8),
		assertion(Rating =~= Expected).

	test(k_one, deterministic(Rating =~= 1.0)) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [k(1)]),
		knn_item_recommender::predict_rating(Model, u, c, Rating).

	test(overlap_fallback, deterministic(Rating =~= 2.0)) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [min_overlap(2)]),
		knn_item_recommender::predict_rating(Model, u, c, Rating).

	test(threshold_fallback, deterministic(Rating =~= 2.0)) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [min_similarity(2.0)]),
		knn_item_recommender::predict_rating(Model, u, c, Rating).

	test(self_exclusion, deterministic(Rating =~= 3.0)) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::predict_rating(Model, u, a, Rating).

	test(metrics, true) :-
		forall(member(Metric-Expected, [pearson_similarity-2.0, spearman_similarity-2.0, jaccard_similarity-2.0, msd_similarity-2.6666666666666665, item_knn_metric(0.5)-2.0]),
			(knn_item_recommender::learn(item_knn_ratings, Model, [similarity_metric(Metric)]),
			 knn_item_recommender::predict_rating(Model, u, c, Rating), Rating =~= Expected)).

	test(nonpositive_metric_fallback, true) :-
		forall(member(Score, [0.0, -1.0]),
			(knn_item_recommender::learn(item_knn_ratings, Model, [similarity_metric(item_knn_metric(Score))]),
			 knn_item_recommender::predict_rating(Model, u, c, Rating), Rating =~= 2.0)).

	test(invalid_metric_score, error(domain_error(similarity_score, [bad]))) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [similarity_metric(item_knn_metric(bad))]),
		knn_item_recommender::predict_rating(Model, u, c, _).

	test(unknown_identifiers, deterministic) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::predict_rating(Model, unknown, c, ItemMean),
		knn_item_recommender::predict_rating(Model, u, unknown, UserMean),
		knn_item_recommender::predict_rating(Model, unknown, unknown, GlobalMean),
		assertion(ItemMean =~= 5.0), assertion(UserMean =~= 2.0), assertion(GlobalMean =~= 3.0).

	test(recommendation_matches_prediction, deterministic) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::recommend(Model, u, 10, [c-Score]),
		knn_item_recommender::predict_rating(Model, u, c, Rating),
		assertion(Score =~= Rating).

	test(no_candidates, deterministic(Recommendations == [])) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::recommend(Model, v, 10, Recommendations).

	test(invalid_options, error(domain_error(option, min_overlap(0)))) :-
		knn_item_recommender::learn(item_knn_ratings, _, [min_overlap(0)]).

	test(duplicate_ratings, error(domain_error(duplicate_rating, alice-m1))) :-
		knn_item_recommender::learn(duplicate_rating, _).

	test(query_variable, error(instantiation_error)) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::predict_rating(Model, u, _, _).

	test(nonpositive_n, error(domain_error(positive_integer, -1))) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::recommend(Model, u, -1, _).

	test(incomplete_model, variant(Model, Copy)) :-
		Model = knn_item_model(_, _, _, _, _), copy_term(Model, Copy),
		\+ knn_item_recommender::valid_recommender(Model).

	test(tampered_profiles, fail) :-
		knn_item_recommender::learn(item_knn_ratings, knn_item_model(Ratings, _, Mean, Scale, Diagnostics)),
		knn_item_recommender::valid_recommender(knn_item_model(Ratings, [], Mean, Scale, Diagnostics)).

	test(export_round_trip, deterministic) :-
		knn_item_recommender::learn(item_knn_ratings, Model),
		^^file_path('item_knn_export.pl', File),
		knn_item_recommender::export_to_file(item_knn_ratings, Model, item_knn_saved, File),
		logtalk_load(File), {item_knn_saved(Loaded)},
		knn_item_recommender::valid_recommender(Loaded).

	test(print, deterministic) :-
		^^suppress_text_output,
		knn_item_recommender::learn(item_knn_ratings, Model),
		knn_item_recommender::print_recommender(Model).

	test(filter_before_k, deterministic(Rating =~= 1.0)) :-
		knn_item_recommender::learn(item_knn_filtered_ratings, Model, [k(1)]),
		knn_item_recommender::predict_rating(Model, u, c, Rating).

	test(neighbor_tie_order, deterministic(Rating =~= 3.0)) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [k(1), similarity_metric(item_knn_metric(0.5))]),
		knn_item_recommender::predict_rating(Model, u, c, Rating).

	test(no_scale, deterministic) :-
		knn_item_recommender::learn(item_knn_unscaled_ratings, Model),
		Model = knn_item_model(_, _, _, none, _),
		knn_item_recommender::predict_rating(Model, u, c, Rating),
		Expected is (2 / sqrt(5) + 3 * 0.8) / (2 / sqrt(5) + 0.8),
		assertion(Rating =~= Expected).

	test(reversed_scale, error(domain_error(rating_scale, 5-1))) :-
		knn_item_recommender::learn(item_knn_scaled_ratings(5, 1), _).

	test(nonnumeric_scale, error(type_error(number, bad))) :-
		knn_item_recommender::learn(item_knn_scaled_ratings(bad, 5), _).

	test(nondeterministic_metric, error(domain_error(similarity_score, [0.5, 1.0]))) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [similarity_metric(item_knn_multiple_metric)]),
		knn_item_recommender::predict_rating(Model, u, c, _).

	test(failing_metric, error(domain_error(similarity_score, []))) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [similarity_metric(item_knn_failing_metric)]),
		knn_item_recommender::predict_rating(Model, u, c, _).

	test(missing_metric, error(domain_error(option, similarity_metric(missing_item_knn_metric)))) :-
		knn_item_recommender::learn(item_knn_ratings, _, [similarity_metric(missing_item_knn_metric)]).

	test(tampered_counts, fail) :-
		knn_item_recommender::learn(item_knn_ratings, knn_item_model(Ratings, Profiles, Mean, Scale, _)),
		knn_item_recommender::valid_recommender(knn_item_model(Ratings, Profiles, Mean, Scale,
			[model(knn_item_recommender), rating_count(5), options([k(3), similarity_metric(cosine_similarity), min_overlap(1), min_similarity(0.0), clip_to_scale(true)]), user_count(2), item_count(999), neighbor_axis(item)])).

	test(default_options, deterministic(Model == Explicit)) :-
		knn_item_recommender::learn(movie_ratings, Model),
		knn_item_recommender::learn(movie_ratings, Explicit, []),
		knn_item_recommender::valid_recommender(Model).

	test(duplicate_options, deterministic(Rating =~= 1.0)) :-
		knn_item_recommender::learn(item_knn_ratings, Model, [k(1), k(2)]),
		knn_item_recommender::valid_recommender(Model),
		knn_item_recommender::predict_rating(Model, u, c, Rating).

:- end_object.

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
		comment is 'Unit tests for the "slope_one_recommender" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	cover(slope_one_recommender).

	cleanup :-
		^^clean_file('slope_export.pl').

	test(slope_one_recommender_movie_score, deterministic(Score =~= Expected)) :-
		slope_one_recommender::learn(movie_ratings, Model),
		slope_one_recommender::score(Model, alice, m6, Score),
		Expected is 10 / 3.

	test(slope_one_recommender_score_implemented_locally, deterministic) :-
		slope_one_recommender::predicate_property(score(_, _, _, _), defined_in(slope_one_recommender)).

	test(slope_one_recommender_learn, deterministic(ground(Model))) :-
		slope_one_recommender::learn(slope_ratings, Model).

	test(slope_one_recommender_reference_prediction, deterministic(Rating =~= 4.0)) :-
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::score(Model, u, c, Rating).

	test(slope_one_recommender_weighted_prediction, deterministic(Rating =~= 3.6666666666666665)) :-
		slope_one_recommender::learn(slope_weighted_ratings, Model),
		slope_one_recommender::score(Model, u, c, Rating).

	test(slope_one_recommender_deviation_orientation_and_counts, deterministic(Difference =~= -2.5)) :-
		slope_one_recommender::learn(slope_weighted_ratings, slope_one_model(_, Deviations, _, _, _)),
		memberchk(deviation(a, c, Difference, 2), Deviations).

	test(slope_one_recommender_reverse_lookup, deterministic(Rating =~= 1.0)) :-
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::score(Model, u, a, Rating).

	test(slope_one_recommender_support_filter, deterministic(Rating =~= 3.5)) :-
		slope_one_recommender::learn(slope_weighted_ratings, Model, [min_support(2)]),
		slope_one_recommender::score(Model, u, c, Rating).

	test(slope_one_recommender_support_fallback, deterministic(Rating =~= 2.0)) :-
		slope_one_recommender::learn(slope_ratings, Model, [min_support(3)]),
		slope_one_recommender::score(Model, u, c, Rating).

	test(slope_one_recommender_no_pair_dataset, deterministic(Rating =~= 1.0)) :-
		slope_one_recommender::learn(slope_disconnected_ratings, Model),
		Model = slope_one_model(_, [], _, none, _),
		slope_one_recommender::score(Model, u, b, Rating).

	test(slope_one_recommender_clipping_on, deterministic(Rating == 5)) :-
		slope_one_recommender::learn(slope_clipped_ratings, Model),
		slope_one_recommender::score(Model, u, b, Rating).

	test(slope_one_recommender_clipping_off, deterministic(Rating =~= 9.0)) :-
		slope_one_recommender::learn(slope_clipped_ratings, Model, [clip_to_scale(false)]),
		slope_one_recommender::score(Model, u, b, Rating).

	test(slope_one_recommender_unknown_identifiers, deterministic) :-
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::score(Model, unknown, c, ItemMean),
		slope_one_recommender::score(Model, u, unknown, UserMean),
		slope_one_recommender::score(Model, unknown, unknown, GlobalMean),
		assertion(ItemMean =~= 5.0),
		assertion(UserMean =~= 2.0),
		assertion(GlobalMean =~= 3.0).

	test(slope_one_recommender_recommendation, deterministic) :-
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::recommend(Model, u, 10, [c-Rating]), assertion(Rating =~= 4.0).

	test(slope_one_recommender_no_candidates, deterministic(Recommendations == [])) :-
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::recommend(Model, v, 10, Recommendations).

	test(slope_one_recommender_invalid_support, error(domain_error(option, min_support(0)))) :-
		slope_one_recommender::learn(slope_ratings, _, [min_support(0)]).

	test(slope_one_recommender_duplicate_ratings, error(domain_error(duplicate_rating, alice-m1))) :-
		slope_one_recommender::learn(duplicate_rating, _).

	test(slope_one_recommender_query_variable, error(instantiation_error)) :-
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::score(Model, _, c, _).

	test(slope_one_recommender_tampered_deviations, fail) :-
		slope_one_recommender::learn(slope_ratings, slope_one_model(Ratings, _, Mean, Scale, Diagnostics)),
		slope_one_recommender::valid_recommender(slope_one_model(Ratings, [], Mean, Scale, Diagnostics)).

	test(slope_one_recommender_export_round_trip, deterministic) :-
		slope_one_recommender::learn(slope_ratings, Model),
		^^file_path('slope_export.pl', File),
		slope_one_recommender::export_to_file(slope_ratings, Model, slope_saved, File),
		logtalk_load(File),
		{slope_saved(Loaded)},
		slope_one_recommender::score(Loaded, u, c, Rating), assertion(Rating =~= 4.0).

	test(slope_one_recommender_print, deterministic) :-
		^^suppress_text_output,
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::print_recommender(Model).

	test(slope_one_recommender_default_options, deterministic(Model == Explicit)) :-
		slope_one_recommender::learn(movie_ratings, Model),
		slope_one_recommender::learn(movie_ratings, Explicit, []),
		slope_one_recommender::valid_recommender(Model).

	test(slope_one_recommender_duplicate_options, deterministic(Rating =~= 4.0)) :-
		slope_one_recommender::learn(slope_ratings, Model, [min_support(1), min_support(2)]),
		slope_one_recommender::valid_recommender(Model),
		slope_one_recommender::score(Model, u, c, Rating).

	test(slope_one_recommender_tampered_counts, fail) :-
		slope_one_recommender::learn(slope_ratings, slope_one_model(Ratings, Deviations, Mean, Scale, _)),
		slope_one_recommender::valid_recommender(slope_one_model(Ratings, Deviations, Mean, Scale,
			[model(slope_one_recommender), rating_count(5), options([min_support(1), clip_to_scale(true)]), user_count(2), item_count(3), deviation_pair_count(999)])).

	test(slope_one_recommender_tampered_global_mean, fail) :-
		slope_one_recommender::learn(slope_ratings, slope_one_model(Ratings, Deviations, _, Scale, Diagnostics)),
		slope_one_recommender::valid_recommender(slope_one_model(Ratings, Deviations, 999, Scale, Diagnostics)).

	test(slope_one_recommender_variable_n, error(instantiation_error)) :-
		slope_one_recommender::learn(slope_ratings, Model),
		slope_one_recommender::recommend(Model, u, _, _).

	test(slope_one_recommender_unknown_user_catalog_and_ties, deterministic) :-
		slope_one_recommender::learn(slope_disconnected_ratings, Model),
		slope_one_recommender::recommend(Model, unknown, 10, [b-High, a-Low]),
		assertion(High =~= 5.0),
		assertion(Low =~= 1.0).

:- end_object.

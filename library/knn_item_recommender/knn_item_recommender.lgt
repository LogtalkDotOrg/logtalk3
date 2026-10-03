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


:- object(knn_item_recommender,
	imports(recommender_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-03,
		comment is 'Item-based k-nearest-neighbor recommender with similarity-weighted ratings and pluggable metrics.',
		see_also is [recommender_protocol, cosine_similarity]
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, knn_item_model(Ratings, Profiles, GlobalMean, Scale, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_ratings(Dataset, Ratings),
		^^check_ratings(Dataset, Ratings),
		^^dataset_rating_scale(Dataset, Scale),
		^^global_mean_rating(Ratings, GlobalMean),
		build_profiles(Ratings, Profiles),
		length(Ratings, Count),
		^^users(Ratings, Users),
		length(Users, UserCount),
		length(Profiles, ItemCount),
		^^base_recommender_diagnostics(knn_item_recommender, Count, Options,
			[user_count(UserCount), item_count(ItemCount), neighbor_axis(item)], Diagnostics).

	predict_rating(Model, User, Item, Rating) :-
		^^check_recommender(Model),
		^^check_query_identifiers(User, Item),
		Model = knn_item_model(Ratings, Profiles, GlobalMean, Scale, _),
		^^recommender_options(Model, Options),
		(	member(Item-profile(Vector, _), Profiles),
			item_estimate(Profiles, Vector, User, Item, Estimate, Options) ->
			true
		;	^^fallback_rating(Ratings, GlobalMean, User, Item, Estimate)
		),
		^^option(clip_to_scale(Clip), Options),
		(	Clip == true ->
			^^clip_rating(Scale, Estimate, Rating)
		;	Rating = Estimate
		).

	recommend(Model, User, N, Recommendations) :-
		^^check_recommender(Model),
		Model = knn_item_model(Ratings, _, _, _, _),
		^^recommend_from_ratings(Model, Ratings, User, N, Recommendations).

	build_profiles(Ratings, Profiles) :-
		^^items(Ratings, Items),
		build_item_profiles(Items, Ratings, Profiles).

	build_item_profiles([], _Ratings, []).
	build_item_profiles([Item| Items], Ratings, [Item-profile(Vector, Mean)| Profiles]) :-
		^^item_vector(Ratings, Item, Vector0),
		sort(Vector0, Vector),
		^^item_mean_rating(Ratings, Item, Mean),
		build_item_profiles(Items, Ratings, Profiles).

	item_estimate(Profiles, Vector, User, Item, Estimate, Options) :-
		^^option(similarity_metric(Metric), Options),
		^^option(min_overlap(MinOverlap), Options),
		^^option(min_similarity(MinSimilarity), Options),
		findall(Neighbor-Similarity,
			(	member(Neighbor-profile(OtherVector, _), Profiles),
				Neighbor \== Item,
				member(User-_, OtherVector),
				common_count(Vector, OtherVector, Overlap),
				Overlap >= MinOverlap,
				metric_score(Metric, Vector, OtherVector, Similarity),
				Similarity > 0,
				Similarity >= MinSimilarity
			), Pairs),
		^^option(k(K), Options),
		^^top_k(Pairs, K, Neighbors),
		Neighbors = [_-Scale| _],
		item_totals(Neighbors, Profiles, User, Scale, 0.0, 0.0, Sum, Weight),
		Estimate is Sum / Weight.

	common_count(Vector, OtherVector, Count) :-
		findall(Key, (member(Key-_, Vector), member(Key-_, OtherVector)), Keys),
		length(Keys, Count).

	metric_score(Metric, Vector, OtherVector, Score) :-
		findall(Value, Metric::similarity(Vector, OtherVector, Value), Scores),
		(	Scores = [Score],
			finite_number(Score) ->
			true
		;	domain_error(similarity_score, Scores)
		).

	item_totals([], _Profiles, _User, _Scale, Sum, Weight, Sum, Weight).
	item_totals([Neighbor-Similarity| Neighbors], Profiles, User, Scale, Sum0, Weight0, Sum, Weight) :-
		memberchk(Neighbor-profile(Vector, _), Profiles),
		memberchk(User-Rating, Vector),
		Scaled is Similarity / Scale,
		Sum1 is Sum0 + Scaled * Rating,
		Weight1 is Weight0 + Scaled,
		item_totals(Neighbors, Profiles, User, Scale, Sum1, Weight1, Sum, Weight).

	recommender_valid_data(Model) :-
		ground(Model),
		Model = knn_item_model(Ratings, Profiles, GlobalMean, Scale, Diagnostics),
		valid(list(compound), Diagnostics),
		Ratings \== [], valid(list(compound), Ratings),
		valid_records(Ratings),
		catch(^^check_no_duplicate_ratings(Ratings), error(domain_error(duplicate_rating, _), _), fail),
		valid_scale(Scale, Ratings),
		^^global_mean_rating(Ratings, ExpectedMean), GlobalMean == ExpectedMean,
		build_profiles(Ratings, ExpectedProfiles), Profiles == ExpectedProfiles,
		memberchk(options(Options), Diagnostics),
		^^valid_recommender_metadata(knn_item_recommender, Options, Diagnostics),
		length(Ratings, Count), memberchk(rating_count(StoredCount), Diagnostics), StoredCount == Count,
		length(Profiles, ItemCount),
		^^users(Ratings, Users),
		length(Users, UserCount),
		memberchk(user_count(StoredUserCount), Diagnostics), StoredUserCount == UserCount,
		memberchk(item_count(StoredItemCount), Diagnostics), StoredItemCount == ItemCount,
		memberchk(neighbor_axis(StoredNeighborAxis), Diagnostics), StoredNeighborAxis == item.

	valid_records([]).
	valid_records([rating(User, Item, Rating)| Ratings]) :-
		atomic(User), atomic(Item), finite_number(Rating),
		valid_records(Ratings).

	valid_scale(none, _Ratings).
	valid_scale(scale(Min, Max), Ratings) :-
		finite_number(Min), finite_number(Max), Min =< Max,
		forall(member(rating(_, _, Rating), Ratings), (Rating >= Min, Rating =< Max)).

	finite_number(Value) :-
		number(Value), catch((Zero is Value - Value, Zero =:= 0), _, fail).

	default_option(k(3)).
	default_option(similarity_metric(cosine_similarity)).
	default_option(min_overlap(1)).
	default_option(min_similarity(0.0)).
	default_option(clip_to_scale(true)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(min_overlap(Count)) :-
		valid(positive_integer, Count).
	valid_option(min_similarity(Score)) :-
		finite_number(Score), Score >= 0.
	valid_option(clip_to_scale(Flag)) :-
		once((Flag == true; Flag == false)).
	valid_option(similarity_metric(Metric)) :-
		ground(Metric), current_object(Metric),
		Metric::predicate_property(similarity(_, _, _), (public)),
		Metric::predicate_property(similarity(_, _, _), defined_in(_)).

	recommender_export_template(_, _, Functor, Template) :-
		Template =.. [Functor, 'Recommender'].

	recommender_term_template(knn_item_model(_, _, _, _, _),
		knn_item_model('Ratings', 'Profiles', 'GlobalMean', 'Scale', 'Diagnostics')).

	export_to_clauses(_, Model, Functor, [Clause]) :-
		^^check_recommender(Model),
		Clause =.. [Functor, Model].

	print_recommender(Model) :-
		^^check_recommender(Model),
		^^print_recommender_template(Model),
		writeq(Model), nl.

:- end_object.

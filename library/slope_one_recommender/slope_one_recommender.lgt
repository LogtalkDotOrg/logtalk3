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


:- object(slope_one_recommender,
	imports(recommender_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Weighted Slope One recommender with canonical item-pair deviations and co-rating support counts.',
		see_also is [recommender_protocol]
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		max/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, slope_one_model(Ratings, Deviations, GlobalMean, Scale, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_ratings(Dataset, Ratings),
		^^check_ratings(Dataset, Ratings),
		^^dataset_rating_scale(Dataset, Scale),
		^^global_mean_rating(Ratings, GlobalMean),
		build_deviations(Ratings, Deviations),
		length(Ratings, Count),
		^^users(Ratings, Users), length(Users, UserCount),
		^^items(Ratings, Items), length(Items, ItemCount),
		length(Deviations, PairCount),
		^^base_recommender_diagnostics(slope_one_recommender, Count, Options,
			[user_count(UserCount), item_count(ItemCount), deviation_pair_count(PairCount)], Diagnostics).

	score(Model, User, Item, Rating) :-
		^^check_recommender(Model),
		^^check_query_identifiers(User, Item),
		Model = slope_one_model(Ratings, Deviations, GlobalMean, Scale, _),
		^^recommender_options(Model, Options),
		^^option(min_support(MinSupport), Options),
		(	slope_estimate(Ratings, Deviations, User, Item, MinSupport, Estimate) ->
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
		Model = slope_one_model(Ratings, _, _, _, _),
		^^recommend_from_ratings(Model, Ratings, User, N, Recommendations).

	build_deviations(Ratings, Deviations) :-
		^^users(Ratings, Users),
		findall(pair(Lo, Hi)-Difference,
			(	member(User, Users),
				^^user_vector(Ratings, User, Vector),
				member(Lo-LowRating, Vector),
				member(Hi-HighRating, Vector),
				Lo @< Hi,
				Difference is LowRating - HighRating
			), Pairs),
		keysort(Pairs, Sorted),
		aggregate_deviations(Sorted, Deviations).

	aggregate_deviations([], []).
	aggregate_deviations([pair(Lo, Hi)-Difference| Pairs], [deviation(Lo, Hi, Mean, Count)| Deviations]) :-
		take_pair(Pairs, Lo, Hi, Difference, 1, Sum, Count, Rest),
		Mean is Sum / Count,
		aggregate_deviations(Rest, Deviations).

	take_pair([pair(Lo, Hi)-Difference| Pairs], Lo, Hi, Sum0, Count0, Sum, Count, Rest) :-
		!,
		Sum1 is Sum0 + Difference,
		Count1 is Count0 + 1,
		take_pair(Pairs, Lo, Hi, Sum1, Count1, Sum, Count, Rest).
	take_pair(Pairs, _Lo, _Hi, Sum, Count, Sum, Count, Pairs).

	lookup_deviation(Deviations, Item, Other, Difference, Count) :-
		(	Item @< Other ->
			member(deviation(Item, Other, Difference, Count), Deviations)
		;	member(deviation(Other, Item, Reverse, Count), Deviations),
			Difference is -Reverse
		).

	slope_estimate(Ratings, Deviations, User, Item, MinSupport, Estimate) :-
		findall(Count-Prediction,
			(	member(rating(User, Other, Rating), Ratings),
				Other \== Item,
				lookup_deviation(Deviations, Item, Other, Difference, Count),
				Count >= MinSupport,
				Prediction is Rating + Difference
			), Pairs),
		Pairs \== [],
		findall(Count, member(Count-_, Pairs), Counts),
		max(Counts, Scale),
		slope_totals(Pairs, Scale, 0.0, 0.0, Sum, Weight),
		Estimate is Sum / Weight.

	slope_totals([], _Scale, Sum, Weight, Sum, Weight).
	slope_totals([Count-Prediction| Pairs], Scale, Sum0, Weight0, Sum, Weight) :-
		Scaled is Count / Scale,
		Sum1 is Sum0 + Scaled * Prediction,
		Weight1 is Weight0 + Scaled,
		slope_totals(Pairs, Scale, Sum1, Weight1, Sum, Weight).

	recommender_valid_data(Model) :-
		ground(Model),
		Model = slope_one_model(Ratings, Deviations, GlobalMean, Scale, Diagnostics),
		valid(list(compound), Diagnostics),
		Ratings \== [], valid(list(compound), Ratings), valid_records(Ratings),
		catch(^^check_no_duplicate_ratings(Ratings), error(domain_error(duplicate_rating, _), _), fail),
		valid_scale(Scale, Ratings),
		^^global_mean_rating(Ratings, ExpectedMean), GlobalMean == ExpectedMean,
		build_deviations(Ratings, ExpectedDeviations), Deviations == ExpectedDeviations,
		memberchk(options(Options), Diagnostics),
		^^valid_recommender_metadata(slope_one_recommender, Options, Diagnostics),
		length(Ratings, Count), memberchk(rating_count(StoredCount), Diagnostics), StoredCount == Count,
		^^users(Ratings, Users),
		length(Users, UserCount),
		^^items(Ratings, Items),
		length(Items, ItemCount),
		length(Deviations, PairCount),
		memberchk(user_count(StoredUserCount), Diagnostics), StoredUserCount == UserCount,
		memberchk(item_count(StoredItemCount), Diagnostics), StoredItemCount == ItemCount,
		memberchk(deviation_pair_count(StoredDeviationPairCount), Diagnostics), StoredDeviationPairCount == PairCount.

	valid_records([]).
	valid_records([rating(User, Item, Rating)| Ratings]) :-
		atomic(User), atomic(Item), finite_number(Rating), valid_records(Ratings).

	valid_scale(none, _Ratings).
	valid_scale(scale(Min, Max), Ratings) :-
		finite_number(Min), finite_number(Max), Min =< Max,
		forall(member(rating(_, _, Rating), Ratings), (Rating >= Min, Rating =< Max)).

	finite_number(Value) :-
		number(Value), catch((Zero is Value - Value, Zero =:= 0), _, fail).

	default_option(min_support(1)).
	default_option(clip_to_scale(true)).

	valid_option(min_support(Count)) :-
		valid(positive_integer, Count).
	valid_option(clip_to_scale(Flag)) :-
		once((Flag == true; Flag == false)).

	recommender_export_template(_, _, Functor, Template) :-
		Template =.. [Functor, 'Recommender'].

	recommender_term_template(slope_one_model(_, _, _, _, _),
		slope_one_model('Ratings', 'Deviations', 'GlobalMean', 'Scale', 'Diagnostics')).

	export_to_clauses(_, Model, Functor, [Clause]) :-
		^^check_recommender(Model),
		Clause =.. [Functor, Model].

	print_recommender(Model) :-
		^^check_recommender(Model),
		^^print_recommender_template(Model),
		writeq(Model), nl.

:- end_object.

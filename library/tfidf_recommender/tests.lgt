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
		date is 2026-10-04,
		comment is 'Unit tests for the "tfidf_recommender" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2, reverse/2, append/3
	]).

	cover(tfidf_recommender).

	cleanup :-
		^^clean_file('tfidf_saved.pl'),
		^^clean_file('tfidf_updated.pl').

	test(tfidf_recommender_batch_scores, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, [y,diagonal,x,y], [y-Y,diagonal-D,x-X,y-Repeat]),
		Expected is 1 / sqrt(2),
		assertion(Y =~= Expected),
		assertion(X =~= Expected),
		assertion(D =~= 1.0),
		assertion(Repeat =~= Y),
		tfidf_recommender::score(Model, u, y, Individual),
		assertion(Individual =~= Y).

	test(tfidf_recommender_batch_empty, deterministic(Scores == [])) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, [], Scores).

	test(tfidf_recommender_batch_unknown_item, error(domain_error(catalog_item, missing))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, unknown, [x,missing], _).

	test(tfidf_recommender_batch_empty_checks_user, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, _, [], _).

	test(tfidf_recommender_content_feature_score, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,5)], [x,y], [x-features([a,a,b]),y-features([b,c])]),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, x, Individual),
		tfidf_recommender::score_content(Model, u, features([b,a,a,unseen]), Content),
		assertion(Individual =~= 1.0),
		assertion(Content =~= Individual).

	test(tfidf_recommender_content_vector_score, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, vector([x-0.5,y-0.5,absent-0]), Score),
		assertion(Score =~= 1.0),
		tfidf_recommender::score_content(Model, u, vector([novel-1,x-1,y-1]), Novel),
		Expected is sqrt(2 / 3),
		assertion(Novel =~= Expected).

	test(tfidf_recommender_content_kind_mismatch, error(domain_error(content_representation, features([])))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, unknown, features([]), _).

	test(tfidf_recommender_batch_validates_once, deterministic(Count == 1)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_validation_counter::learn(Dataset, Model),
		tfidf_validation_counter::reset_validation_count,
		tfidf_validation_counter::score_all(Model, u, [x,y,x], _),
		tfidf_validation_counter::validation_count(Count).

	test(tfidf_recommender_content_validates_once, deterministic(Count == 1)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_validation_counter::learn(Dataset, Model),
		tfidf_validation_counter::reset_validation_count,
		tfidf_validation_counter::score_content(Model, u, vector([x-1]), _),
		tfidf_validation_counter::validation_count(Count).

	test(tfidf_recommender_batch_unknown_user, deterministic(Scores == [x-0.0,y-0.0])) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, unknown, [x,y], Scores).

	test(tfidf_recommender_batch_empty_profile, deterministic(Scores == [x-0.0,diagonal-0.0])) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(5)]),
		tfidf_recommender::score_all(Model, u, [x,diagonal], Scores).

	test(tfidf_recommender_batch_empty_validates_once, deterministic(Count == 1)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_validation_counter::learn(Dataset, Model),
		tfidf_validation_counter::reset_validation_count,
		tfidf_validation_counter::score_all(Model, u, [], []),
		tfidf_validation_counter::validation_count(Count).

	test(tfidf_recommender_batch_implemented_locally, deterministic) :-
		tfidf_recommender::predicate_property(score_all(_,_,_,_), defined_in(tfidf_recommender)).

	test(tfidf_recommender_batch_variable_model, error(instantiation_error)) :-
		tfidf_recommender::score_all(_, u, [], _).

	test(tfidf_recommender_batch_invalid_model, error(domain_error(recommender, bad))) :-
		tfidf_recommender::score_all(bad, u, [], _).

	test(tfidf_recommender_batch_nonatomic_user, error(type_error(atomic, user(u)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, user(u), [], _).

	test(tfidf_recommender_batch_variable_list, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, _, _).

	test(tfidf_recommender_batch_nonlist, error(type_error(list, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, bad, _).

	test(tfidf_recommender_batch_open_list, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, [x|_], _).

	test(tfidf_recommender_batch_improper_list, error(type_error(list, [x|bad]))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, [x|bad], _).

	test(tfidf_recommender_batch_variable_item, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, unknown, [x,_], _).

	test(tfidf_recommender_batch_nonatomic_item, error(type_error(atomic, item(x)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, [item(x)], _).

	test(tfidf_recommender_batch_missing_first, error(domain_error(catalog_item, missing))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_all(Model, u, [missing,x], _).

	test(tfidf_recommender_content_frozen_vocabulary, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,5)], [x,y], [x-features([a,a,b]),y-features([b,c])]),
		tfidf_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		tfidf_recommender::score_content(Model, u, features([unseen,unseen]), Unseen),
		tfidf_recommender::score_content(Model, u, features([]), Empty),
		tfidf_recommender::score_content(Model, u, features([a,b]), Single),
		assertion(Unseen =~= 0.0),
		assertion(Empty =~= 0.0),
		assertion(Single < 1.0),
		assertion(lgtunit::variant(Model, Original)).

	test(tfidf_recommender_content_binary_weighting, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u,x,5)], [x], [x-features([genre(a),genre(a),b])]),
		tfidf_recommender::learn(Dataset, Model, [normalization(none),vectorizer_options([weighting(binary)])]),
		tfidf_recommender::score_content(Model, u, features([b,genre(a),unknown]), Score).

	test(tfidf_recommender_content_vector_none, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u,x,5)], [x], [x-vector([a-10,b-1])]),
		tfidf_recommender::learn(Dataset, Model, [normalization(none)]),
		tfidf_recommender::score_content(Model, u, vector([b-0.5,a-5]), Score).

	test(tfidf_recommender_content_unknown_user, deterministic(Score =~= 0.0)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, unknown, vector([x-1]), Score).

	test(tfidf_recommender_content_empty_profile, deterministic(Score =~= 0.0)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(5)]),
		tfidf_recommender::score_content(Model, u, vector([x-1]), Score).

	test(tfidf_recommender_content_empty_vector_space, deterministic(Score =~= 0.0)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-vector([])]), Model),
		tfidf_recommender::score_content(Model, u, vector([novel-1]), Score).

	test(tfidf_recommender_content_implemented_locally, deterministic) :-
		tfidf_recommender::predicate_property(score_content(_,_,_,_), defined_in(tfidf_recommender)).

	test(tfidf_recommender_content_variable_model, error(instantiation_error)) :-
		tfidf_recommender::score_content(_, u, features([]), _).

	test(tfidf_recommender_content_invalid_model, error(domain_error(recommender, bad))) :-
		tfidf_recommender::score_content(bad, u, features([]), _).

	test(tfidf_recommender_content_variable_user, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, _, vector([]), _).

	test(tfidf_recommender_content_variable_descriptor, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, _, _).

	test(tfidf_recommender_content_bad_descriptor, error(domain_error(item_content, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, bad, _).

	test(tfidf_recommender_content_feature_model_kind_mismatch, error(domain_error(content_representation, vector([])))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([a])]), Model),
		tfidf_recommender::score_content(Model, u, vector([]), _).

	test(tfidf_recommender_content_nonground_feature, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([a])]), Model),
		tfidf_recommender::score_content(Model, u, features([genre(_)]), _).

	test(tfidf_recommender_content_nonlist, error(type_error(list, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, vector(bad), _).

	test(tfidf_recommender_content_duplicate_key, error(domain_error(duplicate_feature, x))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, vector([x-1,x-0]), _).

	test(tfidf_recommender_content_negative_weight, error(domain_error(non_negative_finite_weight, -1))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, unknown, vector([x- -1]), _).

	test(tfidf_recommender_content_variable_weight, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, vector([x-_]), _).

	test(tfidf_recommender_content_nonnumeric_weight, error(type_error(number, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, vector([x-bad]), _).

	test(tfidf_recommender_content_nonpair, error(type_error(pair, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score_content(Model, u, vector([bad]), _).

	test(tfidf_recommender_update_rating_replacement, deterministic) :-
		vector_dataset(3, 3, tfidf_dataset(Ratings, Items, Contents)),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::update_ratings(Model, [rating(u,x,5)], Updated),
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,5),rating(u,y,3)], Items, Contents), Fresh),
		assertion(Updated == Fresh),
		tfidf_recommender::score(Updated, u, x, Score),
		assertion(Score =~= 1.0),
		assertion(tfidf_recommender::valid_recommender(Updated)).

	test(tfidf_recommender_update_rating_insertion, deterministic) :-
		vector_dataset(3, 3, tfidf_dataset(Ratings, Items, Contents)),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::update_ratings(Model, [rating(v,diagonal,5)], Updated),
		tfidf_recommender::learn(tfidf_dataset([rating(v,diagonal,5)|Ratings], Items, Contents), Fresh),
		assertion(Updated == Fresh),
		tfidf_recommender::score(Updated, v, diagonal, Score),
		assertion(Score =~= 1.0).

	test(tfidf_recommender_update_empty, deterministic(Updated == Model)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [], Updated).

	test(tfidf_recommender_update_duplicate, error(domain_error(duplicate_rating, u-x))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(u,x,1),rating(u,x,5)], _).

	test(tfidf_recommender_update_weighted_mean_shift, error(domain_error(positive_rating_weight, 0))) :-
		Dataset = tfidf_dataset([rating(u,x,1),rating(u,y,0)], [x,y], [x-vector([a-1]),y-vector([])]),
		tfidf_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		tfidf_recommender::update_ratings(Model, [rating(u,x,-1)], _).

	test(tfidf_recommender_remove_rating_matches_fresh, deterministic) :-
		vector_dataset(3, 3, tfidf_dataset(Ratings, Items, Contents)),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::remove_ratings(Model, [u-x,u-x,missing-item], Updated),
		tfidf_recommender::learn(tfidf_dataset([rating(u,y,3)], Items, Contents), Fresh),
		assertion(Updated == Fresh),
		tfidf_recommender::score(Updated, u, x, Score),
		assertion(Score =~= 0.0),
		assertion(tfidf_recommender::valid_recommender(Updated)),
		tfidf_recommender::remove_ratings(Updated, [u-x], Retry),
		assertion(Retry == Updated).

	test(tfidf_recommender_remove_unknown_pairs, deterministic(Updated == Model)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [unknown-x,u-missing,u-diagonal], Updated).

	test(tfidf_recommender_remove_empty, deterministic(Updated == Model)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [], Updated).

	test(tfidf_recommender_remove_all, error(domain_error(non_empty_ratings, []))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [u-x,u-y], _).

	test(tfidf_recommender_remove_weighted_mean_shift, error(domain_error(positive_rating_weight, 0))) :-
		Dataset = tfidf_dataset([rating(u,x,1),rating(u,y,0),rating(v,x,1)], [x,y], [x-vector([a-1]),y-vector([])]),
		tfidf_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		tfidf_recommender::remove_ratings(Model, [u-x], _).

	test(tfidf_recommender_feedback_feature_equivalence, deterministic) :-
		feature_dataset(tfidf_dataset(Ratings, Items, Contents)),
		forall(
			(	member(Normalization, [none,l2]),
				member(Weighting, [uniform,rating])
			),
			(	Options = [normalization(Normalization),profile_weighting(Weighting),positive_threshold(1)],
				tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model, Options),
				tfidf_recommender::update_ratings(Model, [rating(u,first,2),rating(u,second,4),rating(v,second,3)], Updated),
				tfidf_recommender::learn(tfidf_dataset([rating(u,first,2),rating(u,second,4),rating(v,second,3)], Items, Contents), Fresh, Options),
				assertion(Updated == Fresh),
				tfidf_recommender::remove_ratings(Updated, [u-first], Removed),
				tfidf_recommender::learn(tfidf_dataset([rating(u,second,4),rating(v,second,3)], Items, Contents), Remaining, Options),
				assertion(Removed == Remaining),
				Model = tfidf_model(_,OriginalContents,Vectors,_,Vectorizer,Scale,Diagnostics),
				Removed = tfidf_model(_,OriginalContents,Vectors,_,Vectorizer,Scale,RemovedDiagnostics),
				memberchk(options(Effective), Diagnostics),
				assertion(memberchk(options(Effective), RemovedDiagnostics)),
				assertion(tfidf_recommender::valid_recommender(Removed))
			)
		).

	test(tfidf_recommender_feedback_weighted_vectors, deterministic) :-
		vector_dataset(2, 4, tfidf_dataset(Ratings, Items, Contents)),
		Options = [profile_weighting(rating),positive_threshold(0),normalization(none)],
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model, Options),
		tfidf_recommender::update_ratings(Model, [rating(u,x,4),rating(u,y,2)], Updated),
		tfidf_recommender::score(Updated, u, x, Score),
		Expected is 2 / sqrt(5),
		assertion(Score =~= Expected),
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,4),rating(u,y,2)], Items, Contents), Fresh, Options),
		assertion(Updated == Fresh),
		tfidf_recommender::remove_ratings(Updated, [u-y], Removed),
		tfidf_recommender::score_content(Removed, u, vector([x-3]), Single),
		assertion(Single =~= 1.0).

	test(tfidf_recommender_feedback_mean_reselects_unchanged_items, deterministic) :-
		vector_dataset(1, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(u,diagonal,-10)], Updated),
		tfidf_recommender::score(Updated, u, x, Score),
		Expected is 1 / sqrt(2),
		assertion(Score =~= Expected),
		tfidf_recommender::remove_ratings(Updated, [u-diagonal], Removed),
		assertion(Removed == Model).

	test(tfidf_recommender_feedback_empty_selected_vectors, deterministic) :-
		Contents = [x-vector([a-1]),y-vector([])],
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,2)], [x,y], Contents), Model, [positive_threshold(1),profile_weighting(rating)]),
		tfidf_recommender::update_ratings(Model, [rating(u,y,2)], Updated),
		Updated = tfidf_model(_,_,_,[u-[a-Weight]],_,_,_),
		assertion(Weight =~= 0.5),
		tfidf_recommender::remove_ratings(Updated, [u-y], Removed),
		assertion(Removed == Model).

	test(tfidf_recommender_feedback_threshold_equality_and_unselected_nonpositive, deterministic) :-
		vector_dataset(2, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(2),profile_weighting(rating)]),
		tfidf_recommender::update_ratings(Model, [rating(u,y,0),rating(v,y,-1)], Updated),
		tfidf_recommender::score(Updated, u, x, Score),
		assertion(Score =~= 1.0),
		tfidf_recommender::score(Updated, v, y, Empty),
		assertion(Empty =~= 0.0),
		assertion(tfidf_recommender::valid_recommender(Updated)).

	test(tfidf_recommender_feedback_identity_and_order, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		tfidf_recommender::update_ratings(Model, [rating(u,x,3)], Identical),
		assertion(Identical == Model),
		Updates = [rating(v,x,2),rating(u,x,5),rating(u,diagonal,1)],
		reverse(Updates, Reversed),
		tfidf_recommender::update_ratings(Model, Updates, First),
		tfidf_recommender::update_ratings(Model, Reversed, Second),
		assertion(First == Second),
		tfidf_recommender::remove_ratings(First, [u-x,v-x], Removed),
		tfidf_recommender::remove_ratings(Second, [v-x,u-x,v-x], Other),
		assertion(Removed == Other),
		assertion(lgtunit::variant(Model, Original)),
		assertion(ground(Removed)).

	test(tfidf_recommender_feedback_diagnostic_order_and_extras, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Diagnostics)),
		reverse(Diagnostics, Reversed),
		append([note(before)|Reversed], [extra(after)], WithExtras),
		Model = tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,WithExtras),
		tfidf_recommender::update_ratings(Model, [rating(v,x,2)], Updated),
		tfidf_recommender::remove_ratings(Updated, [v-x], Restored),
		assertion(Restored == Model),
		assertion(tfidf_recommender::valid_recommender(Updated)),
		Updated = tfidf_model(_,_,_,_,_,_,UpdatedDiagnostics),
		assertion(memberchk(rating_count(3), UpdatedDiagnostics)),
		assertion(memberchk(user_count(2), UpdatedDiagnostics)),
		assertion(memberchk(non_empty_profile_count(2), UpdatedDiagnostics)),
		assertion(UpdatedDiagnostics = [note(before)|_]),
		assertion(append(_, [extra(after)], UpdatedDiagnostics)).

	test(tfidf_recommender_feedback_scale_boundaries, deterministic) :-
		tfidf_recommender::learn(tfidf_scale_fixture(1,5), Model),
		tfidf_recommender::update_ratings(Model, [rating(u,x,5),rating(v,x,1)], Updated),
		tfidf_recommender::remove_ratings(Updated, [u-x], Removed),
		assertion(Removed = tfidf_model([rating(v,x,1)],_,_,_,_,scale(1,5),_)),
		assertion(tfidf_recommender::valid_recommender(Removed)).

	test(tfidf_recommender_feedback_disappearing_user, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(v,x,2)], Updated),
		tfidf_recommender::remove_ratings(Updated, [u-y,u-x], Removed),
		Removed = tfidf_model(_,_,_,Profiles,_,_,Diagnostics),
		assertion(\+ member(u-_, Profiles)),
		assertion(memberchk(user_count(1), Diagnostics)),
		tfidf_recommender::score(Removed, u, x, Score),
		assertion(Score =~= 0.0),
		tfidf_recommender::recommend(Removed, u, 3, Full),
		assertion(Full == [y-0.0,x-0.0,diagonal-0.0]),
		tfidf_recommender::recommend(Removed, v, 3, Other),
		assertion(\+ member(x-_, Other)).

	test(tfidf_recommender_feedback_withdrawn_item_scores_normally, deterministic) :-
		Contents = [x-vector([a-1,b-1]),y-vector([a-1])],
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,3),rating(u,y,3)], [x,y], Contents), Model),
		tfidf_recommender::remove_ratings(Model, [u-x], Removed),
		tfidf_recommender::recommend(Removed, u, 2, [x-Score]),
		Expected is 1 / sqrt(2),
		assertion(Score =~= Expected),
		tfidf_recommender::score_all(Removed, u, [x], [x-Batch]),
		assertion(Batch =~= Score),
		tfidf_recommender::update_ratings(Removed, [rating(u,x,3)], Restored),
		assertion(Restored == Model).

	test(tfidf_recommender_feedback_checks_model_once, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_validation_counter::learn(Dataset, Model),
		forall(
			member(Goal, [update_ratings(Model,[],_),update_ratings(Model,[rating(v,x,2)],_),remove_ratings(Model,[],_),remove_ratings(Model,[u-x],_),remove_ratings(Model,[unknown-missing],_)]),
			(	tfidf_validation_counter::reset_validation_count,
				tfidf_validation_counter::Goal,
				tfidf_validation_counter::validation_count(Count),
				assertion(Count == 1)
			)
		).

	test(tfidf_recommender_feedback_failed_rebuild_preserves_original, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,1),rating(u,y,0),rating(v,x,1)], [x,y], [x-vector([a-1]),y-vector([])]),
		tfidf_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		copy_term(Model, Original),
		catch(tfidf_recommender::remove_ratings(Model, [u-x], _), error(domain_error(positive_rating_weight,0),_), Caught = yes),
		assertion(Caught == yes),
		assertion(lgtunit::variant(Model, Original)),
		assertion(tfidf_recommender::valid_recommender(Model)).

	test(tfidf_recommender_feedback_implemented_locally, deterministic) :-
		tfidf_recommender::predicate_property(update_ratings(_,_,_), defined_in(tfidf_recommender)),
		tfidf_recommender::predicate_property(remove_ratings(_,_,_), defined_in(tfidf_recommender)).

	test(tfidf_recommender_update_missing_item, error(domain_error(catalog_item, missing))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(v,missing,5)], _).

	test(tfidf_recommender_update_identical_duplicates, error(domain_error(duplicate_rating, v-x))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(v,x,5),rating(v,x,5)], _).

	test(tfidf_recommender_update_out_of_scale, error(domain_error(rating_scale(1,5), 6))) :-
		tfidf_recommender::learn(tfidf_scale_fixture(1,5), Model),
		tfidf_recommender::update_ratings(Model, [rating(u,x,6)], _).

	test(tfidf_recommender_update_below_scale, error(domain_error(rating_scale(1,5), 0))) :-
		tfidf_recommender::learn(tfidf_scale_fixture(1,5), Model),
		tfidf_recommender::update_ratings(Model, [rating(v,x,0)], _).

	test(tfidf_recommender_update_variable_model, error(instantiation_error)) :-
		tfidf_recommender::update_ratings(_, [], _).

	test(tfidf_recommender_update_invalid_model, error(domain_error(recommender, bad))) :-
		tfidf_recommender::update_ratings(bad, [], _).

	test(tfidf_recommender_update_variable_list, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, _, _).

	test(tfidf_recommender_update_open_list, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(v,x,2)|_], _).

	test(tfidf_recommender_update_nonlist, error(type_error(list, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, bad, _).

	test(tfidf_recommender_update_improper_list, error(type_error(list, [rating(v,x,2)|bad]))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(v,x,2)|bad], _).

	test(tfidf_recommender_update_bad_record, error(type_error(rating, u-x))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [u-x], _).

	test(tfidf_recommender_update_variable_record, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [_], _).

	test(tfidf_recommender_update_variable_user, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(_,x,2)], _).

	test(tfidf_recommender_update_variable_item, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(u,_,2)], _).

	test(tfidf_recommender_update_nonatomic_user, error(type_error(atomic, user(u)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(user(u),x,2)], _).

	test(tfidf_recommender_update_nonatomic_item, error(type_error(atomic, item(x)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(u,item(x),2)], _).

	test(tfidf_recommender_update_variable_value, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(u,x,_)], _).

	test(tfidf_recommender_update_nonnumeric_value, error(type_error(number, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::update_ratings(Model, [rating(u,x,bad)], _).

	test(tfidf_recommender_remove_variable_model, error(instantiation_error)) :-
		tfidf_recommender::remove_ratings(_, [], _).

	test(tfidf_recommender_remove_invalid_model, error(domain_error(recommender, bad))) :-
		tfidf_recommender::remove_ratings(bad, [], _).

	test(tfidf_recommender_remove_variable_list, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, _, _).

	test(tfidf_recommender_remove_nonlist, error(type_error(list, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, bad, _).

	test(tfidf_recommender_remove_open_list, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [u-x|_], _).

	test(tfidf_recommender_remove_improper_list, error(type_error(list, [u-x|bad]))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [u-x|bad], _).

	test(tfidf_recommender_remove_nonpair, error(type_error(pair, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [u-x,bad], _).

	test(tfidf_recommender_remove_variable_pair, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [_], _).

	test(tfidf_recommender_remove_variable_user, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [_-missing], _).

	test(tfidf_recommender_remove_variable_item, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [missing-_], _).

	test(tfidf_recommender_remove_nonatomic_user, error(type_error(atomic, user(u)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [user(u)-missing], _).

	test(tfidf_recommender_remove_nonatomic_item, error(type_error(atomic, item(x)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::remove_ratings(Model, [missing-item(x)], _).

	test(tfidf_recommender_extend_features_refits, deterministic) :-
		feature_dataset(tfidf_dataset(Ratings, Items, Contents)),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::score(Model, u, second, Before),
		tfidf_recommender::extend_catalog(Model, [third-features([a,d])], Extended),
		append(Items, [third], AllItems),
		append(Contents, [third-features([a,d])], AllContents),
		tfidf_recommender::learn(tfidf_dataset(Ratings, AllItems, AllContents), Fresh),
		assertion(Extended == Fresh),
		assertion(tfidf_recommender::valid_recommender(Extended)),
		tfidf_recommender::score(Extended, u, second, After),
		assertion(Before =\= After),
		tfidf_recommender::recommend(Extended, u, 3, Recommendations),
		assertion(member(third-_, Recommendations)).

	test(tfidf_recommender_extend_vectors_preserves_profiles, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::extend_catalog(Model, [novel-vector([x-0.5,y-0.5,z-0])], Extended),
		Model = tfidf_model(Ratings,_,_,Profiles,Vectorizer,Scale,_),
		Extended = tfidf_model(Ratings,_,_,Profiles,Vectorizer,Scale,_),
		tfidf_recommender::score(Extended, u, novel, Score),
		assertion(Score =~= 1.0),
		assertion(tfidf_recommender::valid_recommender(Extended)).

	test(tfidf_recommender_extend_empty, deterministic(Extended == Model)) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::extend_catalog(Model, [], Extended).

	test(tfidf_recommender_extend_existing_id, error(domain_error(new_catalog_item, first))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::extend_catalog(Model, [first-features([a])], _).

	test(tfidf_recommender_replace_features_refits_unrated_content, deterministic) :-
		feature_dataset(tfidf_dataset(Ratings, Items, Contents)),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::replace_content(Model, [second-features([a,d])], Replaced),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, [first-features([a,a,b]),second-features([a,d])]), Fresh),
		assertion(Replaced == Fresh),
		Model = tfidf_model(_,_,_,OriginalProfiles,_,_,_),
		Replaced = tfidf_model(_,_,_,UpdatedProfiles,_,_,_),
		assertion(UpdatedProfiles \== OriginalProfiles),
		assertion(tfidf_recommender::valid_recommender(Replaced)).

	test(tfidf_recommender_replace_rated_vectors_rebuilds, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::replace_content(Model, [x-vector([y-0.25])], Replaced),
		tfidf_recommender::score(Replaced, u, y, Score),
		assertion(Score =~= 1.0),
		assertion(tfidf_recommender::valid_recommender(Replaced)).

	test(tfidf_recommender_replace_unrated_vectors_preserves_profiles, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::replace_content(Model, [diagonal-vector([novel-2])], Replaced),
		Model = tfidf_model(Ratings,_,_,Profiles,Vectorizer,Scale,_),
		Replaced = tfidf_model(Ratings,_,_,Profiles,Vectorizer,Scale,_),
		tfidf_recommender::score(Replaced, u, diagonal, Score),
		assertion(Score =~= 0.0).

	test(tfidf_recommender_replace_empty, deterministic(Replaced == Model)) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::replace_content(Model, [], Replaced).

	test(tfidf_recommender_replace_missing_id, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::replace_content(Model, [missing-features([a])], _).

	test(tfidf_recommender_catalog_feature_options_equivalence, deterministic) :-
		feature_dataset(tfidf_dataset(Ratings, Items, Contents)),
		forall(
			(	member(Weighting, [binary,count,term_frequency,tf_idf(raw),tf_idf(relative),tf_idf(sublinear)]),
				member(IDF, [smooth,classic]),
				member(Normalization, [none,l2]),
				member(ProfileWeighting, [uniform,rating])
			),
			(	Options = [normalization(Normalization),profile_weighting(ProfileWeighting),vectorizer_options([weighting(Weighting),idf(IDF)])],
				tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model, Options),
				tfidf_recommender::score(Model, u, first, Catalog),
				tfidf_recommender::score_content(Model, u, features([b,a,a,unseen,unseen]), Supplied),
				assertion(Catalog =~= Supplied),
				tfidf_recommender::extend_catalog(Model, [third-features([c,c,tag(new)])], Extended),
				tfidf_recommender::learn(tfidf_dataset(Ratings, [third|Items], [third-features([c,c,tag(new)])|Contents]), Fresh, Options),
				assertion(Extended == Fresh),
				tfidf_recommender::replace_content(Extended, [first-features([tag(new),a,a])], Replaced),
				tfidf_recommender::learn(tfidf_dataset(Ratings, [first,second,third], [first-features([tag(new),a,a]),second-features([b,c]),third-features([c,c,tag(new)])]), Replacement, Options),
				assertion(Replaced == Replacement),
				assertion(tfidf_recommender::valid_recommender(Replaced))
			)
		).

	test(tfidf_recommender_catalog_exact_idf_score_changes, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, second, Before),
		Rare is log(3 / 2) + 1,
		ExpectedBefore is 1 / (sqrt(4 * Rare * Rare + 1) * sqrt(1 + Rare * Rare)),
		assertion(Before =~= ExpectedBefore),
		tfidf_recommender::extend_catalog(Model, [third-features([a,d])], Extended),
		tfidf_recommender::score(Extended, u, second, After),
		Common is log(4 / 3) + 1,
		NewRare is log(2) + 1,
		ExpectedAfter is Common / (sqrt(5) * sqrt(Common * Common + NewRare * NewRare)),
		assertion(After =~= ExpectedAfter),
		tfidf_recommender::replace_content(Model, [second-features([a,d])], Replaced),
		tfidf_recommender::score(Replaced, u, second, Replacement),
		ExpectedReplacement is 2 / (sqrt(4 + Rare * Rare) * sqrt(1 + Rare * Rare)),
		assertion(Replacement =~= ExpectedReplacement).

	test(tfidf_recommender_catalog_empty_document_refits_statistics, deterministic) :-
		feature_dataset(tfidf_dataset(Ratings, Items, Contents)),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::extend_catalog(Model, [empty-features([])], Extended),
		tfidf_recommender::learn(tfidf_dataset(Ratings, [empty|Items], [empty-features([])|Contents]), Fresh),
		assertion(Extended == Fresh),
		Model = tfidf_model(_,_,_,_,OriginalVectorizer,_,_),
		Extended = tfidf_model(_,_,_,_,NewVectorizer,_,_),
		assertion(NewVectorizer \== OriginalVectorizer),
		tfidf_recommender::score(Extended, u, empty, Score),
		assertion(Score =~= 0.0).

	test(tfidf_recommender_catalog_recomputes_frequency_filters, deterministic) :-
		feature_dataset(tfidf_dataset(Ratings, Items, Contents)),
		Options = [vectorizer_options([minimum_document_frequency(2),maximum_document_frequency(2),maximum_features(2)])],
		once(tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model, Options)),
		Model = tfidf_model(_,_,_,_,OldVectorizer,_,_),
		text_vectorizer::vocabulary(OldVectorizer, OldVocabulary),
		assertion(OldVocabulary == [b]),
		tfidf_recommender::extend_catalog(Model, [third-features([a,a,d])], Extended),
		Extended = tfidf_model(_,_,_,_,NewVectorizer,_,Diagnostics),
		text_vectorizer::vocabulary(NewVectorizer, NewVocabulary),
		assertion(NewVocabulary == [a,b]),
		assertion(memberchk(feature_count(2), Diagnostics)),
		tfidf_recommender::learn(tfidf_dataset(Ratings, [third|Items], [third-features([a,a,d])|Contents]), Fresh, Options),
		assertion(Extended == Fresh).

	test(tfidf_recommender_catalog_recomputes_vocabulary_limit, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([maximum_features(1)])]),
		tfidf_recommender::extend_catalog(Model, [third-features([c,c,c])], Extended),
		Extended = tfidf_model(_,_,_,Profiles,Vectorizer,_,Diagnostics),
		text_vectorizer::vocabulary(Vectorizer, Vocabulary),
		assertion(Vocabulary == [c]),
		assertion(Profiles == [u-[]]),
		assertion(memberchk(non_empty_profile_count(0), Diagnostics)),
		assertion(tfidf_recommender::valid_recommender(Extended)).

	test(tfidf_recommender_catalog_classic_zero_weight_feature_count, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,1)], [x], [x-features([a])]),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([idf(classic)])]),
		tfidf_recommender::extend_catalog(Model, [y-features([a])], Extended),
		Extended = tfidf_model(_,_,Vectors,Profiles,_,_,Diagnostics),
		assertion(Vectors == [x-[],y-[]]),
		assertion(Profiles == [u-[]]),
		assertion(memberchk(feature_count(1), Diagnostics)),
		assertion(memberchk(item_count(2), Diagnostics)),
		assertion(tfidf_recommender::valid_recommender(Extended)).

	test(tfidf_recommender_catalog_preweighted_equivalence, deterministic) :-
		vector_dataset(3, 3, tfidf_dataset(Ratings, Items, Contents)),
		forall(
			member(Normalization, [none,l2]),
			(	Options = [normalization(Normalization)],
				tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model, Options),
				tfidf_recommender::extend_catalog(Model, [novel-vector([tag(new)-2,x-0.25,zero-0])], Extended),
				tfidf_recommender::learn(tfidf_dataset(Ratings, [novel|Items], [novel-vector([tag(new)-2,x-0.25,zero-0])|Contents]), Fresh, Options),
				assertion(Extended == Fresh),
				tfidf_recommender::replace_content(Extended, [x-vector([y-0.25,zero-0]),diagonal-vector([])], Replaced),
				tfidf_recommender::learn(tfidf_dataset(Ratings, [diagonal,novel,x,y], [diagonal-vector([]),novel-vector([tag(new)-2,x-0.25]),x-vector([y-0.25]),y-vector([y-1])]), Replacement, Options),
				assertion(Replaced == Replacement),
				assertion(tfidf_recommender::valid_recommender(Replaced))
			)
		).

	test(tfidf_recommender_catalog_empty_preweighted_space, deterministic) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-vector([])]), Model),
		tfidf_recommender::extend_catalog(Model, [y-vector([zero-0])], Extended),
		tfidf_recommender::replace_content(Extended, [x-vector([]),y-vector([])], Replaced),
		assertion(Replaced == Extended),
		Replaced = tfidf_model(_,_,_,_,none,_,Diagnostics),
		assertion(memberchk(feature_count(0), Diagnostics)),
		assertion(tfidf_recommender::valid_recommender(Replaced)).

	test(tfidf_recommender_catalog_immutable_and_order_invariant, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		tfidf_recommender::extend_catalog(Model, [third-features([a]),fourth-features([b])], Combined),
		tfidf_recommender::extend_catalog(Model, [fourth-features([b])], First),
		tfidf_recommender::extend_catalog(First, [third-features([a])], Sequential),
		assertion(Combined == Sequential),
		tfidf_recommender::replace_content(Combined, [first-features([a]),third-features([c])], Replaced),
		tfidf_recommender::replace_content(Sequential, [third-features([c]),first-features([a])], Other),
		assertion(Replaced == Other),
		assertion(lgtunit::variant(Model, Original)),
		assertion(ground(Replaced)).

	test(tfidf_recommender_catalog_identical_canonical_replacement, deterministic(Replaced == Model)) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::replace_content(Model, [first-features([b,a,a])], Replaced).

	test(tfidf_recommender_catalog_extra_diagnostics_and_repeated_options, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Diagnostics), [normalization(none),normalization(l2)]),
		reverse(Diagnostics, Reversed),
		append([note(before)|Reversed], [extra(after)], WithExtras),
		Model = tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,WithExtras),
		tfidf_recommender::extend_catalog(Model, [novel-vector([extra-4])], Extended),
		tfidf_recommender::replace_content(Extended, [novel-vector([y-0.25])], Replaced),
		Replaced = tfidf_model(Ratings,_,_,Profiles,Vectorizer,Scale,UpdatedDiagnostics),
		assertion(memberchk(item_count(4), UpdatedDiagnostics)),
		assertion(memberchk(feature_count(2), UpdatedDiagnostics)),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(options(Options), UpdatedDiagnostics)),
		assertion(UpdatedDiagnostics = [note(before)|_]),
		assertion(append(_, [extra(after)], UpdatedDiagnostics)),
		assertion(tfidf_recommender::valid_recommender(Replaced)).

	test(tfidf_recommender_catalog_scale_preservation, deterministic) :-
		tfidf_recommender::learn(tfidf_scale_fixture(1,5), Model),
		tfidf_recommender::extend_catalog(Model, [y-vector([a-3])], Extended),
		tfidf_recommender::replace_content(Extended, [x-vector([a-0.5])], Replaced),
		assertion(Replaced = tfidf_model([rating(u,x,1)],_,_,_,none,scale(1,5),_)),
		assertion(tfidf_recommender::valid_recommender(Replaced)).

	test(tfidf_recommender_catalog_validates_once, deterministic) :-
		feature_dataset(Dataset),
		tfidf_validation_counter::learn(Dataset, Model),
		forall(
			member(Goal, [extend_catalog(Model,[],_),extend_catalog(Model,[third-features([a])],_),replace_content(Model,[],_),replace_content(Model,[first-features([b])],_)]),
			(	tfidf_validation_counter::reset_validation_count,
				tfidf_validation_counter::Goal,
				tfidf_validation_counter::validation_count(Count),
				assertion(Count == 1)
			)
		).

	test(tfidf_recommender_catalog_implemented_locally, deterministic) :-
		tfidf_recommender::predicate_property(extend_catalog(_,_,_), defined_in(tfidf_recommender)),
		tfidf_recommender::predicate_property(replace_content(_,_,_), defined_in(tfidf_recommender)).

	test(tfidf_recommender_catalog_inputs_validated, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		forall(
			(	member(Method, [extend_catalog,replace_content]),
				member(Input-Expected, [
					_-instantiation_error,
					bad-type_error(list,bad),
					[_]-instantiation_error,
					[_|_]-instantiation_error,
					[first-features([a])|bad]-type_error(list,[first-features([a])|bad]),
					[bad]-type_error(pair,bad),
					[_-features([a])]-instantiation_error,
					[item(first)-features([a])]-type_error(atomic,item(first))
				])
			),
			(	Goal =.. [Method, Model, Input, _],
				catch(tfidf_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(tfidf_recommender_extend_content_errors, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		forall(
			member(Input-Expected, [
				[third-features([a]),third-features([b])]-domain_error(duplicate_item,third),
				[third-bad]-domain_error(item_content,bad),
				[third-_]-instantiation_error,
				[third-features([_])]-instantiation_error,
				[third-features(bad)]-type_error(list,bad),
				[third-vector([])]-domain_error(content_representation,vector([])),
				[third-features([a]),fourth-vector([])]-domain_error(content_representation,features([a]))
			]),
			(	catch(tfidf_recommender::extend_catalog(Model, Input, _), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(tfidf_recommender_replace_content_errors, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		forall(
			member(Input-Expected, [
				[first-features([a]),first-features([b])]-domain_error(duplicate_item,first),
				[first-bad]-domain_error(item_content,bad),
				[first-_]-instantiation_error,
				[first-features([_])]-instantiation_error,
				[first-features(bad)]-type_error(list,bad),
				[first-vector([])]-domain_error(content_representation,vector([])),
				[first-features([a]),second-vector([])]-domain_error(content_representation,vector([]))
			]),
			(	catch(tfidf_recommender::replace_content(Model, Input, _), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(tfidf_recommender_catalog_vector_validation, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		forall(
			(	member(Method-Item, [extend_catalog-novel,replace_content-x]),
				member(Content-Expected, [
					features([])-domain_error(content_representation,features([])),
					vector([a-1,a-0])-domain_error(duplicate_feature,a),
					vector([a- -1])-domain_error(non_negative_finite_weight,-1),
					vector([a-bad])-type_error(number,bad),
					vector([a-_])-instantiation_error,
					vector([tag(_)-1])-instantiation_error,
					vector([bad])-type_error(pair,bad)
				])
			),
			(	Goal =.. [Method, Model, [Item-Content], _],
				catch(tfidf_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(tfidf_recommender_catalog_empty_inputs_validate_model, deterministic) :-
		forall(
			member(Goal-Expected, [
				extend_catalog(_,[],_)-instantiation_error,
				replace_content(_,[],_)-instantiation_error,
				extend_catalog(bad,[],_)-domain_error(recommender,bad),
				replace_content(bad,[],_)-domain_error(recommender,bad)
			]),
			(	catch(tfidf_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(tfidf_recommender_extend_empty_vocabulary_error, error(domain_error(non_empty_vocabulary, _))) :-
		Dataset = tfidf_dataset([rating(u,x,1)], [x], [x-features([a])]),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([maximum_document_frequency(1)])]),
		tfidf_recommender::extend_catalog(Model, [y-features([a])], _).

	test(tfidf_recommender_replace_empty_vocabulary_error, error(domain_error(non_empty_vocabulary, _))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([minimum_document_frequency(2)])]),
		tfidf_recommender::replace_content(Model, [second-features([c])], _).

	test(tfidf_recommender_replace_all_empty_features_error, error(domain_error(non_empty_vocabulary, _))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::replace_content(Model, [first-features([]),second-features([])], _).

	test(tfidf_recommender_extended_item_absent_from_original, error(domain_error(catalog_item, third))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::extend_catalog(Model, [third-features([a])], _),
		tfidf_recommender::score(Model, u, third, _).

	test(tfidf_recommender_composed_updates_export_restore, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::extend_catalog(Model, [third-features([a,d])], Extended),
		tfidf_recommender::update_ratings(Extended, [rating(u,first,1),rating(u,third,5),rating(v,second,4)], Rated),
		tfidf_recommender::replace_content(Rated, [second-features([a,d,d])], Replaced),
		tfidf_recommender::remove_ratings(Replaced, [u-third,u-third,missing-item], Final),
		Contents = [first-features([a,a,b]),second-features([a,d,d]),third-features([a,d])],
		tfidf_recommender::learn(tfidf_dataset([rating(u,first,1),rating(v,second,4)], [first,second,third], Contents), Fresh),
		assertion(Final == Fresh),
		tfidf_recommender::export_to_clauses(Dataset, Final, updated, Clauses),
		assertion(Clauses == [updated(Final)]),
		^^file_path('tfidf_updated.pl', File),
		tfidf_recommender::export_to_file(Dataset, Final, tfidf_updated, File),
		logtalk_load(File),
		{tfidf_updated(Loaded)},
		assertion(Loaded == Final),
		assertion(tfidf_recommender::valid_recommender(Loaded)),
		tfidf_recommender::score_all(Loaded, u, [second,third,second], [second-Batch,third-_,second-Repeat]),
		tfidf_recommender::score(Loaded, u, second, Single),
		tfidf_recommender::score_content(Loaded, u, features([d,d,a]), Supplied),
		assertion(Batch =~= Single),
		assertion(Repeat =~= Single),
		assertion(Supplied =~= Single),
		tfidf_recommender::recommend(Loaded, u, 3, Recommendations),
		assertion(member(second-Single, Recommendations)),
		assertion(member(third-_, Recommendations)),
		assertion(\+ member(first-_, Recommendations)).

	test(tfidf_recommender_catalog_filtered_call_determinism, deterministic) :-
		feature_dataset(Dataset),
		forall(
			member(VectorOptions, [[minimum_document_frequency(2),maximum_document_frequency(2),maximum_features(2)],[maximum_features(1)],[weighting(tf_idf(relative)),idf(classic)]]),
			(	once(tfidf_recommender::learn(Dataset, Model, [vectorizer_options(VectorOptions)])),
				lgtunit::deterministic(tfidf_recommender::extend_catalog(Model, [third-features([a,a,d])], _), Extension),
				assertion(Extension == true),
				lgtunit::deterministic(tfidf_recommender::replace_content(Model, [first-features([b,a,a])], _), Replacement),
				assertion(Replacement == true)
			)
		).

	test(tfidf_recommender_uniform_centroid, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, x, Score),
		Expected is 1 / sqrt(2),
		assertion(Score =~= Expected),
		tfidf_recommender::score(Model, u, diagonal, Diagonal),
		assertion(Diagonal =~= 1.0).

	test(tfidf_recommender_weighted_centroid, deterministic) :-
		vector_dataset(2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(2), profile_weighting(rating)]),
		tfidf_recommender::score(Model, u, x, ScoreX),
		tfidf_recommender::score(Model, u, y, ScoreY),
		ExpectedX is 1 / sqrt(5), ExpectedY is 2 / sqrt(5),
		assertion(ScoreX =~= ExpectedX),
		assertion(ScoreY =~= ExpectedY).

	test(tfidf_recommender_tfidf_weights, deterministic) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first, second],
			[first-features([a,a,b]), second-features([b,c])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, [first-[a-FirstA,b-FirstB], second-[b-SecondB,c-SecondC]], _, _, _, _),
			[normalization(none)]),
		IDF is log(3 / 2) + 1, ExpectedA is 2 * IDF,
		assertion(FirstA =~= ExpectedA),
		assertion(FirstB =~= 1.0),
		assertion(SecondB =~= 1.0),
		assertion(SecondC =~= IDF).

	test(tfidf_recommender_unrated_catalog_recommendation, deterministic(Score =~= 1.0)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, u, 10, [diagonal-Score]).

	test(tfidf_recommender_default_positive_threshold, deterministic(Score =~= 0.0)) :-
		vector_dataset(2, 4, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, x, Score).

	test(tfidf_recommender_per_user_mean_thresholds, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,1),rating(u,y,2),rating(v,x,5),rating(v,y,4)],
			[x,y], [x-vector([x-1]),y-vector([y-1])]),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, y, UserScore),
		tfidf_recommender::score(Model, v, x, OtherScore),
		tfidf_recommender::score(Model, u, x, RejectedScore),
		assertion(UserScore =~= 1.0),
		assertion(OtherScore =~= 1.0),
		assertion(RejectedScore =~= 0.0).

	test(tfidf_recommender_threshold_equality, deterministic(Score =~= 1.0)) :-
		vector_dataset(2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(4)]),
		tfidf_recommender::score(Model, u, y, Score).

	test(tfidf_recommender_no_selected_items, deterministic) :-
		vector_dataset(2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(5)]),
		tfidf_recommender::score(Model, u, diagonal, Score),
		assertion(Score =~= 0.0),
		tfidf_recommender::recommend(Model, u, 3, [diagonal-Zero]),
		assertion(Zero =~= 0.0).

	test(tfidf_recommender_unknown_user_zero_ties, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, unknown, 10, [y-ScoreY,x-ScoreX,diagonal-ScoreDiagonal]),
		assertion(ScoreY =~= 0.0),
		assertion(ScoreX =~= 0.0),
		assertion(ScoreDiagonal =~= 0.0).

	test(tfidf_recommender_no_candidates, deterministic(Recommendations == [])) :-
		Dataset = tfidf_dataset([rating(u, x, 5)], [x], [x-vector([x-1])]),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, u, 5, Recommendations).

	test(tfidf_recommender_unknown_item, error(domain_error(catalog_item, missing))) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, missing, _).

	test(tfidf_recommender_empty_content, deterministic(Score =~= 0.0)) :-
		Dataset = tfidf_dataset([rating(u, x, 5)], [x,y], [x-vector([]),y-vector([])]),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, u, y, Score).

	test(tfidf_recommender_empty_selected_vector_denominator, deterministic) :-
		Dataset = tfidf_dataset([rating(u, x, 5),rating(u, y, 5)], [x,y], [x-vector([x-1]), y-vector([])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, _, [u-[x-Weight]], _, _, _)),
		assertion(Weight =~= 0.5).

	test(tfidf_recommender_normalization_effect, deterministic) :-
		Dataset = tfidf_dataset([rating(u, x, 5),rating(u, y, 5)], [x,y], [x-vector([x-10]), y-vector([y-1])]),
		tfidf_recommender::learn(Dataset, Normalized),
		tfidf_recommender::learn(Dataset, Raw, [normalization(none)]),
		tfidf_recommender::score(Normalized, u, x, Equal),
		tfidf_recommender::score(Raw, u, x, Dominant),
		ExpectedEqual is 1 / sqrt(2), ExpectedDominant is 10 / sqrt(101),
		assertion(Equal =~= ExpectedEqual),
		assertion(Dominant =~= ExpectedDominant).

	test(tfidf_recommender_large_vector_values, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, x, 1)], [x], [x-vector([x-1.0e200,y-1.0e200])]),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, u, x, Score).

	test(tfidf_recommender_large_unnormalized_centroid, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, x, 1),rating(u, y, 1)], [x,y], [x-vector([f-1.0e300]),y-vector([f-1.0e300])]),
		tfidf_recommender::learn(Dataset, Model, [normalization(none)]),
		tfidf_recommender::score(Model, u, x, Score).

	test(tfidf_recommender_feature_l2_normalization, deterministic) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, Model),
		Model = tfidf_model(_, _, [first-[a-WeightA,b-WeightB]| _], _, _, _, _),
		IDF is log(3 / 2) + 1,
		Norm is sqrt(4 * IDF * IDF + 1),
		ExpectedA is 2 * IDF / Norm,
		ExpectedB is 1 / Norm,
		assertion(WeightA =~= ExpectedA),
		assertion(WeightB =~= ExpectedB).

	test(tfidf_recommender_unrated_documents_affect_idf, true) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second,third], [first-features([a,a,b]), second-features([b,c]), third-features([a])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, _, _, Vectorizer, _, _)),
		Vectorizer = text_vectorizer_model([feature(a,2,IDF)| _], _),
		Expected is log(4 / 3) + 1,
		assertion(IDF =~= Expected),
		text_vectorizer::diagnostic(Vectorizer, document_count(3)).

	test(tfidf_recommender_empty_documents_count_in_idf, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-features([])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, _, _, Vectorizer, _, _)),
		Vectorizer = text_vectorizer_model([feature(a,1,IDF)], _),
		Expected is log(3 / 2) + 1,
		assertion(IDF =~= Expected).

	test(tfidf_recommender_binary_tags, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second], [first-features([tag,tag]),second-features([tag])]),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([weighting(binary)])]),
		tfidf_recommender::score(Model, u, second, Score).

	test(tfidf_recommender_compound_features, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second], [first-features([genre(action)]),second-features([genre(action)])]),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, second, Score).

	test(tfidf_recommender_classic_zero_weights_vocabulary, true) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second], [first-features([a,b]),second-features([a,c])]),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([idf(classic)])]),
		tfidf_recommender::diagnostic(Model, feature_count(3)).

	test(tfidf_recommender_repeated_top_level_options, deterministic(Score =~= 0.0)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(9),positive_threshold(1)]),
		tfidf_recommender::score(Model, u, diagonal, Score).

	test(tfidf_recommender_repeated_nested_options, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model, [normalization(none),vectorizer_options([weighting(binary),weighting(count)])]),
		Model = tfidf_model(_, _, [first-[a-WeightA,b-WeightB]| _], _, _, _, _),
		assertion(WeightA =~= 1.0),
		assertion(WeightB =~= 1.0),
		tfidf_recommender::valid_recommender(Model).

	test(tfidf_recommender_input_order_independence, deterministic(Model == Other)) :-
		vector_dataset(2, 4, tfidf_dataset(Ratings, Items, Contents)),
		reverse(Ratings, ReversedRatings),
		reverse(Items, ReversedItems),
		reverse(Contents, ReversedContents),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::learn(tfidf_dataset(ReversedRatings, ReversedItems, ReversedContents), Other).

	test(tfidf_recommender_features_order_independence, deterministic(Model == Other)) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, Model),
		OtherDataset = tfidf_dataset([rating(u, first, 5)], [second,first], [second-features([c,b]),first-features([b,a,a])]),
		tfidf_recommender::learn(OtherDataset, Other).

	test(tfidf_recommender_vector_canonicalization, deterministic) :-
		Dataset = tfidf_dataset([rating(u, x, 5)], [x], [x-vector([z-0,b-2,a-1])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, [x-vector([a-1,b-2])], _, _, _, _, _)).

	test(tfidf_recommender_negative_ratings_uniform, deterministic(Score =~= 1.0)) :-
		vector_dataset(-4, -2, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, y, Score).

	test(tfidf_recommender_negative_unselected_rating_weighted, deterministic(Score =~= 1.0)) :-
		vector_dataset(-2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		tfidf_recommender::score(Model, u, y, Score).

	test(tfidf_recommender_selected_nonpositive_rating, error(domain_error(positive_rating_weight, 0))) :-
		vector_dataset(0, 0, Dataset), tfidf_recommender::learn(Dataset, _, [profile_weighting(rating)]).

	test(tfidf_recommender_empty_catalog, error(domain_error(non_empty_catalog, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [], []), _).

	test(tfidf_recommender_missing_all_content, error(domain_error(item_content_coverage, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], []), _).

	test(tfidf_recommender_missing_content, error(domain_error(item_content_coverage, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x,y], [x-vector([])]), _).

	test(tfidf_recommender_extra_content, error(domain_error(item_content_coverage, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-vector([]),y-vector([])]), _).

	test(tfidf_recommender_duplicate_item, error(domain_error(duplicate_item, x))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x,x], [x-vector([])]), _).

	test(tfidf_recommender_duplicate_content, error(domain_error(duplicate_item, x))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-vector([]),x-vector([])]), _).

	test(tfidf_recommender_rated_item_outside_catalog, error(domain_error(catalog_item, y))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,y,1)], [x], [x-vector([])]), _).

	test(tfidf_recommender_variable_item, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [_], [x-vector([])]), _).

	test(tfidf_recommender_compound_item, error(type_error(atomic, item(x)))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [item(x)], [x-vector([])]), _).

	test(tfidf_recommender_mixed_representations, error(domain_error(content_representation, vector([])))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-vector([])]), _).

	test(tfidf_recommender_invalid_descriptor, error(domain_error(item_content, bad))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-bad]), _).

	test(tfidf_recommender_variable_descriptor, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-_]), _).

	test(tfidf_recommender_variable_features, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([_])]), _).

	test(tfidf_recommender_improper_features, error(type_error(list, [a|bad]))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([a|bad])]), _).

	test(tfidf_recommender_no_vocabulary, error(domain_error(non_empty_vocabulary, [[]]))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([])]), _).

	test(tfidf_recommender_duplicate_feature, error(domain_error(duplicate_feature, a))) :-
		bad_vector([a-1,a-2], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(tfidf_recommender_nonnumeric_weight, error(type_error(number, bad))) :-
		bad_vector([a-bad], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(tfidf_recommender_negative_weight, error(domain_error(non_negative_finite_weight, -1))) :-
		bad_vector([a-(-1)], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(tfidf_recommender_variable_weight, error(instantiation_error)) :-
		bad_vector([a-_], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(tfidf_recommender_variable_feature_key, error(instantiation_error)) :-
		bad_vector([_-1], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(tfidf_recommender_nonpair_entry, error(type_error(pair, bad))) :-
		bad_vector([bad], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(tfidf_recommender_variable_entry, error(instantiation_error)) :-
		bad_vector([_], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(tfidf_recommender_vectorizer_options_on_vectors, error(domain_error(option, vectorizer_options([idf(classic)])))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, _, [vectorizer_options([idf(classic)])]).

	test(tfidf_recommender_nested_normalization, error(domain_error(option, vectorizer_options([normalization(l2)])))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, _, [vectorizer_options([normalization(l2)])]).

	test(tfidf_recommender_invalid_option, error(domain_error(option, profile_weighting(bad)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, _, [profile_weighting(bad)]).

	test(tfidf_recommender_options_variable, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, _, _).

	test(tfidf_recommender_query_variable, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, _, x, _).

	test(tfidf_recommender_query_compound, error(type_error(atomic, user(u)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, user(u), x, _).

	test(tfidf_recommender_n_variable, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::recommend(Model, u, _, _).

	test(tfidf_recommender_n_noninteger, error(type_error(integer, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::recommend(Model, u, bad, _).

	test(tfidf_recommender_n_nonpositive, error(domain_error(positive_integer, 0))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::recommend(Model, u, 0, _).

	test(tfidf_recommender_scale_bounds, error(domain_error(rating_scale, 5-1))) :-
		tfidf_recommender::learn(tfidf_scale_fixture(5, 1), _).

	test(tfidf_recommender_out_of_scale_rating, error(domain_error(rating_scale(2,5), 1))) :-
		tfidf_recommender::learn(tfidf_scale_fixture(2, 5), _).

	test(tfidf_recommender_valid_scale, deterministic(Score =~= 1.0)) :-
		tfidf_recommender::learn(tfidf_scale_fixture(1, 5), Model),
		tfidf_recommender::score(Model, u, x, Score).

	test(tfidf_recommender_incomplete_model, true) :-
		Model = tfidf_model(_,_,_,_,_,_,_),
		\+ tfidf_recommender::valid_recommender(Model).

	test(tfidf_recommender_tampered_profiles, fail) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,_,Vectorizer,Scale,Diagnostics)),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,[],Vectorizer,Scale,Diagnostics)).

	test(tfidf_recommender_tampered_vectors, fail) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,_,Profiles,Vectorizer,Scale,Diagnostics)),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,[],Profiles,Vectorizer,Scale,Diagnostics)).

	test(tfidf_recommender_tampered_vectorizer, fail) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,_,Scale,Diagnostics)),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,Profiles,none,Scale,Diagnostics)).

	test(tfidf_recommender_tampered_diagnostics, fail) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Diagnostics)),
		append(Diagnostics, [item_count(99)], Other),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Other)).

	test(tfidf_recommender_extra_diagnostics, deterministic) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Diagnostics)),
		append(Diagnostics, [note(extra)], Other),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Other)).

	test(tfidf_recommender_diagnostics, deterministic(Diagnostics == Enumerated)) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::diagnostics(Model, Diagnostics),
		memberchk(item_count(3), Diagnostics),
		memberchk(user_count(1), Diagnostics),
		memberchk(feature_count(2), Diagnostics),
		memberchk(non_empty_profile_count(1), Diagnostics),
		findall(Diagnostic, tfidf_recommender::diagnostic(Model, Diagnostic), Enumerated).

	test(tfidf_recommender_export_round_trip, deterministic) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, Model),
		^^file_path('tfidf_saved.pl', File),
		tfidf_recommender::export_to_file(Dataset, Model, tfidf_saved, File),
		logtalk_load(File), {tfidf_saved(Loaded)},
		tfidf_recommender::valid_recommender(Loaded),
		tfidf_recommender::score(Model, u, second, Score),
		tfidf_recommender::score(Loaded, u, second, Restored),
		assertion(Score =~= Restored),
		tfidf_recommender::recommend(Model, u, 10, Items),
		tfidf_recommender::recommend(Loaded, u, 10, Items).

	test(tfidf_recommender_export_clause, deterministic(Clauses == [saved(Model)])) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::export_to_clauses(Dataset, Model, saved, Clauses).

	test(tfidf_recommender_print, deterministic) :-
		^^suppress_text_output,
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model), tfidf_recommender::print_recommender(Model).

	test(tfidf_recommender_score_implemented_locally, deterministic) :-
		tfidf_recommender::predicate_property(score(_, _, _, _), defined_in(tfidf_recommender)).

	test(tfidf_recommender_variable_model, error(instantiation_error)) :-
		tfidf_recommender::score(_, u, x, _).

	test(tfidf_recommender_invalid_model, error(domain_error(recommender, bad))) :-
		tfidf_recommender::recommend(bad, u, 1, _).

	test(tfidf_recommender_duplicate_ratings, error(domain_error(duplicate_rating, u-x))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1),rating(u,x,2)], [x], [x-vector([])]), _).

	test(tfidf_recommender_empty_ratings, error(domain_error(non_empty_ratings, _))) :-
		tfidf_recommender::learn(tfidf_dataset([], [x], [x-vector([])]), _).

	test(tfidf_recommender_nonnumeric_rating, error(type_error(number, bad))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,bad)], [x], [x-vector([])]), _).

	test(tfidf_recommender_variable_rating_identifier, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(_,x,1)], [x], [x-vector([])]), _).

	test(tfidf_recommender_compound_rating_identifier, error(type_error(atomic, user(u)))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(user(u),x,1)], [x], [x-vector([])]), _).

	test(tfidf_recommender_inconsistent_rating_count, error(consistency_error(rating_count, 9, 1))) :-
		tfidf_recommender::learn(tfidf_count_fixture, _).

	test(tfidf_recommender_score_recommend_agreement, deterministic) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, u, 1, [second-Recommended]),
		tfidf_recommender::score(Model, u, second, Direct),
		assertion(Recommended =~= Direct).

	test(tfidf_recommender_inconsistent_frequency_bounds, error(domain_error(option, minimum_document_frequency(2)))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, _, [vectorizer_options([minimum_document_frequency(2),maximum_document_frequency(1)])]).

	test(tfidf_recommender_frequency_filter_empty_vocabulary, error(domain_error(non_empty_vocabulary, _))) :-
		Dataset = tfidf_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-features([b])]),
		tfidf_recommender::learn(Dataset, _, [vectorizer_options([minimum_document_frequency(2)])]).

	% auxiliary predicates

	feature_dataset(
		tfidf_dataset([rating(u, first, 5)], [first,second], [first-features([a,a,b]),second-features([b,c])])
	).

	bad_vector(Vector, tfidf_dataset([rating(u,x,1)], [x], [x-vector(Vector)])).

	vector_dataset(
		RatingX,
		RatingY, tfidf_dataset([rating(u, x, RatingX), rating(u, y, RatingY)], [x,y,diagonal], [x-vector([x-1]), y-vector([y-1]), diagonal-vector([x-1,y-1])])
	).

:- end_object.

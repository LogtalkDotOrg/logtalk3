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
		comment is 'Unit tests for the "jaccard_recommender" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2, append/3, reverse/2
	]).

	cover(jaccard_recommender).

	cleanup :-
		^^clean_file('jaccard_saved.pl').

	test(jaccard_recommender_content_replacement_retraining, deterministic(Updated == Expected)) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,1)], [x,y,z],
			[x-features([a]),y-features([b]),z-features([c])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [x-features([c,c])], Updated),
		Changed = jaccard_dataset([rating(u,x,5),rating(u,y,1)], [x,y,z],
			[x-features([c,c]),y-features([b]),z-features([c])]),
		jaccard_recommender::learn(Changed, Expected),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::score(Model, u, z, Before),
		jaccard_recommender::score(Updated, u, z, After),
		assertion(Before =~= 0.0),
		assertion(After =~= 1.0).

	test(jaccard_recommender_content_replacement_empty, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [], Updated).

	test(jaccard_recommender_content_replacement_unknown, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [missing-features([])], _).

	test(jaccard_recommender_content_replacement_duplicate, error(domain_error(duplicate_item, liked_x))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-features([]),liked_x-features([])], _).

	test(jaccard_recommender_content_replacement_kind, error(domain_error(content_representation, vector([])))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-vector([])], _).

	test(jaccard_recommender_content_replacement_support, deterministic(Profiles == [u-[c-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4)], [x,y],
			[x-features([a,b]),y-features([b,c])]),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(4),min_feature_support(2)]),
		jaccard_recommender::replace_content(Model, [x-features([c,c])], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_content_replacement_shared_item, deterministic(Profiles == [u-[genre(new)-1],v-[genre(new)-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(v,x,1)], [x], [x-features([old])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [x-features([genre(new)])], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_content_replacement_unselected, deterministic(Profiles == OriginalProfiles)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		Model = jaccard_model(Ratings,_,_,OriginalProfiles,Scale,Diagnostics),
		jaccard_recommender::replace_content(Model, [disliked-features([new])], Updated),
		Updated = jaccard_model(Ratings,_,_,Profiles,Scale,UpdatedDiagnostics),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(options(Options), UpdatedDiagnostics)),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_content_replacement_unrated, deterministic(Profiles == OriginalProfiles)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		Model = jaccard_model(_,_,_,OriginalProfiles,_,_),
		jaccard_recommender::replace_content(Model, [partial-features([a,b,c])], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::score(Updated, u, partial, Score),
		assertion(Score =~= 1.0),
		jaccard_recommender::recommend(Updated, u, 1, [partial-Score]).

	test(jaccard_recommender_content_replacement_empty_profile, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,5)], [x,y], [x-features([a]),y-features([b])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [x-features([])], Updated),
		Updated = jaccard_model(_,_,_,[u-[]],_,Diagnostics),
		assertion(memberchk(non_empty_profile_count(0), Diagnostics)),
		assertion(memberchk(feature_count(1), Diagnostics)),
		jaccard_recommender::score(Updated, u, y, Score),
		assertion(Score =~= 0.0),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_content_replacement_binary, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x,y], [x-vector([a-1]),y-vector([b-1])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [x-vector([absent-0.0,b-1.0])], Updated),
		Updated = jaccard_model(_, [x-vector([b-1.0]),y-vector([b-1])], _, [u-[b-1]], _, _),
		jaccard_recommender::score_all(Updated, u, [x,y], [x-X,y-Y]),
		jaccard_recommender::score_content(Updated, u, features([b,b]), Content),
		assertion(X =~= 1.0),
		assertion(Y =~= X),
		assertion(Content =~= X),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_content_replacement_identical, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		Model = jaccard_model(_,Contents,_,_,_,_),
		memberchk(liked_x-Content, Contents),
		jaccard_recommender::replace_content(Model, [liked_x-Content], Updated).

	test(jaccard_recommender_content_replacement_batch_order, deterministic(First == Second)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [partial-features([new]),liked_x-features([])], First),
		jaccard_recommender::replace_content(Model, [liked_x-features([]),partial-features([new])], Second),
		jaccard_recommender::replace_content(Model, [partial-features([new])], Intermediate),
		jaccard_recommender::replace_content(Intermediate, [liked_x-features([])], Sequential),
		assertion(First == Sequential).

	test(jaccard_recommender_content_replacement_diagnostics, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x], [x-features([a])]),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		reverse(Diagnostics, Reversed),
		append(Reversed, [note(last)], Tail),
		Model = jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,[note(first)|Tail]),
		jaccard_recommender::replace_content(Model, [x-features([b,c])], Updated),
		Changed = jaccard_dataset([rating(u,x,1)], [x], [x-features([b,c])]),
		jaccard_recommender::learn(Changed, jaccard_model(_,_,_,_,_,ExpectedDiagnostics)),
		reverse(ExpectedDiagnostics, ExpectedReversed),
		append(ExpectedReversed, [note(last)], ExpectedTail),
		Updated = jaccard_model(_,_,_,_,_,UpdatedDiagnostics),
		assertion(UpdatedDiagnostics == [note(first)|ExpectedTail]),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_content_replacement_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::replace_content(Model, [liked_x-features([]),partial-features([a])], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_content_replacement_empty_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::replace_content(Model, [], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_content_replacement_export, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		jaccard_recommender::replace_content(Model, [partial-features([a,b,c])], Updated),
		assertion(lgtunit::variant(Model, Original)),
		jaccard_recommender::export_to_clauses(Dataset, Updated, saved, [saved(Restored)]),
		assertion(lgtunit::variant(Updated, Restored)),
		jaccard_recommender::valid_recommender(Restored),
		jaccard_recommender::recommend(Restored, u, 1, [partial-Score]),
		assertion(Score =~= 1.0).

	test(jaccard_recommender_content_replacement_implemented_locally, deterministic) :-
		jaccard_recommender::predicate_property(replace_content(_, _, _), defined_in(jaccard_recommender)).

	test(jaccard_recommender_content_replacement_variable_model, error(instantiation_error)) :-
		jaccard_recommender::replace_content(_, [], _).

	test(jaccard_recommender_content_replacement_invalid_model, error(domain_error(recommender, bad))) :-
		jaccard_recommender::replace_content(bad, [], _).

	test(jaccard_recommender_content_replacement_variable_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, _, _).

	test(jaccard_recommender_content_replacement_nonlist, error(type_error(list, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, bad, _).

	test(jaccard_recommender_content_replacement_open_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-features([])|_], _).

	test(jaccard_recommender_content_replacement_improper_list, error(type_error(list, [liked_x-features([])|bad]))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-features([])|bad], _).

	test(jaccard_recommender_content_replacement_variable_entry, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [_], _).

	test(jaccard_recommender_content_replacement_nonpair, error(type_error(pair, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [bad], _).

	test(jaccard_recommender_content_replacement_variable_identifier, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [_-features([])], _).

	test(jaccard_recommender_content_replacement_nonatomic_identifier, error(type_error(atomic, item(x)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [item(x)-features([])], _).

	test(jaccard_recommender_content_replacement_variable_descriptor, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-_], _).

	test(jaccard_recommender_content_replacement_invalid_descriptor, error(domain_error(item_content, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-bad], _).

	test(jaccard_recommender_content_replacement_mixed_kinds, error(domain_error(content_representation, vector([])))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-features([]),liked_y-vector([])], _).

	test(jaccard_recommender_content_replacement_nonground_feature, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-features([genre(_)])], _).

	test(jaccard_recommender_content_replacement_nonlist_features, error(type_error(list, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::replace_content(Model, [liked_x-features(bad)], _).

	test(jaccard_recommender_content_replacement_nonbinary_weight, error(domain_error(binary_weight, 0.5))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::replace_content(Model, [x-vector([a-0.5])], _).

	test(jaccard_recommender_content_replacement_negative_weight, error(domain_error(non_negative_finite_weight, -1))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::replace_content(Model, [x-vector([a- -1])], _).

	test(jaccard_recommender_content_replacement_nonnumeric_weight, error(type_error(number, bad))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::replace_content(Model, [x-vector([a-bad])], _).

	test(jaccard_recommender_content_replacement_variable_weight, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::replace_content(Model, [x-vector([a-_])], _).

	test(jaccard_recommender_content_replacement_duplicate_feature, error(domain_error(duplicate_feature, a))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::replace_content(Model, [x-vector([a-1,a-0])], _).

	test(jaccard_recommender_content_replacement_nonpair_vector, error(type_error(pair, bad))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::replace_content(Model, [x-vector([bad])], _).

	test(jaccard_recommender_content_replacement_nonground_key, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::replace_content(Model, [x-vector([genre(_)-1])], _).

	test(jaccard_recommender_rating_update_retraining, deterministic(Updated == Expected)) :-
		Contents = [x-features([a]),y-features([b]),z-features([c])],
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4),rating(u,z,1)], [x,y,z], Contents),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,x,1),rating(v,z,5)], Updated),
		Changed = jaccard_dataset([rating(u,x,1),rating(u,y,4),rating(u,z,1),rating(v,z,5)], [x,y,z], Contents),
		jaccard_recommender::learn(Changed, Expected),
		Updated = jaccard_model(_,_,_,[u-[b-1],v-[c-1]],_,_),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::score(Updated, u, x, Score),
		assertion(Score =~= 0.0).

	test(jaccard_recommender_rating_removal_retraining, deterministic) :-
		Contents = [x-features([a,b]),y-features([b,c]),z-features([d]),w-features([q])],
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4),rating(u,z,1),rating(v,w,3)], [w,x,y,z], Contents),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-x], Updated),
		Changed = jaccard_dataset([rating(u,y,4),rating(u,z,1),rating(v,w,3)], [w,x,y,z], Contents),
		jaccard_recommender::learn(Changed, Expected),
		jaccard_recommender::valid_recommender(Updated),
		assertion(Updated == Expected),
		jaccard_recommender::recommend(Updated, u, 1, [x-Score]),
		Third is 1 / 3,
		assertion(Score =~= Third).

	test(jaccard_recommender_rating_removal_empty, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [], Updated).

	test(jaccard_recommender_rating_removal_missing, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [missing-liked_x,u-missing,u-partial], Updated).

	test(jaccard_recommender_rating_removal_idempotent, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-liked_x,u-liked_x,missing-other], Updated),
		jaccard_recommender::remove_ratings(Updated, [u-liked_x,u-liked_x,missing-other], Retried),
		assertion(Updated == Retried),
		jaccard_recommender::remove_ratings(Model, [u-liked_x], Single),
		assertion(Updated == Single).

	test(jaccard_recommender_rating_removal_final, error(domain_error(non_empty_ratings, []))) :-
		jaccard_recommender::learn(jaccard_scale_fixture(1,5), Model),
		jaccard_recommender::remove_ratings(Model, [u-x,u-x,missing-other], _).

	test(jaccard_recommender_rating_removal_user_disappears, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(v,y,4)], [x,y,z],
			[x-features([a]),y-features([b]),z-features([])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-x], Updated),
		Updated = jaccard_model([rating(v,y,4)],_,_,[v-[b-1]],_,Diagnostics),
		assertion(memberchk(rating_count(1), Diagnostics)),
		assertion(memberchk(user_count(1), Diagnostics)),
		assertion(memberchk(non_empty_profile_count(1), Diagnostics)),
		jaccard_recommender::score(Updated, u, x, Individual),
		jaccard_recommender::score_all(Updated, u, [x,y], [x-X,y-Y]),
		jaccard_recommender::score_content(Updated, u, features([a]), Content),
		assertion(Individual =~= 0.0),
		assertion(X =~= Individual),
		assertion(Y =~= Individual),
		assertion(Content =~= Individual),
		jaccard_recommender::recommend(Updated, u, 3, [z-0.0,y-0.0,x-0.0]),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_removal_other_user_rating, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,1),rating(u,y,5),rating(v,x,3)], [x,y],
			[x-features([a]),y-features([b])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-x], Updated),
		Updated = jaccard_model([rating(u,y,5),rating(v,x,3)],_,_,[u-[b-1],v-[a-1]],_,_),
		jaccard_recommender::recommend(Updated, u, 2, [x-0.0]),
		jaccard_recommender::recommend(Updated, v, 2, [y-0.0]).

	test(jaccard_recommender_rating_removal_unselected_mean_shift, deterministic(Profiles == [u-[a-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4),rating(u,z,1)], [x,y,z],
			[x-features([a]),y-features([b]),z-features([c])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-z], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_removal_support, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4),rating(u,z,1)], [x,y,z],
			[x-features([a,b,b]),y-features([b,c]),z-features([c])]),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(4),min_feature_support(2)]),
		jaccard_recommender::remove_ratings(Model, [u-x,u-x], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,Diagnostics),
		assertion(Profiles == [u-[]]),
		assertion(memberchk(non_empty_profile_count(0), Diagnostics)),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_removal_numeric_threshold, deterministic(Profiles == [u-[b-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,4),rating(u,y,4),rating(u,z,1)], [x,y,z],
			[x-features([a]),y-features([b]),z-features([c])]),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(4)]),
		jaccard_recommender::remove_ratings(Model, [u-x], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_removal_binary_scale, deterministic) :-
		jaccard_recommender::learn(jaccard_scale_fixture(1,5), Model),
		jaccard_recommender::update_ratings(Model, [rating(v,x,5)], Inserted),
		Inserted = jaccard_model(_,Contents,Vectors,_,Scale,Diagnostics),
		jaccard_recommender::remove_ratings(Inserted, [u-x], Updated),
		Updated = jaccard_model([rating(v,x,5)],Contents,Vectors,[v-[x-1]],Scale,UpdatedDiagnostics),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(options(Options), UpdatedDiagnostics)),
		assertion(Scale == scale(1,5)),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_removal_empty_content, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,1),rating(v,y,1)], [x,y],
			[x-features([]),y-features([])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-x], Updated),
		Updated = jaccard_model(_,_,_,[v-[]],none,Diagnostics),
		assertion(memberchk(non_empty_profile_count(0), Diagnostics)),
		assertion(memberchk(feature_count(0), Diagnostics)),
		jaccard_recommender::score(Updated, v, x, Score),
		assertion(Score =~= 0.0).

	test(jaccard_recommender_rating_removal_batch_order, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-liked_x,missing-other,u-liked_y,u-liked_x], First),
		jaccard_recommender::remove_ratings(Model, [u-liked_y,u-liked_x], Second),
		assertion(First == Second),
		jaccard_recommender::remove_ratings(Model, [u-liked_x], Intermediate),
		jaccard_recommender::remove_ratings(Intermediate, [u-liked_y], Sequential),
		assertion(First == Sequential).

	test(jaccard_recommender_rating_removal_reversed_pair, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(x,u,4),rating(v,x,3)], [u,x],
			[u-features([a]),x-features([b])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-x], Updated),
		Updated = jaccard_model([rating(v,x,3),rating(x,u,4)],_,_,_,_,_),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_removal_diagnostics, deterministic) :-
		Contents = [x-features([a]),y-features([b])],
		Dataset = jaccard_dataset([rating(u,x,1),rating(v,y,5)], [x,y], Contents),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		reverse(Diagnostics, Reversed),
		append(Reversed, [note(last)], Tail),
		Model = jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,[note(first)|Tail]),
		jaccard_recommender::remove_ratings(Model, [u-x], Updated),
		jaccard_recommender::learn(jaccard_dataset([rating(v,y,5)], [x,y], Contents), Expected),
		Expected = jaccard_model(_,_,_,_,_,ExpectedDiagnostics),
		reverse(ExpectedDiagnostics, ExpectedReversed),
		append(ExpectedReversed, [note(last)], ExpectedTail),
		Updated = jaccard_model(_,Contents,Vectors,_,Scale,UpdatedDiagnostics),
		assertion(UpdatedDiagnostics == [note(first)|ExpectedTail]),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_removal_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::remove_ratings(Model, [u-liked_x,u-liked_x,missing-other], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_rating_removal_empty_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::remove_ratings(Model, [], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_rating_removal_missing_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::remove_ratings(Model, [unknown-missing], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_rating_removal_export_immutable, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		jaccard_recommender::remove_ratings(Model, [u-liked_x], Updated),
		assertion(lgtunit::variant(Model, Original)),
		assertion(ground(Updated)),
		jaccard_recommender::export_to_clauses(Dataset, Updated, saved, [saved(Restored)]),
		assertion(lgtunit::variant(Updated, Restored)),
		jaccard_recommender::valid_recommender(Restored),
		jaccard_recommender::score(Restored, u, liked_x, Score),
		Third is 1 / 3,
		assertion(Score =~= Third).

	test(jaccard_recommender_rating_removal_composition, deterministic(Restored == Replaced)) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x], [x-features([a])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [y-features([b])], Extended),
		jaccard_recommender::update_ratings(Extended, [rating(v,y,5)], Inserted),
		jaccard_recommender::replace_content(Inserted, [y-features([a,b])], Replaced),
		jaccard_recommender::remove_ratings(Replaced, [v-y,v-y,missing-other], Removed),
		Removed = jaccard_model(_,_,_,[u-[a-1]],_,_),
		jaccard_recommender::recommend(Removed, v, 2, [y-0.0,x-0.0]),
		jaccard_recommender::update_ratings(Removed, [rating(v,y,5)], Restored).

	test(jaccard_recommender_rating_removal_implemented_locally, deterministic) :-
		jaccard_recommender::predicate_property(remove_ratings(_, _, _), defined_in(jaccard_recommender)).

	test(jaccard_recommender_rating_removal_variable_model, error(instantiation_error)) :-
		jaccard_recommender::remove_ratings(_, [], _).

	test(jaccard_recommender_rating_removal_invalid_model, error(domain_error(recommender, bad))) :-
		jaccard_recommender::remove_ratings(bad, [], _).

	test(jaccard_recommender_rating_removal_tampered_model, error(domain_error(recommender, _))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,_,Scale,Diagnostics)),
		jaccard_recommender::remove_ratings(jaccard_model(Ratings,Contents,Vectors,[],Scale,Diagnostics), [unknown-missing], _).

	test(jaccard_recommender_rating_removal_variable_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, _, _).

	test(jaccard_recommender_rating_removal_nonlist, error(type_error(list, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, bad, _).

	test(jaccard_recommender_rating_removal_open_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [unknown-missing|_], _).

	test(jaccard_recommender_rating_removal_improper_list, error(type_error(list, [unknown-missing|bad]))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [unknown-missing|bad], _).

	test(jaccard_recommender_rating_removal_variable_entry, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [_], _).

	test(jaccard_recommender_rating_removal_nonpair, error(type_error(pair, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [unknown-missing,bad], _).

	test(jaccard_recommender_rating_removal_variable_user, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [_-liked_x], _).

	test(jaccard_recommender_rating_removal_variable_item, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-_], _).

	test(jaccard_recommender_rating_removal_nonatomic_user, error(type_error(atomic, user(u)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [user(u)-liked_x], _).

	test(jaccard_recommender_rating_removal_nonatomic_item, error(type_error(atomic, item(x)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-item(x)], _).

	test(jaccard_recommender_rating_removal_final_batch, error(domain_error(non_empty_ratings, []))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::remove_ratings(Model, [u-liked_x,u-liked_y,u-disliked,u-liked_x,missing-other], _).

	test(jaccard_recommender_rating_removal_validates_before_final_guard, error(type_error(pair, bad))) :-
		jaccard_recommender::learn(jaccard_scale_fixture(1,5), Model),
		jaccard_recommender::remove_ratings(Model, [u-x,bad], _).

	test(jaccard_recommender_rating_removal_error_preserves_input, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		catch(jaccard_recommender::remove_ratings(Model, [u-liked_x,User-missing], Updated), error(instantiation_error,_), Caught = true),
		assertion(Caught == true),
		assertion(var(User)),
		assertion(var(Updated)),
		assertion(lgtunit::variant(Model, Original)).

	test(jaccard_recommender_rating_update_empty, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [], Updated).

	test(jaccard_recommender_rating_update_duplicate, error(domain_error(duplicate_rating, u-liked_x))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,liked_x,1),rating(u,liked_x,5)], _).

	test(jaccard_recommender_rating_update_nonrecord, error(type_error(rating, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [bad], _).

	test(jaccard_recommender_rating_update_unknown_item, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,missing,5)], _).

	test(jaccard_recommender_rating_update_outside_scale, error(domain_error(rating_scale(1, 5), 6))) :-
		jaccard_recommender::learn(jaccard_scale_fixture(1,5), Model),
		jaccard_recommender::update_ratings(Model, [rating(u,x,6)], _).

	test(jaccard_recommender_rating_update_variable_record, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [_], _).

	test(jaccard_recommender_rating_update_mean_shift, deterministic(Profiles == [u-[a-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4),rating(u,z,1)], [x,y,z],
			[x-features([a]),y-features([b]),z-features([c])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,x,10)], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::score(Updated, u, y, Score),
		assertion(Score =~= 0.0).

	test(jaccard_recommender_rating_update_insertion_mean_shift, deterministic(Profiles == [u-[a-1,b-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4)], [x,y,z],
			[x-features([a]),y-features([b]),z-features([c])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,z,1)], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,Diagnostics),
		assertion(memberchk(rating_count(3), Diagnostics)),
		jaccard_recommender::recommend(Updated, u, 3, []).

	test(jaccard_recommender_rating_update_support, deterministic(Profiles == [u-[],v-[b-1,c-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,4),rating(u,z,1),rating(v,y,5),rating(v,z,5)], [x,y,z],
			[x-features([a,b]),y-features([b,c]),z-features([b,c])]),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2)]),
		jaccard_recommender::update_ratings(Model, [rating(u,x,10)], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,Diagnostics),
		assertion(memberchk(non_empty_profile_count(1), Diagnostics)),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_update_numeric_threshold, deterministic(Profiles == [u-[a-1,b-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-features([b])]),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(4)]),
		jaccard_recommender::update_ratings(Model, [rating(u,x,4),rating(u,y,4)], Updated),
		Updated = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_update_new_user, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		Model = jaccard_model(_,Contents,Vectors,[u-OriginalProfile],Scale,Diagnostics),
		jaccard_recommender::update_ratings(Model, [rating(v,partial,5)], Updated),
		Updated = jaccard_model(_,Contents,Vectors,[u-OriginalProfile,v-[c-1,d-1]],Scale,UpdatedDiagnostics),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(options(Options), UpdatedDiagnostics)),
		assertion(memberchk(user_count(2), UpdatedDiagnostics)),
		assertion(memberchk(rating_count(4), UpdatedDiagnostics)),
		jaccard_recommender::score(Model, v, identical, Before),
		jaccard_recommender::score(Updated, v, identical, Individual),
		jaccard_recommender::score_all(Updated, v, [identical], [identical-Batch]),
		jaccard_recommender::score_content(Updated, v, features([a,b,c]), Content),
		assertion(Before =~= 0.0),
		assertion(Individual =~= 0.25),
		assertion(Batch =~= Individual),
		assertion(Content =~= Individual),
		jaccard_recommender::recommend(Updated, v, 20, Recommendations),
		assertion(\+ member(partial-_, Recommendations)),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_update_recommendation_exclusion, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,identical,0)], Updated),
		jaccard_recommender::recommend(Model, u, 1, [identical-Before]),
		assertion(Before =~= 1.0),
		jaccard_recommender::recommend(Updated, u, 20, Recommendations),
		assertion(\+ member(identical-_, Recommendations)).

	test(jaccard_recommender_rating_update_identical, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		Model = jaccard_model([Rating|_],_,_,_,_,_),
		jaccard_recommender::update_ratings(Model, [Rating], Updated).

	test(jaccard_recommender_rating_update_batch_order, deterministic(First == Second)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,liked_x,1),rating(v,partial,5)], First),
		jaccard_recommender::update_ratings(Model, [rating(v,partial,5),rating(u,liked_x,1)], Second),
		jaccard_recommender::update_ratings(Model, [rating(v,partial,5)], Intermediate),
		jaccard_recommender::update_ratings(Intermediate, [rating(u,liked_x,1)], Sequential),
		assertion(First == Sequential).

	test(jaccard_recommender_rating_update_scale_boundaries, deterministic) :-
		jaccard_recommender::learn(jaccard_scale_fixture(1,5), Model),
		jaccard_recommender::update_ratings(Model, [rating(u,x,5),rating(v,x,1)], Updated),
		Updated = jaccard_model([rating(u,x,5),rating(v,x,1)],_,_,_,scale(1,5),Diagnostics),
		assertion(memberchk(rating_count(2), Diagnostics)),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_update_scale_free, deterministic(Profiles == [u-[a-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x], [x-features([a])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,x,-2.5)], Updated),
		Updated = jaccard_model([rating(u,x,-2.5)],_,_,Profiles,none,_),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_update_binary_content, deterministic(Score =~= 1.0)) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x,y], [x-vector([a-1]),y-vector([b-1.0])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,x,0),rating(u,y,5.0)], Updated),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::score(Updated, u, y, Score).

	test(jaccard_recommender_rating_update_diagnostics, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-features([b])]),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		reverse(Diagnostics, Reversed),
		append(Reversed, [note(last)], Tail),
		Model = jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,[note(first)|Tail]),
		jaccard_recommender::update_ratings(Model, [rating(v,y,5)], Updated),
		Changed = jaccard_dataset([rating(u,x,1),rating(v,y,5)], [x,y], Contents),
		jaccard_recommender::learn(Changed, jaccard_model(_,_,_,_,_,ExpectedDiagnostics)),
		reverse(ExpectedDiagnostics, ExpectedReversed),
		append(ExpectedReversed, [note(last)], ExpectedTail),
		Updated = jaccard_model(_,_,_,_,_,UpdatedDiagnostics),
		assertion(UpdatedDiagnostics == [note(first)|ExpectedTail]),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_rating_update_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::update_ratings(Model, [rating(u,liked_x,1),rating(v,partial,5)], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_rating_update_empty_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::update_ratings(Model, [], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_rating_update_export_immutable, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		jaccard_recommender::update_ratings(Model, [rating(v,partial,5)], Updated),
		assertion(lgtunit::variant(Model, Original)),
		jaccard_recommender::export_to_clauses(Dataset, Updated, saved, [saved(Restored)]),
		assertion(lgtunit::variant(Updated, Restored)),
		jaccard_recommender::valid_recommender(Restored),
		jaccard_recommender::score(Restored, v, identical, Score),
		assertion(Score =~= 0.25).

	test(jaccard_recommender_model_update_composition, deterministic(Updated == Expected)) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x], [x-features([a])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [y-features([b])], Extended),
		jaccard_recommender::replace_content(Extended, [x-features([b,b])], Replaced),
		jaccard_recommender::update_ratings(Replaced, [rating(u,x,2),rating(v,y,5)], Updated),
		Changed = jaccard_dataset([rating(u,x,2),rating(v,y,5)], [x,y],
			[x-features([b,b]),y-features([b])]),
		jaccard_recommender::learn(Changed, Expected),
		jaccard_recommender::valid_recommender(Model),
		jaccard_recommender::valid_recommender(Extended),
		jaccard_recommender::valid_recommender(Replaced),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::recommend(Updated, u, 1, [y-Score]),
		assertion(Score =~= 1.0).

	test(jaccard_recommender_rating_update_implemented_locally, deterministic) :-
		jaccard_recommender::predicate_property(update_ratings(_, _, _), defined_in(jaccard_recommender)).

	test(jaccard_recommender_rating_update_variable_model, error(instantiation_error)) :-
		jaccard_recommender::update_ratings(_, [], _).

	test(jaccard_recommender_rating_update_invalid_model, error(domain_error(recommender, bad))) :-
		jaccard_recommender::update_ratings(bad, [], _).

	test(jaccard_recommender_rating_update_variable_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, _, _).

	test(jaccard_recommender_rating_update_nonlist, error(type_error(list, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, bad, _).

	test(jaccard_recommender_rating_update_open_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,liked_x,1)|_], _).

	test(jaccard_recommender_rating_update_improper_list, error(type_error(list, [rating(u,liked_x,1)|bad]))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,liked_x,1)|bad], _).

	test(jaccard_recommender_rating_update_wrong_arity, error(type_error(rating, rating(u,liked_x)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,liked_x)], _).

	test(jaccard_recommender_rating_update_variable_user, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(_,liked_x,1)], _).

	test(jaccard_recommender_rating_update_variable_item, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,_,1)], _).

	test(jaccard_recommender_rating_update_nonatomic_user, error(type_error(atomic, user(u)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(user(u),liked_x,1)], _).

	test(jaccard_recommender_rating_update_nonatomic_item, error(type_error(atomic, item(x)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,item(x),1)], _).

	test(jaccard_recommender_rating_update_variable_value, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,liked_x,_)], _).

	test(jaccard_recommender_rating_update_nonnumeric_value, error(type_error(number, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(u,liked_x,bad)], _).

	test(jaccard_recommender_rating_update_below_scale, error(domain_error(rating_scale(1, 5), 0))) :-
		jaccard_recommender::learn(jaccard_scale_fixture(1,5), Model),
		jaccard_recommender::update_ratings(Model, [rating(u,x,0)], _).

	test(jaccard_recommender_rating_update_identical_duplicate, error(domain_error(duplicate_rating, v-partial))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::update_ratings(Model, [rating(v,partial,5),rating(v,partial,5)], _).

	test(jaccard_recommender_union_profile, deterministic(Profiles == [u-[a-1,b-1,c-1]])) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(_, _, _, Profiles, _, _)).

	test(jaccard_recommender_reference_scores, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, identical, Identical),
		jaccard_recommender::score(Model, u, subset, Subset),
		jaccard_recommender::score(Model, u, partial, Partial),
		jaccard_recommender::score(Model, u, disjoint, Disjoint),
		jaccard_recommender::score(Model, u, empty, Empty),
		Third is 1 / 3,
		assertion(Identical =~= 1.0),
		assertion(Subset =~= Third),
		assertion(Partial =~= 0.25),
		assertion(Disjoint =~= 0.0),
		assertion(Empty =~= 0.0).

	test(jaccard_recommender_feature_support_default, deterministic(Model == Explicit)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::learn(Dataset, Explicit, [positive_threshold(user_mean),min_feature_support(1)]),
		jaccard_recommender::default_options(Options),
		assertion(Options == [positive_threshold(user_mean),min_feature_support(1)]).

	test(jaccard_recommender_feature_support_profile, deterministic(Profiles == [u-[b-1]])) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2)]),
		Model = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::valid_recommender(Model),
		jaccard_recommender::recommender_options(Model, Options),
		assertion(Options == [min_feature_support(2),positive_threshold(user_mean)]).

	test(jaccard_recommender_feature_support_scores, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2)]),
		jaccard_recommender::score_all(Model, u, [identical,liked_x,partial],
			[identical-Identical,liked_x-Liked,partial-Partial]),
		jaccard_recommender::score(Model, u, identical, Individual),
		jaccard_recommender::score_content(Model, u, features([a,b,c]), Content),
		Third is 1 / 3,
		assertion(Identical =~= Third),
		assertion(Individual =~= Identical),
		assertion(Content =~= Identical),
		assertion(Liked =~= 0.5),
		assertion(Partial =~= 0.0),
		jaccard_recommender::score_content(Model, u, vector([b-1,a-0]), Binary),
		assertion(Binary =~= 1.0).

	test(jaccard_recommender_feature_support_recommendation, deterministic(Score =~= 1.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2)]),
		jaccard_recommender::extend_catalog(Model, [new-features([b])], Updated),
		jaccard_recommender::recommend(Updated, u, 1, [new-Score]).

	test(jaccard_recommender_feature_support_duplicate_occurrences, deterministic(Profiles == [u-[]])) :-
		Dataset = jaccard_dataset([rating(u,x,5)], [x], [x-features([a,a,a])]),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2)]),
		Model = jaccard_model(_,_,_,Profiles,_,_),
		jaccard_recommender::valid_recommender(Model).

	test(jaccard_recommender_feature_support_ignores_rejected_items, deterministic(Profiles == [u-[b-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,5),rating(u,z,1)], [x,y,z],
			[x-features([a,b]),y-features([b]),z-features([a])]),
		jaccard_recommender::learn(Dataset, jaccard_model(_,_,_,Profiles,_,_), [min_feature_support(2)]).

	test(jaccard_recommender_feature_support_binary_presence, deterministic(Profiles == [u-[a-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,5)], [x,y],
			[x-vector([a-1.0,b-0]),y-vector([a-1,b-1.0])]),
		jaccard_recommender::learn(Dataset, jaccard_model(_,_,_,Profiles,_,_), [min_feature_support(2)]).

	test(jaccard_recommender_feature_support_compound_keys, deterministic(Profiles == [u-[genre(action)-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,5)], [x,y],
			[x-features([genre(action),language(en)]),y-features([genre(action),language(fr)])]),
		jaccard_recommender::learn(Dataset, jaccard_model(_,_,_,Profiles,_,_), [min_feature_support(2)]).

	test(jaccard_recommender_feature_support_per_user, deterministic(Profiles == [u-[b-1],v-[]])) :-
		Dataset = jaccard_dataset([rating(u,x,5),rating(u,y,5),rating(v,x,5)], [x,y],
			[x-features([a,b]),y-features([b,c])]),
		jaccard_recommender::learn(Dataset, jaccard_model(_,_,_,Profiles,_,_), [min_feature_support(2)]).

	test(jaccard_recommender_feature_support_empty_profile, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(3)]),
		jaccard_recommender::valid_recommender(Model),
		jaccard_recommender::score(Model, u, identical, Score),
		assertion(Score =~= 0.0),
		jaccard_recommender::diagnostics(Model, Diagnostics),
		assertion(memberchk(non_empty_profile_count(0), Diagnostics)).

	test(jaccard_recommender_feature_support_duplicate_options, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2),min_feature_support(1)]),
		jaccard_recommender::valid_recommender(Model),
		jaccard_recommender::recommender_options(Model, Options),
		assertion(Options == [min_feature_support(2),min_feature_support(1),positive_threshold(user_mean)]),
		jaccard_recommender::score(Model, u, identical, Filtered),
		Third is 1 / 3,
		assertion(Filtered =~= Third),
		jaccard_recommender::learn(Dataset, Unfiltered, [min_feature_support(1),min_feature_support(2)]),
		jaccard_recommender::score(Unfiltered, u, identical, Original),
		assertion(Original =~= 1.0).

	test(jaccard_recommender_feature_support_extension_preserves_profiles, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2)]),
		Model = jaccard_model(Ratings,_,_,Profiles,Scale,Diagnostics),
		memberchk(options(Options), Diagnostics),
		jaccard_recommender::extend_catalog(Model, [new-features([a,b,c])], Updated),
		Updated = jaccard_model(Ratings,_,_,Profiles,Scale,UpdatedDiagnostics),
		assertion(memberchk(options(Options), UpdatedDiagnostics)),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::score(Updated, u, new, Score),
		Third is 1 / 3,
		assertion(Score =~= Third).

	test(jaccard_recommender_feature_support_export, deterministic(Score =~= 1.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [min_feature_support(2)]),
		jaccard_recommender::extend_catalog(Model, [new-features([b])], Updated),
		jaccard_recommender::export_to_clauses(Dataset, Updated, saved, [saved(Restored)]),
		jaccard_recommender::valid_recommender(Restored),
		jaccard_recommender::score(Restored, u, new, Score).

	test(jaccard_recommender_feature_support_missing_stored_option, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		append(Prefix, [options(_)| Suffix], Diagnostics),
		append(Prefix, [options([positive_threshold(user_mean)])| Suffix], Incomplete),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Incomplete)).

	test(jaccard_recommender_feature_support_tampered_profile, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,_,Scale,Diagnostics), [min_feature_support(2)]),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,Vectors,[u-[a-1,b-1,c-1]],Scale,Diagnostics)).

	test(jaccard_recommender_feature_support_zero, error(domain_error(option, min_feature_support(0)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [min_feature_support(0)]).

	test(jaccard_recommender_feature_support_negative, error(domain_error(option, min_feature_support(-1)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [min_feature_support(-1)]).

	test(jaccard_recommender_feature_support_float, error(domain_error(option, min_feature_support(1.0)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [min_feature_support(1.0)]).

	test(jaccard_recommender_feature_support_nonnumeric, error(domain_error(option, min_feature_support(bad)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [min_feature_support(bad)]).

	test(jaccard_recommender_feature_support_variable, error(domain_error(option, min_feature_support(_)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [min_feature_support(_)]).

	test(jaccard_recommender_catalog_extension, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		jaccard_recommender::score(Model, u, partial, Before),
		jaccard_recommender::extend_catalog(Model, [new-features([c,d,genre(science),c])], Updated),
		assertion(lgtunit::variant(Model, Original)),
		Model = jaccard_model(Ratings, _, _, Profiles, Scale, _),
		Updated = jaccard_model(Ratings, Contents, _, Profiles, Scale, Diagnostics),
		assertion(memberchk(new-features([c,c,d,genre(science)]), Contents)),
		assertion(memberchk(item_count(9), Diagnostics)),
		assertion(memberchk(feature_count(6), Diagnostics)),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::score(Updated, u, new, NewScore),
		assertion(NewScore =~= 0.2),
		jaccard_recommender::score(Updated, u, partial, After),
		assertion(Before =~= After).

	test(jaccard_recommender_catalog_extension_recommendation, deterministic(Score =~= 1.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([a,b,c])], Updated),
		jaccard_recommender::recommend(Updated, u, 1, [new-Score]).

	test(jaccard_recommender_catalog_extension_empty, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [], Updated).

	test(jaccard_recommender_catalog_extension_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::extend_catalog(Model, [new-features([a])], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_catalog_extension_empty_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::extend_catalog(Model, [], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_catalog_extension_repeated, deterministic(Repeated == Combined)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([a])], First),
		jaccard_recommender::extend_catalog(First, [other-features([b,new])], Repeated),
		jaccard_recommender::extend_catalog(Model, [other-features([new,b]),new-features([a])], Combined).

	test(jaccard_recommender_catalog_extension_binary_content, deterministic) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::extend_catalog(Model, [new-vector([z-0.0,b-1.0,a-1]),empty-vector([])], Updated),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::score_all(Updated, u, [new,empty], [new-New,empty-Empty]),
		assertion(New =~= 0.5),
		assertion(Empty =~= 0.0).

	test(jaccard_recommender_catalog_extension_empty_content, deterministic(Score =~= 0.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([])], Updated),
		jaccard_recommender::valid_recommender(Updated),
		jaccard_recommender::score(Updated, u, new, Score).

	test(jaccard_recommender_catalog_extension_unknown_user, deterministic(Score =~= 0.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([a])], Updated),
		jaccard_recommender::recommend(Updated, unknown, 20, Recommendations),
		memberchk(new-Score, Recommendations).

	test(jaccard_recommender_catalog_extension_empty_profile, deterministic(Score =~= 0.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(6)]),
		jaccard_recommender::extend_catalog(Model, [new-features([a,b,c])], Updated),
		jaccard_recommender::score(Updated, u, new, Score).

	test(jaccard_recommender_catalog_extension_extra_diagnostics, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		Extra = [note(extra)| Diagnostics],
		Model = jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Extra),
		jaccard_recommender::extend_catalog(Model, [new-features([a])], Updated),
		Updated = jaccard_model(_,_,_,_,_,[note(extra)| _]),
		jaccard_recommender::valid_recommender(Updated).

	test(jaccard_recommender_catalog_extension_export, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([a,b,c])], Updated),
		jaccard_recommender::export_to_clauses(Dataset, Updated, saved, [saved(Restored)]),
		assertion(lgtunit::variant(Updated, Restored)),
		jaccard_recommender::valid_recommender(Restored),
		jaccard_recommender::score_all(Restored, u, [new,partial], [new-New,partial-Partial]),
		assertion(New =~= 1.0),
		assertion(Partial =~= 0.25),
		jaccard_recommender::recommend(Restored, u, 1, [new-New]).

	test(jaccard_recommender_catalog_extension_original_catalog, error(domain_error(catalog_item, new))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([a])], _),
		jaccard_recommender::score(Model, u, new, _).

	test(jaccard_recommender_catalog_extension_implemented_locally, deterministic) :-
		jaccard_recommender::predicate_property(extend_catalog(_, _, _), defined_in(jaccard_recommender)).

	test(jaccard_recommender_catalog_extension_variable_model, error(instantiation_error)) :-
		jaccard_recommender::extend_catalog(_, [], _).

	test(jaccard_recommender_catalog_extension_invalid_model, error(domain_error(recommender, bad))) :-
		jaccard_recommender::extend_catalog(bad, [], _).

	test(jaccard_recommender_catalog_extension_variable_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, _, _).

	test(jaccard_recommender_catalog_extension_nonlist, error(type_error(list, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, bad, _).

	test(jaccard_recommender_catalog_extension_open_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([])| _], _).

	test(jaccard_recommender_catalog_extension_improper_list, error(type_error(list, [new-features([])|bad]))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([])|bad], _).

	test(jaccard_recommender_catalog_extension_nonpair, error(type_error(pair, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [bad], _).

	test(jaccard_recommender_catalog_extension_variable_entry, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [_], _).

	test(jaccard_recommender_catalog_extension_variable_identifier, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [_-features([])], _).

	test(jaccard_recommender_catalog_extension_nonatomic_identifier, error(type_error(atomic, item(new)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [item(new)-features([])], _).

	test(jaccard_recommender_catalog_extension_collision, error(domain_error(new_catalog_item, identical))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [identical-features([a,b,c])], _).

	test(jaccard_recommender_catalog_extension_duplicate_identifier, error(domain_error(duplicate_item, new))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([]),new-features([a])], _).

	test(jaccard_recommender_catalog_extension_mixed_kinds, error(domain_error(content_representation, vector([a-1])))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([]),other-vector([a-1])], _).

	test(jaccard_recommender_catalog_extension_mismatched_kind, error(domain_error(content_representation, vector([a-1])))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-vector([a-1])], _).

	test(jaccard_recommender_catalog_extension_variable_descriptor, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-_], _).

	test(jaccard_recommender_catalog_extension_invalid_descriptor, error(domain_error(item_content, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-bad], _).

	test(jaccard_recommender_catalog_extension_nonground_feature, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::extend_catalog(Model, [new-features([genre(_)])], _).

	test(jaccard_recommender_catalog_extension_nonbinary_weight, error(domain_error(binary_weight, 0.5))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1])]), Model),
		jaccard_recommender::extend_catalog(Model, [new-vector([a-0.5])], _).

	test(jaccard_recommender_batch_reference_scores, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [partial,identical,partial,subset,empty],
			[partial-First,identical-Identical,partial-Repeat,subset-Subset,empty-Empty]),
		Third is 1 / 3,
		assertion(First =~= 0.25),
		assertion(Identical =~= 1.0),
		assertion(Repeat =~= 0.25),
		assertion(Subset =~= Third),
		assertion(Empty =~= 0.0).

	test(jaccard_recommender_batch_matches_individual_scores, deterministic(Pairs == Individual)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [partial,identical,subset], Pairs),
		jaccard_recommender::score(Model, u, partial, Partial),
		jaccard_recommender::score(Model, u, identical, Identical),
		jaccard_recommender::score(Model, u, subset, Subset),
		Individual = [partial-Partial,identical-Identical,subset-Subset].

	test(jaccard_recommender_batch_rated_items, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [liked_x,disliked], [liked_x-Liked,disliked-Disliked]),
		assertion(Liked =~= 0.6666666666666666),
		assertion(Disliked =~= 0.0).

	test(jaccard_recommender_batch_empty, deterministic(Scores == [])) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [], Scores).

	test(jaccard_recommender_batch_unknown_user, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, unknown, [empty,identical], [empty-Empty,identical-Identical]),
		assertion(Empty =~= 0.0),
		assertion(Identical =~= 0.0).

	test(jaccard_recommender_batch_empty_profile, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(6)]),
		jaccard_recommender::score_all(Model, u, [empty,identical], [empty-Empty,identical-Identical]),
		assertion(Empty =~= 0.0),
		assertion(Identical =~= 0.0).

	test(jaccard_recommender_batch_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::score_all(Model, u, [partial,identical,partial], _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_batch_empty_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::score_all(Model, u, [], []),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_batch_implemented_locally, deterministic) :-
		jaccard_recommender::predicate_property(score_all(_, _, _, _), defined_in(jaccard_recommender)).

	test(jaccard_recommender_batch_variable_model, error(instantiation_error)) :-
		jaccard_recommender::score_all(_, u, [], _).

	test(jaccard_recommender_batch_invalid_model, error(domain_error(recommender, bad))) :-
		jaccard_recommender::score_all(bad, u, [], _).

	test(jaccard_recommender_batch_variable_user, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, _, [], _).

	test(jaccard_recommender_batch_nonatomic_user, error(type_error(atomic, user(u)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, user(u), [], _).

	test(jaccard_recommender_batch_variable_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, _, _).

	test(jaccard_recommender_batch_nonlist, error(type_error(list, items))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, items, _).

	test(jaccard_recommender_batch_improper_list, error(type_error(list, [identical|bad]))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [identical|bad], _).

	test(jaccard_recommender_batch_open_list, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [identical|_], _).

	test(jaccard_recommender_batch_variable_item, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [identical,_], _).

	test(jaccard_recommender_batch_nonatomic_item, error(type_error(atomic, item(x)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [identical,item(x)], _).

	test(jaccard_recommender_batch_unknown_item, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, u, [missing,identical], _).

	test(jaccard_recommender_batch_late_unknown_item, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_all(Model, unknown, [identical,missing], _).

	test(jaccard_recommender_content_reference_scores, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features([c,d]), Partial),
		jaccard_recommender::score_content(Model, u, features([a,b,c]), Identical),
		jaccard_recommender::score_content(Model, u, features([a]), Subset),
		jaccard_recommender::score_content(Model, u, features([new]), Disjoint),
		jaccard_recommender::score_content(Model, u, features([]), Empty),
		Third is 1 / 3,
		assertion(Partial =~= 0.25),
		assertion(Identical =~= 1.0),
		assertion(Subset =~= Third),
		assertion(Disjoint =~= 0.0),
		assertion(Empty =~= 0.0).

	test(jaccard_recommender_content_matches_known_item, deterministic(ContentScore =~= CatalogScore)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, partial, CatalogScore),
		jaccard_recommender::score_content(Model, u, features([c,d]), ContentScore).

	test(jaccard_recommender_content_duplicate_order_invariance, deterministic(First =~= Second)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features([d,c,c]), First),
		jaccard_recommender::score_content(Model, u, features([c,d]), Second),
		assertion(First =~= 0.25).

	test(jaccard_recommender_content_vector_after_feature_training, deterministic(Score =~= 0.25)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([d-1.0,c-1,z-0.0]), Score).

	test(jaccard_recommender_content_features_after_vector_training, deterministic(Score =~= 0.3333333333333333)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1,b-1])]), Model),
		jaccard_recommender::score_content(Model, u, features([b,c]), Score).

	test(jaccard_recommender_content_novel_features_in_union, deterministic(Score =~= 0.5)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features([a,b,c,new,other,extra]), Score).

	test(jaccard_recommender_content_compound_features, deterministic(Score =~= 0.3333333333333333)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x],
			[x-features([genre(action),language(en)])]), Model),
		jaccard_recommender::score_content(Model, u, features([genre(action),language(fr)]), Score).

	test(jaccard_recommender_content_zero_vector, deterministic(Score =~= 0.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-0,b-0.0]), Score).

	test(jaccard_recommender_content_unknown_user, deterministic(Score =~= 0.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, unknown, features([a,b,c]), Score).

	test(jaccard_recommender_content_empty_profile, deterministic(Score =~= 0.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(6)]),
		jaccard_recommender::score_content(Model, u, features([a,b,c]), Score).

	test(jaccard_recommender_content_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		jaccard_validation_counter::learn(Dataset, Model),
		jaccard_validation_counter::reset_validation_count,
		jaccard_validation_counter::score_content(Model, u, features([c,d]), _),
		jaccard_validation_counter::validation_count(Count).

	test(jaccard_recommender_scoring_preserves_model_and_catalog, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		copy_term(Model, Copy),
		jaccard_recommender::recommend(Model, u, 10, Before),
		jaccard_recommender::score_all(Model, u, [identical,partial], _),
		jaccard_recommender::score_content(Model, u, features([a,b,c,new]), Score),
		assertion(Score =~= 0.75),
		assertion(lgtunit::variant(Model, Copy)),
		jaccard_recommender::recommend(Model, u, 10, After),
		assertion(Before == After).

	test(jaccard_recommender_content_scoring_does_not_add_identifier, error(domain_error(catalog_item, new))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features([new]), _),
		jaccard_recommender::score(Model, u, new, _).

	test(jaccard_recommender_content_implemented_locally, deterministic) :-
		jaccard_recommender::predicate_property(score_content(_, _, _, _), defined_in(jaccard_recommender)).

	test(jaccard_recommender_content_variable_model, error(instantiation_error)) :-
		jaccard_recommender::score_content(_, u, features([]), _).

	test(jaccard_recommender_content_invalid_model, error(domain_error(recommender, bad))) :-
		jaccard_recommender::score_content(bad, u, features([]), _).

	test(jaccard_recommender_content_variable_user, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, _, features([]), _).

	test(jaccard_recommender_content_nonatomic_user, error(type_error(atomic, user(u)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, user(u), features([]), _).

	test(jaccard_recommender_content_variable_descriptor, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, _, _).

	test(jaccard_recommender_content_unknown_descriptor, error(domain_error(item_content, words([a])))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, words([a]), _).

	test(jaccard_recommender_content_atom_descriptor, error(domain_error(item_content, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, bad, _).

	test(jaccard_recommender_content_improper_features, error(type_error(list, [a|bad]))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features([a|bad]), _).

	test(jaccard_recommender_content_open_features, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features([a|_]), _).

	test(jaccard_recommender_content_variable_features, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features(_), _).

	test(jaccard_recommender_content_nonground_feature, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, features([genre(_)]), _).

	test(jaccard_recommender_content_rejection_preserves_input, variant(Content, Copy)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		Content = features([genre(_)]),
		copy_term(Content, Copy),
		catch(jaccard_recommender::score_content(Model, u, Content, _), error(instantiation_error, _), true).

	test(jaccard_recommender_content_duplicate_vector_keys, error(domain_error(duplicate_feature, a))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-0,a-1]), _).

	test(jaccard_recommender_content_nonpair_entry, error(type_error(pair, a))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a]), _).

	test(jaccard_recommender_content_variable_entry, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([_]), _).

	test(jaccard_recommender_content_nonground_vector_key, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([genre(_)-1]), _).

	test(jaccard_recommender_content_improper_vector, error(type_error(list, [a-1|bad]))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-1|bad]), _).

	test(jaccard_recommender_content_open_vector, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-1|_]), _).

	test(jaccard_recommender_content_variable_weight, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-_]), _).

	test(jaccard_recommender_content_nonnumeric_weight, error(type_error(number, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-bad]), _).

	test(jaccard_recommender_content_negative_weight, error(domain_error(non_negative_finite_weight, -1))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-(-1)]), _).

	test(jaccard_recommender_content_nonbinary_weight, error(domain_error(binary_weight, 2))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, u, vector([a-2]), _).

	test(jaccard_recommender_content_fractional_weight_unknown_user, error(domain_error(binary_weight, 0.5))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score_content(Model, unknown, vector([a-0.5]), _).

	test(jaccard_recommender_full_catalog_recommendation, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, u, 10, [identical-Identical,subset-Subset,partial-Partial,empty-Empty,disjoint-Disjoint]),
		Third is 1 / 3,
		assertion(Identical =~= 1.0),
		assertion(Subset =~= Third),
		assertion(Partial =~= 0.25),
		assertion(Empty =~= 0.0),
		assertion(Disjoint =~= 0.0).

	test(jaccard_recommender_binary_vectors, deterministic(Score =~= 1.0)) :-
		Dataset = jaccard_dataset([rating(u,x,5)], [x,y], [x-vector([a-1.0,b-0.0]),y-vector([a-1,b-0])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, y, Score).

	test(jaccard_recommender_nonbinary_weight, error(domain_error(binary_weight, 2))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,5)], [x], [x-vector([a-2])]), _).

	test(jaccard_recommender_score_implemented_locally, deterministic) :-
		jaccard_recommender::predicate_property(score(_, _, _, _), defined_in(jaccard_recommender)).

	test(jaccard_recommender_default_options, deterministic(Model == Explicit)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::learn(Dataset, Explicit, [positive_threshold(user_mean)]),
		jaccard_recommender::valid_recommender(Model).

	test(jaccard_recommender_public_option_hooks, deterministic) :-
		jaccard_recommender::default_option(positive_threshold(user_mean)),
		jaccard_recommender::valid_option(positive_threshold(4)),
		jaccard_recommender::default_option(min_feature_support(1)),
		jaccard_recommender::valid_option(min_feature_support(2)).

	test(jaccard_recommender_duplicate_options_first_wins, deterministic(Score =~= 0.25)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(6),positive_threshold(1)]),
		jaccard_recommender::recommender_options(Model, Options),
		assertion(Options == [positive_threshold(6),positive_threshold(1),min_feature_support(1)]),
		jaccard_recommender::score(Model, u, identical, Zero),
		assertion(Zero =~= 0.0),
		jaccard_recommender::learn(Dataset, All, [positive_threshold(1),positive_threshold(6)]),
		jaccard_recommender::score(All, u, disliked, Score).

	test(jaccard_recommender_threshold_equality, deterministic(Score =~= 1.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(4)]),
		jaccard_recommender::score(Model, u, identical, Score).

	test(jaccard_recommender_numeric_threshold, deterministic(Score =~= 0.6666666666666666)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(5)]),
		jaccard_recommender::score(Model, u, identical, Score).

	test(jaccard_recommender_no_selected_items, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model, [positive_threshold(6)]),
		jaccard_recommender::score(Model, u, identical, Score),
		assertion(Score =~= 0.0),
		jaccard_recommender::recommend(Model, u, 10, [subset-Subset,partial-Partial,identical-Identical,empty-Empty,disjoint-Disjoint]),
		assertion(Subset =~= 0.0),
		assertion(Partial =~= 0.0),
		assertion(Identical =~= 0.0),
		assertion(Empty =~= 0.0),
		assertion(Disjoint =~= 0.0).

	test(jaccard_recommender_per_user_thresholds, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,1),rating(u,y,2),rating(v,x,5),rating(v,y,4)], [x,y], [x-features([a]),y-features([b])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, y, User),
		jaccard_recommender::score(Model, v, x, Other),
		jaccard_recommender::score(Model, u, x, Rejected),
		assertion(User =~= 1.0),
		assertion(Other =~= 1.0),
		assertion(Rejected =~= 0.0).

	test(jaccard_recommender_duplicate_feature_invariance, deterministic(Vectors == [x-[a-1,b-1],y-[a-1,b-1]])) :-
		Dataset = jaccard_dataset([rating(u,x,5)], [x,y], [x-features([b,a,a]),y-features([a,b,b])]),
		jaccard_recommender::learn(Dataset, Model),
		Model = jaccard_model(_, [x-features([a,a,b]),y-features([a,b,b])], Vectors, _, _, _),
		jaccard_recommender::score(Model, u, y, Score),
		assertion(Score =~= 1.0).

	test(jaccard_recommender_descriptor_equivalence, deterministic(FeatureScores == VectorScores)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,5)], [x,y], [x-features([a,b,a]),y-features([b,c])]), Features),
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,5)], [x,y], [x-vector([b-1.0,a-1,z-0]),y-vector([c-1,b-1])]), Vectors),
		jaccard_recommender::recommend(Features, u, 10, FeatureScores),
		jaccard_recommender::recommend(Vectors, u, 10, VectorScores).

	test(jaccard_recommender_compound_features, deterministic(Score =~= 0.3333333333333333)) :-
		Dataset = jaccard_dataset([rating(u,x,0)], [x,y], [x-features([genre(action),language(en)]),y-features([genre(action),language(fr)])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, y, Score).

	test(jaccard_recommender_negative_ratings, deterministic(Score =~= 1.0)) :-
		Dataset = jaccard_dataset([rating(u,x,-2),rating(u,y,0)], [x,y], [x-features([a]),y-features([b])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, y, Score).

	test(jaccard_recommender_all_empty_contents, deterministic) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x,y], [x-features([]),y-features([])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, y, Score),
		assertion(Score =~= 0.0),
		jaccard_recommender::recommend(Model, u, 1, [y-Relevance]),
		assertion(Relevance =~= 0.0).

	test(jaccard_recommender_empty_binary_vectors, deterministic(Score =~= 0.0)) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x,y], [x-vector([a-0]),y-vector([])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, y, Score).

	test(jaccard_recommender_unknown_user, deterministic(Score =~= 0.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, unknown, identical, Score).

	test(jaccard_recommender_unknown_user_ties, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, unknown, 2, [subset-First,partial-Second]),
		assertion(First =~= 0.0),
		assertion(Second =~= 0.0).

	test(jaccard_recommender_no_candidates, deterministic(Recommendations == [])) :-
		Dataset = jaccard_dataset([rating(u,x,1)], [x], [x-features([a])]),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, u, 1, Recommendations).

	test(jaccard_recommender_top_one, deterministic(Score =~= 1.0)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, u, 1, [identical-Score]).

	test(jaccard_recommender_unknown_item, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, missing, _).

	test(jaccard_recommender_variable_user, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, _, identical, _).

	test(jaccard_recommender_variable_item, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, _, _).

	test(jaccard_recommender_nonatomic_user, error(type_error(atomic, user(u)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, user(u), identical, _).

	test(jaccard_recommender_nonatomic_item, error(type_error(atomic, item(x)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::score(Model, u, item(x), _).

	test(jaccard_recommender_variable_model, error(instantiation_error)) :-
		jaccard_recommender::score(_, u, x, _).

	test(jaccard_recommender_invalid_model, error(domain_error(recommender, bad))) :-
		jaccard_recommender::recommend(bad, u, 1, _).

	test(jaccard_recommender_variable_n, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, u, _, _).

	test(jaccard_recommender_noninteger_n, error(type_error(integer, 1.5))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, u, 1.5, _).

	test(jaccard_recommender_nonpositive_n, error(domain_error(positive_integer, 0))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, u, 0, _).

	test(jaccard_recommender_recommend_variable_user, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, _, 1, _).

	test(jaccard_recommender_recommend_nonatomic_user, error(type_error(atomic, user(u)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::recommend(Model, user(u), 1, _).

	test(jaccard_recommender_variable_options, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, _).

	test(jaccard_recommender_nonlist_options, error(type_error(list, options))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, options).

	test(jaccard_recommender_variable_option, error(instantiation_error)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [_]).

	test(jaccard_recommender_invalid_option, error(domain_error(option, profile_weighting(rating)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [profile_weighting(rating)]).

	test(jaccard_recommender_invalid_threshold, error(domain_error(option, positive_threshold(bad)))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [positive_threshold(bad)]).

	test(jaccard_recommender_noncompound_option, error(type_error(compound, bad))) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, _, [bad]).

	test(jaccard_recommender_fractional_weight, error(domain_error(binary_weight, 0.5))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-0.5])]), _).

	test(jaccard_recommender_negative_weight, error(domain_error(non_negative_finite_weight, -1))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-(-1)])]), _).

	test(jaccard_recommender_nonnumeric_weight, error(type_error(number, bad))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-bad])]), _).

	test(jaccard_recommender_variable_weight, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-_])]), _).

	test(jaccard_recommender_duplicate_vector_key, error(domain_error(duplicate_feature, a))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-0,a-1])]), _).

	test(jaccard_recommender_nonpair_entry, error(type_error(pair, a))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a])]), _).

	test(jaccard_recommender_variable_entry, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([_])]), _).

	test(jaccard_recommender_nonground_feature, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-features([category(_)])]), _).

	test(jaccard_recommender_nonground_vector_key, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([category(_)-1])]), _).

	test(jaccard_recommender_improper_features, error(type_error(list, [a|bad]))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-features([a|bad])]), _).

	test(jaccard_recommender_improper_vector, error(type_error(list, [a-1|bad]))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-vector([a-1|bad])]), _).

	test(jaccard_recommender_variable_features, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-features(_)]), _).

	test(jaccard_recommender_variable_descriptor, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-_]), _).

	test(jaccard_recommender_invalid_descriptor, error(domain_error(item_content, words([a])))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-words([a])]), _).

	test(jaccard_recommender_mixed_descriptors, error(domain_error(content_representation, vector([a-1])))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x,y],
			[x-features([a]),y-vector([a-1])]), _).

	test(jaccard_recommender_empty_catalog, error(domain_error(non_empty_catalog, _))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [], []), _).

	test(jaccard_recommender_duplicate_catalog_item, error(domain_error(duplicate_item, x))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x,x], [x-features([])]), _).

	test(jaccard_recommender_duplicate_content, error(domain_error(duplicate_item, x))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-features([]),x-features([])]), _).

	test(jaccard_recommender_missing_content, error(domain_error(item_content_coverage, _))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x,y], [x-features([])]), _).

	test(jaccard_recommender_no_content, error(domain_error(item_content_coverage, _))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], []), _).

	test(jaccard_recommender_extra_content, error(domain_error(item_content_coverage, _))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [x], [x-features([]),y-features([])]), _).

	test(jaccard_recommender_rated_item_missing, error(domain_error(catalog_item, x))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [y], [y-features([])]), _).

	test(jaccard_recommender_variable_catalog_item, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [_], [x-features([])]), _).

	test(jaccard_recommender_nonatomic_catalog_item, error(type_error(atomic, item(x)))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1)], [item(x)], [item(x)-features([])]), _).

	test(jaccard_recommender_empty_ratings, error(domain_error(non_empty_ratings, _))) :-
		jaccard_recommender::learn(jaccard_dataset([], [x], [x-features([])]), _).

	test(jaccard_recommender_duplicate_ratings, error(domain_error(duplicate_rating, u-x))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,1),rating(u,x,2)], [x], [x-features([])]), _).

	test(jaccard_recommender_variable_rating_user, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_dataset([rating(_,x,1)], [x], [x-features([])]), _).

	test(jaccard_recommender_nonatomic_rating_item, error(type_error(atomic, item(x)))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,item(x),1)], [x], [x-features([])]), _).

	test(jaccard_recommender_nonnumeric_rating, error(type_error(number, bad))) :-
		jaccard_recommender::learn(jaccard_dataset([rating(u,x,bad)], [x], [x-features([])]), _).

	test(jaccard_recommender_inconsistent_count, error(consistency_error(rating_count, 9, 1))) :-
		jaccard_recommender::learn(jaccard_count_fixture(9), _).

	test(jaccard_recommender_variable_count, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_count_fixture(_), _).

	test(jaccard_recommender_noninteger_count, error(type_error(integer, bad))) :-
		jaccard_recommender::learn(jaccard_count_fixture(bad), _).

	test(jaccard_recommender_nonpositive_count, error(domain_error(positive_integer, 0))) :-
		jaccard_recommender::learn(jaccard_count_fixture(0), _).

	test(jaccard_recommender_scale_not_clipping, deterministic(Score =~= 1.0)) :-
		jaccard_recommender::learn(jaccard_scale_fixture(-2, 2), Model),
		jaccard_recommender::score(Model, u, x, Score).

	test(jaccard_recommender_variable_scale, error(instantiation_error)) :-
		jaccard_recommender::learn(jaccard_scale_fixture(_, 5), _).

	test(jaccard_recommender_nonnumeric_scale, error(type_error(number, bad))) :-
		jaccard_recommender::learn(jaccard_scale_fixture(bad, 5), _).

	test(jaccard_recommender_reversed_scale, error(domain_error(rating_scale, 5-1))) :-
		jaccard_recommender::learn(jaccard_scale_fixture(5, 1), _).

	test(jaccard_recommender_outside_scale, error(domain_error(rating_scale(2, 5), 1))) :-
		jaccard_recommender::learn(jaccard_scale_fixture(2, 5), _).

	test(jaccard_recommender_partial_model, variant(Model, Copy)) :-
		Model = jaccard_model(_, _, _, _, _, _),
		copy_term(Model, Copy),
		\+ jaccard_recommender::valid_recommender(Model).

	test(jaccard_recommender_tampered_vectors, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,_,Profiles,Scale,Diagnostics)),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,[],Profiles,Scale,Diagnostics)).

	test(jaccard_recommender_tampered_profiles, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,_,Scale,Diagnostics)),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,Vectors,[u-[]],Scale,Diagnostics)).

	test(jaccard_recommender_noncanonical_contents, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		reverse(Contents, Other),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Other,Vectors,Profiles,Scale,Diagnostics)).

	test(jaccard_recommender_noncanonical_ratings, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		reverse(Ratings, Other),
		jaccard_recommender::valid_recommender(jaccard_model(Other,Contents,Vectors,Profiles,Scale,Diagnostics)).

	test(jaccard_recommender_tampered_scale, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,_,Diagnostics)),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,Vectors,Profiles,scale(1,2),Diagnostics)).

	test(jaccard_recommender_tampered_diagnostics, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		append(Diagnostics, [item_count(99)], Other),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Other)).

	test(jaccard_recommender_missing_options, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,_)),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,
			[model(jaccard_recommender),rating_count(3),options([])])).

	test(jaccard_recommender_invalid_stored_rating, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(_,Contents,Vectors,Profiles,Scale,Diagnostics)),
		jaccard_recommender::valid_recommender(jaccard_model([rating(u,liked_x,bad)],Contents,Vectors,Profiles,Scale,Diagnostics)).

	test(jaccard_recommender_nonbinary_stored_content, fail) :-
		jaccard_recommender::learn(jaccard_count_fixture(1), jaccard_model(Ratings,_,Vectors,Profiles,Scale,Diagnostics)),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,[x-vector([x-2])],Vectors,Profiles,Scale,Diagnostics)).

	test(jaccard_recommender_empty_stored_contents, fail) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,_,Vectors,Profiles,Scale,Diagnostics)),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,[],Vectors,Profiles,Scale,Diagnostics)).

	test(jaccard_recommender_extra_diagnostics, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Diagnostics)),
		append(Diagnostics, [note(extra)], Other),
		jaccard_recommender::valid_recommender(jaccard_model(Ratings,Contents,Vectors,Profiles,Scale,Other)).

	test(jaccard_recommender_diagnostics, deterministic(Enumerated == Diagnostics)) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::diagnostics(Model, Diagnostics),
		memberchk(user_count(1), Diagnostics),
		memberchk(item_count(8), Diagnostics),
		memberchk(feature_count(5), Diagnostics),
		memberchk(non_empty_profile_count(1), Diagnostics),
		memberchk(content_representation(features), Diagnostics),
		findall(Term, jaccard_recommender::diagnostic(Model, Term), Enumerated).

	test(jaccard_recommender_export_clause, deterministic(Clauses == [saved(Model)])) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::export_to_clauses(Dataset, Model, saved, Clauses).

	test(jaccard_recommender_export_round_trip, deterministic) :-
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		^^file_path('jaccard_saved.pl', File),
		jaccard_recommender::export_to_file(Dataset, Model, jaccard_saved, File),
		logtalk_load(File),
		{jaccard_saved(Loaded)},
		assertion(Model == Loaded),
		jaccard_recommender::valid_recommender(Loaded),
		jaccard_recommender::score(Loaded, u, identical, Score),
		assertion(Score =~= 1.0),
		jaccard_recommender::recommend(Model, u, 10, Recommendations),
		jaccard_recommender::recommend(Loaded, u, 10, Recommendations).

	test(jaccard_recommender_print, deterministic) :-
		^^suppress_text_output,
		feature_dataset(Dataset),
		jaccard_recommender::learn(Dataset, Model),
		jaccard_recommender::print_recommender(Model).

	% auxiliary predicates

	feature_dataset(
		jaccard_dataset(
			[rating(u,liked_x,5),rating(u,liked_y,4),rating(u,disliked,1)],
			[liked_x,liked_y,disliked,identical,subset,partial,disjoint,empty],
			[
				liked_x-features([a,b]),liked_y-features([b,c]),disliked-features([z]),
				identical-features([a,b,c]),subset-features([a]),partial-features([c,d]),
				disjoint-features([z]),empty-features([])
			]
		)
	).

:- end_object.

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
		^^clean_file('tfidf_saved.pl').

	test(uniform_centroid, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, x, Score),
		Expected is 1 / sqrt(2),
		assertion(Score =~= Expected),
		tfidf_recommender::score(Model, u, diagonal, Diagonal),
		assertion(Diagonal =~= 1.0).

	test(weighted_centroid, deterministic) :-
		vector_dataset(2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(2), profile_weighting(rating)]),
		tfidf_recommender::score(Model, u, x, ScoreX),
		tfidf_recommender::score(Model, u, y, ScoreY),
		ExpectedX is 1 / sqrt(5), ExpectedY is 2 / sqrt(5),
		assertion(ScoreX =~= ExpectedX),
		assertion(ScoreY =~= ExpectedY).

	test(tfidf_weights, deterministic) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first, second],
			[first-features([a,a,b]), second-features([b,c])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, [first-[a-FirstA,b-FirstB], second-[b-SecondB,c-SecondC]], _, _, _, _),
			[normalization(none)]),
		IDF is log(3 / 2) + 1, ExpectedA is 2 * IDF,
		assertion(FirstA =~= ExpectedA),
		assertion(FirstB =~= 1.0),
		assertion(SecondB =~= 1.0),
		assertion(SecondC =~= IDF).

	test(unrated_catalog_recommendation, deterministic(Score =~= 1.0)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, u, 10, [diagonal-Score]).

	test(default_positive_threshold, deterministic(Score =~= 0.0)) :-
		vector_dataset(2, 4, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, x, Score).

	test(per_user_mean_thresholds, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,1),rating(u,y,2),rating(v,x,5),rating(v,y,4)],
			[x,y], [x-vector([x-1]),y-vector([y-1])]),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, y, UserScore),
		tfidf_recommender::score(Model, v, x, OtherScore),
		tfidf_recommender::score(Model, u, x, RejectedScore),
		assertion(UserScore =~= 1.0),
		assertion(OtherScore =~= 1.0),
		assertion(RejectedScore =~= 0.0).

	test(threshold_equality, deterministic(Score =~= 1.0)) :-
		vector_dataset(2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(4)]),
		tfidf_recommender::score(Model, u, y, Score).

	test(no_selected_items, deterministic) :-
		vector_dataset(2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(5)]),
		tfidf_recommender::score(Model, u, diagonal, Score),
		assertion(Score =~= 0.0),
		tfidf_recommender::recommend(Model, u, 3, [diagonal-Zero]),
		assertion(Zero =~= 0.0).

	test(unknown_user_zero_ties, deterministic) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, unknown, 10, [y-ScoreY,x-ScoreX,diagonal-ScoreDiagonal]),
		assertion(ScoreY =~= 0.0),
		assertion(ScoreX =~= 0.0),
		assertion(ScoreDiagonal =~= 0.0).

	test(no_candidates, deterministic(Recommendations == [])) :-
		Dataset = tfidf_dataset([rating(u, x, 5)], [x], [x-vector([x-1])]),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, u, 5, Recommendations).

	test(unknown_item, error(domain_error(catalog_item, missing))) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, missing, _).

	test(empty_content, deterministic(Score =~= 0.0)) :-
		Dataset = tfidf_dataset([rating(u, x, 5)], [x,y], [x-vector([]),y-vector([])]),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, u, y, Score).

	test(empty_selected_vector_denominator, deterministic) :-
		Dataset = tfidf_dataset([rating(u, x, 5),rating(u, y, 5)], [x,y], [x-vector([x-1]), y-vector([])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, _, [u-[x-Weight]], _, _, _)),
		assertion(Weight =~= 0.5).

	test(normalization_effect, deterministic) :-
		Dataset = tfidf_dataset([rating(u, x, 5),rating(u, y, 5)], [x,y], [x-vector([x-10]), y-vector([y-1])]),
		tfidf_recommender::learn(Dataset, Normalized),
		tfidf_recommender::learn(Dataset, Raw, [normalization(none)]),
		tfidf_recommender::score(Normalized, u, x, Equal),
		tfidf_recommender::score(Raw, u, x, Dominant),
		ExpectedEqual is 1 / sqrt(2), ExpectedDominant is 10 / sqrt(101),
		assertion(Equal =~= ExpectedEqual),
		assertion(Dominant =~= ExpectedDominant).

	test(large_vector_values, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, x, 1)], [x], [x-vector([x-1.0e200,y-1.0e200])]),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, u, x, Score).

	test(large_unnormalized_centroid, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, x, 1),rating(u, y, 1)], [x,y], [x-vector([f-1.0e300]),y-vector([f-1.0e300])]),
		tfidf_recommender::learn(Dataset, Model, [normalization(none)]),
		tfidf_recommender::score(Model, u, x, Score).

	test(feature_l2_normalization, deterministic) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, Model),
		Model = tfidf_model(_, _, [first-[a-WeightA,b-WeightB]| _], _, _, _, _),
		IDF is log(3 / 2) + 1,
		Norm is sqrt(4 * IDF * IDF + 1),
		ExpectedA is 2 * IDF / Norm,
		ExpectedB is 1 / Norm,
		assertion(WeightA =~= ExpectedA),
		assertion(WeightB =~= ExpectedB).

	test(unrated_documents_affect_idf, true) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second,third], [first-features([a,a,b]), second-features([b,c]), third-features([a])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, _, _, Vectorizer, _, _)),
		Vectorizer = text_vectorizer_model([feature(a,2,IDF)| _], _),
		Expected is log(4 / 3) + 1,
		assertion(IDF =~= Expected),
		text_vectorizer::diagnostic(Vectorizer, document_count(3)).

	test(empty_documents_count_in_idf, deterministic) :-
		Dataset = tfidf_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-features([])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, _, _, _, Vectorizer, _, _)),
		Vectorizer = text_vectorizer_model([feature(a,1,IDF)], _),
		Expected is log(3 / 2) + 1,
		assertion(IDF =~= Expected).

	test(binary_tags, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second], [first-features([tag,tag]),second-features([tag])]),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([weighting(binary)])]),
		tfidf_recommender::score(Model, u, second, Score).

	test(compound_features, deterministic(Score =~= 1.0)) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second], [first-features([genre(action)]),second-features([genre(action)])]),
		tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, second, Score).

	test(classic_zero_weights_vocabulary, true) :-
		Dataset = tfidf_dataset([rating(u, first, 5)], [first,second], [first-features([a,b]),second-features([a,c])]),
		tfidf_recommender::learn(Dataset, Model, [vectorizer_options([idf(classic)])]),
		tfidf_recommender::diagnostic(Model, feature_count(3)).

	test(repeated_top_level_options, deterministic(Score =~= 0.0)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model, [positive_threshold(9),positive_threshold(1)]),
		tfidf_recommender::score(Model, u, diagonal, Score).

	test(repeated_nested_options, deterministic) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, Model, [normalization(none),vectorizer_options([weighting(binary),weighting(count)])]),
		Model = tfidf_model(_, _, [first-[a-WeightA,b-WeightB]| _], _, _, _, _),
		assertion(WeightA =~= 1.0),
		assertion(WeightB =~= 1.0),
		tfidf_recommender::valid_recommender(Model).

	test(input_order_independence, deterministic(Model == Other)) :-
		vector_dataset(2, 4, tfidf_dataset(Ratings, Items, Contents)),
		reverse(Ratings, ReversedRatings),
		reverse(Items, ReversedItems),
		reverse(Contents, ReversedContents),
		tfidf_recommender::learn(tfidf_dataset(Ratings, Items, Contents), Model),
		tfidf_recommender::learn(tfidf_dataset(ReversedRatings, ReversedItems, ReversedContents), Other).

	test(features_order_independence, deterministic(Model == Other)) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, Model),
		OtherDataset = tfidf_dataset([rating(u, first, 5)], [second,first], [second-features([c,b]),first-features([b,a,a])]),
		tfidf_recommender::learn(OtherDataset, Other).

	test(vector_canonicalization, deterministic) :-
		Dataset = tfidf_dataset([rating(u, x, 5)], [x], [x-vector([z-0,b-2,a-1])]),
		tfidf_recommender::learn(Dataset, tfidf_model(_, [x-vector([a-1,b-2])], _, _, _, _, _)).

	test(negative_ratings_uniform, deterministic(Score =~= 1.0)) :-
		vector_dataset(-4, -2, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::score(Model, u, y, Score).

	test(negative_unselected_rating_weighted, deterministic(Score =~= 1.0)) :-
		vector_dataset(-2, 4, Dataset),
		tfidf_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		tfidf_recommender::score(Model, u, y, Score).

	test(selected_nonpositive_rating, error(domain_error(positive_rating_weight, 0))) :-
		vector_dataset(0, 0, Dataset), tfidf_recommender::learn(Dataset, _, [profile_weighting(rating)]).

	test(empty_catalog, error(domain_error(non_empty_catalog, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [], []), _).

	test(missing_all_content, error(domain_error(item_content_coverage, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], []), _).

	test(missing_content, error(domain_error(item_content_coverage, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x,y], [x-vector([])]), _).

	test(extra_content, error(domain_error(item_content_coverage, _))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-vector([]),y-vector([])]), _).

	test(duplicate_item, error(domain_error(duplicate_item, x))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x,x], [x-vector([])]), _).

	test(duplicate_content, error(domain_error(duplicate_item, x))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-vector([]),x-vector([])]), _).

	test(rated_item_outside_catalog, error(domain_error(catalog_item, y))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,y,1)], [x], [x-vector([])]), _).

	test(variable_item, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [_], [x-vector([])]), _).

	test(compound_item, error(type_error(atomic, item(x)))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [item(x)], [x-vector([])]), _).

	test(mixed_representations, error(domain_error(content_representation, vector([])))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-vector([])]), _).

	test(invalid_descriptor, error(domain_error(item_content, bad))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-bad]), _).

	test(variable_descriptor, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-_]), _).

	test(variable_features, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([_])]), _).

	test(improper_features, error(type_error(list, [a|bad]))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([a|bad])]), _).

	test(no_vocabulary, error(domain_error(non_empty_vocabulary, [[]]))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1)], [x], [x-features([])]), _).

	test(duplicate_feature, error(domain_error(duplicate_feature, a))) :-
		bad_vector([a-1,a-2], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(nonnumeric_weight, error(type_error(number, bad))) :-
		bad_vector([a-bad], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(negative_weight, error(domain_error(non_negative_finite_weight, -1))) :-
		bad_vector([a-(-1)], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(variable_weight, error(instantiation_error)) :-
		bad_vector([a-_], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(variable_feature_key, error(instantiation_error)) :-
		bad_vector([_-1], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(nonpair_entry, error(type_error(pair, bad))) :-
		bad_vector([bad], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(variable_entry, error(instantiation_error)) :-
		bad_vector([_], Dataset),
		tfidf_recommender::learn(Dataset, _).

	test(vectorizer_options_on_vectors, error(domain_error(option, vectorizer_options([idf(classic)])))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, _, [vectorizer_options([idf(classic)])]).

	test(nested_normalization, error(domain_error(option, vectorizer_options([normalization(l2)])))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, _, [vectorizer_options([normalization(l2)])]).

	test(invalid_option, error(domain_error(option, profile_weighting(bad)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, _, [profile_weighting(bad)]).

	test(options_variable, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, _, _).

	test(query_variable, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, _, x, _).

	test(query_compound, error(type_error(atomic, user(u)))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::score(Model, user(u), x, _).

	test(n_variable, error(instantiation_error)) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::recommend(Model, u, _, _).

	test(n_noninteger, error(type_error(integer, bad))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::recommend(Model, u, bad, _).

	test(n_nonpositive, error(domain_error(positive_integer, 0))) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, Model), tfidf_recommender::recommend(Model, u, 0, _).

	test(scale_bounds, error(domain_error(rating_scale, 5-1))) :-
		tfidf_recommender::learn(tfidf_scale_fixture(5, 1), _).

	test(out_of_scale_rating, error(domain_error(rating_scale(2,5), 1))) :-
		tfidf_recommender::learn(tfidf_scale_fixture(2, 5), _).

	test(valid_scale, deterministic(Score =~= 1.0)) :-
		tfidf_recommender::learn(tfidf_scale_fixture(1, 5), Model),
		tfidf_recommender::score(Model, u, x, Score).

	test(incomplete_model, variant(Model, Copy)) :-
		Model = tfidf_model(_,_,_,_,_,_,_), copy_term(Model, Copy),
		\+ tfidf_recommender::valid_recommender(Model).

	test(tampered_profiles, fail) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,_,Vectorizer,Scale,Diagnostics)),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,[],Vectorizer,Scale,Diagnostics)).

	test(tampered_vectors, fail) :-
		vector_dataset(3, 3, Dataset),
		tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,_,Profiles,Vectorizer,Scale,Diagnostics)),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,[],Profiles,Vectorizer,Scale,Diagnostics)).

	test(tampered_vectorizer, fail) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,_,Scale,Diagnostics)),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,Profiles,none,Scale,Diagnostics)).

	test(tampered_diagnostics, fail) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Diagnostics)),
		append(Diagnostics, [item_count(99)], Other),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Other)).

	test(extra_diagnostics, deterministic) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Diagnostics)),
		append(Diagnostics, [note(extra)], Other),
		tfidf_recommender::valid_recommender(tfidf_model(Ratings,Contents,Vectors,Profiles,Vectorizer,Scale,Other)).

	test(diagnostics, deterministic(Diagnostics == Enumerated)) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::diagnostics(Model, Diagnostics),
		memberchk(item_count(3), Diagnostics),
		memberchk(user_count(1), Diagnostics),
		memberchk(feature_count(2), Diagnostics),
		memberchk(non_empty_profile_count(1), Diagnostics),
		findall(Diagnostic, tfidf_recommender::diagnostic(Model, Diagnostic), Enumerated).

	test(export_round_trip, deterministic) :-
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

	test(export_clause, deterministic(Clauses == [saved(Model)])) :-
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::export_to_clauses(Dataset, Model, saved, Clauses).

	test(print, deterministic) :-
		^^suppress_text_output,
		vector_dataset(3, 3, Dataset), tfidf_recommender::learn(Dataset, Model), tfidf_recommender::print_recommender(Model).

	test(score_implemented_locally, deterministic) :-
		tfidf_recommender::predicate_property(score(_, _, _, _), defined_in(tfidf_recommender)).

	test(variable_model, error(instantiation_error)) :-
		tfidf_recommender::score(_, u, x, _).

	test(invalid_model, error(domain_error(recommender, bad))) :-
		tfidf_recommender::recommend(bad, u, 1, _).

	test(duplicate_ratings, error(domain_error(duplicate_rating, u-x))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,1),rating(u,x,2)], [x], [x-vector([])]), _).

	test(empty_ratings, error(domain_error(non_empty_ratings, _))) :-
		tfidf_recommender::learn(tfidf_dataset([], [x], [x-vector([])]), _).

	test(nonnumeric_rating, error(type_error(number, bad))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(u,x,bad)], [x], [x-vector([])]), _).

	test(variable_rating_identifier, error(instantiation_error)) :-
		tfidf_recommender::learn(tfidf_dataset([rating(_,x,1)], [x], [x-vector([])]), _).

	test(compound_rating_identifier, error(type_error(atomic, user(u)))) :-
		tfidf_recommender::learn(tfidf_dataset([rating(user(u),x,1)], [x], [x-vector([])]), _).

	test(inconsistent_rating_count, error(consistency_error(rating_count, 9, 1))) :-
		tfidf_recommender::learn(tfidf_count_fixture, _).

	test(score_recommend_agreement, deterministic) :-
		feature_dataset(Dataset), tfidf_recommender::learn(Dataset, Model),
		tfidf_recommender::recommend(Model, u, 1, [second-Recommended]),
		tfidf_recommender::score(Model, u, second, Direct),
		assertion(Recommended =~= Direct).

	test(inconsistent_frequency_bounds, error(domain_error(option, minimum_document_frequency(2)))) :-
		feature_dataset(Dataset),
		tfidf_recommender::learn(Dataset, _, [vectorizer_options([minimum_document_frequency(2),maximum_document_frequency(1)])]).

	test(frequency_filter_empty_vocabulary, error(domain_error(non_empty_vocabulary, _))) :-
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

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
		date is 2026-10-05,
		comment is 'Unit tests for the "bm25_recommender" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2, append/3, reverse/2
	]).

	cover(bm25_recommender).

	cleanup :-
		^^clean_file('bm25_saved.pl').

	test(bm25_recommender_numerical_bm25_scores, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score(Model, u, x, Same),
		bm25_recommender::score(Model, u, y, Other),
		ExpectedSame is 2 * log(1.6) * 4.4 / 3.2,
		ExpectedOther is 2 * log(1.6) * 2.2 / 3.1,
		assertion(Same =~= ExpectedSame),
		assertion(Other =~= ExpectedOther),
		assertion(Same > 1),
		assertion(bm25_recommender::valid_recommender(Model)).

	test(bm25_recommender_corpus_counts, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, bm25_model(_,_,_,[u-[a-Query]],bm25_corpus(3,Average,Statistics),none,_)),
		assertion(Query =~= 2.0),
		assertion(Average =~= 2.0),
		memberchk(a-statistics(2,IDF), Statistics),
		Expected is log(1.6),
		assertion(IDF =~= Expected).

	test(bm25_recommender_count_vector_equivalence, deterministic) :-
		feature_dataset(Features),
		vector_dataset(Vectors),
		bm25_recommender::learn(Features, FeatureModel),
		bm25_recommender::learn(Vectors, VectorModel),
		bm25_recommender::score(FeatureModel, u, y, First),
		bm25_recommender::score(VectorModel, u, y, Second),
		assertion(First =~= Second).

	test(bm25_recommender_empty_catalog_content, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1)], [x,y], [x-features([]),y-features([])]),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score(Model, u, y, Score),
		assertion(Score =~= 0.0),
		assertion(Model = bm25_model(_,_,[x-[],y-[]],[u-[]],bm25_corpus(2,_,[]),none,_)),
		assertion(bm25_recommender::valid_recommender(Model)).

	test(bm25_recommender_k1_zero, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model, [k1(0)]),
		bm25_recommender::score(Model, u, x, First),
		bm25_recommender::score(Model, u, y, Second),
		Expected is 2 * log(1.6),
		assertion(First =~= Expected),
		assertion(Second =~= Expected).

	test(bm25_recommender_b_zero, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model, [b(0)]),
		bm25_recommender::score(Model, u, y, Score),
		Expected is 2 * log(1.6),
		assertion(Score =~= Expected).

	test(bm25_recommender_recommendation_exclusion, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::recommend(Model, u, 10, [y-Score,empty-Zero]),
		assertion(Score > 0),
		assertion(Zero =~= 0.0).

	test(bm25_recommender_count_requires_integer, error(type_error(integer, 1.5))) :-
		bm25_recommender::learn(bm25_dataset([rating(u,x,1)], [x], [x-vector([a-1.5])]), _).

	test(bm25_recommender_missing_item, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score(Model, unknown, missing, _).

	test(bm25_recommender_parameter_formula_matrix, deterministic) :-
		feature_dataset(Dataset),
		forall(
			(	member(K1, [0,0.5,1.2,2]),
				member(B, [0,0.75,1])
			),
			(	bm25_recommender::learn(Dataset, Model, [k1(K1),b(B)]),
				bm25_recommender::score(Model, u, x, Same),
				bm25_recommender::score(Model, u, y, Other),
				ExpectedSame is 2 * log(1.6) * 2 * (K1 + 1) / (2 + K1),
				ExpectedOther is 2 * log(1.6) * (K1 + 1) / (1 + K1 * (1 + B)),
				assertion(Same =~= ExpectedSame),
				assertion(Other =~= ExpectedOther),
				assertion(bm25_recommender::valid_recommender(Model))
			)
		).

	test(bm25_recommender_raw_count_profiles, deterministic) :-
		feature_dataset(bm25_dataset(_,Items,Contents)),
		Dataset = bm25_dataset([rating(u,x,2),rating(u,y,4)], Items, Contents),
		bm25_recommender::learn(Dataset, bm25_model(_,_,_,[u-[a-MeanA,z-MeanZ]],_,_,_), [positive_threshold(0)]),
		assertion(MeanA =~= 1.5),
		assertion(MeanZ =~= 1.5),
		bm25_recommender::learn(Dataset, Weighted, [positive_threshold(0),profile_weighting(rating)]),
		Weighted = bm25_model(_,_,_,[u-[a-RatingA,z-RatingZ]],_,_,_),
		ExpectedA is 4 / 3,
		assertion(RatingA =~= ExpectedA),
		assertion(RatingZ =~= 2.0),
		bm25_recommender::score(Weighted, u, x, Score),
		ExpectedScore is ExpectedA * log(1.6) * 4.4 / 3.2,
		assertion(Score =~= ExpectedScore).

	test(bm25_recommender_empty_selected_document_denominator, deterministic) :-
		feature_dataset(bm25_dataset(_,Items,Contents)),
		Dataset = bm25_dataset([rating(u,x,2),rating(u,empty,4)], Items, Contents),
		bm25_recommender::learn(Dataset, bm25_model(_,_,_,[u-[a-Coefficient]],_,_,_), [positive_threshold(0),profile_weighting(rating)]),
		Expected is 2 / 3,
		assertion(Coefficient =~= Expected).

	test(bm25_recommender_mean_and_threshold_equality, deterministic) :-
		feature_dataset(bm25_dataset(_,Items,Contents)),
		Dataset = bm25_dataset([rating(u,x,2),rating(u,y,4)], Items, Contents),
		bm25_recommender::learn(Dataset, Mean),
		bm25_recommender::learn(Dataset, Threshold, [positive_threshold(4)]),
		Mean = bm25_model(_,_,Weights,Profiles,Corpus,Scale,_),
		Threshold = bm25_model(_,_,Weights,Profiles,Corpus,Scale,_),
		assertion(Profiles == [u-[a-1.0,z-3.0]]).

	test(bm25_recommender_unknown_user_zero_ties, deterministic(Recommendations == [y-0.0,x-0.0,empty-0.0])) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::recommend(Model, unknown, 10, Recommendations).

	test(bm25_recommender_empty_profile, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model, [positive_threshold(6)]),
		bm25_recommender::score(Model, u, x, Score),
		assertion(Score =~= 0.0),
		bm25_recommender::recommend(Model, u, 3, Recommendations),
		assertion(Recommendations == [y-0.0,empty-0.0]).

	test(bm25_recommender_disjoint_and_empty_content, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1)], [x,y,empty], [x-features([a]),y-features([b]),empty-features([])]),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score(Model, u, y, Disjoint),
		bm25_recommender::score(Model, u, empty, Empty),
		assertion(Disjoint =~= 0.0),
		assertion(Empty =~= 0.0).

	test(bm25_recommender_compound_and_numeric_features, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1)], [x,y], [x-features([tag(a),1,1.0]),y-features([1.0,tag(a)])]),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score(Model, u, y, Score),
		IDF is log(1.2),
		Expected is 2 * IDF * 2.2 / (1 + 1.2 * (0.25 + 0.75 * 2 / 2.5)),
		assertion(Score =~= Expected).

	test(bm25_recommender_zeros_and_empty_count_space, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1)], [x,y], [x-vector([a-0,b-0.0]),y-vector([])]),
		bm25_recommender::learn(Dataset, Model),
		Model = bm25_model(_,Contents,_,_,bm25_corpus(2,Average,[]),_,_),
		assertion(Contents == [x-vector([]),y-vector([])]),
		assertion(Average =~= 0.0),
		assertion(bm25_recommender::valid_recommender(Model)).

	test(bm25_recommender_repeated_options_first_wins, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model, [k1(0),k1(2),b(0),b(1)]),
		bm25_recommender::score(Model, u, x, First),
		bm25_recommender::score(Model, u, y, Second),
		assertion(First =~= Second),
		assertion(bm25_recommender::valid_recommender(Model)),
		bm25_recommender::recommender_options(Model, Options),
		assertion(memberchk(k1(2), Options)),
		assertion(memberchk(b(1), Options)).

	test(bm25_recommender_option_hooks_public, deterministic) :-
		bm25_recommender::valid_option(k1(0)),
		bm25_recommender::valid_option(b(1)),
		bm25_recommender::default_option(k1(K1)),
		assertion(K1 =~= 1.2).

	test(bm25_recommender_saturation_and_length_effects, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1)], [x,y,z], [x-vector([a-1]),y-vector([a-10]),z-vector([a-1,b-9])]),
		bm25_recommender::learn(Dataset, Model, [b(0)]),
		bm25_recommender::score(Model, u, x, First),
		bm25_recommender::score(Model, u, y, Repeated),
		assertion(Repeated > First),
		assertion(Repeated < 10 * First),
		bm25_recommender::learn(Dataset, LengthModel, [b(1)]),
		bm25_recommender::score(LengthModel, u, x, Short),
		bm25_recommender::score(LengthModel, u, z, Long),
		assertion(Short > Long).

	test(bm25_recommender_selected_nonpositive_weight, error(domain_error(positive_rating_weight, 0))) :-
		bm25_recommender::learn(bm25_dataset([rating(u,x,0)], [x], [x-vector([])]), _, [profile_weighting(rating)]).

	test(bm25_recommender_unselected_nonpositive_weight, deterministic) :-
		feature_dataset(bm25_dataset(_,Items,Contents)),
		Dataset = bm25_dataset([rating(u,x,1),rating(u,y,-1)], Items, Contents),
		bm25_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		assertion(bm25_recommender::valid_recommender(Model)).

	test(bm25_recommender_invalid_options, deterministic) :-
		feature_dataset(Dataset),
		forall(
			member(Option, [k1(-1),k1(_),b(-1),b(1.1),b(_),profile_weighting(bad),positive_threshold(bad),normalization(l2),vectorizer_options([]),delta(1)]),
			(	catch(bm25_recommender::learn(Dataset, _, [Option]), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, domain_error(option,Option)))
			)
		).

	test(bm25_recommender_content_validation, deterministic) :-
		forall(
			member(Content-Expected, [
				_-instantiation_error,
				bad-domain_error(item_content,bad),
				features(bad)-type_error(list,bad),
				features([_])-instantiation_error,
				vector([a-1.0])-type_error(integer,1.0),
				vector([a- -1])-domain_error(non_negative_finite_weight,-1),
				vector([a-bad])-type_error(number,bad),
				vector([a-_])-instantiation_error,
				vector([a-0,a-1])-domain_error(duplicate_feature,a),
				vector([bad])-type_error(pair,bad)
			]),
			(	Dataset = bm25_dataset([rating(u,x,1)], [x], [x-Content]),
				catch(bm25_recommender::learn(Dataset, _), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_dataset_validation, deterministic) :-
		forall(
			member(Dataset-Expected, [
				bm25_dataset([], [x], [x-features([a])])-domain_error(non_empty_ratings,_),
				bm25_dataset([rating(u,x,1),rating(u,x,2)], [x], [x-features([a])])-domain_error(duplicate_rating,u-x),
				bm25_dataset([rating(u,x,1)], [], [])-domain_error(non_empty_catalog,_),
				bm25_dataset([rating(u,x,1)], [x,x], [x-features([a])])-domain_error(duplicate_item,x),
				bm25_dataset([rating(u,x,1)], [x], [])-domain_error(item_content_coverage,_),
				bm25_dataset([rating(u,missing,1)], [x], [x-features([a])])-domain_error(catalog_item,missing),
				bm25_dataset([rating(u,x,1)], [x,y], [x-features([a]),y-vector([a-1])])-domain_error(content_representation,vector([a-1]))
			]),
			(	catch(bm25_recommender::learn(Dataset, _), error(Error,_), Caught = Error),
				assertion(Caught = Expected)
			)
		).

	test(bm25_recommender_scale_and_ratings, deterministic) :-
		bm25_recommender::learn(bm25_scale_fixture(1,5), Model),
		assertion(Model = bm25_model(_,_,_,_,_,scale(1,5),_)),
		assertion(bm25_recommender::valid_recommender(Model)).

	test(bm25_recommender_invalid_scale, error(domain_error(rating_scale, 5-1))) :-
		bm25_recommender::learn(bm25_scale_fixture(5,1), _).

	test(bm25_recommender_outside_scale, error(domain_error(rating_scale(2,5), 1))) :-
		bm25_recommender::learn(bm25_scale_fixture(2,5), _).

	test(bm25_recommender_query_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Goal-Expected, [
				score(_,u,x,_)-instantiation_error,
				score(bad,u,x,_)-domain_error(recommender,bad),
				score(Model,_,x,_)-instantiation_error,
				score(Model,u,_,_)-instantiation_error,
				score(Model,user(u),x,_)-type_error(atomic,user(u)),
				score(Model,u,item(x),_)-type_error(atomic,item(x)),
				recommend(Model,u,0,_)-domain_error(positive_integer,0),
				recommend(Model,u,_,_)-instantiation_error,
				recommend(Model,u,bad,_)-type_error(integer,bad)
			]),
			(	catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_model_tampering, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics)),
		Corpus = bm25_corpus(N,Average,Statistics),
		forall(
			member(Bad, [
				bm25_model([],Contents,Weights,Profiles,Corpus,Scale,Diagnostics),
				bm25_model(Ratings,[],Weights,Profiles,Corpus,Scale,Diagnostics),
				bm25_model(Ratings,Contents,[],Profiles,Corpus,Scale,Diagnostics),
				bm25_model(Ratings,Contents,Weights,[],Corpus,Scale,Diagnostics),
				bm25_model(Ratings,Contents,Weights,Profiles,bm25_corpus(9,Average,Statistics),Scale,Diagnostics),
				bm25_model(Ratings,Contents,Weights,Profiles,bm25_corpus(N,0.0,Statistics),Scale,Diagnostics),
				bm25_model(Ratings,Contents,Weights,Profiles,bm25_corpus(N,Average,[]),Scale,Diagnostics),
				bm25_model(Ratings,Contents,Weights,Profiles,Corpus,scale(0,1),Diagnostics),
				bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,[])
			]),
			assertion(\+ bm25_recommender::valid_recommender(Bad))
		).

	test(bm25_recommender_invalid_model_preserves_variables, deterministic) :-
		Model = bm25_model(_,_,_,_,_,_,_),
		copy_term(Model, Original),
		assertion(\+ bm25_recommender::valid_recommender(Model)),
		assertion(lgtunit::variant(Model, Original)).

	test(bm25_recommender_diagnostic_validation_and_extras, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics)),
		Model = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,[note(custom)|Diagnostics]),
		assertion(bm25_recommender::valid_recommender(Model)),
		assertion(\+ bm25_recommender::valid_recommender(bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,[item_count(3)|Diagnostics]))),
		Partial = [model(bm25_recommender),rating_count(1),options([k1(1.2),b(0.75)]),user_count(1)],
		assertion(\+ bm25_recommender::valid_recommender(bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Partial))).

	test(bm25_recommender_export_round_trip, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::export_to_clauses(Dataset, Model, saved, Clauses),
		assertion(Clauses == [saved(Model)]),
		^^file_path('bm25_saved.pl', File),
		bm25_recommender::export_to_file(Dataset, Model, bm25_saved, File),
		logtalk_load(File),
		{bm25_saved(Loaded)},
		assertion(Loaded == Model),
		assertion(bm25_recommender::valid_recommender(Loaded)).

	test(bm25_recommender_print_model, deterministic) :-
		^^suppress_text_output,
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::print_recommender(Model).

	test(bm25_recommender_core_predicate_ownership, deterministic) :-
		bm25_recommender::predicate_property(score(_,_,_,_), defined_in(bm25_recommender)),
		bm25_recommender::predicate_property(recommend(_,_,_,_), defined_in(bm25_recommender)).

	test(bm25_recommender_batch_scores, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score_all(Model, u, [y,x,y,empty], [y-Other,x-Same,y-Repeat,empty-Zero]),
		ExpectedOther is 2 * log(1.6) * 2.2 / 3.1,
		ExpectedSame is 2 * log(1.6) * 4.4 / 3.2,
		assertion(Other =~= ExpectedOther),
		assertion(Same =~= ExpectedSame),
		assertion(Repeat =~= Other),
		assertion(Zero =~= 0.0),
		bm25_recommender::score(Model, u, y, Individual),
		assertion(Individual =~= Other).

	test(bm25_recommender_batch_empty, deterministic(Scores == [])) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score_all(Model, u, [], Scores).

	test(bm25_recommender_batch_unknown_user, deterministic(Scores == [x-0.0,y-0.0])) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score_all(Model, unknown, [x,y], Scores).

	test(bm25_recommender_batch_validates_once, deterministic) :-
		feature_dataset(Dataset),
		bm25_validation_counter::learn(Dataset, Model),
		forall(
			member(Items, [[],[y,x,y]]),
			(	bm25_validation_counter::reset_validation_count,
				bm25_validation_counter::score_all(Model, u, Items, _),
				bm25_validation_counter::validation_count(Count),
				assertion(Count == 1)
			)
		).

	test(bm25_recommender_batch_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Goal-Expected, [
				score_all(_,u,[],_)-instantiation_error,
				score_all(bad,u,[],_)-domain_error(recommender,bad),
				score_all(Model,_,[],_)-instantiation_error,
				score_all(Model,user(u),[],_)-type_error(atomic,user(u)),
				score_all(Model,u,_,_)-instantiation_error,
				score_all(Model,u,[x|_],_)-instantiation_error,
				score_all(Model,u,[x|bad],_)-type_error(list,[x|bad]),
				score_all(Model,u,bad,_)-type_error(list,bad),
				score_all(Model,unknown,[x,_],_)-instantiation_error,
				score_all(Model,u,[item(x)],_)-type_error(atomic,item(x)),
				score_all(Model,unknown,[x,missing],_)-domain_error(catalog_item,missing),
				score_all(Model,u,[missing,x],_)-domain_error(catalog_item,missing)
			]),
			(	catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_batch_implemented_locally, deterministic) :-
		bm25_recommender::predicate_property(score_all(_,_,_,_), defined_in(bm25_recommender)).

	test(bm25_recommender_content_known_item_agreement, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score(Model, u, y, Catalog),
		bm25_recommender::score_content(Model, u, features([z,a,z,z]), Supplied),
		assertion(Supplied =~= Catalog).

	test(bm25_recommender_content_unseen_padding_penalty, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		bm25_recommender::score_content(Model, u, features([a,z,z,z]), Unpadded),
		bm25_recommender::score_content(Model, u, features([a,z,z,z,novel,novel,novel]), Padded),
		Expected is 2 * log(1.6) * 2.2 / (1 + 1.2 * (0.25 + 0.75 * 7 / 2)),
		assertion(Padded =~= Expected),
		assertion(Padded < Unpadded),
		assertion(lgtunit::variant(Model, Original)).

	test(bm25_recommender_content_no_length_penalty_at_b_zero, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model, [b(0)]),
		bm25_recommender::score_content(Model, u, features([a]), First),
		bm25_recommender::score_content(Model, u, features([a,unseen,unseen]), Padded),
		assertion(Padded =~= First).

	test(bm25_recommender_content_count_vector_length, deterministic) :-
		vector_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score_content(Model, u, vector([unseen-3,z-3,a-1,zero-0.0]), Score),
		Expected is 2 * log(1.6) * 2.2 / (1 + 1.2 * (0.25 + 0.75 * 7 / 2)),
		assertion(Score =~= Expected).

	test(bm25_recommender_content_empty_and_unknown, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score_content(Model, u, features([]), Empty),
		bm25_recommender::score_content(Model, u, features([novel]), Unseen),
		bm25_recommender::score_content(Model, unknown, features([a]), Unknown),
		assertion(Empty =~= 0.0),
		assertion(Unseen =~= 0.0),
		assertion(Unknown =~= 0.0).

	test(bm25_recommender_content_all_empty_trained_corpus, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1)], [x], [x-features([])]),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::score_content(Model, u, features([novel]), Score),
		assertion(Score =~= 0.0).

	test(bm25_recommender_content_validates_once, deterministic(Count == 1)) :-
		feature_dataset(Dataset),
		bm25_validation_counter::learn(Dataset, Model),
		bm25_validation_counter::reset_validation_count,
		bm25_validation_counter::score_content(Model, u, features([a]), _),
		bm25_validation_counter::validation_count(Count).

	test(bm25_recommender_content_query_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Goal-Expected, [
				score_content(_,u,features([]),_)-instantiation_error,
				score_content(bad,u,features([]),_)-domain_error(recommender,bad),
				score_content(Model,_,features([]),_)-instantiation_error,
				score_content(Model,user(u),features([]),_)-type_error(atomic,user(u)),
				score_content(Model,u,_,_)-instantiation_error,
				score_content(Model,u,bad,_)-domain_error(item_content,bad),
				score_content(Model,unknown,features([_]),_)-instantiation_error,
				score_content(Model,u,features([a|bad]),_)-type_error(list,[a|bad]),
				score_content(Model,u,features([a|_]),_)-instantiation_error,
				score_content(Model,u,vector([]),_)-domain_error(content_representation,vector([]))
			]),
			(	catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_content_vector_validation, deterministic) :-
		vector_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Content-Expected, [
				features([])-domain_error(content_representation,features([])),
				vector([a-1.5])-type_error(integer,1.5),
				vector([a-1.0])-type_error(integer,1.0),
				vector([a- -1])-domain_error(non_negative_finite_weight,-1),
				vector([a-_])-instantiation_error,
				vector([a-bad])-type_error(number,bad),
				vector([a-0,a-1])-domain_error(duplicate_feature,a),
				vector([bad])-type_error(pair,bad),
				vector([tag(_)-1])-instantiation_error
			]),
			(	catch(bm25_recommender::score_content(Model, unknown, Content, _), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_content_implemented_locally, deterministic) :-
		bm25_recommender::predicate_property(score_content(_,_,_,_), defined_in(bm25_recommender)).

	test(bm25_recommender_update_matches_fresh_training, deterministic) :-
		feature_dataset(bm25_dataset(Ratings,Items,Contents)),
		bm25_recommender::learn(bm25_dataset(Ratings,Items,Contents), Model),
		bm25_recommender::update_ratings(Model, [rating(u,x,1),rating(u,y,5),rating(v,x,4)], Updated),
		bm25_recommender::learn(bm25_dataset([rating(u,x,1),rating(u,y,5),rating(v,x,4)], Items, Contents), Fresh),
		assertion(Updated == Fresh),
		Model = bm25_model(_,OriginalContents,Weights,_,Corpus,Scale,_),
		Updated = bm25_model(_,OriginalContents,Weights,_,Corpus,Scale,_),
		assertion(bm25_recommender::valid_recommender(Updated)).

	test(bm25_recommender_update_empty, deterministic(Updated == Model)) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::update_ratings(Model, [], Updated).

	test(bm25_recommender_update_duplicate, error(domain_error(duplicate_rating, u-x))) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::update_ratings(Model, [rating(u,x,1),rating(u,x,5)], _).

	test(bm25_recommender_update_mean_shift_weight_error, error(domain_error(positive_rating_weight, 0))) :-
		Dataset = bm25_dataset([rating(u,x,1),rating(u,y,0)], [x,y], [x-features([a]),y-features([])]),
		bm25_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		bm25_recommender::update_ratings(Model, [rating(u,x,-1)], _).

	test(bm25_recommender_removal_fresh_equivalence_and_retry, deterministic) :-
		feature_dataset(bm25_dataset(_,Items,Contents)),
		bm25_recommender::learn(bm25_dataset([rating(u,x,5),rating(u,y,4),rating(v,y,1)], Items, Contents), Model),
		bm25_recommender::remove_ratings(Model, [u-x,u-x,missing-unknown], Removed),
		bm25_recommender::learn(bm25_dataset([rating(u,y,4),rating(v,y,1)], Items, Contents), Fresh),
		assertion(Removed == Fresh),
		bm25_recommender::remove_ratings(Removed, [u-x], Retry),
		assertion(Retry == Removed),
		bm25_recommender::recommend(Removed, u, 10, [x-Score,empty-Zero]),
		assertion(Score > 0),
		assertion(Zero =~= 0.0).

	test(bm25_recommender_removal_missing_pairs, deterministic(Removed == Model)) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::remove_ratings(Model, [u-y,unknown-x,unknown-missing], Removed).

	test(bm25_recommender_removal_empty, deterministic(Removed == Model)) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::remove_ratings(Model, [], Removed).

	test(bm25_recommender_removal_last_global_rating, error(domain_error(non_empty_ratings, []))) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::remove_ratings(Model, [u-x,u-x], _).

	test(bm25_recommender_removal_weighted_mean_shift, error(domain_error(positive_rating_weight, 0))) :-
		Dataset = bm25_dataset([rating(u,x,1),rating(u,y,0),rating(v,x,1)], [x,y], [x-features([a]),y-features([])]),
		bm25_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		bm25_recommender::remove_ratings(Model, [u-x], _).

	test(bm25_recommender_removal_user_disappearance, deterministic) :-
		feature_dataset(bm25_dataset(_,Items,Contents)),
		bm25_recommender::learn(bm25_dataset([rating(u,x,5),rating(v,y,1)], Items, Contents), Model),
		bm25_recommender::remove_ratings(Model, [u-x], Removed),
		Removed = bm25_model(_,_,_,Profiles,_,_,Diagnostics),
		assertion(\+ member(u-_, Profiles)),
		assertion(memberchk(user_count(1), Diagnostics)),
		bm25_recommender::recommend(Removed, u, 3, Recommendations),
		assertion(Recommendations == [y-0.0,x-0.0,empty-0.0]),
		bm25_recommender::recommend(Removed, v, 3, Other),
		assertion(\+ member(y-_, Other)).

	test(bm25_recommender_feedback_equivalence_matrix, deterministic) :-
		feature_dataset(FeatureDataset),
		vector_dataset(VectorDataset),
		forall(
			(	member(bm25_dataset(Ratings,Items,Contents), [FeatureDataset,VectorDataset]),
				member(Weighting, [uniform,rating]),
				member(Threshold, [user_mean,0])
			),
			(	Options = [profile_weighting(Weighting),positive_threshold(Threshold),k1(2),b(1)],
				bm25_recommender::learn(bm25_dataset(Ratings,Items,Contents), Model, Options),
				bm25_recommender::update_ratings(Model, [rating(u,x,2),rating(u,y,4),rating(v,y,3)], Updated),
				bm25_recommender::learn(bm25_dataset([rating(u,x,2),rating(u,y,4),rating(v,y,3)], Items, Contents), Fresh, Options),
				assertion(Updated == Fresh),
				bm25_recommender::remove_ratings(Updated, [u-y], Removed),
				bm25_recommender::learn(bm25_dataset([rating(u,x,2),rating(v,y,3)], Items, Contents), Remaining, Options),
				assertion(Removed == Remaining),
				Model = bm25_model(_,OriginalContents,Weights,_,Corpus,Scale,_),
				Removed = bm25_model(_,OriginalContents,Weights,_,Corpus,Scale,_),
				assertion(bm25_recommender::valid_recommender(Removed))
			)
		).

	test(bm25_recommender_feedback_diagnostics_and_extras, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics)),
		reverse(Diagnostics, Reversed),
		append([note(before)|Reversed], [extra(after)], Extras),
		Model = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Extras),
		bm25_recommender::update_ratings(Model, [rating(v,y,3)], Updated),
		Updated = bm25_model(_,_,_,_,_,_,UpdatedDiagnostics),
		assertion(memberchk(rating_count(2), UpdatedDiagnostics)),
		assertion(memberchk(user_count(2), UpdatedDiagnostics)),
		assertion(memberchk(non_empty_profile_count(2), UpdatedDiagnostics)),
		assertion(UpdatedDiagnostics = [note(before)|_]),
		assertion(append(_, [extra(after)], UpdatedDiagnostics)),
		bm25_recommender::remove_ratings(Updated, [v-y], Restored),
		assertion(Restored == Model),
		assertion(bm25_recommender::valid_recommender(Updated)).

	test(bm25_recommender_feedback_order_and_immutability, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		bm25_recommender::update_ratings(Model, [rating(u,x,5)], Identical),
		assertion(Identical == Model),
		Updates = [rating(u,x,2),rating(u,y,4),rating(v,y,3)],
		reverse(Updates, Reversed),
		bm25_recommender::update_ratings(Model, Updates, First),
		bm25_recommender::update_ratings(Model, Reversed, Second),
		assertion(First == Second),
		bm25_recommender::remove_ratings(First, [u-y,v-y], Removed),
		bm25_recommender::remove_ratings(Second, [v-y,u-y,u-y], Other),
		assertion(Removed == Other),
		assertion(ground(Removed)),
		assertion(lgtunit::variant(Model, Original)).

	test(bm25_recommender_feedback_validates_once, deterministic) :-
		feature_dataset(Dataset),
		bm25_validation_counter::learn(Dataset, Model),
		forall(
			member(Goal, [update_ratings(Model,[],_),update_ratings(Model,[rating(v,y,3)],_),remove_ratings(Model,[],_),remove_ratings(Model,[u-y,missing-unknown],_)]),
			(	bm25_validation_counter::reset_validation_count,
				bm25_validation_counter::Goal,
				bm25_validation_counter::validation_count(Count),
				assertion(Count == 1)
			)
		).

	test(bm25_recommender_feedback_scale_boundaries, deterministic) :-
		bm25_recommender::learn(bm25_scale_fixture(1,5), Model),
		bm25_recommender::update_ratings(Model, [rating(u,x,5),rating(v,x,1)], Updated),
		bm25_recommender::remove_ratings(Updated, [u-x], Removed),
		assertion(Removed = bm25_model([rating(v,x,1)],_,_,_,_,scale(1,5),_)),
		assertion(bm25_recommender::valid_recommender(Removed)),
		forall(
			member(Value, [0,6]),
			(	catch(bm25_recommender::update_ratings(Model, [rating(v,x,Value)], _), error(Error,_), Caught = Error),
				assertion(Caught == domain_error(rating_scale(1,5),Value))
			)
		).

	test(bm25_recommender_feedback_model_and_list_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			(	member(Method, [update_ratings,remove_ratings]),
				member(Input-Expected, [
					_-instantiation_error,
					bad-type_error(list,bad),
					[_|_]-instantiation_error,
					[x|bad]-type_error(list,[x|bad])
				])
			),
			(	Goal =.. [Method,Model,Input,_],
				catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		),
		forall(
			member(Goal-Expected, [
				update_ratings(_,[],_)-instantiation_error,
				remove_ratings(_,[],_)-instantiation_error,
				update_ratings(bad,[],_)-domain_error(recommender,bad),
				remove_ratings(bad,[],_)-domain_error(recommender,bad)
			]),
			(	catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_update_record_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Updates-Expected, [
				[_]-instantiation_error,
				[u-x]-type_error(rating,u-x),
				[rating(_,x,1)]-instantiation_error,
				[rating(u,_,1)]-instantiation_error,
				[rating(user(u),x,1)]-type_error(atomic,user(u)),
				[rating(u,item(x),1)]-type_error(atomic,item(x)),
				[rating(u,x,_)]-instantiation_error,
				[rating(u,x,bad)]-type_error(number,bad),
				[rating(u,missing,1)]-domain_error(catalog_item,missing),
				[rating(v,y,1),rating(v,y,1)]-domain_error(duplicate_rating,v-y)
			]),
			(	catch(bm25_recommender::update_ratings(Model, Updates, _), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_removal_pair_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Pairs-Expected, [
				[_]-instantiation_error,
				[bad]-type_error(pair,bad),
				[u-y,bad]-type_error(pair,bad),
				[_-unknown]-instantiation_error,
				[unknown-_]-instantiation_error,
				[user(u)-missing]-type_error(atomic,user(u)),
				[unknown-item(x)]-type_error(atomic,item(x))
			]),
			(	catch(bm25_recommender::remove_ratings(Model, Pairs, _), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_feedback_failed_rebuild_preserves_original, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1),rating(u,y,0),rating(v,x,1)], [x,y], [x-features([a]),y-features([])]),
		bm25_recommender::learn(Dataset, Model, [profile_weighting(rating)]),
		copy_term(Model, Original),
		catch(bm25_recommender::remove_ratings(Model, [u-x], _), error(domain_error(positive_rating_weight,0),_), Caught = yes),
		assertion(Caught == yes),
		assertion(lgtunit::variant(Model, Original)),
		assertion(bm25_recommender::valid_recommender(Model)).

	test(bm25_recommender_feedback_implemented_locally, deterministic) :-
		bm25_recommender::predicate_property(update_ratings(_,_,_), defined_in(bm25_recommender)),
		bm25_recommender::predicate_property(remove_ratings(_,_,_), defined_in(bm25_recommender)).

	test(bm25_recommender_extension_refits_features, deterministic) :-
		feature_dataset(bm25_dataset(Ratings,Items,Contents)),
		bm25_recommender::learn(bm25_dataset(Ratings,Items,Contents), Model),
		bm25_recommender::score(Model, u, x, Before),
		bm25_recommender::extend_catalog(Model, [new-features([a,new,new])], Extended),
		append(Items, [new], NewItems),
		append(Contents, [new-features([a,new,new])], NewContents),
		bm25_recommender::learn(bm25_dataset(Ratings,NewItems,NewContents), Fresh),
		assertion(Extended == Fresh),
		Model = bm25_model(_,_,OldWeights,Profiles,OldCorpus,Scale,_),
		Extended = bm25_model(Ratings,_,NewWeights,Profiles,NewCorpus,Scale,Diagnostics),
		assertion(OldWeights \== NewWeights),
		assertion(OldCorpus \== NewCorpus),
		assertion(memberchk(item_count(4), Diagnostics)),
		assertion(memberchk(feature_count(3), Diagnostics)),
		assertion(memberchk(average_document_length(2.25), Diagnostics)),
		bm25_recommender::score(Extended, u, x, After),
		Expected is 2 * log(5 / 3.5) * 4.4 / (2 + 1.2 * (0.25 + 0.75 * 2 / 2.25)),
		assertion(After =~= Expected),
		assertion(After < Before),
		assertion(bm25_recommender::valid_recommender(Extended)).

	test(bm25_recommender_extension_refits_count_vectors, deterministic) :-
		vector_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::extend_catalog(Model, [new-vector([a-1,new-2,zero-0.0])], Extended),
		feature_dataset(FeatureDataset),
		bm25_recommender::learn(FeatureDataset, FeatureModel),
		bm25_recommender::extend_catalog(FeatureModel, [new-features([a,new,new])], FeatureExtended),
		Extended = bm25_model(_,_,Weights,Profiles,Corpus,_,_),
		FeatureExtended = bm25_model(_,_,Weights,Profiles,Corpus,_,_),
		bm25_recommender::score(Extended, u, new, Score),
		Expected is 2 * log(5 / 3.5) * 2.2 / (1 + 1.2 * (0.25 + 0.75 * 3 / 2.25)),
		assertion(Score =~= Expected).

	test(bm25_recommender_extension_empty, deterministic(Extended == Model)) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::extend_catalog(Model, [], Extended).

	test(bm25_recommender_extension_existing_id, error(domain_error(new_catalog_item, x))) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::extend_catalog(Model, [x-features([])], _).

	test(bm25_recommender_extension_duplicate_id, error(domain_error(duplicate_item, new))) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::extend_catalog(Model, [new-features([a]),new-features([z])], _).

	test(bm25_recommender_replacement_rated_fresh_equivalence, deterministic) :-
		feature_dataset(bm25_dataset(Ratings,Items,_)),
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::replace_content(Model, [x-features([z,z,z])], Replaced),
		bm25_recommender::learn(bm25_dataset(Ratings,Items,[x-features([z,z,z]),y-features([a,z,z,z]),empty-features([])]), Fresh),
		assertion(Replaced == Fresh),
		Replaced = bm25_model(Ratings,_,_,[u-[z-3.0]],bm25_corpus(3,Average,Statistics),_,_),
		ExpectedAverage is 7 / 3,
		assertion(Average =~= ExpectedAverage),
		assertion(memberchk(z-statistics(2,_), Statistics)),
		bm25_recommender::score(Replaced, u, y, Score),
		Expected is 3 * log(4 / 2.5) * 6.6 / (3 + 1.2 * (0.25 + 0.75 * 4 / (7 / 3))),
		assertion(Score =~= Expected),
		assertion(bm25_recommender::valid_recommender(Replaced)).

	test(bm25_recommender_replacement_unrated_refits_both_kinds, deterministic) :-
		feature_dataset(FeatureDataset),
		vector_dataset(VectorDataset),
		forall(
			member(Dataset-Descriptor, [FeatureDataset-features([a,a]),VectorDataset-vector([a-2])]),
			(	bm25_recommender::learn(Dataset, Model),
				bm25_recommender::score(Model, u, x, Before),
				bm25_recommender::replace_content(Model, [y-Descriptor], Replaced),
				Model = bm25_model(Ratings,_,OldWeights,Profiles,OldCorpus,Scale,_),
				Replaced = bm25_model(Ratings,_,Weights,Profiles,Corpus,Scale,_),
				assertion(OldCorpus \== Corpus),
				assertion(OldWeights \== Weights),
				bm25_recommender::score(Replaced, u, x, After),
				Expected is 2 * log(4 / 2.5) * 4.4 / (2 + 1.2 * (0.25 + 0.75 * 2 / (4 / 3))),
				assertion(After =~= Expected),
				assertion(After < Before),
				assertion(bm25_recommender::valid_recommender(Replaced))
			)
		).

	test(bm25_recommender_replacement_empty_and_identical, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::replace_content(Model, [], Empty),
		assertion(Empty == Model),
		bm25_recommender::replace_content(Model, [y-features([z,a,z,z])], Identical),
		assertion(Identical == Model).

	test(bm25_recommender_replacement_missing_id, error(domain_error(catalog_item, missing))) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::replace_content(Model, [missing-features([])], _).

	test(bm25_recommender_replacement_duplicate_id, error(domain_error(duplicate_item, x))) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::replace_content(Model, [x-features([a]),x-features([z])], _).

	test(bm25_recommender_catalog_fresh_equivalence_matrix, deterministic) :-
		feature_dataset(FeatureDataset),
		vector_dataset(VectorDataset),
		forall(
			(	member(data(Dataset,Addition,Replacement), [
					data(FeatureDataset,features([a,q,q]),features([q,q,a,a])),
					data(VectorDataset,vector([a-1,q-2]),vector([q-2,a-2]))
				]),
				member(K1, [0,2]),
				member(B, [0,1]),
				member(Weighting, [uniform,rating]),
				member(Threshold, [user_mean,0])
			),
			(	Dataset = bm25_dataset(Ratings,Items,Contents),
				Options = [k1(K1),b(B),profile_weighting(Weighting),positive_threshold(Threshold)],
				bm25_recommender::learn(Dataset, Model, Options),
				bm25_recommender::extend_catalog(Model, [new-Addition], Extended),
				append(Items, [new], NewItems),
				append(Contents, [new-Addition], NewContents),
				bm25_recommender::learn(bm25_dataset(Ratings,NewItems,NewContents), FreshExtended, Options),
				assertion(Extended == FreshExtended),
				bm25_recommender::replace_content(Extended, [y-Replacement], Replaced),
				Contents = [x-X,y-_,empty-Empty],
				bm25_recommender::learn(bm25_dataset(Ratings,NewItems,[x-X,y-Replacement,empty-Empty,new-Addition]), FreshReplaced, Options),
				assertion(Replaced == FreshReplaced),
				assertion(ground(Replaced)),
				assertion(bm25_recommender::valid_recommender(Replaced))
			)
		).

	test(bm25_recommender_extension_empty_document_statistics, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::extend_catalog(Model, [new-features([])], Extended),
		Model = bm25_model(Ratings,_,_,Profiles,_,Scale,_),
		Extended = bm25_model(Ratings,_,_,Profiles,bm25_corpus(4,Average,Statistics),Scale,_),
		assertion(Average =~= 1.5),
		memberchk(a-statistics(2,IDF), Statistics),
		ExpectedIDF is log(2),
		assertion(IDF =~= ExpectedIDF),
		bm25_recommender::score(Extended, u, x, Score),
		Expected is 2 * log(2) * 4.4 / (2 + 1.2 * (0.25 + 0.75 * 2 / 1.5)),
		assertion(Score =~= Expected).

	test(bm25_recommender_catalog_all_empty_transitions, deterministic) :-
		forall(
			member(Empty-NonEmpty, [features([])-features([a,a]),vector([])-vector([a-2])]),
			(	bm25_recommender::learn(bm25_dataset([rating(u,x,5)], [x], [x-Empty]), Model),
				bm25_recommender::extend_catalog(Model, [y-NonEmpty], Extended),
				Extended = bm25_model(_,_,_,[u-[]],bm25_corpus(2,Average,_),_,_),
				assertion(Average =~= 1.0),
				bm25_recommender::score(Extended, u, y, Zero),
				assertion(Zero =~= 0.0),
				bm25_recommender::replace_content(Extended, [x-NonEmpty], NonEmptyModel),
				bm25_recommender::score(NonEmptyModel, u, y, Positive),
				assertion(Positive > 0),
				bm25_recommender::replace_content(NonEmptyModel, [x-Empty,y-Empty], Restored),
				Restored = bm25_model(_,_,_,[u-[]],bm25_corpus(2,FinalAverage,[]),_,_),
				assertion(FinalAverage =~= 0.0),
				assertion(bm25_recommender::valid_recommender(Restored)),
				bm25_recommender::score_content(Restored, u, NonEmpty, FinalZero),
				assertion(FinalZero =~= 0.0)
			)
		).

	test(bm25_recommender_catalog_order_sequential_and_compound_features, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		Additions = [one-features([term(a),term(a),7]),two-features([a])],
		reverse(Additions, Reversed),
		bm25_recommender::extend_catalog(Model, Additions, Batched),
		bm25_recommender::extend_catalog(Model, Reversed, Reordered),
		assertion(Batched == Reordered),
		bm25_recommender::extend_catalog(Model, [one-features([term(a),7,term(a)])], First),
		bm25_recommender::extend_catalog(First, [two-features([a])], Sequential),
		assertion(Batched == Sequential),
		bm25_recommender::replace_content(Batched, [one-features([term(a),term(a),7]),x-features([term(a)])], Replaced),
		bm25_recommender::replace_content(Batched, [x-features([term(a)]),one-features([7,term(a),term(a)])], ReplacedOther),
		assertion(Replaced == ReplacedOther),
		bm25_recommender::score(Replaced, u, one, Score),
		assertion(Score > 0),
		assertion(lgtunit::variant(Model, Original)).

	test(bm25_recommender_catalog_metadata_options_and_scale, deterministic) :-
		Options = [k1(0),k1(2),b(1),b(0)],
		bm25_recommender::learn(bm25_scale_fixture(1,5), bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics), Options),
		reverse(Diagnostics, Reversed),
		append([note(before)|Reversed], [extra(after)], Extras),
		Model = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Extras),
		bm25_recommender::extend_catalog(Model, [new-vector([a-1,z-2])], Extended),
		bm25_recommender::replace_content(Extended, [x-vector([a-1])], Replaced),
		Replaced = bm25_model(Ratings,_,_,_,_,Scale,UpdatedDiagnostics),
		memberchk(options(Effective), Diagnostics),
		assertion(memberchk(options(Effective), UpdatedDiagnostics)),
		assertion(UpdatedDiagnostics = [note(before)|_]),
		assertion(append(_, [extra(after)], UpdatedDiagnostics)),
		assertion(memberchk(item_count(2), UpdatedDiagnostics)),
		assertion(memberchk(feature_count(2), UpdatedDiagnostics)),
		assertion(bm25_recommender::valid_recommender(Replaced)).

	test(bm25_recommender_catalog_validates_once, deterministic) :-
		feature_dataset(Dataset),
		bm25_validation_counter::learn(Dataset, Model),
		forall(
			member(Goal, [extend_catalog(Model,[],_),extend_catalog(Model,[new-features([a])],_),replace_content(Model,[],_),replace_content(Model,[x-features([z])],_)]),
			(	bm25_validation_counter::reset_validation_count,
				bm25_validation_counter::Goal,
				bm25_validation_counter::validation_count(Count),
				assertion(Count == 1)
			)
		).

	test(bm25_recommender_catalog_model_and_list_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			(	member(Method, [extend_catalog,replace_content]),
				member(Input-Expected, [_-instantiation_error,bad-type_error(list,bad),[_|_]-instantiation_error,[x|bad]-type_error(list,[x|bad])])
			),
			(	Goal =.. [Method,Model,Input,_],
				catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		),
		forall(
			member(Goal-Expected, [extend_catalog(_,[],_)-instantiation_error,replace_content(_,[],_)-instantiation_error,extend_catalog(bad,[],_)-domain_error(recommender,bad),replace_content(bad,[],_)-domain_error(recommender,bad)]),
			(	catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_catalog_descriptor_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			(	member(Method-Item, [extend_catalog-new,replace_content-x]),
				member(Contents-Expected, [
					[_]-instantiation_error,
					[bad]-type_error(pair,bad),
					[_-features([])]-instantiation_error,
					[item(x)-features([])]-type_error(atomic,item(x)),
					[Item-_]-instantiation_error,
					[Item-bad]-domain_error(item_content,bad),
					[Item-features(_)]-instantiation_error,
					[Item-features(bad)]-type_error(list,bad),
					[Item-features([_])]-instantiation_error,
					[Item-vector([])]-domain_error(content_representation,vector([])),
					[Item-features([]),z-vector([])]-domain_error(content_representation,vector([]))
				])
			),
			(	Goal =.. [Method,Model,Contents,_],
				catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_catalog_count_validation, deterministic) :-
		vector_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			(	member(Method-Item, [extend_catalog-new,replace_content-x]),
				member(Content-Expected, [
					vector(_)-instantiation_error,
					vector(bad)-type_error(list,bad),
					vector([_])-instantiation_error,
					vector([bad])-type_error(pair,bad),
					vector([_-1])-instantiation_error,
					vector([a-_])-instantiation_error,
					vector([a-bad])-type_error(number,bad),
					vector([a-(-1)])-domain_error(non_negative_finite_weight,-1),
					vector([a-1.0])-type_error(integer,1.0),
					vector([a-1.5])-type_error(integer,1.5),
					vector([a-0,a-1])-domain_error(duplicate_feature,a),
					features([])-domain_error(content_representation,features([]))
				])
			),
			(	Goal =.. [Method,Model,[Item-Content],_],
				catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_catalog_failed_update_preserves_original, deterministic) :-
		vector_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		copy_term(Model, Original),
		catch(bm25_recommender::extend_catalog(Model, [new-vector([a-1]),other-vector([b-1.5])], _), error(type_error(integer,1.5),_), Caught = yes),
		assertion(Caught == yes),
		assertion(lgtunit::variant(Model, Original)),
		assertion(bm25_recommender::valid_recommender(Model)),
		catch(bm25_recommender::score(Model, u, new, _), error(domain_error(catalog_item,new),_), Missing = yes),
		assertion(Missing == yes).

	test(bm25_recommender_composed_updates_and_export_restoration, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		bm25_recommender::extend_catalog(Model, [new-features([a,z])], Extended),
		bm25_recommender::update_ratings(Extended, [rating(u,new,4),rating(v,y,3)], Rated),
		bm25_recommender::replace_content(Rated, [new-features([z,z])], Replaced),
		bm25_recommender::remove_ratings(Replaced, [u-x,u-x,missing-unknown], Removed),
		FreshDataset = bm25_dataset([rating(u,new,4),rating(v,y,3)], [x,y,empty,new], [x-features([a,a]),y-features([a,z,z,z]),empty-features([]),new-features([z,z])]),
		bm25_recommender::learn(FreshDataset, Fresh),
		assertion(Removed == Fresh),
		bm25_recommender::score_all(Removed, u, [y,x,empty,y], Scores),
		Scores = [y-Y,x-X,empty-Zero,y-Y],
		assertion(Y > 0),
		assertion(X =~= 0.0),
		assertion(Zero =~= 0.0),
		bm25_recommender::score_content(Removed, u, features([a,z,z,z]), ContentScore),
		assertion(ContentScore =~= Y),
		bm25_recommender::recommend(Removed, u, 4, Recommendations),
		assertion(Recommendations == [y-Y,x-X,empty-Zero]),
		bm25_recommender::export_to_clauses(Dataset, Removed, saved, Clauses),
		assertion(Clauses == [saved(Removed)]),
		^^file_path('bm25_saved.pl', File),
		bm25_recommender::export_to_file(Dataset, Removed, bm25_updated, File),
		logtalk_load(File, [reload(always)]),
		{bm25_updated(Loaded)},
		assertion(Loaded == Removed),
		assertion(bm25_recommender::valid_recommender(Loaded)),
		bm25_recommender::score_all(Loaded, u, [y,x,empty,y], Scores).

	test(bm25_recommender_catalog_implemented_locally, deterministic) :-
		bm25_recommender::predicate_property(extend_catalog(_,_,_), defined_in(bm25_recommender)),
		bm25_recommender::predicate_property(replace_content(_,_,_), defined_in(bm25_recommender)).

	test(bm25_recommender_ordered_numeric_identifiers_and_features, deterministic) :-
		Dataset = bm25_dataset([rating(u,1,5)], [z,1,1.0,a], [z-features([]),1-features([8,8]),1.0-features([8.0]),a-features([8])]),
		bm25_recommender::learn(Dataset, Model, [k1(0)]),
		bm25_recommender::score_all(Model, u, [z,1.0,1,1,a,z], Scores),
		Scores = [z-0.0,1.0-0.0,1-Positive,1-Positive,a-Positive,z-0.0],
		Expected is 2 * log(5 / 2.5),
		assertion(Positive =~= Expected),
		bm25_recommender::recommend(Model, u, 4, [a-Positive,z-0.0,1.0-0.0]),
		assertion(bm25_recommender::valid_recommender(Model)).

	test(bm25_recommender_batch_original_error_order, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Items-Expected, [[missing,item(x)]-domain_error(catalog_item,missing),[item(x),missing]-type_error(atomic,item(x)),[y,missing,_]-domain_error(catalog_item,missing),[y,_,missing]-instantiation_error]),
			(	catch(bm25_recommender::score_all(Model, u, Items, _), error(Error,_), Caught = Error),
				assertion(Caught == Expected)
			)
		).

	test(bm25_recommender_duplicate_batch_workload, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		list::length(Seeds, 100),
		findall(y, member(_,Seeds), Items),
		bm25_recommender::score(Model, u, y, Score),
		findall(y-Score, member(_,Seeds), Expected),
		bm25_recommender::score_all(Model, u, Items, Scores),
		assertion(Scores == Expected).

	test(bm25_recommender_affected_profile_build_counts, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,5),rating(v,y,4),rating(w,y,3)], [x,y,empty], [x-features([a,a]),y-features([a,z]),empty-features([])]),
		bm25_validation_counter::learn(Dataset, Model),
		forall(
			member(Goal-Expected, [
				update_ratings(Model,[rating(u,y,4)],_)-4,
				update_ratings(Model,[rating(u,x,5),rating(v,y,2)],_)-4,
				update_ratings(Model,[rating(u,x,5)],_)-3,
				remove_ratings(Model,[u-x],_)-3,
				extend_catalog(Model,[new-features([a])],_)-3,
				replace_content(Model,[x-features([z])],_)-4,
				replace_content(Model,[empty-features([a])],_)-3,
				replace_content(Model,[y-features([z,a])],_)-3
			]),
			(	bm25_validation_counter::reset_validation_count,
				bm25_validation_counter::reset_profile_count,
				bm25_validation_counter::Goal,
				bm25_validation_counter::profile_count(Count),
				assertion(Count == Expected),
				bm25_validation_counter::validation_count(1)
			)
		).

	test(bm25_recommender_affected_profiles_preserve_other_users, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,5),rating(v,y,4)], [x,y], [x-vector([a-2]),y-vector([z-3])]),
		bm25_recommender::learn(Dataset, Model),
		Model = bm25_model(_,_,_,Profiles,_,_,_),
		memberchk(v-VProfile, Profiles),
		bm25_recommender::update_ratings(Model, [rating(u,y,4)], Updated),
		Updated = bm25_model(_,_,_,UpdatedProfiles,_,_,_),
		assertion(memberchk(v-VProfile, UpdatedProfiles)),
		bm25_recommender::replace_content(Updated, [x-vector([new-4])], Replaced),
		Replaced = bm25_model(_,_,_,ReplacedProfiles,_,_,_),
		assertion(memberchk(v-VProfile, ReplacedProfiles)),
		assertion(bm25_recommender::valid_recommender(Replaced)).

	test(bm25_recommender_catalog_removal_fresh_equivalence, deterministic) :-
		feature_dataset(FeatureDataset),
		vector_dataset(VectorDataset),
		forall(
			(	member(bm25_dataset(_,Items,Contents), [FeatureDataset,VectorDataset]),
				member(Weighting, [uniform,rating])
			),
			(	Dataset = bm25_dataset([rating(u,x,5),rating(u,y,4),rating(v,y,3)], Items, Contents),
				Options = [profile_weighting(Weighting),k1(2),b(1)],
				bm25_recommender::learn(Dataset, Model, Options),
				bm25_recommender::remove_catalog(Model, [x,x,missing], Removed),
				memberchk(y-YContent, Contents),
				memberchk(empty-Empty, Contents),
				bm25_recommender::learn(bm25_dataset([rating(u,y,4),rating(v,y,3)], [y,empty], [y-YContent,empty-Empty]), Fresh, Options),
				assertion(Removed == Fresh),
				assertion(bm25_recommender::valid_recommender(Removed)),
				bm25_recommender::remove_catalog(Removed, [x,x,missing], Retry),
				assertion(Retry == Removed)
			)
		).

	test(bm25_recommender_catalog_removal_unrated_refit, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		Model = bm25_model(Ratings,_,_,Profiles,_,Scale,_),
		bm25_recommender::remove_catalog(Model, [empty], Removed),
		Removed = bm25_model(Ratings,_,_,Profiles,bm25_corpus(2,Average,_),Scale,Diagnostics),
		assertion(Average =~= 3.0),
		assertion(memberchk(item_count(2), Diagnostics)),
		bm25_recommender::score(Removed, u, x, Score),
		Expected is 2 * log(3 / 2.5) * 4.4 / 2.9,
		assertion(Score =~= Expected).

	test(bm25_recommender_catalog_removal_user_disappearance_and_noops, deterministic) :-
		feature_dataset(bm25_dataset(_,Items,Contents)),
		bm25_recommender::learn(bm25_dataset([rating(u,x,5),rating(v,y,3)], Items, Contents), Model),
		copy_term(Model, Original),
		bm25_recommender::remove_catalog(Model, [], Empty),
		assertion(Empty == Model),
		bm25_recommender::remove_catalog(Model, [missing,missing], Missing),
		assertion(Missing == Model),
		bm25_recommender::remove_catalog(Model, [x], Removed),
		Removed = bm25_model([rating(v,y,3)],_,_,[v-_],_,_,_),
		bm25_recommender::recommend(Removed, u, 3, [y-0.0,empty-0.0]),
		assertion(lgtunit::variant(Model, Original)),
		assertion(bm25_recommender::valid_recommender(Removed)).

	test(bm25_recommender_catalog_removal_nonempty_and_mean_guards, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Items-Expected, [[x,y,empty]-domain_error(non_empty_catalog,[]),[x]-domain_error(non_empty_ratings,[]),[x,y,empty,bad(id)]-type_error(atomic,bad(id))]),
			(	catch(bm25_recommender::remove_catalog(Model, Items, _), error(Error,_), Caught = Error),
				assertion(Caught == Expected)
			)
		),
		WeightedDataset = bm25_dataset([rating(u,x,1),rating(u,y,0),rating(v,empty,1)], [x,y,empty], [x-features([a]),y-features([]),empty-features([])]),
		bm25_recommender::learn(WeightedDataset, Weighted, [profile_weighting(rating)]),
		catch(bm25_recommender::remove_catalog(Weighted, [x], _), error(domain_error(positive_rating_weight,0),_), Failed = yes),
		assertion(Failed == yes),
		assertion(bm25_recommender::valid_recommender(Weighted)).

	test(bm25_recommender_catalog_removal_input_validation, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model),
		forall(
			member(Goal-Expected, [remove_catalog(_,[],_)-instantiation_error,remove_catalog(bad,[],_)-domain_error(recommender,bad),remove_catalog(Model,_,_)-instantiation_error,remove_catalog(Model,bad,_)-type_error(list,bad),remove_catalog(Model,[_|_],_)-instantiation_error,remove_catalog(Model,[missing,_],_)-instantiation_error,remove_catalog(Model,[item(x)],_)-type_error(atomic,item(x))]),
			(	catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_catalog_removal_validates_once, deterministic) :-
		feature_dataset(Dataset),
		bm25_validation_counter::learn(Dataset, Model),
		forall(
			member(Items, [[],[missing],[empty]]),
			(	bm25_validation_counter::reset_validation_count,
				bm25_validation_counter::remove_catalog(Model, Items, _),
				bm25_validation_counter::validation_count(1)
			)
		).

	test(bm25_recommender_scale_edit_preserves_fitted_state_and_scores, deterministic) :-
		bm25_recommender::learn(bm25_scale_fixture(1,5), Model),
		copy_term(Model, Original),
		Model = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,_,Diagnostics),
		bm25_recommender::score(Model, u, x, Score),
		forall(
			member(Scale, [none,scale(0,10),scale(1,1),scale(1,5)]),
			(	bm25_recommender::set_rating_scale(Model, Scale, Updated),
				Updated = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics),
				assertion(bm25_recommender::valid_recommender(Updated)),
				bm25_recommender::score(Updated, u, x, Score)
			)
		),
		bm25_recommender::set_rating_scale(Model, scale(1,5), Same),
		assertion(Same == Model),
		assertion(lgtunit::variant(Model, Original)).

	test(bm25_recommender_scale_edit_followup_updates, deterministic) :-
		bm25_recommender::learn(bm25_scale_fixture(1,5), Model),
		bm25_recommender::set_rating_scale(Model, scale(1,1), Tightened),
		catch(bm25_recommender::update_ratings(Tightened, [rating(v,x,2)], _), error(domain_error(rating_scale(1,1),2),_), Failed = yes),
		assertion(Failed == yes),
		bm25_recommender::set_rating_scale(Tightened, none, Unbounded),
		bm25_recommender::update_ratings(Unbounded, [rating(v,x,10)], Updated),
		assertion(bm25_recommender::valid_recommender(Updated)).

	test(bm25_recommender_scale_edit_input_validation, deterministic) :-
		bm25_recommender::learn(bm25_scale_fixture(1,5), Model),
		forall(
			member(Goal-Expected, [
				set_rating_scale(_,none,_)-instantiation_error,
				set_rating_scale(bad,none,_)-domain_error(recommender,bad),
				set_rating_scale(Model,_,_)-instantiation_error,
				set_rating_scale(Model,bad,_)-domain_error(rating_scale,bad),
				set_rating_scale(Model,scale(_,5),_)-instantiation_error,
				set_rating_scale(Model,scale(1,_),_)-instantiation_error,
				set_rating_scale(Model,scale(bad,5),_)-type_error(number,bad),
				set_rating_scale(Model,scale(1,bad),_)-type_error(number,bad),
				set_rating_scale(Model,scale(5,1),_)-domain_error(rating_scale,5-1),
				set_rating_scale(Model,scale(2,5),_)-domain_error(rating_scale(2,5),1)
			]),
			(	catch(bm25_recommender::Goal, error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, Expected))
			)
		).

	test(bm25_recommender_scale_edit_validates_once_without_profile_rebuild, deterministic) :-
		feature_dataset(Dataset),
		bm25_validation_counter::learn(Dataset, Model),
		bm25_validation_counter::reset_validation_count,
		bm25_validation_counter::reset_profile_count,
		bm25_validation_counter::set_rating_scale(Model, scale(1,5), _),
		bm25_validation_counter::validation_count(1),
		bm25_validation_counter::profile_count(1).

	test(bm25_recommender_query_saturation_formula_and_raw_profiles, deterministic) :-
		feature_dataset(Features),
		vector_dataset(Vectors),
		forall(
			(	member(Dataset, [Features,Vectors]),
				member(K3, [0,2,100])
			),
			(	bm25_recommender::learn(Dataset, Model, [query_saturation(K3)]),
				Model = bm25_model(_,_,_,[u-[a-Raw]],_,_,_),
				assertion(Raw =~= 2.0),
				bm25_recommender::score(Model, u, y, Score),
				Expected is (2 * (K3 + 1) / (2 + K3)) * log(1.6) * 2.2 / 3.1,
				assertion(Score =~= Expected),
				bm25_recommender::score_all(Model, u, [y,y,empty], [y-Score,y-Score,empty-0.0]),
				bm25_recommender::recommend(Model, u, 3, [y-Score,empty-0.0]),
				assertion(bm25_recommender::valid_recommender(Model))
			)
		).

	test(bm25_recommender_query_saturation_fractional_profile, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1),rating(u,empty,1)], [x,y,empty], [x-vector([a-1]),y-vector([a-1]),empty-vector([])]),
		bm25_recommender::learn(Dataset, Model, [query_saturation(2),k1(0)]),
		Model = bm25_model(_,_,_,[u-[a-Raw]],_,_,_),
		assertion(Raw =~= 0.5),
		bm25_recommender::score(Model, u, y, Score),
		Expected is (0.5 * 3 / 2.5) * log(4 / 2.5),
		assertion(Score =~= Expected).

	test(bm25_recommender_query_saturation_default_none_exact, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Default),
		bm25_recommender::learn(Dataset, Explicit, [query_saturation(none)]),
		Default = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,_),
		Explicit = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,_),
		bm25_recommender::score_all(Default, u, [x,y,empty], Scores),
		bm25_recommender::score_all(Explicit, u, [x,y,empty], Scores),
		bm25_recommender::default_option(query_saturation(none)),
		assertion(bm25_recommender::valid_option(query_saturation(0))),
		assertion(bm25_recommender::valid_option(query_saturation(2.5))).

	test(bm25_recommender_query_saturation_content_padding_and_empty_profiles, deterministic) :-
		feature_dataset(Dataset),
		bm25_recommender::learn(Dataset, Model, [query_saturation(2)]),
		bm25_recommender::score(Model, u, y, Score),
		bm25_recommender::score_content(Model, u, features([a,z,z,z]), Score),
		bm25_recommender::score_content(Model, u, features([a,z,z,z,new,new]), Padded),
		assertion(Padded < Score),
		bm25_recommender::score(Model, unknown, x, Zero),
		assertion(Zero =~= 0.0),
		bm25_recommender::learn(bm25_dataset([rating(u,x,1)],[x],[x-features([])]), Empty, [query_saturation(0)]),
		bm25_recommender::score_content(Empty, u, features([a]), EmptyScore),
		assertion(EmptyScore =~= 0.0).

	test(bm25_recommender_query_saturation_options_and_current_model_validation, deterministic) :-
		feature_dataset(Dataset),
		forall(
			member(Value, [-1,bad,_]),
			(	catch(bm25_recommender::learn(Dataset, _, [query_saturation(Value)]), error(Error,_), Caught = Error),
				assertion(lgtunit::variant(Caught, domain_error(option,query_saturation(Value))))
			)
		),
		bm25_recommender::learn(Dataset, Model, [query_saturation(0),query_saturation(2)]),
		bm25_recommender::score(Model, u, y, Score),
		Expected is log(1.6) * 2.2 / 3.1,
		assertion(Score =~= Expected),
		Model = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics),
		memberchk(options(Options), Diagnostics),
		findall(
			Option,
			(	member(Option, Options),
				Option \= query_saturation(_)
			),
			Partial
		),
		once(list::select(options(Options), Diagnostics, options(Partial), OldDiagnostics)),
		assertion(\+ bm25_recommender::valid_recommender(bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,OldDiagnostics))).

	test(bm25_recommender_composed_maintenance_saturation_and_export, deterministic) :-
		feature_dataset(Features),
		vector_dataset(Vectors),
		forall(
			(	member(data(Dataset,Addition,Replacement), [data(Features,features([a,z]),features([z,z])),data(Vectors,vector([a-1,z-1]),vector([z-2]))]),
				member(Saturation, [none,0,2])
			),
			(	Options = [query_saturation(Saturation),profile_weighting(rating),positive_threshold(0)],
				bm25_recommender::learn(Dataset, bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,Diagnostics), Options),
				Model = bm25_model(Ratings,Contents,Weights,Profiles,Corpus,Scale,[note(maintenance)|Diagnostics]),
				bm25_recommender::extend_catalog(Model, [new-Addition], Extended),
				bm25_recommender::update_ratings(Extended, [rating(u,new,4),rating(v,y,3)], Rated),
				bm25_recommender::replace_content(Rated, [new-Replacement], Replaced),
				bm25_recommender::remove_catalog(Replaced, [x,x,missing], Removed),
				bm25_recommender::set_rating_scale(Removed, scale(3,5), Final),
				memberchk(y-YContent, Contents),
				memberchk(empty-Empty, Contents),
				FreshDataset = bm25_dataset([rating(u,new,4),rating(v,y,3)], [y,empty,new], [y-YContent,empty-Empty,new-Replacement]),
				bm25_recommender::learn(FreshDataset, FreshUnbounded, Options),
				bm25_recommender::set_rating_scale(FreshUnbounded, scale(3,5), Fresh),
				Fresh = bm25_model(NewRatings,NewContents,NewWeights,NewProfiles,NewCorpus,NewScale,NewDiagnostics),
				Expected = bm25_model(NewRatings,NewContents,NewWeights,NewProfiles,NewCorpus,NewScale,[note(maintenance)|NewDiagnostics]),
				assertion(Final == Expected),
				assertion(bm25_recommender::valid_recommender(Final)),
				bm25_recommender::score(Final, u, y, Score),
				bm25_recommender::score_content(Final, u, YContent, Score),
				bm25_recommender::score_all(Final, u, [y,y,empty], [y-Score,y-Score,empty-0.0]),
				bm25_recommender::recommend(Final, u, 3, [y-Score,empty-0.0]),
				bm25_recommender::export_to_clauses(Dataset, Final, saved, [saved(Final)]),
				^^file_path('bm25_saved.pl', File),
				bm25_recommender::export_to_file(Dataset, Final, bm25_maintenance, File),
				logtalk_load(File, [reload(always)]),
				{bm25_maintenance(Loaded)},
				assertion(Loaded == Final),
				assertion(bm25_recommender::valid_recommender(Loaded)),
				bm25_recommender::score(Loaded, u, y, Score)
			)
		).

	test(bm25_recommender_query_saturation_large_finite_parameter, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,5)], [x,y,empty], [x-vector([a-1000000000]),y-vector([a-1]),empty-vector([])]),
		bm25_recommender::learn(Dataset, Default),
		bm25_recommender::learn(Dataset, Saturated, [query_saturation(1.0e300)]),
		bm25_recommender::score(Default, u, y, Expected),
		bm25_recommender::score(Saturated, u, y, Score),
		assertion(Score =~= Expected),
		assertion(Score > 0),
		assertion(Score < 1.0e300),
		assertion(bm25_recommender::valid_recommender(Saturated)).

	test(bm25_recommender_catalog_removal_empty_space_and_order, deterministic) :-
		Dataset = bm25_dataset([rating(u,x,1),rating(v,y,1)], [x,y,z], [x-vector([]),y-vector([]),z-vector([])]),
		bm25_recommender::learn(Dataset, Model, [query_saturation(0)]),
		bm25_recommender::remove_catalog(Model, [z,x], First),
		bm25_recommender::remove_catalog(Model, [x,z,z], Second),
		assertion(First == Second),
		bm25_recommender::remove_catalog(Model, [z], Intermediate),
		bm25_recommender::remove_catalog(Intermediate, [x], Sequential),
		assertion(First == Sequential),
		bm25_recommender::score(First, unknown, y, Score),
		assertion(Score =~= 0.0),
		assertion(bm25_recommender::valid_recommender(First)).

	test(bm25_recommender_new_maintenance_predicates_implemented_locally, deterministic) :-
		bm25_recommender::predicate_property(remove_catalog(_,_,_), defined_in(bm25_recommender)),
		bm25_recommender::predicate_property(set_rating_scale(_,_,_), defined_in(bm25_recommender)).

	% auxiliary predicates

	feature_dataset(bm25_dataset([rating(u,x,5)], [x,y,empty], [x-features([a,a]),y-features([a,z,z,z]),empty-features([])])).

	vector_dataset(bm25_dataset([rating(u,x,5)], [x,y,empty], [x-vector([a-2]),y-vector([a-1,z-3]),empty-vector([])])).

:- end_object.

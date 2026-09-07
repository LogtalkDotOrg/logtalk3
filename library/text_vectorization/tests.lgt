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
		date is 2026-09-07,
		comment is 'Unit tests for the text vectorization library.'
	]).

	cover(text_vectorizer).

	:- uses(list, [
		memberchk/2
	]).

	test(learn_default_vocabulary, deterministic(Vocabulary == [a,b,c])) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer),
		text_vectorizer::vocabulary(Vectorizer, Vocabulary).

	test(learn_default_smoothed_idf, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, text_vectorizer_model([feature(a, 2, IDF)| _], _)),
		Expected is log(4 / 3) + 1,
		^^approximately_equal(IDF, Expected, 1.0e-12).

	test(learn_classic_idf, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, text_vectorizer_model([feature(a, 2, IDF)| _], _), [idf(classic)]),
		Expected is log(3 / 2),
		^^approximately_equal(IDF, Expected, 1.0e-12).

	test(transform_binary, deterministic(Vector == [a-1,b-1])) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(binary)]),
		text_vectorizer::transform(Vectorizer, [b,a,a,unknown], Vector).

	test(transform_count, deterministic(Vector == [a-2,b-1])) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(count)]),
		text_vectorizer::transform(Vectorizer, [b,a,a,unknown], Vector).

	test(transform_term_frequency, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(term_frequency)]),
		text_vectorizer::transform(Vectorizer, [b,a,a,unknown], [a-A,b-B]),
		ExpectedA is 2 / 3,
		ExpectedB is 1 / 3,
		^^approximately_equal(A, ExpectedA, 1.0e-12),
		^^approximately_equal(B, ExpectedB, 1.0e-12).

	test(transform_tfidf_raw, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer),
		text_vectorizer::transform(Vectorizer, [b,a,a], [a-A,b-B]),
		IDF is log(4 / 3) + 1,
		ExpectedA is 2 * IDF,
		^^approximately_equal(A, ExpectedA, 1.0e-12),
		^^approximately_equal(B, IDF, 1.0e-12).

	test(transform_tfidf_relative, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(tf_idf(relative))]),
		text_vectorizer::transform(Vectorizer, [b,a,a], [a-A,b-B]),
		IDF is log(4 / 3) + 1,
		ExpectedA is 2 / 3 * IDF,
		ExpectedB is 1 / 3 * IDF,
		^^approximately_equal(A, ExpectedA, 1.0e-12),
		^^approximately_equal(B, ExpectedB, 1.0e-12).

	test(transform_tfidf_sublinear, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(tf_idf(sublinear))]),
		text_vectorizer::transform(Vectorizer, [a,a], [a-A]),
		IDF is log(4 / 3) + 1,
		Expected is (1 + log(2)) * IDF,
		^^approximately_equal(A, Expected, 1.0e-12).

	test(transform_l1_normalization, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(count), normalization(l1)]),
		text_vectorizer::transform(Vectorizer, [a,a,b], [a-A,b-B]),
		ExpectedA is 2 / 3,
		ExpectedB is 1 / 3,
		^^approximately_equal(A, ExpectedA, 1.0e-12),
		^^approximately_equal(B, ExpectedB, 1.0e-12).

	test(transform_l2_normalization, deterministic) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(count), normalization(l2)]),
		text_vectorizer::transform(Vectorizer, [a,a,b], [a-A,b-B]),
		ExpectedA is 2 / sqrt(5),
		ExpectedB is 1 / sqrt(5),
		^^approximately_equal(A, ExpectedA, 1.0e-12),
		^^approximately_equal(B, ExpectedB, 1.0e-12).

	test(transform_oov_document, deterministic(Vector == [])) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer),
		text_vectorizer::transform(Vectorizer, [unknown], Vector).

	test(transform_all_preserves_order, deterministic(Vectors == [[a-1], [b-1]])) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [weighting(binary)]),
		text_vectorizer::transform_all(Vectorizer, [[a], [b]], Vectors).

	test(learn_transform_equivalence, deterministic(Vectors == Expected)) :-
		corpus(Corpus),
		text_vectorizer::learn_transform(Corpus, Vectorizer, Vectors, [weighting(count)]),
		text_vectorizer::transform_all(Vectorizer, Corpus, Expected).

	test(document_frequency_filter, deterministic(Vocabulary == [a,b])) :-
		text_vectorizer::learn([[a,b,c], [a,b], [a]], Vectorizer, [minimum_document_frequency(2)]),
		text_vectorizer::vocabulary(Vectorizer, Vocabulary).

	test(maximum_features_frequency_and_tie_break, deterministic(Vocabulary == [a,c])) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer, [maximum_features(2)]),
		text_vectorizer::vocabulary(Vectorizer, Vocabulary).

	test(arbitrary_ground_features, deterministic(Vector == [[a,b]-1,[b,c]-1])) :-
		text_vectorizer::learn([[[a,b],[b,c]], [[a,b]]], Vectorizer, [weighting(binary)]),
		text_vectorizer::transform(Vectorizer, [[b,c],[a,b]], Vector).

	test(diagnostics, deterministic([DocumentCount, VocabularySize] == [3,3])) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer),
		text_vectorizer::diagnostics(Vectorizer, Diagnostics),
		memberchk(document_count(DocumentCount), Diagnostics),
		memberchk(vocabulary_size(VocabularySize), Diagnostics).

	test(valid_vectorizer, true) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, Vectorizer),
		text_vectorizer::valid_vectorizer(Vectorizer).

	test(empty_corpus, error(domain_error(non_empty_corpus, []))) :-
		text_vectorizer::learn([], _).

	test(empty_vocabulary, error(domain_error(non_empty_vocabulary, [[],[]]))) :-
		text_vectorizer::learn([[],[]], _).

	test(nonground_feature, error(instantiation_error)) :-
		text_vectorizer::learn([[a,_]], _).

	test(invalid_frequency_bound, error(domain_error(option, maximum_document_frequency(4)))) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, _, [maximum_document_frequency(4)]).

	test(invalid_option, error(domain_error(option, weighting(unsupported)))) :-
		corpus(Corpus),
		text_vectorizer::learn(Corpus, _, [weighting(unsupported)]).

	test(invalid_vectorizer, error(domain_error(text_vectorizer, invalid))) :-
		text_vectorizer::transform(invalid, [a], _).

	% auxiliary predicates

	corpus([[a,a,b], [a,c], [b,c,c]]).

:- end_object.

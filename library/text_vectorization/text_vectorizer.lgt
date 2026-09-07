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


:- object(text_vectorizer,
	implements(text_vectorizer_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-07,
		comment is 'Binary, count, term-frequency, and TF-IDF sparse text vectorization.',
		see_also is [text_vectorizer_protocol]
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, occurrences/2, sort/4
	]).

	:- uses(type, [
		check/2, valid/2
	]).

	learn(Corpus, Vectorizer) :-
		learn(Corpus, Vectorizer, []).

	learn(Corpus, Vectorizer, UserOptions) :-
		check_corpus(Corpus),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		length(Corpus, DocumentCount),
		check_frequency_options(Options, DocumentCount),
		corpus_counts(Corpus, TotalCounts, DocumentCounts, InputFeatureCount),
		^^option(idf(IDF), Options),
		build_features(TotalCounts, DocumentCounts, DocumentCount, IDF, Features0),
		filter_features(Features0, Options, Features1),
		limit_features(Features1, Options, Features),
		( 	Features == [] ->
			domain_error(non_empty_vocabulary, Corpus)
		;	true
		),
		length(Features, VocabularySize),
		Diagnostics = [
			model(text_vectorizer),
			options(Options),
			document_count(DocumentCount),
			vocabulary_size(VocabularySize),
			input_feature_count(InputFeatureCount)
		],
		Vectorizer = text_vectorizer_model(Features, Diagnostics).

	transform(Vectorizer, Document, Vector) :-
		check_vectorizer(Vectorizer),
		check_document(Document),
		Vectorizer = text_vectorizer_model(Features, Diagnostics),
		memberchk(options(Options), Diagnostics),
		occurrences(Document, Counts),
		in_vocabulary_counts(Features, Counts, InVocabularyCounts),
		counts_total(InVocabularyCounts, 0, Total),
		^^option(weighting(Weighting), Options),
		weight_counts(InVocabularyCounts, Total, Weighting, Weighted),
		^^option(normalization(Normalization), Options),
		normalize_vector(Normalization, Weighted, Vector).

	transform_all(Vectorizer, Corpus, Vectors) :-
		check_vectorizer(Vectorizer),
		check_corpus_documents(Corpus),
		transform_documents(Corpus, Vectorizer, Vectors).

	learn_transform(Corpus, Vectorizer, Vectors) :-
		learn_transform(Corpus, Vectorizer, Vectors, []).

	learn_transform(Corpus, Vectorizer, Vectors, Options) :-
		learn(Corpus, Vectorizer, Options),
		transform_documents(Corpus, Vectorizer, Vectors).

	transform_documents([], _, []).
	transform_documents([Document| Documents], Vectorizer, [Vector| Vectors]) :-
		transform(Vectorizer, Document, Vector),
		transform_documents(Documents, Vectorizer, Vectors).

	check_vectorizer(Vectorizer) :-
		( 	var(Vectorizer) ->
			instantiation_error
		;	valid_vectorizer_term(Vectorizer) ->
			true
		;	domain_error(text_vectorizer, Vectorizer)
		).

	valid_vectorizer(Vectorizer) :-
		catch(check_vectorizer(Vectorizer), _, fail).

	vocabulary(Vectorizer, Vocabulary) :-
		check_vectorizer(Vectorizer),
		Vectorizer = text_vectorizer_model(Features, _),
		feature_names(Features, Vocabulary).

	diagnostics(Vectorizer, Diagnostics) :-
		check_vectorizer(Vectorizer),
		Vectorizer = text_vectorizer_model(_, Diagnostics).

	diagnostic(Vectorizer, Diagnostic) :-
		diagnostics(Vectorizer, Diagnostics),
		member(Diagnostic, Diagnostics).

	vectorizer_options(Vectorizer, Options) :-
		diagnostics(Vectorizer, Diagnostics),
		memberchk(options(Options), Diagnostics).

	default_option(weighting(tf_idf(raw))).
	default_option(idf(smooth)).
	default_option(normalization(none)).
	default_option(minimum_document_frequency(1)).
	default_option(maximum_document_frequency(all)).
	default_option(maximum_features(all)).

	valid_option(weighting(Weighting)) :-
		once((
			Weighting == binary
		;	Weighting == count
		;	Weighting == term_frequency
		;	Weighting == tf_idf(raw)
		;	Weighting == tf_idf(relative)
		;	Weighting == tf_idf(sublinear)
		)).
	valid_option(idf(IDF)) :-
		once((
			IDF == smooth
		;	IDF == classic
		)).
	valid_option(normalization(Normalization)) :-
		once((
			Normalization == none
		;	Normalization == l1
		;	Normalization == l2
		)).
	valid_option(minimum_document_frequency(Minimum)) :-
		valid(positive_integer, Minimum).
	valid_option(maximum_document_frequency(Maximum)) :-
		(	Maximum == all ->
			true
		;	valid(positive_integer, Maximum)
		).
	valid_option(maximum_features(Maximum)) :-
		(	Maximum == all ->
			true
		;	valid(positive_integer, Maximum)
		).

	check_corpus(Corpus) :-
		check(list, Corpus),
		( 	Corpus == [] ->
			domain_error(non_empty_corpus, Corpus)
		;	check_corpus_documents(Corpus)
		).

	check_corpus_documents(Corpus) :-
		check(list, Corpus),
		check_documents(Corpus).

	check_documents([]).
	check_documents([Document| Documents]) :-
		check_document(Document),
		check_documents(Documents).

	check_document(Document) :-
		check(list, Document),
		check_features(Document).

	check_features([]).
	check_features([Feature| Features]) :-
		( 	ground(Feature) ->
			true
		;	instantiation_error
		),
		check_features(Features).

	check_frequency_options(Options, DocumentCount) :-
		^^option(minimum_document_frequency(Minimum), Options),
		^^option(maximum_document_frequency(Maximum), Options),
		( 	Maximum == all ->
			EffectiveMaximum = DocumentCount
		;	Maximum =< DocumentCount ->
			EffectiveMaximum = Maximum
		;	domain_error(option, maximum_document_frequency(Maximum))
		),
		( 	Minimum =< EffectiveMaximum ->
			true
		;	domain_error(option, minimum_document_frequency(Minimum))
		).

	corpus_counts(Corpus, TotalCounts, DocumentCounts, InputFeatureCount) :-
		flatten_documents(Corpus, AllFeatures),
		occurrences(AllFeatures, TotalCounts),
		length(AllFeatures, InputFeatureCount),
		document_unique_features(Corpus, UniqueFeatures),
		occurrences(UniqueFeatures, DocumentCounts).

	flatten_documents([], []).
	flatten_documents([Document| Documents], Features) :-
		append(Document, Tail, Features),
		flatten_documents(Documents, Tail).

	document_unique_features([], []).
	document_unique_features([Document| Documents], Features) :-
		sort(Document, Unique),
		append(Unique, Tail, Features),
		document_unique_features(Documents, Tail).

	build_features([], [], _, _, []).
	build_features([Feature-Total| Totals], [Feature-DF| DFs], DocumentCount, IDF, [candidate(Feature, Total, DF, Weight)| Features]) :-
		idf_weight(IDF, DocumentCount, DF, Weight),
		build_features(Totals, DFs, DocumentCount, IDF, Features).

	idf_weight(classic, DocumentCount, DocumentFrequency, Weight) :-
		Weight is log(DocumentCount / DocumentFrequency).
	idf_weight(smooth, DocumentCount, DocumentFrequency, Weight) :-
		Weight is log((1 + DocumentCount) / (1 + DocumentFrequency)) + 1.

	filter_features(Features, Options, Filtered) :-
		^^option(minimum_document_frequency(Minimum), Options),
		^^option(maximum_document_frequency(Maximum), Options),
		filter_features(Features, Minimum, Maximum, Filtered).

	filter_features([], _, _, []).
	filter_features([candidate(Feature, Total, DF, IDF)| Features], Minimum, Maximum, Filtered) :-
		( 	DF >= Minimum,
			(Maximum == all; DF =< Maximum) ->
			Filtered = [candidate(Feature, Total, DF, IDF)| Tail]
		;	Filtered = Tail
		),
		filter_features(Features, Minimum, Maximum, Tail).

	limit_features(Features, Options, Limited) :-
		^^option(maximum_features(Maximum), Options),
		( 	Maximum == all ->
			candidates_features(Features, Limited)
		;	decorate_features(Features, Decorated),
			sort(0, @=<, Decorated, Sorted),
			take_features(Maximum, Sorted, Selected),
			sort(0, @=<, Selected, SelectedStandard),
			selected_features(SelectedStandard, Limited)
		).

	decorate_features([], []).
	decorate_features([candidate(Feature, Total, DF, IDF)| Features], [rank(NegativeTotal, Feature, DF, IDF)| Decorated]) :-
		NegativeTotal is -Total,
		decorate_features(Features, Decorated).

	take_features(0, _, []) :-
		!.
	take_features(_, [], []).
	take_features(N, [rank(_, Feature, DF, IDF)| Ranked], [selected(Feature, DF, IDF)| Selected]) :-
		N2 is N - 1,
		take_features(N2, Ranked, Selected).

	selected_features([], []).
	selected_features([selected(Feature, DF, IDF)| Selected], [feature(Feature, DF, IDF)| Features]) :-
		selected_features(Selected, Features).

	candidates_features([], []).
	candidates_features([candidate(Feature, _, DF, IDF)| Candidates], [feature(Feature, DF, IDF)| Features]) :-
		candidates_features(Candidates, Features).

	in_vocabulary_counts([], _, []).
	in_vocabulary_counts([feature(Feature, _, IDF)| Features], Counts, InVocabulary) :-
		( 	member(Feature-Count, Counts) ->
			InVocabulary = [count(Feature, Count, IDF)| Tail]
		;	InVocabulary = Tail
		),
		in_vocabulary_counts(Features, Counts, Tail).

	counts_total([], Total, Total).
	counts_total([count(_, Count, _)| Counts], Total0, Total) :-
		Total1 is Total0 + Count,
		counts_total(Counts, Total1, Total).

	weight_counts([], _, _, []).
	weight_counts([count(Feature, Count, IDF)| Counts], Total, Weighting, Weighted) :-
		feature_weight(Weighting, Count, Total, IDF, Weight),
		( 	Weight =:= 0 ->
			Weighted = Tail
		;	Weighted = [Feature-Weight| Tail]
		),
		weight_counts(Counts, Total, Weighting, Tail).

	feature_weight(binary, _, _, _, 1).
	feature_weight(count, Count, _, _, Count).
	feature_weight(term_frequency, Count, Total, _, Weight) :-
		Weight is Count / Total.
	feature_weight(tf_idf(raw), Count, _, IDF, Weight) :-
		!,
		Weight is Count * IDF.
	feature_weight(tf_idf(relative), Count, Total, IDF, Weight) :-
		!,
		Weight is Count / Total * IDF.
	feature_weight(tf_idf(sublinear), Count, _, IDF, Weight) :-
		Weight is (1 + log(Count)) * IDF.

	normalize_vector(none, Vector, Vector).
	normalize_vector(l1, Vector, Normalized) :-
		l1_norm(Vector, 0.0, Norm),
		normalize_by(Norm, Vector, Normalized).
	normalize_vector(l2, Vector, Normalized) :-
		l2_square_norm(Vector, 0.0, SquareNorm),
		Norm is sqrt(SquareNorm),
		normalize_by(Norm, Vector, Normalized).

	l1_norm([], Norm, Norm).
	l1_norm([_-Weight| Vector], Norm0, Norm) :-
		Norm1 is Norm0 + abs(Weight),
		l1_norm(Vector, Norm1, Norm).

	l2_square_norm([], Norm, Norm).
	l2_square_norm([_-Weight| Vector], Norm0, Norm) :-
		Norm1 is Norm0 + Weight * Weight,
		l2_square_norm(Vector, Norm1, Norm).

	normalize_by(0.0, _, []) :-
		!.
	normalize_by(Norm, Vector, Normalized) :-
		normalize_weights(Vector, Norm, Normalized).

	normalize_weights([], _, []).
	normalize_weights([Feature-Weight| Vector], Norm, [Feature-NormalizedWeight| Normalized]) :-
		NormalizedWeight is Weight / Norm,
		normalize_weights(Vector, Norm, Normalized).

	valid_vectorizer_term(text_vectorizer_model(Features, Diagnostics)) :-
		Diagnostics = [
			model(text_vectorizer),
			options(Options),
			document_count(DocumentCount),
			vocabulary_size(VocabularySize),
			input_feature_count(InputFeatureCount)
		],
		valid(positive_integer, DocumentCount),
		valid(non_negative_integer, InputFeatureCount),
		Features = [_| _],
		valid_features(Features, none, DocumentCount),
		length(Features, VocabularySize),
		catch(^^check_options(Options), _, fail),
		length(Options, 6),
		memberchk(weighting(_), Options),
		memberchk(idf(_), Options),
		memberchk(normalization(_), Options),
		memberchk(minimum_document_frequency(_), Options),
		memberchk(maximum_document_frequency(_), Options),
		memberchk(maximum_features(_), Options).

	valid_features([], _, _).
	valid_features([feature(Feature, DF, IDF)| Features], Previous, DocumentCount) :-
		ground(Feature),
		valid(positive_integer, DF),
		( 	Previous == none -> true; Previous @< Feature),
		number(IDF),
		DF =< DocumentCount,
		valid_features(Features, Feature, DocumentCount).

	feature_names([], []).
	feature_names([feature(Feature, _, _)| Features], [Feature| Vocabulary]) :-
		feature_names(Features, Vocabulary).

:- end_object.

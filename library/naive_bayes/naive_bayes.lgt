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


:- object(naive_bayes,
	implements(classifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-16,
		comment is 'Naive Bayes classifier with Laplace smoothing and Gaussian distribution support. Learns from a dataset object implementing the ``dataset_protocol`` protocol and returns a classifier term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Naive Bayes is a probabilistic classifier based on Bayes theorem with strong (naive) independence assumptions between features.',
			'Categorical features' - 'Uses Laplace smoothing to handle unseen feature values.',
			'Continuous features' - 'Uses Gaussian (normal) distribution to model numeric features.',
			'Classifier representation' - 'The learned classifier is represented as ``Functor(Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams)`` where ``FeatureParams`` contains the learned probabilities or statistics for each feature.',
			'Export format' - 'The classifier can be exported as a list of clauses.'
		],
		see_also is [dataset_protocol, c45]
	]).

	:- public(predict_probabilities/3).
	:- mode(predict_probabilities(+compound, +list, -list), one).
	:- info(predict_probabilities/3, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier. Returns a list of ``Class-Probability`` pairs. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

	:- uses(list, [
		length/2, memberchk/2, msort/2, nth1/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(format, [
		format/2, format/3
	]).

	% learn/2 - main entry point
	learn(Dataset, Classifier) :-
		% Get attribute information from dataset
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		% Get all examples from dataset
		findall(
			Id-Class-AttributeValues,
			Dataset::example(Id, Class, AttributeValues),
			Examples
		),
		% Extract instances and labels
		examples_to_instances_labels(Examples, AttributeNames, Instances, Labels),
		% Determine feature types from dataset attributes
		determine_feature_types_from_dataset(Attributes, FeatureTypes),
		% Learn class priors
		learn_class_priors(Labels, Classes, ClassPriors),
		% Learn feature parameters
		learn_feature_params(Instances, Labels, Classes, FeatureTypes, Attributes, FeatureParams),
		% Build classifier term
		Classifier = nb_classifier(Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams).

	% Convert examples to instances (list of values) and labels
	examples_to_instances_labels([], _, [], []).
	examples_to_instances_labels([_-Class-AttributeValues| Examples], AttributeNames, [Instance| Instances], [Class| Labels]) :-
		extract_values(AttributeNames, AttributeValues, Instance),
		examples_to_instances_labels(Examples, AttributeNames, Instances, Labels).

	extract_values([], _, []).
	extract_values([Attribute| Attributes], AttributeValues, [Value| Values]) :-
		memberchk(Attribute-Value, AttributeValues),
		extract_values(Attributes, AttributeValues, Values).

	% Determine feature types from dataset attribute definitions
	determine_feature_types_from_dataset([], []).
	determine_feature_types_from_dataset([_-Values| Rest], [Type| Types]) :-
		(	Values == continuous ->
			Type = continuous
		;	Type = categorical
		),
		determine_feature_types_from_dataset(Rest, Types).

	% Learn class prior probabilities (returns lists instead of asserting)
	learn_class_priors(Labels, UniqueClasses, ClassPriors) :-
		length(Labels, Total),
		msort(Labels, Sorted),
		remove_duplicates_sorted(Sorted, UniqueClasses),
		compute_priors_list(UniqueClasses, Labels, Total, ClassPriors).

	remove_duplicates_sorted([], []).
	remove_duplicates_sorted([X], [X]) :- !.
	remove_duplicates_sorted([X, X| Xs], Ys) :-
		!,
		remove_duplicates_sorted([X| Xs], Ys).
	remove_duplicates_sorted([X, Y| Xs], [X| Ys]) :-
		remove_duplicates_sorted([Y| Xs], Ys).

	compute_priors_list([], _, _, []).
	compute_priors_list([Class| Classes], Labels, Total, [Class-Prior| Priors]) :-
		count_occurrences(Labels, Class, Count),
		Prior is Count / Total,
		compute_priors_list(Classes, Labels, Total, Priors).

	% Learn feature parameters for all features
	learn_feature_params(Instances, Labels, Classes, FeatureTypes, Attributes, FeatureParams) :-
		learn_feature_params_(Instances, Labels, Classes, FeatureTypes, Attributes, 1, FeatureParams).

	learn_feature_params_(_, _, _, [], [], _, []) :-
		!.
	learn_feature_params_(Instances, Labels, Classes, [Type| Types], [Attr-Values| Attrs], Index, [Param| Params]) :-
		learn_feature_param(Type, Index, Attr, Values, Classes, Instances, Labels, Param),
		NextIndex is Index + 1,
		learn_feature_params_(Instances, Labels, Classes, Types, Attrs, NextIndex, Params).

	learn_feature_param(categorical, Index, Attr, Values, Classes, Instances, Labels, feature(Attr, categorical, Probs)) :-
		learn_categorical_probs(Index, Values, Classes, Instances, Labels, Probs).
	learn_feature_param(continuous, Index, Attr, _, Classes, Instances, Labels, feature(Attr, continuous, Stats)) :-
		learn_continuous_stats(Index, Classes, Instances, Labels, Stats).

	% Learn categorical feature probabilities with Laplace smoothing
	learn_categorical_probs(Index, Values, Classes, Instances, Labels, Probs) :-
		length(Values, NumUniqueValues),
		learn_categorical_probs_by_class(Index, Values, NumUniqueValues, Classes, Instances, Labels, Probs).

	learn_categorical_probs_by_class(_, _, _, [], _, _, []) :-
		!.
	learn_categorical_probs_by_class(Index, UniqueValues, NumUniqueValues, [Class| Classes], Instances, Labels, [Class-ValueProbs| Pairs]) :-
		get_instances_for_class(Instances, Labels, Class, ClassInstances),
		length(ClassInstances, ClassCount),
		compute_value_probs_list(Index, UniqueValues, NumUniqueValues, ClassInstances, ClassCount, ValueProbs),
		learn_categorical_probs_by_class(Index, UniqueValues, NumUniqueValues, Classes, Instances, Labels, Pairs).

	compute_value_probs_list(_, [], _, _, _, []) :-
		!.
	compute_value_probs_list(Index, [Value| Values], NumUniqueValues, ClassInstances, ClassCount, [Value-Prob| Pairs]) :-
		count_feature_value(ClassInstances, Index, Value, Count),
		% Laplace smoothing: add 1 to count, add NumUniqueValues to total
		Prob is (Count + 1) / (ClassCount + NumUniqueValues),
		compute_value_probs_list(Index, Values, NumUniqueValues, ClassInstances, ClassCount, Pairs).

	% Learn continuous feature statistics (mean and variance for Gaussian)
	learn_continuous_stats(Index, Classes, Instances, Labels, Stats) :-
		learn_continuous_stats_by_class(Index, Classes, Instances, Labels, Stats).

	learn_continuous_stats_by_class(_, [], _, _, []) :-
		!.
	learn_continuous_stats_by_class(Index, [Class| Classes], Instances, Labels, [Class-stats(Mean, Variance)| Pairs]) :-
		get_instances_for_class(Instances, Labels, Class, ClassInstances),
		get_feature_column(ClassInstances, Index, Values),
		arithmetic_mean(Values, Mean),
		variance(Values, Variance),
		learn_continuous_stats_by_class(Index, Classes, Instances, Labels, Pairs).

	% Prediction using classifier term
	predict(Classifier, Instance, Class) :-
		predict_probabilities(Classifier, Instance, Probabilities),
		max_probability(Probabilities, Class, _).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier = nb_classifier(Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams),
		extract_values(AttributeNames, Instance, Values),
		compute_posteriors(Values, Classes, ClassPriors, FeatureTypes, FeatureParams, Probabilities).

	compute_posteriors(_, [], _, _, _, []) :-
		!.
	compute_posteriors(Values, [Class| Classes], ClassPriors, FeatureTypes, FeatureParams, [Class-Posterior| Pairs]) :-
		memberchk(Class-Prior, ClassPriors),
		compute_likelihood(Values, Class, FeatureTypes, FeatureParams, 1, Likelihood),
		Posterior is Prior * Likelihood,
		compute_posteriors(Values, Classes, ClassPriors, FeatureTypes, FeatureParams, Pairs).

	compute_likelihood([], _, _, _, _, 1).
	compute_likelihood([Value| Values], Class, [Type| Types], [Param| Params], Index, Likelihood) :-
		feature_likelihood(Type, Param, Value, Class, FeatureLikelihood),
		NextIndex is Index + 1,
		compute_likelihood(Values, Class, Types, Params, NextIndex, RestLikelihood),
		Likelihood is FeatureLikelihood * RestLikelihood.

	feature_likelihood(categorical, feature(_, categorical, Probs), Value, Class, Likelihood) :-
		(	memberchk(Class-ValueProbs, Probs),
			memberchk(Value-Likelihood, ValueProbs) ->
			true
		;	% If value not seen in training, use smoothing
			Likelihood = 0.0001
		).
	feature_likelihood(continuous, feature(_, continuous, Stats), Value, Class, Likelihood) :-
		memberchk(Class-stats(Mean, Variance), Stats),
		gaussian_probability(Value, Mean, Variance, Likelihood).

	% Gaussian probability density function
	gaussian_probability(X, Mean, Variance, Probability) :-
		Variance > 0,
		!,
		Exponent is -((X - Mean) ** 2) / (2 * Variance),
		Coefficient is 1 / sqrt(2 * 3.14159265359 * Variance),
		Probability is Coefficient * exp(Exponent).
	gaussian_probability(_, _, _, 1.0). % Handle zero variance

	% Helper predicates
	count_occurrences([], _, 0).
	count_occurrences([Head| Tail], Element, Count) :-
		(	Head == Element ->
			count_occurrences(Tail, Element, RestCount),
			Count is RestCount + 1
		;	count_occurrences(Tail, Element, Count)
		).

	get_feature_column([], _, []).
	get_feature_column([Instance| Instances], Index, [Value| Values]) :-
		nth1(Index, Instance, Value),
		get_feature_column(Instances, Index, Values).

	get_instances_for_class([], [], _, []).
	get_instances_for_class([Instance| Instances], [Label| Labels], Class, [Instance| FilteredInstances]) :-
		Label == Class,
		!,
		get_instances_for_class(Instances, Labels, Class, FilteredInstances).
	get_instances_for_class([_| Instances], [_| Labels], Class, FilteredInstances) :-
		get_instances_for_class(Instances, Labels, Class, FilteredInstances).

	count_feature_value([], _, _, 0).
	count_feature_value([Instance| Instances], Index, Value, Count) :-
		nth1(Index, Instance, FeatureValue),
		(	FeatureValue == Value ->
			count_feature_value(Instances, Index, Value, RestCount),
			Count is RestCount + 1
		;	count_feature_value(Instances, Index, Value, Count)
		).

	max_probability([Class-Probability], Class, Probability) :-
		!.
	max_probability([Class1-Probability1, Class2-Probability2| Pairs], MaxClass, MaxProbability) :-
		(	Probability1 >= Probability2 ->
			max_probability([Class1-Probability1| Pairs], MaxClass, MaxProbability)
		;	max_probability([Class2-Probability2| Pairs], MaxClass, MaxProbability)
		).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	% classifier_to_clauses/4 - convert classifier to a clause that can be loaded
	% The exported clause is: Functor(Classifier)
	% This can be loaded and then used with predict/3 and predict_probability/3
	classifier_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	% classifier_to_file/4 - export classifier to a file
	classifier_to_file(Dataset, Classifier, Functor, File) :-
		classifier_to_clauses(Dataset, Classifier, Functor, Clauses),
		open(File, write, Stream),
		write_file_header(Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_file_header(Functor, Stream) :-
		format(Stream, '%% Naive Bayes classifier exported by naive_bayes library~n', []),
		format(Stream, '%% Load this file and use the classifier with:~n', []),
		format(Stream, '%%   ~w(Classifier),~n', [Functor]),
		format(Stream, '%%   naive_bayes::predict(Classifier, Instance, Class)~n', []),
		format(Stream, '%%   naive_bayes::predict_probability(Classifier, Instance, Probabilities)~n~n', []).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	% print_classifier/1 - pretty print the classifier
	print_classifier(Classifier) :-
		Classifier = nb_classifier(Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams),
		format('Naive Bayes Classifier~n', []),
		format('======================~n~n', []),
		format('Classes: ~w~n~n', [Classes]),
		format('Class Priors:~n', []),
		print_priors(ClassPriors),
		nl,
		format('Features:~n', []),
		print_features(AttributeNames, FeatureTypes, FeatureParams).

	print_priors([]).
	print_priors([Class-Prior| Rest]) :-
		Percentage is Prior * 100,
		format('  ~w: ~2f%~n', [Class, Percentage]),
		print_priors(Rest).

	print_features([], [], []).
	print_features([Name| Names], [Type| Types], [Param| Params]) :-
		format('~n  ~w (~w):~n', [Name, Type]),
		print_feature_param(Type, Param),
		print_features(Names, Types, Params).

	print_feature_param(categorical, feature(_, categorical, ClassProbs)) :-
		print_categorical_probs(ClassProbs).
	print_feature_param(continuous, feature(_, continuous, ClassStats)) :-
		print_continuous_stats(ClassStats).

	print_categorical_probs([]).
	print_categorical_probs([Class-ValueProbs| Rest]) :-
		format('    Class ~w:~n', [Class]),
		print_value_probs(ValueProbs),
		print_categorical_probs(Rest).

	print_value_probs([]).
	print_value_probs([Value-Prob| Rest]) :-
		Percentage is Prob * 100,
		format('      ~w: ~2f%~n', [Value, Percentage]),
		print_value_probs(Rest).

	print_continuous_stats([]).
	print_continuous_stats([Class-stats(Mean, Variance)| Rest]) :-
		format('    Class ~w: mean=~4f, variance=~4f~n', [Class, Mean, Variance]),
		print_continuous_stats(Rest).

:- end_object.

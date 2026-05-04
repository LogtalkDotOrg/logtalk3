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
	imports(classifier_common)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'Naive Bayes classifier with Laplace smoothing and Gaussian distribution support. Learns from a dataset object implementing the ``dataset_protocol`` protocol and returns a classifier term that can be used for prediction and exported as predicate clauses.',
		see_also is [dataset_protocol, isolation_forest, c45, knn, nearest_centroid, random_forest, ada_boost]
	]).

	:- public(predict_probabilities/3).
	:- mode(predict_probabilities(+compound, +list, -list), one).
	:- info(predict_probabilities/3, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier. Returns a list of ``Class-Probability`` pairs. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2, nth1/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(type, [
		valid/2
	]).

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
	remove_duplicates_sorted([X], [X]) :-
		!.
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

	predict(Classifier, Instance, Class) :-
		predict_probabilities(Classifier, Instance, Probabilities),
		max_probability(Probabilities, Class, _).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier =.. [_, Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams],
		extract_values(AttributeNames, Instance, Values),
		compute_posteriors(Values, Classes, ClassPriors, FeatureTypes, FeatureParams, Probabilities).

	compute_posteriors(_, [], _, _, _, []) :-
		!.
	compute_posteriors(Values, [Class| Classes], ClassPriors, FeatureTypes, FeatureParams, [Class-Posterior| Pairs]) :-
		memberchk(Class-Prior, ClassPriors),
		compute_likelihood(Values, Class, FeatureTypes, FeatureParams, 1, 1, Likelihood),
		Posterior is Prior * Likelihood,
		compute_posteriors(Values, Classes, ClassPriors, FeatureTypes, FeatureParams, Pairs).

	compute_likelihood([], _, _, _, _, Likelihood, Likelihood).
	compute_likelihood([Value| Values], Class, [Type| Types], [Param| Params], Index, Likelihood0, Likelihood) :-
		feature_likelihood(Type, Param, Value, Class, FeatureLikelihood),
		NextIndex is Index + 1,
		Likelihood1 is FeatureLikelihood * Likelihood0,
		compute_likelihood(Values, Class, Types, Params, NextIndex, Likelihood1, Likelihood).

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

	% Auxiliary predicates
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

	classifier_diagnostics_data(Classifier, [
		model(naive_bayes),
		classes(Classes),
		attributes(AttributeNames),
		feature_types(FeatureTypes)
	]) :-
		classifier_data(Classifier, Classes, _ClassPriors, AttributeNames, FeatureTypes, _FeatureParams).

	check_classifier(Classifier) :-
		(   classifier_data(Classifier, Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams),
			^^valid_class_values(Classes),
			valid_class_priors(ClassPriors, Classes),
			^^valid_attribute_names(AttributeNames),
			^^valid_feature_types(FeatureTypes, [continuous, categorical]),
			length(AttributeNames, FeatureCount),
			length(FeatureTypes, FeatureCount),
			length(FeatureParams, FeatureCount),
			valid_feature_params(FeatureParams, AttributeNames, FeatureTypes, Classes) ->
			true
		;   domain_error(classifier, Classifier)
		).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(nb_classifier(_Classes, _ClassPriors, _AttributeNames, _FeatureTypes, _FeatureParams), nb_classifier('Classes', 'ClassPriors', 'AttributeNames', 'FeatureTypes', 'FeatureParams')).

	valid_class_priors(ClassPriors, Classes) :-
		length(ClassPriors, Count),
		length(Classes, Count),
		valid_class_priors(ClassPriors, Classes, [], 0.0, Sum),
		Delta is abs(Sum - 1.0),
		Delta =< 1.0e-6.

	valid_class_priors([], _Classes, _SeenClasses, Sum, Sum).
	valid_class_priors([Class-Prior| ClassPriors], Classes, SeenClasses, Sum0, Sum) :-
		memberchk(Class, Classes),
		\+ member(Class, SeenClasses),
		valid(positive_float, Prior),
		Sum1 is Sum0 + Prior,
		valid_class_priors(ClassPriors, Classes, [Class| SeenClasses], Sum1, Sum).

	valid_feature_params([], [], [], _Classes).
	valid_feature_params([feature(Attribute, categorical, Probs)| FeatureParams], [Attribute| AttributeNames], [categorical| FeatureTypes], Classes) :-
		valid_categorical_probs(Probs, Classes),
		valid_feature_params(FeatureParams, AttributeNames, FeatureTypes, Classes).
	valid_feature_params([feature(Attribute, continuous, Stats)| FeatureParams], [Attribute| AttributeNames], [continuous| FeatureTypes], Classes) :-
		valid_continuous_stats(Stats, Classes),
		valid_feature_params(FeatureParams, AttributeNames, FeatureTypes, Classes).

	valid_categorical_probs(Probs, Classes) :-
		length(Probs, Count),
		length(Classes, Count),
		valid_categorical_probs(Probs, Classes, []).

	valid_categorical_probs([], _Classes, _SeenClasses).
	valid_categorical_probs([Class-ValueProbs| Probs], Classes, SeenClasses) :-
		memberchk(Class, Classes),
		\+ member(Class, SeenClasses),
		valid_value_probabilities(ValueProbs, 0.0, Sum),
		Delta is abs(Sum - 1.0),
		Delta =< 1.0e-6,
		valid_categorical_probs(Probs, Classes, [Class| SeenClasses]).

	valid_value_probabilities([], Sum, Sum).
	valid_value_probabilities([Value-Probability| ValueProbabilities], Sum0, Sum) :-
		nonvar(Value),
		valid(positive_float, Probability),
		Sum1 is Sum0 + Probability,
		valid_value_probabilities(ValueProbabilities, Sum1, Sum).

	valid_continuous_stats(Stats, Classes) :-
		length(Stats, Count),
		length(Classes, Count),
		valid_continuous_stats(Stats, Classes, []).

	valid_continuous_stats([], _Classes, _SeenClasses).
	valid_continuous_stats([Class-stats(Mean, Variance)| Stats], Classes, SeenClasses) :-
		memberchk(Class, Classes),
		\+ member(Class, SeenClasses),
		valid(float, Mean),
		valid(non_negative_float, Variance),
		valid_continuous_stats(Stats, Classes, [Class| SeenClasses]).

	classifier_data(Classifier, Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams) :-
		Classifier =.. [_Functor, Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams].

	print_classifier(Classifier) :-
		classifier_data(Classifier, Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams),
		format('Naive Bayes Classifier~n', []),
		format('======================~n~n', []),
		^^print_classifier_template(Classifier),
		format('Classes: ~w~n~n', [Classes]),
		format('Class Priors:~n', []),
		print_priors(ClassPriors),
		format('~nFeatures:~n', []),
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

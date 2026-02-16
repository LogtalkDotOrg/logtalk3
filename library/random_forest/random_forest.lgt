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


:- object(random_forest,
	implements(classifier_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-16,
		comment is 'Random Forest classifier using C4.5 decision trees as base learners. Builds an ensemble of decision trees trained on bootstrap samples with random feature subsets and combines their predictions through majority voting.',
		remarks is [
			'Algorithm' - 'Random Forest is an ensemble learning method that constructs multiple decision trees during training and outputs the class that is the mode of the classes predicted by individual trees.',
			'Bootstrap sampling' - 'Each tree is trained on a bootstrap sample (random sample with replacement) of the training data.',
			'Feature randomization' - 'At each tree, a random subset of features is selected. The default number of features is sqrt(total_features).',
			'Classifier representation' - 'The learned classifier is represented as ``random_forest_classifier(Trees, ClassValues, Options)`` where ``Trees`` is a list of ``tree(Tree, AttributeNames)`` pairs.'
		],
		see_also is [dataset_protocol, c45, knn, naive_bayes]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a classifier from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Classifier', 'Options']
	]).

	:- public(predict_probabilities/3).
	:- mode(predict_probabilities(+compound, +list, -list), one).
	:- info(predict_probabilities/3, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier. Returns a list of ``Class-Probability`` pairs sorted by descending probability. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

	:- uses(c45, [
		learn/2 as c45_learn/2,
		predict/3 as c45_predict/3
	]).

	:- uses(fast_random, [
		between/3 as random_between/3,
		permutation/2 as random_permutation/2
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, nth1/3, take/3
	]).

	:- uses(pairs, [
		keys/2, keys_values/3
	]).

	:- uses(type, [
		valid/2
	]).

	% learn/2 - learns a classifier with default options
	learn(Dataset, Classifier) :-
		learn(Dataset, Classifier, []).

	% learn/3 - learns a classifier with specified options
	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(number_of_trees(NumTrees), Options),
		% Get attribute information from dataset
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		length(AttributeNames, NumFeatures),
		% Get class values
		Dataset::class_values(ClassValues),
		% Determine max features per tree
		(	^^option(maximum_features_per_tree(MaxFeatures), Options) ->
			true
		;	% Default: sqrt(num_features)
			MaxFeatures is max(1, floor(sqrt(NumFeatures)))
		),
		% Build the forest
		build_forest(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, Trees),
		% Create classifier term
		Classifier = random_forest_classifier(Trees, ClassValues, Options).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	% build_forest/6 - builds the specified number of trees
	build_forest(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, Trees) :-
		build_forest_acc(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, 1, [], Trees).

	build_forest_acc(_, NumTrees, _, _, _, TreeID, Acc, Trees) :-
		TreeID > NumTrees,
		!,
		Trees = Acc.
	build_forest_acc(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, TreeID, Acc, Trees) :-
		% Select random feature subset
		select_feature_subset(AttributeNames, Attributes, MaxFeatures, SelectedNames, SelectedAttributes),
		% Create bootstrap dataset object
		create_bootstrap_dataset(Dataset, SelectedNames, SelectedAttributes, BootstrapDataset),
		% Train a C4.5 tree on the bootstrap sample
		c45_learn(BootstrapDataset, Tree),
		% Clean up bootstrap dataset
		abolish_object(BootstrapDataset),
		% Add tree to accumulator
		NextTreeID is TreeID + 1,
		build_forest_acc(Dataset, NumTrees, Attributes, AttributeNames, MaxFeatures, NextTreeID, [tree(Tree, SelectedNames)| Acc], Trees).

	% select_feature_subset/5 - randomly selects a subset of features
	select_feature_subset(AttributeNames, Attributes, MaxFeatures, SelectedNames, SelectedAttributes) :-
		% Pair names with attributes for shuffling
		keys_values(Pairs, AttributeNames, Attributes),
		% Randomly permute and take first MaxFeatures
		random_permutation(Pairs, ShuffledPairs),
		take(MaxFeatures, ShuffledPairs, SelectedPairs),
		% Unzip the pairs
		keys_values(SelectedPairs, SelectedNames, SelectedAttributes).

	% create_bootstrap_dataset/4 - creates a temporary dataset with bootstrap sample
	create_bootstrap_dataset(Dataset, SelectedNames, SelectedAttributes, BootstrapDataset) :-
		% Get all examples
		findall(
			Id-Class-AVs,
			Dataset::example(Id, Class, AVs),
			Examples
		),
		length(Examples, N),
		% Create bootstrap sample (sample with replacement)
		bootstrap_sample(Examples, N, BootstrapExamples),
		% Get class info
		Dataset::class(ClassName),
		Dataset::class_values(ClassValues),
		% Build the object dynamically
		create_bootstrap_object(BootstrapDataset, SelectedNames, SelectedAttributes, ClassName, ClassValues, BootstrapExamples).

	bootstrap_sample(Examples, N, BootstrapExamples) :-
		bootstrap_sample(Examples, N, N, BootstrapExamples).

	bootstrap_sample(_, _, 0, []) :-
		!.
	bootstrap_sample(Examples, TotalExamples, Remaining, [NewIdx-Class-AVs| BootstrapExamples]) :-
		random_between(1, TotalExamples, Idx),
		nth1(Idx, Examples, Example),
		NewRemaining is Remaining - 1,
		NewIdx is TotalExamples - Remaining + 1,
		Example = _-Class-AVs,
		bootstrap_sample(Examples, TotalExamples, NewRemaining, BootstrapExamples).

	create_bootstrap_object(Name, SelectedNames, SelectedAttributes, ClassName, ClassValues, Examples) :-
		% Build attribute_values clauses
		build_attribute_clauses(SelectedNames, SelectedAttributes, AttributeClauses),
		% Build example clauses (filtering to selected attributes)
		build_example_clauses(Examples, SelectedNames, ExampleClauses),
		% Create the object with all clauses
		append(AttributeClauses, ExampleClauses, DataClauses),
		AllClauses = [class(ClassName), class_values(ClassValues)| DataClauses],
		create_object(
			Name,
			[implements(dataset_protocol)],
			[],
			AllClauses
		).

	build_attribute_clauses([], [], []).
	build_attribute_clauses([Name| Names], [Name-Values| Attrs], [attribute_values(Name, Values)| Clauses]) :-
		build_attribute_clauses(Names, Attrs, Clauses).

	build_example_clauses([], _, []).
	build_example_clauses([Id-Class-AVs| Examples], SelectedNames, [example(Id, Class, FilteredAVs)| Clauses]) :-
		filter_attribute_values(SelectedNames, AVs, FilteredAVs),
		build_example_clauses(Examples, SelectedNames, Clauses).

	filter_attribute_values([], _, []).
	filter_attribute_values([Name| Names], AVs, [Name-Value| FilteredAVs]) :-
		memberchk(Name-Value, AVs),
		filter_attribute_values(Names, AVs, FilteredAVs).

	% predict/3 - predicts the class for an instance using majority voting
	predict(Classifier, Instance, Class) :-
		predict_probabilities(Classifier, Instance, Probabilities),
		max_probability(Probabilities, Class, _).

	% predict_probabilities/3 - returns class probabilities based on tree votes
	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier = random_forest_classifier(Trees, ClassValues, _Options),
		% Collect predictions from all trees
		collect_predictions(Trees, Instance, Predictions),
		% Count votes for each class
		count_class_votes(Predictions, ClassValues, Votes),
		% Normalize to probabilities
		length(Predictions, TotalVotes),
		normalize_votes(Votes, TotalVotes, Probabilities).

	collect_predictions([], _, []).
	collect_predictions([tree(Tree, AttributeNames)| Trees], Instance, [Prediction| Predictions]) :-
		% Filter instance to only include attributes used by this tree
		filter_instance(Instance, AttributeNames, FilteredInstance),
		% Get prediction from C4.5 tree
		c45_predict(Tree, FilteredInstance, Prediction),
		collect_predictions(Trees, Instance, Predictions).

	filter_instance(_, [], []) :-
		!.
	filter_instance(Instance, [Name| Names], [Name-Value| FilteredInstance]) :-
		memberchk(Name-Value, Instance),
		filter_instance(Instance, Names, FilteredInstance).

	count_class_votes(Predictions, ClassValues, Votes) :-
		count_votes_for_classes(ClassValues, Predictions, Votes).

	count_votes_for_classes([], _, []).
	count_votes_for_classes([Class| Classes], Predictions, [Class-Count| Votes]) :-
		count_occurrences(Predictions, Class, 0, Count),
		count_votes_for_classes(Classes, Predictions, Votes).

	count_occurrences([], _, Count, Count).
	count_occurrences([Class| Classes], Class, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		count_occurrences(Classes, Class, Count1, Count).
	count_occurrences([_| Classes], Class, Count0, Count) :-
		count_occurrences(Classes, Class, Count0, Count).

	normalize_votes([], _, []).
	normalize_votes([Class-Count| Votes], Total, [Class-Probability| ClassProbabilities]) :-
		(	Total > 0 ->
			Probability is float(Count / Total)
		;	Probability is 0.0
		),
		normalize_votes(Votes, Total, ClassProbabilities).

	max_probability([Class-Probability], Class, Probability) :-
		!.
	max_probability([Class1-Prob1, Class2-Prob2| Rest], MaxClass, MaxProb) :-
		(	Prob1 >= Prob2 ->
			max_probability([Class1-Prob1| Rest], MaxClass, MaxProb)
		;	max_probability([Class2-Prob2| Rest], MaxClass, MaxProb)
		).

	% classifier_to_clauses/4 - exports classifier as a clause
	classifier_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	% classifier_to_file/4 - exports classifier to a file
	classifier_to_file(Dataset, Classifier, Functor, File) :-
		classifier_to_clauses(Dataset, Classifier, Functor, Clauses),
		open(File, write, Stream),
		write_file_header(Classifier, Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_file_header(Classifier, Functor, Stream) :-
		Classifier = random_forest_classifier(Trees, _, _),
		length(Trees, NumTrees),
		format(Stream, '%% Random Forest classifier exported by random_forest library~n', []),
		format(Stream, '%% Number of trees: ~w~n', [NumTrees]),
		format(Stream, '%% Load this file and use the classifier with:~n', []),
		format(Stream, '%%   ~w(Classifier),~n', [Functor]),
		format(Stream, '%%   random_forest::predict(Classifier, Instance, Class)~n', []),
		format(Stream, '%%   random_forest::predict_probabilities(Classifier, Instance, Probabilities)~n~n', []).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	% print_classifier/1 - pretty prints the classifier
	print_classifier(Classifier) :-
		Classifier = random_forest_classifier(Trees, ClassValues, Options),
		length(Trees, NumTrees),
		format('Random Forest Classifier~n', []),
		format('========================~n~n', []),
		format('Number of trees: ~w~n', [NumTrees]),
		format('Class values: ~w~n', [ClassValues]),
		format('Options: ~w~n~n', [Options]),
		format('Trees:~n', []),
		print_trees(Trees, 1).

	print_trees([], _).
	print_trees([tree(Tree, AttributeNames)| Trees], N) :-
		format('  Tree ~w (features: ~w):~n', [N, AttributeNames]),
		print_tree_summary(Tree),
		N1 is N + 1,
		print_trees(Trees, N1).

	print_tree_summary(leaf(Class)) :-
		format('    -> leaf(~w)~n', [Class]).
	print_tree_summary(tree(Attr, _)) :-
		format('    -> tree rooted at ~w~n', [Attr]).
	print_tree_summary(tree(Attr, threshold(_), _, _)) :-
		format('    -> tree rooted at ~w (continuous)~n', [Attr]).

	% Default options
	default_option(number_of_trees(10)).

	% Option validation
	valid_option(number_of_trees(N)) :-
		valid(positive_integer, N).
	valid_option(maximum_features_per_tree(N)) :-
		valid(positive_integer, N).

:- end_object.

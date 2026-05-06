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


:- object(gradient_boosting_classifier,
	imports(classifier_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Gradient boosting classifier using multinomial additive models fitted by regression trees to softmax residuals.',
		see_also is [dataset_protocol, regression_tree, gradient_boosting_regression, ada_boost, random_forest]
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
		comment is 'Predicts class probabilities for a new instance using a softmax over the additive stage scores.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

	:- uses(regression_tree, [
		learn/3 as tree_learn/3, predict/3 as tree_predict/3, valid_regressor/1 as valid_tree/1
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, reverse/2
	]).

	:- uses(numberlist, [
		softmax/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Classifier) :-
		learn(Dataset, Classifier, []).

	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		Dataset::class_values(Classes),
		initial_scores(Classes, Examples, InitialScores),
		initialize_score_vectors(Examples, InitialScores, ScoreVectors0),
		build_tree_options(Options, TreeOptions),
		^^option(number_of_estimators(NumberOfEstimators), Options),
		^^option(learning_rate(LearningRate), Options),
		build_stages(Classes, Attributes, Examples, NumberOfEstimators, LearningRate, TreeOptions, 1, ScoreVectors0, [], StageTrees0),
		reverse(StageTrees0, StageTrees),
		Classifier = gradient_boosting_classifier(Classes, InitialScores, StageTrees, Options).

	predict(Classifier, Instance, Class) :-
		predict_probabilities(Classifier, Instance, Probabilities),
		max_probability(Probabilities, Class, _).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier = gradient_boosting_classifier(Classes, InitialScores, StageTrees, _Options),
		predict_scores(StageTrees, Instance, InitialScores, FinalScores),
		softmax(FinalScores, Probabilities0),
		zip_probabilities(Classes, Probabilities0, Probabilities).

	initial_scores(Classes, Examples, Scores) :-
		length(Examples, TotalCount),
		initial_scores(Classes, Examples, TotalCount, Scores).

	initial_scores([], _Examples, _TotalCount, []).
	initial_scores([Class| Classes], Examples, TotalCount, [Score| Scores]) :-
		class_count(Examples, Class, 0, Count),
		Prior is Count / TotalCount,
		SafePrior is max(Prior, 1.0e-12),
		Score is log(SafePrior),
		initial_scores(Classes, Examples, TotalCount, Scores).

	class_count([], _Class, Count, Count).
	class_count([_-Class-_| Examples], Class, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		class_count(Examples, Class, Count1, Count).
	class_count([_| Examples], Class, Count0, Count) :-
		class_count(Examples, Class, Count0, Count).

	initialize_score_vectors([], _InitialScores, []).
	initialize_score_vectors([_| Examples], InitialScores, [InitialScores| ScoreVectors]) :-
		initialize_score_vectors(Examples, InitialScores, ScoreVectors).

	build_tree_options(Options, [maximum_depth(MaximumDepth), minimum_samples_leaf(MinimumSamplesLeaf), minimum_variance_reduction(MinimumVarianceReduction), feature_scaling(FeatureScaling)]) :-
		^^option(maximum_depth(MaximumDepth), Options),
		^^option(minimum_samples_leaf(MinimumSamplesLeaf), Options),
		^^option(minimum_variance_reduction(MinimumVarianceReduction), Options),
		^^option(feature_scaling(FeatureScaling), Options).

	build_stages(_Classes, _Attributes, _Examples, NumberOfEstimators, _LearningRate, _TreeOptions, Round, ScoreVectors, StageTrees, StageTrees) :-
		Round > NumberOfEstimators,
		!,
		ScoreVectors \== [].
	build_stages(Classes, Attributes, Examples, NumberOfEstimators, LearningRate, TreeOptions, Round, ScoreVectors0, StageTrees0, StageTrees) :-
		probability_vectors(ScoreVectors0, ProbabilityVectors),
		build_stage_trees(Classes, Attributes, Examples, ProbabilityVectors, LearningRate, TreeOptions, 1, [], ReversedClassTrees, 0.0, MaxResidual),
		reverse(ReversedClassTrees, ClassTrees),
		(   MaxResidual =< 1.0e-8 ->
			StageTrees = StageTrees0
		;   update_score_vectors(Examples, Classes, ClassTrees, ScoreVectors0, ScoreVectors1),
			NextRound is Round + 1,
			build_stages(Classes, Attributes, Examples, NumberOfEstimators, LearningRate, TreeOptions, NextRound, ScoreVectors1, [stage_trees(ClassTrees)| StageTrees0], StageTrees)
		).

	probability_vectors([], []).
	probability_vectors([Scores| ScoreVectors], [Probabilities| ProbabilityVectors]) :-
		softmax(Scores, Probabilities),
		probability_vectors(ScoreVectors, ProbabilityVectors).

	build_stage_trees([], _Attributes, _Examples, _ProbabilityVectors, _LearningRate, _TreeOptions, _Index, ClassTrees, ClassTrees, MaxResidual, MaxResidual).
	build_stage_trees([Class| Classes], Attributes, Examples, ProbabilityVectors, LearningRate, TreeOptions, Index, ClassTrees0, ClassTrees, MaxResidual0, MaxResidual) :-
		create_residual_examples(Examples, ProbabilityVectors, Index, Class, ResidualExamples, ResidualMax),
		create_residual_dataset(Attributes, ResidualExamples, ResidualDataset),
		tree_learn(ResidualDataset, Tree, TreeOptions),
		abolish_object(ResidualDataset),
		MaxResidual1 is max(MaxResidual0, ResidualMax),
		NextIndex is Index + 1,
		build_stage_trees(Classes, Attributes, Examples, ProbabilityVectors, LearningRate, TreeOptions, NextIndex, [class_tree(Class, LearningRate, Tree)| ClassTrees0], ClassTrees, MaxResidual1, MaxResidual).

	create_residual_examples([], [], _Index, _Class, [], 0.0).
	create_residual_examples([Id-Class-AttributeValues| Examples], [Probabilities| ProbabilityVectors], Index, TargetClass, [example(Id, Residual, AttributeValues)| ResidualExamples], MaxResidual) :-
		probability_at(Index, Probabilities, Probability),
		(   Class == TargetClass ->
			Target = 1.0
		;   Target = 0.0
		),
		Residual is Target - Probability,
		ResidualMagnitude is abs(Residual),
		create_residual_examples(Examples, ProbabilityVectors, Index, TargetClass, ResidualExamples, RestMax),
		MaxResidual is max(ResidualMagnitude, RestMax).

	probability_at(1, [Probability| _], Probability) :-
		!.
	probability_at(Index, [_| Probabilities], Probability) :-
		NextIndex is Index - 1,
		probability_at(NextIndex, Probabilities, Probability).

	create_residual_dataset(Attributes, ResidualExamples, ResidualDataset) :-
		build_attribute_clauses(Attributes, AttributeClauses),
		append(AttributeClauses, ResidualExamples, DataClauses),
		AllClauses = [target(residual)| DataClauses],
		create_object(ResidualDataset, [implements(regression_dataset_protocol)], [], AllClauses).

	build_attribute_clauses([], []).
	build_attribute_clauses([Attribute-Values| Attributes], [attribute_values(Attribute, Values)| Clauses]) :-
		build_attribute_clauses(Attributes, Clauses).

	update_score_vectors([], _Classes, _ClassTrees, [], []).
	update_score_vectors([_Id-_Class-AttributeValues| Examples], Classes, ClassTrees, [Scores0| ScoreVectors0], [Scores| ScoreVectors]) :-
		update_instance_scores(Classes, ClassTrees, AttributeValues, Scores0, Scores),
		update_score_vectors(Examples, Classes, ClassTrees, ScoreVectors0, ScoreVectors).

	update_instance_scores([], _ClassTrees, _AttributeValues, [], []).
	update_instance_scores([Class| Classes], ClassTrees, AttributeValues, [Score0| Scores0], [Score| Scores]) :-
		class_tree_prediction(ClassTrees, Class, AttributeValues, Prediction),
		Score is Score0 + Prediction,
		update_instance_scores(Classes, ClassTrees, AttributeValues, Scores0, Scores).

	class_tree_prediction([class_tree(Class, LearningRate, Tree)| _], Class, AttributeValues, Prediction) :-
		!,
		tree_predict(Tree, AttributeValues, TreePrediction),
		Prediction is LearningRate * TreePrediction.
	class_tree_prediction([_| ClassTrees], Class, AttributeValues, Prediction) :-
		class_tree_prediction(ClassTrees, Class, AttributeValues, Prediction).

	predict_scores([], _Instance, Scores, Scores).
	predict_scores([stage_trees(ClassTrees)| StageTrees], Instance, Scores0, Scores) :-
		update_instance_scores_with_stage(ClassTrees, Instance, Scores0, Scores1),
		predict_scores(StageTrees, Instance, Scores1, Scores).

	update_instance_scores_with_stage([], _Instance, [], []).
	update_instance_scores_with_stage([class_tree(_Class, LearningRate, Tree)| ClassTrees], Instance, [Score0| Scores0], [Score| Scores]) :-
		tree_predict(Tree, Instance, TreePrediction),
		Score is Score0 + LearningRate * TreePrediction,
		update_instance_scores_with_stage(ClassTrees, Instance, Scores0, Scores).

	zip_probabilities([], [], []).
	zip_probabilities([Class| Classes], [Probability| Probabilities0], [Class-Probability| Probabilities]) :-
		zip_probabilities(Classes, Probabilities0, Probabilities).

	max_probability([Class-Probability], Class, Probability) :-
		!.
	max_probability([Class1-Probability1, Class2-Probability2| Probabilities], Class, Probability) :-
		(   Probability1 >= Probability2 ->
			max_probability([Class1-Probability1| Probabilities], Class, Probability)
		;   max_probability([Class2-Probability2| Probabilities], Class, Probability)
		).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		Classifier = gradient_boosting_classifier(Classes, InitialScores, StageTrees, Options),
		length(StageTrees, StageCount),
		Diagnostics = [
			model(gradient_boosting_classifier),
			classes(Classes),
			initial_scores(InitialScores),
			stage_count(StageCount),
			options(Options)
		].

	check_classifier(Classifier) :-
		(   Classifier = gradient_boosting_classifier(Classes, InitialScores, StageTrees, Options),
			^^valid_class_values(Classes),
			length(Classes, ClassCount),
			valid(list(float, ClassCount), InitialScores),
			catch(::check_options(Options), _Error, fail),
			valid_stage_trees(StageTrees, Classes) ->
			true
		;   domain_error(classifier, Classifier)
		).

	valid_stage_trees([], _Classes).
	valid_stage_trees([stage_trees(ClassTrees)| StageTrees], Classes) :-
		valid_class_trees(ClassTrees, Classes),
		valid_stage_trees(StageTrees, Classes).

	valid_class_trees([], _Classes).
	valid_class_trees([class_tree(Class, LearningRate, Tree)| ClassTrees], Classes) :-
		memberchk(Class, Classes),
		valid(positive_float, LearningRate),
		valid_tree(Tree),
		valid_class_trees(ClassTrees, Classes).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(gradient_boosting_classifier(_Classes, _InitialScores, _StageTrees, _Options), gradient_boosting_classifier('Classes', 'InitialScores', 'StageTrees', 'Options')).

	print_classifier(Classifier) :-
		Classifier = gradient_boosting_classifier(Classes, InitialScores, StageTrees, Options),
		format('Gradient Boosting Classifier~n', []),
		format('===========================~n~n', []),
		^^print_classifier_template(Classifier),
		format('Classes: ~w~n', [Classes]),
		format('Initial scores: ~w~n', [InitialScores]),
		length(StageTrees, StageCount),
		format('Stages: ~w~n', [StageCount]),
		format('Options: ~w~n', [Options]).

	default_option(number_of_estimators(25)).
	default_option(learning_rate(0.1)).
	default_option(maximum_depth(3)).
	default_option(minimum_samples_leaf(1)).
	default_option(minimum_variance_reduction(0.0)).
	default_option(feature_scaling(false)).

	valid_option(number_of_estimators(NumberOfEstimators)) :-
		valid(positive_integer, NumberOfEstimators).
	valid_option(learning_rate(LearningRate)) :-
		valid(positive_float, LearningRate).
	valid_option(maximum_depth(MaximumDepth)) :-
		valid(positive_integer, MaximumDepth).
	valid_option(minimum_samples_leaf(MinimumSamplesLeaf)) :-
		valid(positive_integer, MinimumSamplesLeaf).
	valid_option(minimum_variance_reduction(MinimumVarianceReduction)) :-
		valid(non_negative_float, MinimumVarianceReduction).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.

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
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Unit tests for the "linear_svm_classifier" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- private(dataset_classifier_/2).
	:- dynamic(dataset_classifier_/2).

	cover(linear_svm_classifier).

	setup :-
		retractall(dataset_classifier_(_, _)).

	cleanup :-
		retractall(dataset_classifier_(_, _)),
		^^clean_file('test_output.pl'),
		^^clean_file('bias_output.pl').

	test(linear_svm_learn_2_weather, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(weather, Classifier),
		retractall(dataset_classifier_(weather, _)),
		assertz(dataset_classifier_(weather, Classifier)).

	test(linear_svm_valid_classifier_1, deterministic(linear_svm_classifier::valid_classifier(Classifier))) :-
		dataset_classifier(weather, Classifier).

	test(linear_svm_invalid_classifier_1, fail) :-
		dataset_classifier(weather, linear_svm_classifier(Classes, Encoders, _Models, Options)),
		linear_svm_classifier::valid_classifier(linear_svm_classifier(Classes, Encoders, [class_model(yes, 0.0, [0.0]), class_model(no, 0.0, [0.0])], Options)).

	test(linear_svm_predict_3_weather_yes, deterministic(Prediction == yes)) :-
		dataset_classifier(weather, Classifier),
		linear_svm_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(linear_svm_predict_3_weather_no, deterministic(Prediction == no)) :-
		dataset_classifier(weather, Classifier),
		linear_svm_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-strong], Prediction).

	test(linear_svm_learn_2_mixed, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(mixed, Classifier),
		retractall(dataset_classifier_(mixed, _)),
		assertz(dataset_classifier_(mixed, Classifier)).

	test(linear_svm_predict_3_mixed_no, deterministic(Prediction == no)) :-
		dataset_classifier(mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-26, income-36000, student-yes, credit_rating-fair], Prediction).

	test(linear_svm_predict_3_mixed_yes, deterministic(Prediction == yes)) :-
		dataset_classifier(mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Prediction).

	test(linear_svm_learn_2_missing_mixed, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(missing_mixed, Classifier),
		retractall(dataset_classifier_(missing_mixed, _)),
		assertz(dataset_classifier_(missing_mixed, Classifier)).

	test(linear_svm_predict_3_missing_mixed_yes, deterministic(Prediction == yes)) :-
		dataset_classifier(missing_mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-_, income-70500, student-no, credit_rating-fair], Prediction).

	test(linear_svm_predict_3_missing_mixed_no, deterministic(Prediction == no)) :-
		dataset_classifier(missing_mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-28, income-_, student-yes, credit_rating-excellent], Prediction).

	test(linear_svm_predict_3_unseen_categorical_value, error(domain_error(attribute_value(credit_rating, [fair, excellent]), poor))) :-
		dataset_classifier(mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-40, income-50000, student-yes, credit_rating-poor], _Prediction).

	test(linear_svm_learn_3_custom_options, deterministic([LearningRate, MaximumIterations, Tolerance, L2Regularization] == [0.05, 1500, 1.0e-6, 0.02])) :-
		linear_svm_classifier::learn(svm_bias_only([yes, no], [yes, no]), linear_svm_classifier(_Classes, _Encoders, _Models, Options), [learning_rate(0.05), maximum_iterations(1500), l2_regularization(0.02)]),
		memberchk(learning_rate(LearningRate), Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(l2_regularization(L2Regularization), Options).

	test(linear_svm_learn_2_iris_small, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(iris_small, Classifier),
		retractall(dataset_classifier_(iris_small, _)),
		assertz(dataset_classifier_(iris_small, Classifier)).

	test(linear_svm_predict_3_iris_setosa, deterministic(Prediction == setosa)) :-
		dataset_classifier(iris_small, Classifier),
		linear_svm_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(linear_svm_predict_3_iris_versicolor, deterministic(Prediction == versicolor)) :-
		dataset_classifier(iris_small, Classifier),
		linear_svm_classifier::predict(Classifier, [sepal_length-6.7, sepal_width-3.1, petal_length-4.7, petal_width-1.5], Prediction).

	test(linear_svm_predict_3_iris_virginica, deterministic(Prediction == virginica)) :-
		dataset_classifier(iris_small, Classifier),
		linear_svm_classifier::predict(Classifier, [sepal_length-6.4, sepal_width-3.0, petal_length-5.8, petal_width-2.2], Prediction).

	test(linear_svm_export_to_clauses_4, deterministic(Prediction == yes)) :-
		dataset_classifier(weather, Classifier),
		linear_svm_classifier::export_to_clauses(_Dataset, Classifier, classifier, [classifier(ExportedClassifier)]),
		linear_svm_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(linear_svm_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		dataset_classifier(weather, Classifier),
		linear_svm_classifier::export_to_file(weather, Classifier, classify, File).

	test(linear_svm_export_to_file_4_loaded, deterministic(Prediction == yes)) :-
		^^file_path('test_output.pl', File),
		dataset_classifier(weather, Classifier),
		linear_svm_classifier::export_to_file(weather, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		linear_svm_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(linear_svm_diagnostics_2, deterministic((Model == linear_svm_classifier, Options1 == Options2))) :-
		dataset_classifier(weather, Classifier),
		linear_svm_classifier::diagnostics(Classifier, Diagnostics),
		linear_svm_classifier::classifier_options(Classifier, Options1),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options2), Diagnostics).

	test(linear_svm_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		dataset_classifier(mixed, Classifier),
		linear_svm_classifier::print_classifier(Classifier).

	test(linear_svm_classifier_fixture_cache_hit, deterministic(Stored == [Classifier])) :-
		dataset_classifier(weather, Classifier),
		dataset_classifier(weather, Classifier),
		findall(Model, dataset_classifier_(weather, Model), Stored).

	test(linear_svm_classifier_fixture_cache_miss, deterministic(Stored == [Classifier])) :-
		Dataset = svm_bias_only([yes, no], [yes, no]),
		retractall(dataset_classifier_(Dataset, _)),
		dataset_classifier(Dataset, Classifier),
		dataset_classifier(Dataset, Classifier),
		findall(Model, dataset_classifier_(Dataset, Model), Stored).

	test(linear_svm_classifier_incomplete_models, all(check_incomplete_classifier(Classifier))) :-
		Cases = [
			_,
			svm(_, [], [], []),
			svm([yes| _], [], [], []),
			svm([yes], _, [], []),
			svm([yes], [continuous(x, _, 1.0)], [], []),
			svm([yes], [continuous(x, 0.0, _)], [], []),
			svm([yes], [], _, []),
			svm([yes], [], [class_model(_, 0.0, [])], []),
			svm([yes], [], [class_model(yes, _, [])], []),
			svm([yes], [], [class_model(yes, 0.0, _)], []),
			svm([yes], [], [class_model(yes, 0.0, [_])], []),
			svm([yes], [], [class_model(yes, 0.0, [0.0| _])], []),
			svm([yes], [], [class_model(yes, 0.0, [])| _], []),
			svm([yes], [], [class_model(yes, 0.0, [])], _),
			svm([yes], [], [class_model(yes, 0.0, [])], [learning_rate(_)]),
			svm([yes], [], [class_model(yes, 0.0, [])], [learning_rate(0.1)| _])
		],
		member(Classifier, Cases).

	test(linear_svm_classifier_malformed_models, all(check_malformed_classifier(Classifier))) :-
		Cases = [
			invalid,
			svm([], [], []),
			svm([], [], [], []),
			svm([yes, yes], [], [], []),
			svm([yes], [], [], []),
			svm([yes], [], [class_model(no, 0.0, [])], []),
			svm([yes, no], [], [class_model(yes, 0.0, []), class_model(yes, 0.0, [])], []),
			svm([yes], [], [class_model(yes, 0, [])], []),
			svm([yes], [], [class_model(yes, 0.0, [0.0])], []),
			svm([yes], [continuous(x, 0.0, 1.0)], [class_model(yes, 0.0, [0, 0])], []),
			svm([yes], [continuous(x, 0.0, 0.0)], [class_model(yes, 0.0, [0.0, 0.0])], []),
			svm([yes], [continuous(x, 0.0, -1.0)], [class_model(yes, 0.0, [0.0, 0.0])], []),
			svm([yes], [invalid(x)], [class_model(yes, 0.0, [])], []),
			svm([yes], [categorical(x, [])], [class_model(yes, 0.0, [0.0])], []),
			svm([yes], [categorical(x, [a, a])], [class_model(yes, 0.0, [0.0, 0.0, 0.0])], []),
			svm([yes], [continuous(x, 0.0, 1.0), continuous(x, 0.0, 1.0)], [class_model(yes, 0.0, [0.0, 0.0, 0.0, 0.0])], []),
			svm([yes], [], [class_model(yes, 0.0, [])], [learning_rate(0)])
		],
		member(Classifier, Cases).

	test(linear_svm_classifier_custom_functor_and_stored_options, all(linear_svm_classifier::check_classifier(Classifier))) :-
		Classifiers = [
			custom([yes, no], [], [class_model(no, 0.0, []), class_model(yes, 0.0, [])], []),
			custom([yes], [], [class_model(yes, 0.0, [])], [learning_rate(0.2)]),
			custom([yes], [], [class_model(yes, 0.0, [])], [learning_rate(0.2), learning_rate(0.1)])
		],
		member(Classifier, Classifiers).

	test(linear_svm_classifier_bias_only_balanced, deterministic(Prediction == yes)) :-
		linear_svm_classifier::learn(svm_bias_only([yes, no], [yes, no]), Classifier),
		linear_svm_classifier::check_classifier(Classifier),
		linear_svm_classifier::predict(Classifier, [], Prediction),
		linear_svm_classifier::diagnostics(Classifier, Diagnostics),
		assertion(memberchk(encoded_features(0), Diagnostics)).

	test(linear_svm_classifier_bias_only_unbalanced, deterministic(Prediction == no)) :-
		linear_svm_classifier::learn(svm_bias_only([yes, no], [no, yes, no]), Classifier),
		linear_svm_classifier::predict(Classifier, [], Prediction),
		linear_svm_classifier::check_classifier(Classifier).

	test(linear_svm_classifier_bias_only_single_class, deterministic(Prediction == yes)) :-
		linear_svm_classifier::learn(svm_bias_only([yes], [yes]), Classifier),
		linear_svm_classifier::predict(Classifier, [], Prediction),
		linear_svm_classifier::check_classifier(Classifier).

	test(linear_svm_classifier_bias_only_custom_round_trip, deterministic(Prediction == no)) :-
		Classifier = custom([yes, no], [], [class_model(yes, -0.5, []), class_model(no, 0.5, [])], []),
		linear_svm_classifier::check_classifier(Classifier),
		assertion(linear_svm_classifier::valid_classifier(Classifier)),
		linear_svm_classifier::export_to_clauses(svm_bias_only([yes, no], [yes, no]), Classifier, classifier, [classifier(Exported)]),
		^^file_path('bias_output.pl', File),
		linear_svm_classifier::export_to_file(svm_bias_only([yes, no], [yes, no]), Exported, bias_classifier, File),
		logtalk_load(File),
		{bias_classifier(Loaded)},
		assertion(Loaded == Classifier),
		linear_svm_classifier::predict(Loaded, [], Prediction),
		^^suppress_text_output,
		linear_svm_classifier::print_classifier(Loaded).

	test(linear_svm_classifier_empty_dataset, error(domain_error(non_empty_dataset, svm_bias_only([yes], [])))) :-
		linear_svm_classifier::learn(svm_bias_only([yes], []), _).

	test(linear_svm_classifier_continuous_statistics, all(check_continuous_statistics(Values, Expected))) :-
		Cases = [
			[_, _]-[0.0, 1.0],
			[7]-[7.0, 1.0],
			[_, 7, _]-[7.0, 1.0],
			[7, 7, 7]-[7.0, 1.0],
			[-1, 1.0]-[0.0, 1.0],
			[1, 3, 5]-[3.0, Scale],
			[1000001, 1000003, 1000005]-[1000003.0, Scale]
		],
		Scale is sqrt(8.0 / 3.0),
		member(Values-Expected, Cases).

	test(linear_svm_classifier_feature_layouts, deterministic) :-
		Encoders = [continuous(x, 2.0, 2.0), categorical(color, [red, green, blue]), continuous(y, 0.0, 1.0)],
		Cases = [
			[x-4, color-red, y-3]-[1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 3.0, 0.0],
			[x-4, color-green, y-3]-[1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 3.0, 0.0],
			[x-4, color-blue, y-3]-[1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 3.0, 0.0],
			[x-_, color-_, y-_]-[0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0],
			[]-[0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0]
		],
		forall(member(Instance-Expected, Cases), check_feature_layout(Encoders, Instance, Expected)),
		check_feature_layout([categorical(first, [a, b]), categorical(second, [c, d])], [first-b, second-c], [0.0, 1.0, 0.0, 1.0, 0.0, 0.0]),
		check_feature_layout([categorical(first, [a]), categorical(second, [c, d])], [], [0.0, 1.0, 0.0, 0.0, 1.0]),
		check_feature_layout([categorical(first, [a])], [first-a], [1.0, 0.0]),
		check_feature_layout([], [], []).

	test(linear_svm_classifier_continuous_prediction_type_error, error(type_error(number, invalid))) :-
		Classifier = svm([yes], [continuous(x, 0.0, 1.0)], [class_model(yes, 0.0, [1.0, 0.0])], []),
		linear_svm_classifier::predict(Classifier, [x-invalid], _).

	test(linear_svm_classifier_invalid_options, all(check_invalid_option(Option))) :-
		Options = [learning_rate(0), learning_rate(-1), learning_rate(invalid), maximum_iterations(0), maximum_iterations(-1), maximum_iterations(1.5), tolerance(-1), tolerance(invalid), l2_regularization(-1), l2_regularization(invalid)],
		member(Option, Options).

	test(linear_svm_classifier_option_list_errors, deterministic) :-
		catch(linear_svm_classifier::learn(weather, _, _), error(UnboundError, _), true),
		assertion(UnboundError == instantiation_error),
		catch(linear_svm_classifier::learn(weather, _, [invalid]), error(AtomError, _), true),
		assertion(AtomError == type_error(compound, invalid)),
		catch(linear_svm_classifier::learn(weather, _, invalid), error(ListError, _), true),
		assertion(ListError == type_error(list, invalid)).

	test(linear_svm_classifier_one_step_and_duplicate_options, deterministic) :-
		linear_svm_classifier::learn(svm_continuous([-1, 1]), Classifier, [learning_rate(0.2), learning_rate(0.1), maximum_iterations(1), tolerance(0), l2_regularization(0)]),
		Classifier =.. [_, _, _, [class_model(yes, Bias, Weights)| _], Options],
		assertion(Bias =~= 0.0),
		assertion(Weights =~= [-0.2, 0.0]),
		assertion(memberchk(learning_rate(0.2), Options)).

	test(linear_svm_classifier_two_step_regularized_update, deterministic) :-
		linear_svm_classifier::learn(svm_continuous([-1, 1]), Classifier, [maximum_iterations(2), tolerance(0)]),
		Classifier =.. [_, _, _, [class_model(yes, Bias, Weights)| _], _],
		Expected is -0.1 - 0.1 / sqrt(2.0) * (1.0 - 0.01 * 0.1),
		assertion(Bias =~= 0.0),
		assertion(Weights =~= [Expected, 0.0]).

	test(linear_svm_classifier_tolerance_exit, deterministic(Weight =~= -1.0e-9)) :-
		linear_svm_classifier::learn(svm_continuous([-1, 1]), OneStep, [maximum_iterations(1)]),
		linear_svm_classifier::learn(svm_continuous([-1, 1]), EarlyExit, [maximum_iterations(20), tolerance(1)]),
		OneStep =.. [_, _, _, Models, _],
		EarlyExit =.. [_, _, _, Models, _],
		linear_svm_classifier::learn(svm_continuous([-1, 1]), TinyStep, [learning_rate(1.0e-9), maximum_iterations(20)]),
		TinyStep =.. [_, _, _, [class_model(yes, _, [Weight| _])| _], _].

	test(linear_svm_classifier_stationary_exit_and_unregularized_bias, deterministic) :-
		linear_svm_classifier::learn(svm_bias_only([yes, no], [yes, no]), Stationary, [tolerance(0)]),
		Stationary =.. [_, _, _, [class_model(_, Bias, [])| _], _],
		assertion(Bias =~= 0.0),
		linear_svm_classifier::learn(svm_bias_only([yes, no], [yes, yes, no]), Unpenalized, [maximum_iterations(2), l2_regularization(0)]),
		linear_svm_classifier::learn(svm_bias_only([yes, no], [yes, yes, no]), Penalized, [maximum_iterations(2), l2_regularization(10)]),
		Unpenalized =.. [_, _, _, Models, _],
		Penalized =.. [_, _, _, Models, _].

	test(linear_svm_classifier_hinge_margin_gradients, all(check_margin_gradient(Bias, Label, Expected))) :-
		Cases = [0.0-yes-[-1.0, -2.0], 1.0-yes-[0.0, 0.0], 2.0-yes-[0.0, 0.0], 0.0-no-[1.0, 2.0], -1.0-no-[0.0, 0.0]],
		member(Bias-Label-Expected, Cases).

	test(linear_svm_classifier_weight_regularization_and_delta, deterministic) :-
		linear_svm_classifier << update_weights([1.0], [-2.0], 0.5, 0.1, 0.2, Weights, 0.0, Delta),
		assertion(Weights =~= [1.18]),
		assertion(Delta =~= 0.18).

	test(linear_svm_classifier_first_model_ties_and_negative_scores, all(check_score_selection(Models, Expected))) :-
		Cases = [
			[class_model(a, 0.0, [])]-a,
			[class_model(a, 0.0, []), class_model(b, 0.0, [])]-a,
			[class_model(a, -2.0, []), class_model(b, -1.0, []), class_model(c, -1.0, [])]-b,
			[class_model(c, 0.0, []), class_model(b, -1.0, []), class_model(a, 0.0, [])]-c,
			[class_model(a, 1.0, []), class_model(b, 0.0, []), class_model(c, 2.0, [])]-c
		],
		member(Models-Expected, Cases).

	test(linear_svm_classifier_binary_training_equivalence, deterministic) :-
		Datasets = [weather, mixed, missing_mixed, svm_continuous([-1, 1]), svm_bias_only([yes, no], [yes, yes, no])],
		Options = [[maximum_iterations(3), tolerance(0), l2_regularization(0)], [maximum_iterations(3), tolerance(0), l2_regularization(0.1)], [maximum_iterations(20), tolerance(1)]],
		forall(member(Dataset, Datasets), forall(member(UserOptions, Options), check_training_equivalence(Dataset, UserOptions))).

	test(linear_svm_classifier_multiclass_training_equivalence, deterministic) :-
		Datasets = [iris_small, svm_bias_only([a, b, c], [a, b, c]), svm_bias_only([a, b, c, d], [a, b, c, d]), svm_bias_only([yes], [yes])],
		forall(member(Dataset, Datasets), check_training_equivalence(Dataset, [maximum_iterations(3), tolerance(0)])).

	test(linear_svm_classifier_binary_symmetry_and_class_order, deterministic) :-
		linear_svm_classifier::learn(svm_continuous([-1, 1]), Classifier, [maximum_iterations(3), tolerance(0)]),
		Classifier =.. [_, _, _, [class_model(yes, Bias, Weights), class_model(no, NegativeBias, NegativeWeights)], _],
		ExpectedBias is -Bias,
		numberlist::rescale(Weights, -1.0, ExpectedWeights),
		assertion(NegativeBias =~= ExpectedBias),
		assertion(NegativeWeights =~= ExpectedWeights),
		linear_svm_classifier::learn(svm_bias_only([no, yes], [yes, no]), Reordered),
		linear_svm_classifier::predict(Reordered, [], Prediction),
		assertion(Prediction == no).

	test(linear_svm_classifier_weighted_prediction_selection, deterministic(Prediction == yes)) :-
		Classifier = custom([yes, no, other], [continuous(x, 0.0, 1.0)], [class_model(no, 0.5, [-0.6, 0.0]), class_model(yes, -0.1, [0.2, 0.0]), class_model(other, -1.0, [0.5, 0.0])], []),
		linear_svm_classifier::predict(Classifier, [x-1], Prediction).

	test(linear_svm_classifier_population_statistics_equivalence, all(check_population_statistics(Values))) :-
		Offset1 is 1.0e12 + 1.0,
		Offset2 is 1.0e12 + 3.0,
		Offset3 is 1.0e12 + 5.0,
		Cases = [[7], [7, 7, 7], [-1, 1.0], [1, 3, 5], [Offset1, Offset2, Offset3]],
		member(Values, Cases).

	test(linear_svm_classifier_high_cardinality_dimensions, deterministic) :-
		linear_svm_classifier::learn(svm_high_cardinality(32, 64), Classifier, [maximum_iterations(2), tolerance(0)]),
		linear_svm_classifier::check_classifier(Classifier),
		linear_svm_classifier::diagnostics(Classifier, Diagnostics),
		assertion(memberchk(encoded_features(66), Diagnostics)),
		linear_svm_classifier::predict(Classifier, [color-1, shape-17], yes),
		linear_svm_classifier::predict(Classifier, [color-32, shape-16], no).

	% auxiliary predicates

	check_population_statistics(Values) :-
		population::arithmetic_mean(Values, Mean),
		population::variance(Values, Variance),
		(	Variance > 0.0 ->
			Scale is sqrt(Variance)
		;	Scale = 1.0
		),
		check_continuous_statistics(Values, [Mean, Scale]).

	check_training_equivalence(Dataset, UserOptions) :-
		linear_svm_classifier::learn(Dataset, Classifier, UserOptions),
		Classifier =.. [_, Classes, Encoders, Models, Options],
		findall(Id-Class-Instance, Dataset::example(Id, Class, Instance), Examples),
		linear_svm_classifier << examples_to_rows(Examples, Encoders, Rows),
		linear_svm_classifier << encoders_feature_count(Encoders, FeatureCount),
		length(Rows, Count),
		Scale is 1.0 / Count,
		linear_algebra::new_vector(FeatureCount, 0.0, Zeroes),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(learning_rate(LearningRate), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(l2_regularization(Regularization), Options),
		Settings = training(MaximumIterations, LearningRate, Tolerance, Regularization, Scale, Zeroes),
		linear_svm_classifier << train_class_models(Classes, Rows, Settings, Reference),
		check_model_vectors(Models, Reference).

	check_model_vectors([], []).
	check_model_vectors([class_model(Class, Bias, Weights)| Models], [class_model(Class, ReferenceBias, ReferenceWeights)| Reference]) :-
		assertion([Bias| Weights] =~= [ReferenceBias| ReferenceWeights]),
		check_model_vectors(Models, Reference).

	check_continuous_statistics(Values, Expected) :-
		linear_svm_classifier::learn(svm_continuous(Values), Classifier, [maximum_iterations(1)]),
		Classifier =.. [_, _, [continuous(x, Mean, Scale)], _, _],
		assertion([Mean, Scale] =~= Expected).

	check_feature_layout(Encoders, Instance, Expected) :-
		linear_svm_classifier << encode_instance(Encoders, Instance, Features),
		assertion(Features =~= Expected).

	check_invalid_option(Option) :-
		catch(linear_svm_classifier::learn(weather, _, [Option]), error(Error, _), true),
		assertion(Error == domain_error(option, Option)).

	check_margin_gradient(Bias, Label, Expected) :-
		linear_svm_classifier << accumulate_binary_gradients([[2.0]-Label], yes, Bias, [0.0], 0.0, BiasGradient, [0.0], [WeightGradient]),
		assertion([BiasGradient, WeightGradient] =~= Expected).

	check_score_selection(Models, Expected) :-
		linear_svm_classifier::predict(custom([a, b, c], [], Models, []), [], Actual),
		assertion(Actual == Expected).

	check_incomplete_classifier(Classifier) :-
		catch(linear_svm_classifier::check_classifier(Classifier), error(Error, _), true),
		assertion(Error == instantiation_error),
		assertion(\+ linear_svm_classifier::valid_classifier(Classifier)).

	check_malformed_classifier(Classifier) :-
		catch(linear_svm_classifier::check_classifier(Classifier), error(Error, _), true),
		assertion(Error == domain_error(classifier, Classifier)),
		assertion(\+ linear_svm_classifier::valid_classifier(Classifier)).

	dataset_classifier(Dataset, Classifier) :-
		(	dataset_classifier_(Dataset, Classifier) ->
			true
		;	linear_svm_classifier::learn(Dataset, Classifier),
			assertz(dataset_classifier_(Dataset, Classifier))
		).

:- end_object.

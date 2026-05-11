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


:- object(qda_classifier,
	imports(classifier_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Quadratic Discriminant Analysis classifier for continuous datasets using class-specific regularized covariance models.',
		see_also is [dataset_protocol, lda_classifier, nearest_centroid_classifier]
	]).

	:- public(predict_scores/3).
	:- mode(predict_scores(+compound, +list, -list), one).
	:- info(predict_scores/3, [
		comment is 'Predicts class discriminant scores for a new instance.',
		argnames is ['Classifier', 'Instance', 'Scores']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(linear_algebra, [
		add_matrices/3, determinant/2, dot_product/3, inverse_matrix/2, matrix_column_means/2,
		matrix_vector_product/3, new_matrix/4, outer_product/3, scale_matrix/3, shift_matrix_diagonal/3,
		subtract_vectors/3
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		check_continuous_attributes(Attributes, AttributeNames),
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples_nonvar(Dataset, Examples),
		Dataset::class_values(Classes),
		^^option(feature_scaling(FeatureScaling), Options),
		^^build_linear_encoders(Attributes, Examples, FeatureScaling, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		class_models(Classes, Rows, Options, Models),
		Classifier = qda_classifier(Encoders, Models, Options).

	predict(Classifier, Instance, Class) :-
		predict_scores(Classifier, Instance, Scores),
		max_score(Scores, Class, _).

	predict_scores(Classifier, Instance, Scores) :-
		Classifier = qda_classifier(Encoders, Models, _Options),
		encode_instance(Encoders, Instance, Features),
		model_scores(Models, Features, Scores).

	check_continuous_attributes(Attributes, AttributeNames) :-
		attributes_are_continuous(Attributes),
		attribute_names(Attributes, AttributeNames),
		^^valid_attribute_names(AttributeNames).

	attributes_are_continuous([]).
	attributes_are_continuous([Attribute-Values| Attributes]) :-
		(   Values == continuous ->
			true
		;   domain_error(continuous_attribute, Attribute)
		),
		attributes_are_continuous(Attributes).

	attribute_names([], []).
	attribute_names([Attribute-_| Attributes], [Attribute| AttributeNames]) :-
		attribute_names(Attributes, AttributeNames).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([_-Class-AttributeValues| Examples], Encoders, [Class-Features| Rows]) :-
		encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		memberchk(Attribute-Value, AttributeValues),
		(   number(Value) ->
			Feature is (Value - Mean) / Scale
		;   type_error(number, Value)
		),
		encode_instance(Encoders, AttributeValues, Features).

	class_models([], _Rows, _Options, []).
	class_models([Class| Classes], Rows, Options, [class_model(Class, Prior, Mean, Precision, LogDeterminant, Constant)| Models]) :-
		collect_class_rows(Rows, Class, ClassRows),
		length(ClassRows, ClassCount),
		length(Rows, TotalCount),
		Prior is ClassCount / TotalCount,
		matrix_column_means(ClassRows, Mean),
		class_covariance(ClassRows, Mean, Covariance),
		^^option(regularization(Regularization), Options),
		shift_matrix_diagonal(Covariance, Regularization, RegularizedCovariance),
		inverse_matrix(RegularizedCovariance, Precision),
		determinant(RegularizedCovariance, Determinant0),
		Determinant is max(Determinant0, 1.0e-12),
		LogDeterminant is log(Determinant),
		Constant is log(Prior) - 0.5 * LogDeterminant,
		class_models(Classes, Rows, Options, Models).

	collect_class_rows([], _Class, []).
	collect_class_rows([Class-Features| Rows], Class, [Features| ClassRows]) :-
		!,
		collect_class_rows(Rows, Class, ClassRows).
	collect_class_rows([_| Rows], Class, ClassRows) :-
		collect_class_rows(Rows, Class, ClassRows).

	class_covariance(ClassRows, Mean, Covariance) :-
		feature_count(ClassRows, FeatureCount),
		new_matrix(FeatureCount, FeatureCount, 0.0, ZeroMatrix),
		accumulate_scatter_rows(ClassRows, Mean, ZeroMatrix, Scatter),
		length(ClassRows, Count),
		Denominator0 is Count - 1,
		(   Denominator0 > 0 ->
			Denominator = Denominator0
		;   Denominator = 1
		),
		Scale is 1.0 / Denominator,
		scale_matrix(Scatter, Scale, Covariance).

	feature_count([Features| _], FeatureCount) :-
		length(Features, FeatureCount).

	accumulate_scatter_rows([], _Mean, Scatter, Scatter).
	accumulate_scatter_rows([Row| Rows], Mean, Scatter0, Scatter) :-
		subtract_vectors(Row, Mean, Difference),
		outer_product(Difference, Difference, OuterProduct),
		add_matrices(Scatter0, OuterProduct, Scatter1),
		accumulate_scatter_rows(Rows, Mean, Scatter1, Scatter).

	model_scores([], _Features, []).
	model_scores([class_model(Class, _Prior, Mean, Precision, _LogDeterminant, Constant)| Models], Features, [Class-Score| Scores]) :-
		subtract_vectors(Features, Mean, Difference),
		matrix_vector_product(Precision, Difference, Projected),
		dot_product(Difference, Projected, Quadratic),
		Score is Constant - 0.5 * Quadratic,
		model_scores(Models, Features, Scores).

	max_score([Class-Score], Class, Score) :-
		!.
	max_score([Class1-Score1, Class2-Score2| Scores], Class, Score) :-
		(   Score1 >= Score2 ->
			max_score([Class1-Score1| Scores], Class, Score)
		;   max_score([Class2-Score2| Scores], Class, Score)
		).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		Classifier = qda_classifier(Encoders, Models, Options),
		classes_from_models(Models, Classes),
		length(Encoders, FeatureCount),
		Diagnostics = [
			model(qda_classifier),
			classes(Classes),
			feature_count(FeatureCount),
			options(Options)
		].

	classes_from_models([], []).
	classes_from_models([class_model(Class, _Prior, _Mean, _Precision, _LogDeterminant, _Constant)| Models], [Class| Classes]) :-
		classes_from_models(Models, Classes).

	check_classifier(Classifier) :-
		(   Classifier = qda_classifier(Encoders, Models, Options),
			^^valid_linear_encoders(Encoders),
			valid_continuous_encoders(Encoders),
			catch(::check_options(Options), _Error, fail),
			length(Encoders, FeatureCount),
			valid_models(Models, FeatureCount) ->
			true
		;   domain_error(classifier, Classifier)
		).

	valid_continuous_encoders([]).
	valid_continuous_encoders([continuous(_, _, _)| Encoders]) :-
		valid_continuous_encoders(Encoders).

	valid_models([], _FeatureCount).
	valid_models([class_model(Class, Prior, Mean, Precision, LogDeterminant, Constant)| Models], FeatureCount) :-
		atom(Class),
		valid(positive_float, Prior),
		valid(list(float, FeatureCount), Mean),
		valid_square_matrix(Precision, FeatureCount),
		valid(float, LogDeterminant),
		valid(float, Constant),
		valid_models(Models, FeatureCount).

	valid_square_matrix(Matrix, Size) :-
		valid(list, Matrix),
		length(Matrix, Size),
		valid_square_matrix_rows(Matrix, Size).

	valid_square_matrix_rows([], _Size).
	valid_square_matrix_rows([Row| Rows], Size) :-
		valid(list(float, Size), Row),
		valid_square_matrix_rows(Rows, Size).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(qda_classifier(_Encoders, _Models, _Options), qda_classifier('Encoders', 'Models', 'Options')).

	print_classifier(Classifier) :-
		Classifier = qda_classifier(Encoders, Models, Options),
		format('QDA Classifier~n', []),
		format('==============~n~n', []),
		^^print_classifier_template(Classifier),
		format('Encoders: ~w~n', [Encoders]),
		format('Models: ~w~n', [Models]),
		format('Options: ~w~n', [Options]).

	default_option(feature_scaling(true)).
	default_option(regularization(1.0e-6)).

	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(regularization(Regularization)) :-
		valid(positive_float, Regularization).

:- end_object.

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


:- protocol(classifier_protocol).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Protocol for machine learning classifiers.',
		see_also is [adaptive_boosting_classifier, c45_classifier, gradient_boosting_classifier, isolation_forest_anomaly_detector, kernel_svm_classifier, knn_classifier, lda_classifier, linear_svm_classifier, logistic_regression_classifier, naive_bayes_classifier, nearest_centroid_classifier, qda_classifier, random_forest_classifier, sgd_classifier]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one).
	:- info(learn/2, [
		comment is 'Learns a classifier from the given dataset object.',
		argnames is ['Dataset', 'Classifier']
	]).

	:- public(predict/3).
	:- mode(predict(+compound, +list, -atom), one).
	:- info(predict/3, [
		comment is 'Predicts the class label for a new instance using the learned classifier. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Class']
	]).

	:- public(check_classifier/1).
	:- mode(check_classifier(@compound), one_or_error).
	:- info(check_classifier/1, [
		comment is 'Checks that a learned classifier term is structurally valid for the receiving implementation. Throws an exception when the term is not a valid classifier representation.',
		argnames is ['Classifier'],
		exceptions is [
			'``Classifier`` is a variable' - instantiation_error,
			'``Classifier`` is neither a variable nor a valid classifier' - domain_error(classifier, 'Classifier')
		]
	]).

	:- public(valid_classifier/1).
	:- mode(valid_classifier(@compound), zero_or_one).
	:- info(valid_classifier/1, [
		comment is 'True when a learned classifier term is structurally valid for the receiving implementation. Succeeds iff ``check_classifier/1`` succeeds without throwing an exception.',
		argnames is ['Classifier']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics and metadata associated with a learned classifier in a representation-independent way.',
		argnames is ['Classifier', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms for a learned classifier.',
		argnames is ['Classifier', 'Diagnostic']
	]).

	:- public(classifier_options/2).
	:- mode(classifier_options(+compound, -list(compound)), zero_or_one).
	:- info(classifier_options/2, [
		comment is 'Returns the effective training options recorded in a learned classifier diagnostics list.',
		argnames is ['Classifier', 'Options']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a classifier into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized classifier term, a noun such as ``classifier`` or ``model`` is usually clearer than a verb such as ``classify``.',
		argnames is ['Dataset', 'Classifier', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a classifier to a file. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized classifier term, a noun such as ``classifier`` or ``model`` is usually clearer than a verb such as ``classify``.',
		argnames is ['Dataset', 'Classifier', 'Functor', 'File']
	]).

	:- public(print_classifier/1).
	:- mode(print_classifier(+compound), one).
	:- info(print_classifier/1, [
		comment is 'Prints a classifier to the current output stream in a human-readable format.',
		argnames is ['Classifier']
	]).

:- end_protocol.

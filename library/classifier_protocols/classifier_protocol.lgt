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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Protocol for machine learning classifiers.',
		see_also is [c45, isolation_forest, knn, naive_bayes, nearest_centroid, random_forest]
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

	:- public(classifier_to_clauses/4).
	:- mode(classifier_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(classifier_to_clauses/4, [
		comment is 'Converts a classifier into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses.',
		argnames is ['Dataset', 'Classifier', 'Functor', 'Clauses']
	]).

	:- public(classifier_to_file/4).
	:- mode(classifier_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(classifier_to_file/4, [
		comment is 'Exports a classifier to a file. ``Functor`` is the functor for the generated predicate clauses.',
		argnames is ['Dataset', 'Classifier', 'Functor', 'File']
	]).

	:- public(print_classifier/1).
	:- mode(print_classifier(+compound), one).
	:- info(print_classifier/1, [
		comment is 'Prints a classifier to the current output stream in a human-readable format.',
		argnames is ['Classifier']
	]).

:- end_protocol.

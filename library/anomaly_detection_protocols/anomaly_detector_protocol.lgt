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


:- protocol(anomaly_detector_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Protocol for machine learning anomaly detectors.',
		see_also is [anomaly_dataset_protocol, anomaly_detector_common, isolation_forest, knn_distance, lof]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one).
	:- info(learn/2, [
		comment is 'Learns an anomaly detector from the given dataset object.',
		argnames is ['Dataset', 'AnomalyDetector']
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns an anomaly detector from the given dataset object using the specified options.',
		argnames is ['Dataset', 'AnomalyDetector', 'Options']
	]).

	:- public(check_anomaly_detector/1).
	:- mode(check_anomaly_detector(@compound), one_or_error).
	:- info(check_anomaly_detector/1, [
		comment is 'Checks that a learned anomaly detector term is structurally valid for the receiving implementation. Throws an exception when the term is not a valid anomaly detector representation.',
		argnames is ['AnomalyDetector']
	]).

	:- public(valid_anomaly_detector/1).
	:- mode(valid_anomaly_detector(@compound), zero_or_one).
	:- info(valid_anomaly_detector/1, [
		comment is 'True when a learned anomaly detector term is structurally valid for the receiving implementation. Succeeds iff ``check_anomaly_detector/1`` succeeds without throwing an exception.',
		argnames is ['AnomalyDetector']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics and metadata associated with a learned anomaly detector in a representation-independent way.',
		argnames is ['AnomalyDetector', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms for a learned anomaly detector.',
		argnames is ['AnomalyDetector', 'Diagnostic']
	]).

	:- public(anomaly_detector_options/2).
	:- mode(anomaly_detector_options(+compound, -list(compound)), one).
	:- info(anomaly_detector_options/2, [
		comment is 'Returns the effective training options recorded in a learned anomaly detector.',
		argnames is ['AnomalyDetector', 'Options']
	]).

	:- public(predict/3).
	:- mode(predict(+compound, +list, -atom), one).
	:- info(predict/3, [
		comment is 'Predicts whether a new instance is ``normal`` or ``anomaly`` using the learned detector. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['AnomalyDetector', 'Instance', 'Prediction']
	]).

	:- public(predict/4).
	:- mode(predict(+compound, +list, -atom, +list(compound)), one).
	:- info(predict/4, [
		comment is 'Predicts whether a new instance is ``normal`` or ``anomaly`` using the learned detector and the specified options. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['AnomalyDetector', 'Instance', 'Prediction', 'Options']
	]).

	:- public(score/3).
	:- mode(score(+compound, +list, -float), one).
	:- info(score/3, [
		comment is 'Computes an anomaly score in the interval ``[0.0, 1.0]`` for a new instance. Larger scores indicate more anomalous instances.',
		argnames is ['AnomalyDetector', 'Instance', 'Score']
	]).

	:- public(score_all/3).
	:- mode(score_all(+object_identifier, +compound, -list), one).
	:- info(score_all/3, [
		comment is 'Computes anomaly scores for all examples in a dataset and returns a list of ``Id-Class-Score`` triples sorted by descending score.',
		argnames is ['Dataset', 'AnomalyDetector', 'Scores']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts an anomaly detector into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized detector term, a noun such as ``detector`` or ``model`` is usually clearer than a verb such as ``detect``.',
		argnames is ['Dataset', 'AnomalyDetector', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports an anomaly detector to a file. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized detector term, a noun such as ``detector`` or ``model`` is usually clearer than a verb such as ``detect``.',
		argnames is ['Dataset', 'AnomalyDetector', 'Functor', 'File']
	]).

	:- public(print_anomaly_detector/1).
	:- mode(print_anomaly_detector(+compound), one).
	:- info(print_anomaly_detector/1, [
		comment is 'Prints an anomaly detector to the current output stream in a human-readable format.',
		argnames is ['AnomalyDetector']
	]).

:- end_protocol.

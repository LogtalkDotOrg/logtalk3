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


:- protocol(probabilistic_classifier_protocol,
	extends(classifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Protocol for classifiers that can estimate class probabilities.',
		see_also is [classifier_protocol, adaptive_boosting_classifier, gradient_boosting_classifier, kernel_svm_classifier, knn_classifier, logistic_regression_classifier, naive_bayes_classifier, nearest_centroid_classifier, random_forest_classifier, sgd_classifier]
	]).

	:- public(predict_probabilities/3).
	:- mode(predict_probabilities(+compound, +list, -list), one).
	:- info(predict_probabilities/3, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

:- end_protocol.

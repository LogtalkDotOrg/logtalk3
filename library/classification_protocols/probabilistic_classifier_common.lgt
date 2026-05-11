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


:- category(probabilistic_classifier_common,
	implements(probabilistic_classifier_protocol),
	extends(classifier_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Shared predicates for probabilistic classifiers.'
	]).

	:- protected(predict_from_probabilities/3).
	:- mode(predict_from_probabilities(+compound, +list, -atom), one).
	:- info(predict_from_probabilities/3, [
		comment is 'Predicts the class label for a new instance by selecting the maximum-probability class returned by ``predict_probabilities/3``.',
		argnames is ['Classifier', 'Instance', 'Class']
	]).

	:- protected(max_probability/3).
	:- mode(max_probability(+list(pair), -atom, -float), one).
	:- info(max_probability/3, [
		comment is 'Selects the class-probability pair with maximum probability from a non-empty list of ``Class-Probability`` pairs.',
		argnames is ['Probabilities', 'Class', 'Probability']
	]).

	predict_from_probabilities(Classifier, Instance, Class) :-
		::predict_probabilities(Classifier, Instance, Probabilities),
		max_probability(Probabilities, Class, _).

	max_probability([Class-Probability], Class, Probability) :-
		!.
	max_probability([Class1-Probability1, Class2-Probability2| Probabilities], Class, Probability) :-
		(	Probability1 >= Probability2 ->
			max_probability([Class1-Probability1| Probabilities], Class, Probability)
		;	max_probability([Class2-Probability2| Probabilities], Class, Probability)
		).

:- end_category.

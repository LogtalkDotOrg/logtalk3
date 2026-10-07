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


:- protocol(feature_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Protocol for datasets used with feature selection algorithms. Generalizes classification_protocols::dataset_protocol to also support regression targets and unsupervised use (no target at all), reusing the same attribute_values/2 predicate for familiarity.'
	]).

	:- public(attribute_values/2).
	:- mode(attribute_values(?atomic, -list(atomic)), zero_or_more).
	:- mode(attribute_values(?atomic, -atom), zero_or_more).
	:- info(attribute_values/2, [
		comment is 'Enumerates by backtracking the candidate features and their possible values. Each atomic feature name must be declared once. For discrete features, ``Values`` is a list of possible values. For continuous (numeric) features, ``Values`` is the atom ``continuous``. Never includes the target.',
		argnames is ['Feature', 'Values']
	]).

	:- public(example_count/1).
	:- mode(example_count(-positive_integer), one).
	:- info(example_count/1, [
		comment is 'Returns the number of examples. The declared count must match the number of examples enumerated by example/3.',
		argnames is ['Count']
	]).

	:- public(example/3).
	:- mode(example(-integer, -list(pair), ?term), zero_or_more).
	:- info(example/3, [
		comment is 'Enumerates by backtracking the examples in the dataset. Each example has an ``Id``, a list of ``Feature-Value`` pairs with distinct declared feature names, and a ``Target`` value. A missing feature is omitted or has an unbound value. ``Target`` is an atom for classification, a number for regression, or an unbound variable when unknown.',
		argnames is ['Id', 'Features', 'Target']
	]).

:- end_protocol.

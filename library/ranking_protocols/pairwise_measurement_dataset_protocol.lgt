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


:- protocol(pairwise_measurement_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Protocol for pairwise measurement datasets.',
		remarks is [
			'Validity requirements' - 'Datasets are expected to declare each item once, use only declared items in measurements, assign numeric measurement values, and assign a positive weight to each measurement between distinct items.',
			'Semantics' - 'Each ``measurement(Item1, Item2, Value, Weight)`` fact describes a weighted signed scalar observation for the oriented edge ``Item1 -> Item2``. Positive values favor ``Item1`` over ``Item2``, negative values favor ``Item2`` over ``Item1``, and zero values denote neutral observations.'
		]
	]).

	:- public(item/1).
	:- mode(item(-atom), zero_or_more).
	:- info(item/1, [
		comment is 'Enumerates by backtracking the items that can be ranked.',
		argnames is ['Item']
	]).

	:- public(measurement/4).
	:- mode(measurement(-atom, -atom, -number, -number), zero_or_more).
	:- info(measurement/4, [
		comment is 'Enumerates by backtracking weighted pairwise signed measurements where ``Value`` is the observed scalar measurement for the oriented edge ``Item1 -> Item2`` and ``Weight`` is its positive confidence or repetition weight.',
		argnames is ['Item1', 'Item2', 'Value', 'Weight']
	]).

:- end_protocol.

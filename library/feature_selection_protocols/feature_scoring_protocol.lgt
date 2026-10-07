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


:- protocol(feature_scoring_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Protocol for pluggable feature scoring criteria used by filter-based feature selection.'
	]).

	:- public(score/3).
	:- mode(score(+list, +list, -number), one_or_error).
	:- info(score/3, [
		comment is 'Computes a relevance score from aligned feature values and targets in dataset order. Excludes unbound feature values and, for supervised criteria, unbound targets. Unsupervised criteria ignore targets. Higher scores indicate greater relevance, but score scales and thresholds are criterion-specific.',
		argnames is ['Values', 'Targets', 'Score'],
		exceptions is [
			'A checked scoring input list or discretization parameter is unbound' - instantiation_error,
			'A checked scoring input is not a list' - type_error(list, 'Input'),
			'A complete categorical value or target is not atomic' - type_error(atomic, 'Value'),
			'A complete feature value required to be numeric is not numeric' - type_error(number, 'Value'),
			'A discretization bin count is not an integer' - type_error(integer, 'Count'),
			'A discretization bin count is not positive' - domain_error(positive_integer, 'Count'),
			'A discretization specification is invalid' - domain_error(discretization, 'Discretization'),
			'Checked scoring input list lengths differ' - consistency_error(list_length, 'ValueCount', 'TargetCount')
		]
	]).

:- end_protocol.

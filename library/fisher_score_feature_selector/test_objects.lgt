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


:- object(fisher_selection_probe,
	extends(fisher_score_feature_selector)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-08,
		comment is 'Synthetic score-boundary probe for Fisher largest-gap selection.'
	]).

	:- public(select/2).
	:- mode(select(+list(pair), -list(atomic)), one).
	:- info(select/2, [
		comment is 'Exposes the largest-gap hook for synthetic score references.',
		argnames is ['Scores', 'Selected']
	]).

	select(Scores, Selected) :-
		^^filter_selection(largest_gap, Scores, Selected).

:- end_object.


:- object(fisher_multiclass_dataset,
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Unbalanced three-class Fisher fixture with tied informative features and a constant feature.'
	]).

	attribute_values(signal, continuous).
	attribute_values(copy, continuous).
	attribute_values(constant, continuous).

	example_count(6).

	example(1, [signal-1, copy-1, constant-0.1], a).
	example(2, [signal-2, copy-2, constant-0.1], a).
	example(3, [signal-3, copy-3, constant-0.1], a).
	example(4, [signal-6, copy-6, constant-0.1], b).
	example(5, [signal-8, copy-8, constant-0.1], b).
	example(6, [signal-10, copy-10, constant-0.1], c).

:- end_object.


:- object(fisher_missing_dataset,
	extends(fisher_multiclass_dataset)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Fisher fixture with absent features, unbound observations, and an unbound target.'
	]).

	example(1, [signal-1, copy-1, constant-0.1], a).
	example(2, [signal-2, constant-0.1], a).
	example(3, [signal-4, copy-4, constant-0.1], b).
	example(4, [signal-5, constant-0.1], b).
	example(5, [signal-1000, copy-1000, constant-0.1], _).
	example(6, [signal-_, copy-_, constant-_], a).

:- end_object.

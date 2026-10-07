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


:- object(mrmr_toy,
	implements(feature_dataset_protocol)).

	attribute_values(signal, [0, 1]).
	attribute_values(copy, [0, 1]).
	attribute_values(distinct, [0, 1]).
	attribute_values(constant, [0]).

	class(label).

	class_values([0, 1, 2, 3]).

	example_count(4).

	example(1, [signal-0, copy-0, distinct-0, constant-0], 0).
	example(2, [signal-0, copy-0, distinct-1, constant-0], 1).
	example(3, [signal-1, copy-1, distinct-0, constant-0], 2).
	example(4, [signal-1, copy-1, distinct-1, constant-0], 3).

:- end_object.


:- object(mrmr_rows(_Declarations, _Rows),
	implements(feature_dataset_protocol)).

	:- uses(list, [
		member/2, length/2
	]).

	attribute_values(Feature, Declaration) :-
		parameter(1, Declarations),
		member(Feature-Declaration, Declarations).

	class(label).

	class_values([0, 1, 2, 3]).

	example_count(Count) :-
		parameter(2, Rows),
		length(Rows, Count).

	example(Id, Features, Target) :-
		parameter(2, Rows),
		member(example(Id, Features, Target), Rows).

:- end_object.


:- object(mrmr_missing,
	implements(feature_dataset_protocol)).

	attribute_values(signal, continuous).
	attribute_values(copy, [low, high]).
	attribute_values(distinct, [left, right]).

	class(label).

	class_values([0, 1, 2, 3]).

	example_count(8).

	example(1, [signal-1, copy-low, distinct-left], 0).
	example(2, [signal-2, copy-low, distinct-right], 1).
	example(3, [signal-3, copy-high, distinct-left], 2).
	example(4, [signal-4, copy-high, distinct-right], 3).
	example(5, [signal-1000, copy-_, distinct-left], 0).
	example(6, [copy-low, distinct-left], 0).
	example(7, [signal-1000, copy-high, distinct-right], _).
	example(8, [signal-1000, copy-high, distinct-_], 3).

:- end_object.


:- object(mrmr_negative,
	implements(feature_dataset_protocol)).

	attribute_values(signal, [0, 1]).
	attribute_values(noise, [0, 1]).
	attribute_values(noise_copy, [0, 1]).

	class(label).

	class_values([0, 1]).

	example_count(4).

	example(1, [signal-0, noise-0, noise_copy-0], 0).
	example(2, [signal-0, noise-1, noise_copy-1], 0).
	example(3, [signal-1, noise-0, noise_copy-0], 1).
	example(4, [signal-1, noise-1, noise_copy-1], 1).

:- end_object.


:- object(mrmr_redundancy_probe,
	imports(feature_redundancy)).

	:- public(pair_mi/3).
	:- mode(pair_mi(+list(pair), +list(pair), -float), one).
	:- info(pair_mi/3, [
		comment is 'Exposes prepared-column redundancy for unit tests.',
		argnames is ['Left', 'Right', 'Information']
	]).

	pair_mi(Left, Right, Information) :-
		^^feature_pair_mutual_information(Left, Right, Information).

:- end_object.

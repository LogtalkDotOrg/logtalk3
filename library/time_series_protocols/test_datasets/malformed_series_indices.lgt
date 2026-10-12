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


:- object(index_series(_Case_),
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Fixed observation-index validation fixtures.',
		parameters is ['Case' - 'The index validation case.']
	]).

	observation(Index, Value) :-
		indexed_value(_Case_, Index, Value).

	series_length(Length) :-
		declared_length(_Case_, Length).

	indexed_value(variable, _, 6).
	indexed_value(mixed, 1, 0).
	indexed_value(mixed, _, 6).
	indexed_value(float, 1.0, 6).
	indexed_value(compound, index(1), 6).
	indexed_value(zero, 0, 6).
	indexed_value(negative, -1, 6).
	indexed_value(duplicate, 1, 0).
	indexed_value(duplicate, 1, 6).
	indexed_value(missing_start, 2, 6).
	indexed_value(gap, 1, 0).
	indexed_value(gap, 3, 6).
	indexed_value(unordered, 3, 6).
	indexed_value(unordered, 1, 0).
	indexed_value(unordered, 2, _).
	indexed_value(singleton, 1, 6).
	indexed_value(variable_length, 1, 6).

	declared_length(variable, 1).
	declared_length(mixed, 2).
	declared_length(float, 1).
	declared_length(compound, 1).
	declared_length(zero, 1).
	declared_length(negative, 1).
	declared_length(duplicate, 2).
	declared_length(missing_start, 1).
	declared_length(gap, 2).
	declared_length(unordered, 3).
	declared_length(singleton, 1).
	declared_length(variable_length, _).
	declared_length(empty, 1).

:- end_object.
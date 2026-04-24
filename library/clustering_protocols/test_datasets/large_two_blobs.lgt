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


:- object(large_two_blobs,
	implements(clustering_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Larger deterministic two-blob dataset for clustering performance benchmarks.'
	]).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(Id, [x-X, y-Y]) :-
		left_row(Row),
		left_column(Column),
		Id is Row * 10 + Column + 1,
		X is Column / 10,
		Y is Row / 10.

	example(Id, [x-X, y-Y]) :-
		right_row(Row),
		right_column(Column),
		Id is Row * 10 + Column + 51,
		X is 5.0 + Column / 10,
		Y is 5.0 + Row / 10.

	left_row(0).
	left_row(1).
	left_row(2).
	left_row(3).
	left_row(4).

	right_row(0).
	right_row(1).
	right_row(2).
	right_row(3).
	right_row(4).

	left_column(0).
	left_column(1).
	left_column(2).
	left_column(3).
	left_column(4).
	left_column(5).
	left_column(6).
	left_column(7).
	left_column(8).
	left_column(9).

	right_column(0).
	right_column(1).
	right_column(2).
	right_column(3).
	right_column(4).
	right_column(5).
	right_column(6).
	right_column(7).
	right_column(8).
	right_column(9).

:- end_object.

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


:- object(shuttle_anomalies,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Subset of the Statlog Shuttle dataset from the UCI Machine Learning Repository. The Shuttle dataset is a well-known benchmark for anomaly detection. The dataset has 9 continuous attributes representing sensor readings from the NASA Space Shuttle. Class 1 (Rad Flow) is the majority class (normal), while all other classes are treated as anomalies. This subset contains 50 instances. Original dataset: Catlett, J. (1991). UCI Machine Learning Repository.'
	]).

	attribute_values(a1, continuous).
	attribute_values(a2, continuous).
	attribute_values(a3, continuous).
	attribute_values(a4, continuous).
	attribute_values(a5, continuous).
	attribute_values(a6, continuous).
	attribute_values(a7, continuous).
	attribute_values(a8, continuous).
	attribute_values(a9, continuous).

	class(shuttle_class).

	class_values([normal, anomaly]).

	example(Id, Class, [a1-A1, a2-A2, a3-A3, a4-A4, a5-A5, a6-A6, a7-A7, a8-A8, a9-A9]) :-
		example_(Id, Class, [A1, A2, A3, A4, A5, A6, A7, A8, A9]).

	% Rad Flow (normal class) instances
	example_(  1, normal,  [55, 42, 13, 42, 55, 13, 0,  0,  0]).
	example_(  2, normal,  [53, 40, 13, 40, 53, 13, 0,  0,  0]).
	example_(  3, normal,  [54, 41, 13, 41, 54, 13, 0,  0,  0]).
	example_(  4, normal,  [56, 43, 13, 43, 56, 13, 0,  0,  0]).
	example_(  5, normal,  [57, 44, 13, 44, 57, 13, 0,  0,  0]).
	example_(  6, normal,  [52, 39, 13, 39, 52, 13, 0,  0,  0]).
	example_(  7, normal,  [51, 38, 13, 38, 51, 13, 0,  0,  0]).
	example_(  8, normal,  [58, 45, 13, 45, 58, 13, 0,  0,  0]).
	example_(  9, normal,  [50, 37, 13, 37, 50, 13, 0,  0,  0]).
	example_( 10, normal,  [59, 46, 13, 46, 59, 13, 0,  0,  0]).
	example_( 11, normal,  [54, 42, 12, 42, 54, 12, 0,  0,  0]).
	example_( 12, normal,  [55, 43, 12, 43, 55, 12, 0,  0,  0]).
	example_( 13, normal,  [56, 44, 12, 44, 56, 12, 0,  0,  0]).
	example_( 14, normal,  [53, 41, 12, 41, 53, 12, 0,  0,  0]).
	example_( 15, normal,  [57, 45, 12, 45, 57, 12, 0,  0,  0]).
	example_( 16, normal,  [52, 40, 12, 40, 52, 12, 0,  0,  0]).
	example_( 17, normal,  [58, 46, 12, 46, 58, 12, 0,  0,  0]).
	example_( 18, normal,  [51, 39, 12, 39, 51, 12, 0,  0,  0]).
	example_( 19, normal,  [60, 47, 13, 47, 60, 13, 0,  0,  0]).
	example_( 20, normal,  [49, 36, 13, 36, 49, 13, 0,  0,  0]).
	example_( 21, normal,  [54, 41, 13, 42, 55, 13, 1,  1,  0]).
	example_( 22, normal,  [55, 42, 13, 43, 56, 13, 1,  1,  0]).
	example_( 23, normal,  [53, 40, 13, 41, 54, 13, 1,  1,  0]).
	example_( 24, normal,  [56, 43, 13, 44, 57, 13, 1,  1,  0]).
	example_( 25, normal,  [57, 44, 13, 45, 58, 13, 1,  1,  0]).
	example_( 26, normal,  [52, 39, 13, 40, 53, 13, 1,  1,  0]).
	example_( 27, normal,  [58, 45, 13, 46, 59, 13, 1,  1,  0]).
	example_( 28, normal,  [51, 38, 13, 39, 52, 13, 1,  1,  0]).
	example_( 29, normal,  [59, 46, 13, 47, 60, 13, 1,  1,  0]).
	example_( 30, normal,  [50, 37, 13, 38, 51, 13, 1,  1,  0]).
	example_( 31, normal,  [55, 42, 13, 42, 54, 12, -1, -1, 0]).
	example_( 32, normal,  [54, 41, 13, 41, 53, 12, -1, -1, 0]).
	example_( 33, normal,  [56, 43, 13, 43, 55, 12, -1, -1, 0]).
	example_( 34, normal,  [53, 40, 13, 40, 52, 12, -1, -1, 0]).
	example_( 35, normal,  [57, 44, 13, 44, 56, 12, -1, -1, 0]).
	example_( 36, normal,  [52, 39, 13, 39, 51, 12, -1, -1, 0]).
	example_( 37, normal,  [58, 45, 13, 45, 57, 12, -1, -1, 0]).
	example_( 38, normal,  [51, 38, 13, 38, 50, 12, -1, -1, 0]).
	example_( 39, normal,  [59, 46, 13, 46, 58, 12, -1, -1, 0]).
	example_( 40, normal,  [50, 37, 13, 37, 49, 12, -1, -1, 0]).
	% Anomaly instances (non-Rad Flow classes: Fpv Close, Fpv Open, High, Bypass, Bpv Close, Bpv Open)
	example_( 41, anomaly, [80, 37, 43, 37, 43, 6,  -37, -37, 0]).
	example_( 42, anomaly, [82, 38, 44, 38, 44, 6,  -38, -38, 0]).
	example_( 43, anomaly, [78, 36, 42, 36, 42, 6,  -36, -36, 0]).
	example_( 44, anomaly, [85, 40, 45, 40, 45, 5,  -40, -40, 0]).
	example_( 45, anomaly, [37, 32, 5,  32, 40, 8,  5,   8,  3]).
	example_( 46, anomaly, [35, 30, 5,  30, 38, 8,  5,   8,  3]).
	example_( 47, anomaly, [39, 34, 5,  34, 42, 8,  5,   8,  3]).
	example_( 48, anomaly, [76, 70, 6,  70, 80, 10, 4,   10, 4]).
	example_( 49, anomaly, [74, 68, 6,  68, 78, 10, 4,   10, 4]).
	example_( 50, anomaly, [72, 66, 6,  66, 76, 10, 4,   10, 4]).

:- end_object.

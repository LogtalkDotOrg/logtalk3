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


:- object(mixed_distance_behaviors,
	implements(anomaly_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-19,
		comment is 'Compact mixed-feature anomaly fixture for exercising continuous-plus-categorical distance behavior and missing-value handling.'
	]).

	attribute_values(size, continuous).
	attribute_values(weight, continuous).
	attribute_values(color, [red, blue]).
	attribute_values(shape, [round, square]).

	class(status).

	class_values([normal, anomaly]).

	example(1, normal,  [size-10.0, weight-100.0, color-red,  shape-round]).
	example(2, normal,  [size-10.2, weight-100.3, color-red,  shape-round]).
	example(3, normal,  [size-9.8,  weight-99.7,  color-red,  shape-round]).
	example(4, normal,  [size-10.4, weight-100.6, color-red,  shape-round]).
	example(5, normal,  [size-9.6,  weight-99.4,  color-red,  shape-round]).
	example(6, normal,  [size-10.1, weight-100.1, color-red,  shape-round]).
	example(7, anomaly, [size-12.8, weight-104.8, color-blue, shape-round]).
	example(8, anomaly, [size-7.4,  weight-95.1,  color-red,  shape-square]).

:- end_object.

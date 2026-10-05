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


:- object(modified_z_score_empty_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

:- end_object.


:- object(modified_z_score_singleton_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-1.00]).

:- end_object.


:- object(modified_z_score_featureless_anomalies,
	implements(anomaly_dataset_protocol)).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, []).

:- end_object.


:- object(modified_z_score_high_dimensional_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).
	attribute_values(x4, continuous).
	attribute_values(x5, continuous).
	attribute_values(x6, continuous).
	attribute_values(x7, continuous).
	attribute_values(x8, continuous).
	attribute_values(x9, continuous).
	attribute_values(x10, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x1- -1.0, x2- -1.0, x3- -1.0, x4- -1.0, x5- -1.0, x6- -1.0, x7- -1.0, x8- -1.0, x9- -1.0, x10- -1.0]).
	example(2, normal, [x1-1.0, x2-1.0, x3-1.0, x4-1.0, x5-1.0, x6-1.0, x7-1.0, x8-1.0, x9-1.0, x10-1.0]).

:- end_object.


:- object(modified_z_score_contaminated_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-9.8]).
	example(2, normal, [x-10.0]).
	example(3, normal, [x-10.1]).
	example(4, normal, [x-10.2]).
	example(5, normal, [x-10.3]).
	example(6, normal, [x-10.4]).
	example(7, normal, [x-10.5]).
	example(8, normal, [x-10.6]).
	example(9, normal, [x-10.7]).
	example(10, normal, [x-10.9]).
	example(11, anomaly, [x-100.0]).

:- end_object.

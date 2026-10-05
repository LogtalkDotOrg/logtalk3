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


:- object(cusum_empty_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).

	class(label).

	class_values([normal, anomaly]).

:- end_object.


:- object(cusum_singleton_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [t1-1.00]).

:- end_object.


:- object(cusum_featureless_anomalies,
	implements(anomaly_dataset_protocol)).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, []).

:- end_object.


:- object(cusum_shift_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).
	attribute_values(t2, continuous).
	attribute_values(t3, continuous).
	attribute_values(t4, continuous).
	attribute_values(t5, continuous).
	attribute_values(t6, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [t1-0.10,  t2- -0.05, t3-0.03,  t4- -0.08, t5-0.05,  t6-0.00]).
	example(2, normal, [t1- -0.12, t2-0.07,  t3- -0.02, t4-0.09,  t5- -0.04, t6-0.02]).
	example(3, normal, [t1-0.05,  t2-0.02,  t3- -0.06, t4-0.04,  t5-0.01,  t6- -0.03]).
	example(4, normal, [t1- -0.08, t2-0.11,  t3-0.04,  t4- -0.02, t5-0.03,  t6-0.06]).
	example(5, normal, [t1-0.09,  t2- -0.07, t3-0.08,  t4- -0.01, t5- -0.02, t6-0.04]).
	example(6, normal, [t1- -0.04, t2-0.03,  t3- -0.09, t4-0.06,  t5-0.02,  t6- -0.05]).
	example(7, normal, [t1-0.07,  t2-0.01,  t3-0.05,  t4- -0.04, t5- -0.06, t6-0.03]).
	example(8, normal, [t1- -0.09, t2-0.04,  t3- -0.03, t4-0.08,  t5-0.00,  t6- -0.01]).

	example(9, anomaly, [t1-1.20,  t2-1.35,  t3-1.50,  t4-1.40,  t5-1.55,  t6-1.60]).
	example(10, anomaly, [t1-1.10, t2-1.25,  t3-1.45,  t4-1.50,  t5-1.60,  t6-1.70]).
	example(11, anomaly, [t1- -1.15, t2- -1.30, t3- -1.40, t4- -1.50, t5- -1.55, t6- -1.65]).
	example(12, anomaly, [t1- -1.05, t2- -1.20, t3- -1.35, t4- -1.45, t5- -1.60, t6- -1.70]).

:- end_object.


:- object(cusum_high_dimensional_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).
	attribute_values(t2, continuous).
	attribute_values(t3, continuous).
	attribute_values(t4, continuous).
	attribute_values(t5, continuous).
	attribute_values(t6, continuous).
	attribute_values(t7, continuous).
	attribute_values(t8, continuous).
	attribute_values(t9, continuous).
	attribute_values(t10, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [t1- -1.0, t2- -1.0, t3- -1.0, t4- -1.0, t5- -1.0, t6- -1.0, t7- -1.0, t8- -1.0, t9- -1.0, t10- -1.0]).
	example(2, normal, [t1-1.0, t2-1.0, t3-1.0, t4-1.0, t5-1.0, t6-1.0, t7-1.0, t8-1.0, t9-1.0, t10-1.0]).

:- end_object.


:- object(cusum_status_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).
	attribute_values(t2, continuous).
	attribute_values(t3, continuous).

	class(status).

	class_values([stable, drift]).

	example(1, stable, [t1-0.05,  t2- -0.03, t3-0.02]).
	example(2, stable, [t1- -0.04, t2-0.06,  t3- -0.01]).
	example(3, stable, [t1-0.03,  t2-0.01,  t3- -0.02]).

	example(4, drift, [t1-1.20, t2-1.25, t3-1.30]).
	example(5, drift, [t1- -1.15, t2- -1.20, t3- -1.25]).

:- end_object.

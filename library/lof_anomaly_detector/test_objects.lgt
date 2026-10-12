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


:- object(lof_identical_query_fixture,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-0.00]).
	example(2, normal, [x-10.00]).
	example(3, normal, [x-10.10]).
	example(4, normal, [x-10.20]).

:- end_object.


:- object(lof_empty_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

:- end_object.


:- object(lof_singleton_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-1.00]).

:- end_object.


:- object(lof_missing_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-0.0, y-_]).
	example(2, normal, [x-_, y-_]).
	example(3, normal, [x-1.0, y-_]).
	example(4, normal, [x-_, y-2.0]).

:- end_object.


:- object(lof_line_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-0.0]).
	example(2, normal, [x-1.0]).
	example(3, normal, [x-3.0]).

:- end_object.


:- object(lof_tied_anomalies(_Swapped_),
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(Id, normal, [x-0.0]) :-
		( _Swapped_ == true -> Id = 2 ; Id = 1 ).
	example(Id, normal, [x-2.0]) :-
		( _Swapped_ == true -> Id = 1 ; Id = 2 ).
	example(3, normal, [x-3.0]).

:- end_object.


:- object(lof_scaling_anomalies(_Count_),
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(Id, normal, [x-X, y-Y]) :-
		enumerate_id(1, _Count_, Id),
		X is sin(Id),
		Y is Id mod 7.

	enumerate_id(Current, Limit, Id) :-
		Current =< Limit,
		(	Id = Current
		;	Next is Current + 1,
			enumerate_id(Next, Limit, Id)
		).

:- end_object.


:- object(lof_duplicate_points,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-1.0]).
	example(2, normal, [x-1.0]).
	example(3, normal, [x-1.0]).

:- end_object.


:- object(lof_duplicate_ids,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-0.0]).
	example(1, normal, [x-1.0]).

:- end_object.


:- object(lof_invalid_numeric,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-oops]).

:- end_object.

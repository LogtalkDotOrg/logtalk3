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


:- object(mixed_anomalies,
	implements(anomaly_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-19,
		comment is 'Small mixed-feature anomaly dataset combining continuous, categorical, and missing values. Normal instances cluster around common customer profiles while anomalies use uncommon combinations or extreme numeric values.'
	]).

	attribute_values(age, continuous).
	attribute_values(income, continuous).
	attribute_values(student, [yes, no]).
	attribute_values(credit_rating, [fair, excellent]).

	class(status).

	class_values([normal, anomaly]).

	example( 1, normal,  [age-25, income-35000, student-yes, credit_rating-fair]).
	example( 2, normal,  [age-30, income-50000, student-no,  credit_rating-excellent]).
	example( 3, normal,  [age-45, income-75000, student-no,  credit_rating-fair]).
	example( 4, normal,  [age-22, income-28000, student-yes, credit_rating-fair]).
	example( 5, normal,  [age-35, income-60000, student-yes, credit_rating-excellent]).
	example( 6, normal,  [age-28, income-42000, student-no,  credit_rating-fair]).
	example( 7, normal,  [age-40, income-68000, student-yes, credit_rating-excellent]).
	example( 8, normal,  [age-33, income-54000, student-no,  credit_rating-fair]).
	example( 9, normal,  [age-27, income-39000, student-yes, credit_rating-fair]).
	example(10, normal,  [age-38, income-62000, student-no,  credit_rating-excellent]).
	example(11, normal,  [age-31, income-47000, student-_,    credit_rating-fair]).
	example(12, normal,  [age-36, income-59000, student-yes, credit_rating-_]).
	example(13, anomaly, [age-19, income-150000, student-no,  credit_rating-excellent]).
	example(14, anomaly, [age-62, income-12000,  student-yes, credit_rating-fair]).
	example(15, anomaly, [age-24, income-220000, student-_,    credit_rating-excellent]).
	example(16, anomaly, [age-58, income-18000,  student-no,  credit_rating-_]).

:- end_object.

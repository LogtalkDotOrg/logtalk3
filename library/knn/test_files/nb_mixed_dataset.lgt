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


:- object(nb_mixed_dataset,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-15,
		comment is 'Mixed dataset (categorical + continuous features) for Naive Bayes testing.'
	]).

	attribute_values(age, continuous).
	attribute_values(income, continuous).
	attribute_values(student, [yes, no]).
	attribute_values(credit_rating, [fair, excellent]).

	class(buys_computer).

	class_values([yes, no]).

	example(1, no,  [age-25, income-35000, student-yes, credit_rating-fair]).
	example(2, no,  [age-30, income-50000, student-no,  credit_rating-excellent]).
	example(3, yes, [age-45, income-75000, student-no,  credit_rating-fair]).
	example(4, no,  [age-22, income-28000, student-yes, credit_rating-fair]).
	example(5, yes, [age-35, income-60000, student-yes, credit_rating-excellent]).

:- end_object.

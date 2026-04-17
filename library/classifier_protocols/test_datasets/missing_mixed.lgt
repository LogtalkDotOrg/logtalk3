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


:- object(missing_mixed,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-12,
		comment is 'Mixed dataset with missing continuous and categorical features for classifier testing.'
	]).

	attribute_values(age, continuous).
	attribute_values(income, continuous).
	attribute_values(student, [yes, no]).
	attribute_values(credit_rating, [fair, excellent]).

	class(buys_computer).

	class_values([yes, no]).

	example(1, yes, [age-46, income-72000, student-no, credit_rating-fair]).
	example(2, yes, [age-43, income-68000, student-no, credit_rating-fair]).
	example(3, no,  [age-24, income-30000, student-yes, credit_rating-excellent]).
	example(4, no,  [age-27, income-34000, student-yes, credit_rating-excellent]).
	example(5, yes, [age-_,  income-70000, student-no, credit_rating-fair]).
	example(6, no,  [age-_,  income-29000, student-yes, credit_rating-excellent]).
	example(7, yes, [age-50, income-_,     student-no, credit_rating-fair]).
	example(8, no,  [age-29, income-_,     student-yes, credit_rating-excellent]).
	example(9, yes, [age-48, income-71000, student-_,  credit_rating-fair]).
	example(10, no, [age-26, income-31000, student-_,  credit_rating-excellent]).

:- end_object.
